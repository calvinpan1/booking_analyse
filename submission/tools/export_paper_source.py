#!/usr/bin/env python3
"""Export a clean, self-contained LaTeX source tree from a pinned commit of the
Overleaf clone.

    python3 export_paper_source.py --clone <overleaf clone> --commit <ref> --out <dir>
                                   [--main papers/x.tex] [--no-compile] [--no-compare]

Steps: `git archive <ref>` into a temp dir; strip comments from the main .tex
(comment-only lines, trailing comments, comment environments, unused note
macros); copy only the files the main .tex uses (inputs, images, class, bst);
compile once to learn the cited keys and write a single bibliography/references.bib
restricted to them; compile the clean tree; compile the untouched archive too and
diff the two PDFs' text (must be identical). Never edit the exported tree by hand:
re-run this script on a newer commit instead.
"""
import argparse, os, re, shutil, subprocess, sys, tempfile

IMG_EXT = ['.png', '.pdf', '.jpg', '.jpeg']
NOTE_MACROS = ['note', 'oldtext', 'newtext', 'remark']
ANON_BLOCK = """\\author{Anonymous Author(s)}
\\affiliation{%
  \\institution{Anonymous Institution}
  \\country{}
}
"""


def anonymize(text):
    """Replace the author/affiliation block (first \\author up to \\begin{abstract}) by a
    placeholder, so the source carries no name, e-mail or ORCID. Under the class's
    `anonymous` option the PDF is unchanged."""
    lines = text.split('\n')
    first = next((i for i, l in enumerate(lines) if l.startswith('\\author')), None)
    last = next((i for i, l in enumerate(lines) if l.strip().startswith('\\begin{abstract}')), None)
    if first is None or last is None or last <= first:
        sys.exit('anonymize: could not locate the author block')
    for i in range(first, last):
        if re.search(r'\\(acks|thanks)\\b', lines[i]):
            sys.exit('anonymize: unexpected \\acks/\\thanks in the author block')
    return '\n'.join(lines[:first] + ANON_BLOCK.split('\n') + lines[last:])


def run(cmd, cwd=None, check=True, **kw):
    r = subprocess.run(cmd, cwd=cwd, text=True, capture_output=True, **kw)
    if check and r.returncode:
        sys.exit(f"command failed: {' '.join(cmd)}\n{r.stdout[-3000:]}\n{r.stderr[-3000:]}")
    return r


def git_archive(clone, ref, dest):
    os.makedirs(dest, exist_ok=True)
    p1 = subprocess.Popen(['git', '-C', clone, 'archive', ref], stdout=subprocess.PIPE)
    run(['tar', '-x', '-C', dest], stdin=p1.stdout)
    p1.wait()
    if p1.returncode:
        sys.exit('git archive failed')


# ---------------------------------------------------------------- stripping
def strip_trailing_comment(line):
    """Cut the text of a trailing comment but keep the % (it still eats the newline)."""
    out, i, n = [], 0, len(line)
    while i < n:
        c = line[i]
        if c == '\\' and i + 1 < n:
            out.append(line[i:i + 2]); i += 2; continue
        if c == '%':
            return ''.join(out) + '%'
        out.append(c); i += 1
    return line


def strip_tex(text):
    lines = text.split('\n')
    out, in_comment_env = [], False
    for ln in lines:
        s = ln.strip()
        if s == r'\begin{comment}':
            in_comment_env = True; continue
        if s == r'\end{comment}':
            in_comment_env = False; continue
        if in_comment_env:
            continue
        if s.startswith('%'):
            continue                         # comment-only line
        ln = strip_trailing_comment(ln)
        if ln.strip() == '%':
            continue
        out.append(ln.rstrip())
    # collapse runs of blank lines to one
    res, blank = [], False
    for ln in out:
        if ln == '':
            if blank:
                continue
            blank = True
        else:
            blank = False
        res.append(ln)
    text = '\n'.join(res).strip('\n') + '\n'
    # drop definitions of note macros that are no longer used
    for m in NOTE_MACROS:
        uses = len(re.findall(r'\\' + m + r'\b', text))
        defs = re.findall(r'^\\newcommand\{\\' + m + r'\}.*\n', text, flags=re.M)
        if defs and uses == len(defs):
            text = re.sub(r'^\\newcommand\{\\' + m + r'\}.*\n', '', text, flags=re.M)
    return text


# ------------------------------------------------------------- dependencies
def resolve(root, name, exts):
    for ext in [''] + exts:
        p = os.path.join(root, name + ext)
        if os.path.isfile(p):
            return name + ext
    return None


def deps_of(root, relpath, seen, missing):
    """Recursively collect the files a .tex file uses (paths relative to root)."""
    if relpath in seen:
        return
    seen.add(relpath)
    text = open(os.path.join(root, relpath), encoding='utf-8').read()
    # remove comments before scanning so commented-out includes are ignored
    text = '\n'.join(strip_trailing_comment(l) for l in text.split('\n') if not l.lstrip().startswith('%'))
    for m in re.finditer(r'\\(?:input|include)\{([^}]+)\}', text):
        r = resolve(root, m.group(1).strip(), ['.tex'])
        (deps_of(root, r, seen, missing) if r else missing.append(m.group(1)))
    for m in re.finditer(r'\\includegraphics(?:\[[^\]]*\])?\{([^}]+)\}', text):
        r = resolve(root, m.group(1).strip(), IMG_EXT)
        (seen.add(r) if r else missing.append(m.group(1)))
    for m in re.finditer(r'\\documentclass(?:\[[^\]]*\])?\{([^}]+)\}', text):
        r = resolve(root, m.group(1).strip(), ['.cls'])
        if r:
            seen.add(r)
    for m in re.finditer(r'\\bibliographystyle\{([^}]+)\}', text):
        r = resolve(root, m.group(1).strip(), ['.bst'])
        if r:
            seen.add(r)


def cited_keys(aux_path):
    keys = set()
    for ln in open(aux_path, encoding='utf-8', errors='replace'):
        m = re.match(r'\\citation\{(.*)\}', ln.strip())
        if m:
            keys.update(k.strip() for k in m.group(1).split(','))
    return keys


def drop_fields(entry, fields):
    """Remove `field = {...},` lines (possibly spanning several lines) from a BibTeX entry."""
    out, skip, depth = [], False, 0
    for ln in entry.split('\n'):
        if not skip:
            m = re.match(r'\s*(\w+)\s*=\s*[{"]', ln)
            if m and m.group(1).lower() in fields:
                skip, depth = True, 0
        if skip:
            depth += ln.count('{') - ln.count('}')
            if depth <= 0 and (ln.rstrip().endswith(',') or ln.rstrip().endswith('}') or ln.rstrip().endswith('"')):
                skip = False
            continue
        out.append(ln)
    return '\n'.join(out)


def filter_bib(bib_paths, keys):
    """Return the text of the entries whose key is in `keys`, plus @string/@preamble."""
    kept, seen_keys = [], set()
    for p in bib_paths:
        text = open(p, encoding='utf-8').read()
        # split into entries at lines starting with '@'
        parts = re.split(r'\n(?=@)', '\n' + text)
        for part in parts:
            part = part.strip('\n')
            if not part.startswith('@'):
                continue
            head = re.match(r'@(\w+)\s*[{(]\s*([^,\s]*)', part)
            if not head:
                continue
            kind, key = head.group(1).lower(), head.group(2)
            if kind in ('string', 'preamble') or (key in keys and key not in seen_keys):
                part = drop_fields(part, ('abstract', 'file', 'keywords', 'annote'))
                kept.append(part.rstrip() + '\n')
                seen_keys.add(key)
    return '\n'.join(kept), keys - seen_keys


# ------------------------------------------------------------------ compile
def compile_tex(root, main, tag):
    """Run latexmk on root/main; return (pdf path, log text, warnings)."""
    r = run(['latexmk', '-pdf', '-interaction=nonstopmode', '-halt-on-error', main], cwd=root, check=False)
    base = os.path.splitext(os.path.basename(main))[0]
    log_path = os.path.join(root, base + '.log')
    log = open(log_path, encoding='utf-8', errors='replace').read() if os.path.exists(log_path) else ''
    if r.returncode:
        sys.exit(f'[{tag}] latexmk failed:\n{r.stdout[-4000:]}')
    warn = [l for l in log.split('\n') if re.search(
        r"Citation .* undefined|Reference .* undefined|File .* not found|Package .* Error|No file .*\.bbl", l)]
    return os.path.join(root, base + '.pdf'), log, warn


def pdf_text(pdf):
    return run(['pdftotext', '-layout', pdf, '-']).stdout


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('--clone', required=True)
    ap.add_argument('--commit', default='HEAD')
    ap.add_argument('--out', required=True)
    ap.add_argument('--main', default='papers/pilot_experiment_chi2027_article.tex')
    ap.add_argument('--no-compile', action='store_true')
    ap.add_argument('--no-compare', action='store_true')
    ap.add_argument('--no-anonymize', action='store_true', help='keep the real author block')
    a = ap.parse_args()

    sha = run(['git', '-C', a.clone, 'rev-parse', a.commit]).stdout.strip()
    tmp = tempfile.mkdtemp(prefix='paper_export_')
    src = os.path.join(tmp, 'archive')
    git_archive(a.clone, sha, src)

    # 1. dependency closure on the archived (untouched) tree
    seen, missing = set(), []
    deps_of(src, a.main, seen, missing)
    if missing:
        sys.exit(f'unresolved inputs/graphics: {missing}')

    # 2. clean tree
    out = os.path.abspath(a.out)
    if os.path.exists(out):
        shutil.rmtree(out)
    for rel in sorted(seen):
        dst = os.path.join(out, rel)
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        if rel.endswith('.tex'):
            t = strip_tex(open(os.path.join(src, rel), encoding='utf-8').read())
            if rel == a.main and not a.no_anonymize:
                t = anonymize(t)
            open(dst, 'w', encoding='utf-8').write(t)
        else:
            shutil.copy2(os.path.join(src, rel), dst)

    # 3. bibliography restricted to the cited keys (learned from a first compile of the archive)
    main_text = open(os.path.join(out, a.main), encoding='utf-8').read()
    bib_m = re.search(r'\\bibliography\{([^}]+)\}', main_text)
    if bib_m:
        if a.no_compile:
            for b in bib_m.group(1).split(','):
                r = resolve(src, b.strip(), ['.bib']); shutil.copy2(os.path.join(src, r), os.path.join(out, r))
        else:
            pdf_ref, _, _ = compile_tex(src, a.main, 'archive')
            aux = os.path.join(src, os.path.splitext(os.path.basename(a.main))[0] + '.aux')
            keys = cited_keys(aux)
            bibs = [os.path.join(src, resolve(src, b.strip(), ['.bib'])) for b in bib_m.group(1).split(',')]
            bibtext, unresolved = filter_bib(bibs, keys)
            if unresolved:
                sys.exit(f'cited keys not found in any .bib: {sorted(unresolved)}')
            os.makedirs(os.path.join(out, 'bibliography'), exist_ok=True)
            open(os.path.join(out, 'bibliography', 'references.bib'), 'w', encoding='utf-8').write(bibtext)
            main_text = main_text.replace(bib_m.group(0), r'\bibliography{bibliography/references}')
            open(os.path.join(out, a.main), 'w', encoding='utf-8').write(main_text)
            print(f'bibliography: {len(keys)} cited keys kept')

    open(os.path.join(out, 'EXPORTED_FROM.txt'), 'w').write(
        f'Overleaf project {os.path.basename(os.path.abspath(a.clone))}\ncommit {sha}\nmain {a.main}\n'
        'Generated by export_paper_source.py; do not edit by hand.\n')

    if a.no_compile:
        print(f'exported {len(seen)} files to {out} (not compiled)'); return

    # 4. compile the clean tree and compare with the archive
    pdf_clean, log, warn = compile_tex(out, a.main, 'clean')
    if warn:
        print('WARNINGS in clean build:'); print('\n'.join(warn)); sys.exit(1)
    n_pages = run(['python3', '-c', f"import pypdf;print(len(pypdf.PdfReader('{pdf_clean}').pages))"]).stdout.strip()
    print(f'clean build OK: {n_pages} pages, no missing citation/reference/file warning')
    if not a.no_compare:
        t_ref, t_clean = pdf_text(pdf_ref), pdf_text(pdf_clean)
        if t_ref == t_clean:
            print('pdftotext: clean PDF identical to archive PDF')
        else:
            import difflib
            d = list(difflib.unified_diff(t_ref.split('\n'), t_clean.split('\n'), 'archive', 'clean', lineterm='', n=0))
            print(f'pdftotext DIFFERS ({len(d)} diff lines):'); print('\n'.join(d[:80])); sys.exit(1)
    # remove build products from the export
    run(['latexmk', '-C', a.main], cwd=out, check=False)
    for f in os.listdir(out):
        if re.search(r'\.(aux|bbl|blg|fdb_latexmk|fls|log|out|synctex\.gz)$', f):
            os.remove(os.path.join(out, f))
    print(f'exported clean source to {out}')
    shutil.rmtree(tmp)


if __name__ == '__main__':
    main()
