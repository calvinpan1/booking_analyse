#!/usr/bin/env python3
"""Consistency checks on an assembled submission folder (see build_submission.sh).

    python3 check_submission.py <out dir> [--values <validated values_BookingAnalysis.tex>]
                                [--figures <validated figures dir>] [--names "Surname1,Surname2"]

Exit status 1 if any check fails. Checks: manifest checksums; one top-level folder
and no junk inside every zip; anonymization grep over everything extracted; the
wrapper README names exactly the inner zips; chi_precheck on the clean main .tex;
page count of paper.pdf; values/figures in the paper source equal the validated
outputs; sizes; uniform timestamps inside the zips.
"""
import argparse, hashlib, os, re, subprocess, sys, zipfile

JUNK = re.compile(r'(^|/)(\.DS_Store|__pycache__|\.git|node_modules|\.venv[^/]*|\.Rhistory|\.RData|\.idea|\.vscode)(/|$)|\.pyc$|\.pyo$')
DEFAULT_NAMES = 'mayaux,belletti,chaves ferreira,calvin sean,damien'
INSTITUTIONS = r'dauphine|paris school|sorbonne|virginia\.edu|\bedhec\b|\bcnrs\b|\bpsl\b|\bleep\b|laboratoire'
EMAIL = re.compile(r'(?<![\w/@])[\w.+-]+@(?!example\.)[\w-]+\.[\w.-]*[a-z]{2,}', re.I)
FAILS = []


def fail(msg):
    FAILS.append(msg); print('FAIL', msg)


def ok(msg):
    print('ok  ', msg)


def sha256(p):
    h = hashlib.sha256()
    with open(p, 'rb') as f:
        for chunk in iter(lambda: f.read(1 << 20), b''):
            h.update(chunk)
    return h.hexdigest()


def check_zip(path):
    z = zipfile.ZipFile(path)
    names = z.namelist()
    tops = {n.split('/')[0] for n in names}
    if len(tops) != 1 or not any(n.count('/') >= 1 for n in names):
        fail(f'{os.path.basename(path)}: expected exactly one top-level folder, got {sorted(tops)}')
    junk = [n for n in names if JUNK.search(n)]
    if junk:
        fail(f'{os.path.basename(path)}: junk entries {junk[:10]}')
    stamps = {i.date_time for i in z.infolist()}
    if len(stamps) > 1:
        fail(f'{os.path.basename(path)}: {len(stamps)} distinct timestamps inside (expected 1)')
    return z, names


def scan_text(root, names_re):
    hits = []
    for dp, dn, fn in os.walk(root):
        for f in fn:
            p = os.path.join(dp, f)
            if re.search(r'\.(png|jpg|jpeg|pdf|parquet|zip|gz|ico|woff2?|ttf|cls|bst)$', f, re.I):
                continue
            try:
                t = open(p, encoding='utf-8', errors='ignore').read()
            except OSError:
                continue
            for i, ln in enumerate(t.split('\n'), 1):
                low = ln.lower()
                if f == 'package-lock.json' and re.search(r'github\.com/(sponsors/|[\w-]+/[\w.-]+\?sponsor=1)', low):
                    continue   # npm registry metadata of third-party packages
                if names_re.search(low) or re.search(INSTITUTIONS, low) or 'github.com' in low \
                        or '/users/' in low or 'c:\\users' in low or EMAIL.search(ln):
                    hits.append(f'{os.path.relpath(p, root)}:{i}: {ln.strip()[:120]}')
    return hits


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('out')
    ap.add_argument('--values'); ap.add_argument('--figures')
    ap.add_argument('--names', default=DEFAULT_NAMES)
    a = ap.parse_args()
    out = os.path.abspath(a.out)
    names_re = re.compile('|'.join(re.escape(n.strip().lower()) for n in a.names.split(',') if n.strip()))

    # 1. manifest checksums
    man = os.path.join(out, 'MANIFEST.txt')
    n = 0
    for ln in open(man):
        m = re.match(r'([0-9a-f]{64})\s+(\d+)\s+(\S+)$', ln.strip())
        if not m:
            continue
        p = os.path.join(out, 'build', m.group(3)) if m.group(3).startswith('supplementary_materials/') else os.path.join(out, m.group(3))
        n += 1
        if not os.path.exists(p):
            fail(f'manifest: {m.group(3)} missing'); continue
        if sha256(p) != m.group(1) or os.path.getsize(p) != int(m.group(2)):
            fail(f'manifest: checksum/size mismatch for {m.group(3)}')
    ok(f'manifest: {n} files listed')
    if re.search(r'<sha>', open(man).read()):
        fail('manifest: unfilled <sha> placeholders (extension/oTree/scraping branch commits)')

    # 2. zips
    wrapper = os.path.join(out, 'supplementary_materials.zip')
    z, names = check_zip(wrapper)
    inner = [n for n in names if n.endswith('.zip')]
    if 'supplementary_materials/README.md' not in names:
        fail('wrapper zip has no README.md')
    if len([n for n in names if not n.endswith('/')]) != len(inner) + 1:
        fail(f'wrapper zip should hold README + zips only, holds {names}')
    ok(f'wrapper zip: README + {len(inner)} inner zips, one top-level folder')
    stage = os.path.join(out, 'build', 'check'); subprocess.run(['rm', '-rf', stage]); os.makedirs(stage)
    z.extractall(stage)
    for iz in inner:
        zi, ni = check_zip(os.path.join(stage, iz))
        top = ni[0].split('/')[0]
        if f'{top}/README.md' not in ni:
            fail(f'{iz}: no README.md at the top of the package')
        zi.extractall(os.path.join(stage, 'inner'))
    ok('inner zips: one top-level folder, README present, no junk')
    zp, np_ = check_zip(os.path.join(out, 'paper_source.zip')); zp.extractall(os.path.join(stage, 'src'))

    # 3. wrapper README names exactly the inner zips
    readme = open(os.path.join(stage, 'supplementary_materials', 'README.md')).read()
    listed = set(re.findall(r'`([\w-]+\.zip)`', readme)); present = {os.path.basename(i) for i in inner}
    if listed != present:
        fail(f'wrapper README lists {sorted(listed)} but zip holds {sorted(present)}')
    else:
        ok('wrapper README names exactly the inner zips')

    # 4. anonymization grep over everything extracted
    hits = scan_text(stage, names_re)
    if hits:
        fail(f'anonymization: {len(hits)} hits'); print('\n'.join('      ' + h for h in hits[:40]))
    else:
        ok('anonymization grep clean (names, institutions, github, /Users, e-mails)')

    # 5. paper checks
    tex = os.path.join(stage, 'src', 'paper_source', 'papers', 'pilot_experiment_chi2027_article.tex')
    pre = os.path.expanduser('~/.claude/skills/write-for-chi/scripts/chi_precheck.py')
    if os.path.exists(pre):
        r = subprocess.run(['python3', pre, tex], capture_output=True, text=True)
        print('---- chi_precheck.py ----'); print(r.stdout[-3000:]); print('-------------------------')
    pdf = os.path.join(out, 'paper.pdf')
    try:
        import pypdf; ok(f'paper.pdf: {len(pypdf.PdfReader(pdf).pages)} pages')
    except Exception as e:
        fail(f'paper.pdf unreadable: {e}')
    src_values = os.path.join(stage, 'src', 'paper_source', 'values', 'values_BookingAnalysis.tex')
    if a.values:
        if open(src_values, 'rb').read() == open(a.values, 'rb').read():
            ok('values file in the paper source equals the validated outputs')
        else:
            fail('values file in the paper source differs from the validated outputs')
    if a.figures:
        bad = []
        for dp, dn, fn in os.walk(os.path.join(stage, 'src', 'paper_source', 'illustrations')):
            for f in fn:
                cand = os.path.join(a.figures, f)
                if os.path.exists(cand) and open(cand, 'rb').read() != open(os.path.join(dp, f), 'rb').read():
                    bad.append(f)
        (fail(f'figures differ from validated outputs: {bad}') if bad else ok('figures equal the validated outputs'))

    # 6. sizes
    for f in ('supplementary_materials.zip', 'paper_source.zip', 'paper.pdf'):
        print(f'      {f}: {os.path.getsize(os.path.join(out, f)) / 1e6:.1f} MB')

    print('\nALL CHECKS PASSED' if not FAILS else f'\n{len(FAILS)} CHECK(S) FAILED')
    sys.exit(1 if FAILS else 0)


if __name__ == '__main__':
    main()
