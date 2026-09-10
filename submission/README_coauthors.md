# CHI 2027 submission folder

What is here and how it was produced. Nothing in this folder is edited by hand:
everything is regenerated from pinned commits by the tools on the
`chi2027-submission` branch of the `booking_analyse` repository (`submission/tools/`).

- `supplementary_materials.zip`: the single file uploaded to PCS as supplementary
  material. Wrapper README plus four zips (extension, oTree, scraping, analysis).
- `paper.pdf`: the submitted PDF, compiled from the clean source below.
- `paper_source.zip`: the LaTeX source, exported from the Overleaf project at the
  commit pinned in `MANIFEST.txt`: only the files the paper uses, comments and
  notes stripped, author block anonymized, bibliography restricted to the cited
  entries. The PDF text is byte-for-byte the same as the one compiled from the
  full Overleaf project (checked by the export script). Keep editing on
  Overleaf; re-run the export to refresh this file.
- `MANIFEST.txt`: sha256 and size of every file, and the commit of every source
  repository (Overleaf history label "CHI 2027 submitted", and the
  `supplementary-materials-chi2027` branch of each code repository).
- `build/`: staging area of the last build and check; not part of the submission.

The raw exports of the experiment contain personal data and are neither here
nor in any repository.
