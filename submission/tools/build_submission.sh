#!/usr/bin/env bash
# Assemble the CHI 2027 submission folder:
#   build_submission.sh <overleaf clone> <overleaf commit> <analysis commit> <inner zips dir> <out dir>
# <inner zips dir> must hold the three curated zips (extension, oTree, scraping).
# Produces in <out dir>: paper_source.zip, paper.pdf, supplementary_materials.zip,
# README.md (wrapper README), MANIFEST.txt, and build/ (staging, not for sharing).
set -euo pipefail
CLONE=$1; PAPER_SHA=$2; ANALYSIS_SHA=$3; ZIPS=$4; OUT=$5
HERE=$(cd "$(dirname "$0")" && pwd); SUB=$(dirname "$HERE"); REPO=$(dirname "$SUB")
STAMP="202609100000"   # fixed mtime on every packed file: no working-session timestamps
mkdir -p "$OUT"; rm -rf "$OUT/build"; mkdir -p "$OUT/build/supplementary_materials"

# 1. paper source (clean export from the pinned Overleaf commit) and PDF
python3 "$HERE/export_paper_source.py" --clone "$CLONE" --commit "$PAPER_SHA" --out "$OUT/build/paper_source"
( cd "$OUT/build/paper_source" && latexmk -pdf -interaction=nonstopmode -quiet papers/pilot_experiment_chi2027_article.tex >/dev/null \
  && cp pilot_experiment_chi2027_article.pdf "$OUT/paper.pdf" && latexmk -C papers/pilot_experiment_chi2027_article.tex >/dev/null \
  && rm -f pilot_experiment_chi2027_article.* )
rm -rf "$SUB/paper_source"; cp -R "$OUT/build/paper_source" "$SUB/paper_source"   # tracked copy, diffable later
( cd "$OUT/build" && find paper_source -exec touch -t "$STAMP" {} + && rm -f "$OUT/paper_source.zip" && zip -qrX "$OUT/paper_source.zip" paper_source )

# 2. analysis package from git archive of the pinned commit
git -C "$REPO" archive --format=zip --prefix=analysis/ -o "$OUT/build/supplementary_materials/analysis-replication-package.zip" "$ANALYSIS_SHA" \
  main.R README.md .gitattributes build analysis tools raw/choice_sets_with_substitutes.json

# 3. wrapper
for z in booking-extension-supplementary-materials.zip otree-application-supplementary-materials.zip scraping-choice-set-supplementary-materials.zip; do
  cp "$ZIPS/$z" "$OUT/build/supplementary_materials/"; done
cp "$SUB/README_supplementary.md" "$OUT/build/supplementary_materials/README.md"
( cd "$OUT/build" && find supplementary_materials -exec touch -t "$STAMP" {} + && rm -f "$OUT/supplementary_materials.zip" && zip -qrX "$OUT/supplementary_materials.zip" supplementary_materials )

# 4. manifest
{
  sed -e "s|^submission_date: .*|submission_date: $(date +%F)|" \
      -e "s|^overleaf_commit: <sha>|overleaf_commit: $(git -C "$CLONE" rev-parse "$PAPER_SHA")|" \
      -e "s|^booking_analyse_commit: <sha>|booking_analyse_commit: $(git -C "$REPO" rev-parse "$ANALYSIS_SHA")|" \
      -e "s|^booking_otree_experiment_commit: <sha>|booking_otree_experiment_commit: $(git ls-remote https://github.com/DamienMAYAUX/booking_otree_experiment.git refs/heads/supplementary-materials-chi2027 | cut -f1 | grep . || echo '<sha>')|" \
      -e "s|^booking_plugin_commit: <sha>|booking_plugin_commit: $(git ls-remote https://github.com/chaves/booking_plugin.git refs/heads/supplementary-materials-chi2027 | cut -f1 | grep . || echo '<sha>')|" \
      -e "s|^bookiing_scraping_commit: <sha>|bookiing_scraping_commit: $(git ls-remote https://github.com/chaves/bookiing_scraping refs/heads/supplementary-materials-chi2027 | cut -f1 | grep . || echo '<sha>')|" \
      "$SUB/MANIFEST.template.txt"
  ( cd "$OUT/build/supplementary_materials" && for f in *.zip; do printf '%s  %s  supplementary_materials/%s\n' "$(shasum -a 256 "$f" | cut -d' ' -f1)" "$(stat -f %z "$f")" "$f"; done )
  ( cd "$OUT" && for f in supplementary_materials.zip paper_source.zip paper.pdf; do printf '%s  %s  %s\n' "$(shasum -a 256 "$f" | cut -d' ' -f1)" "$(stat -f %z "$f")" "$f"; done )
} > "$OUT/MANIFEST.txt"
cp "$OUT/MANIFEST.txt" "$SUB/MANIFEST.txt"; cp "$SUB/README_coauthors.md" "$OUT/README.md"
echo "built $OUT; now run: python3 $HERE/check_submission.py $OUT"
