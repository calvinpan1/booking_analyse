# Supplementary materials

Supplementary materials for the submission *Auditing E-Commerce Interfaces With
Browser-Based Field Experiments: Effects of Visual Elements on Consumer Behavior*.
Everything here is anonymized for review; public repositories will be linked in
the camera-ready version. The paper is self-contained; these materials document
the implementation and let the analyses be reproduced.

The archive contains four folders, each shipped as its own zip with its own
README:

1. `booking-extension-supplementary-materials.zip` (folder `plug-in/`): the
   source code of the web browser extension that rewrites the Booking.com pages
   shown to participants (cue visibility, clutter level, choice-set control) and
   logs their behavior. TypeScript sources, the compiled extension, a reviewer
   demo that runs without the experiment server, configuration files and
   preflight tools.
2. `otree-application-supplementary-materials.zip` (folder `otree/`): the oTree
   application run in the laboratory, including the exact instructions,
   comprehension checks, questionnaire wording and the oral instructions script
   read to participants at the start of each session.
3. `scraping-choice-set-supplementary-materials.zip` (folder `scraping/`): the
   scraping and choice-set construction pipeline, from the collection of
   Booking.com search results to the clustered choice sets used by the
   extension, with the collected property data (no personal data).
4. `analysis-replication-package.zip` (folder `analysis/`): the replication
   package for the analyses. One entry point (`main.R`) rebuilds the analysis
   dataset from the raw exports and produces every number and figure of the
   paper.

## Data

No participant-level data is included. The raw exports of the experiment
(extension logs, oTree exports) contain personal data and are not distributed;
they will be made available under a data agreement. The analysis package ships
a generator of synthetic raw exports in the same formats so the full pipeline
can be run end to end without them. The scraping package includes the collected
Booking.com property data, which contains no personal data.

## Known discrepancies

- `otree/oral_instructions.md` states a participation fee of EUR 10; the fee
  actually paid, and shown on screen by the application, is EUR 12 (see the
  oTree README).
- The oTree application draws a random city order per participant but does not
  use it: cities were shown in the fixed order of `otree/_static/cities.json`
  (see the oTree README and the Methods section).
