# Shared path resolution for the 00a/00b/00c cleaning scripts.
#
# Convention (mirrors the synthetic-data skill's SSD_DATA_ROOT convention,
# generalised to one shared env var across all of the user's projects):
#   SSD_DATA_ROOT/<PROJECT_NAME>/raw/     -- raw, participant-identifiable
#                                             exports (oTree wide CSVs,
#                                             extension JSON/HTML). Read-only
#                                             from this pipeline's point of
#                                             view.
#   SSD_DATA_ROOT/<PROJECT_NAME>/inputs/  -- target directory: everything
#                                             this pipeline WRITES (still
#                                             real, participant-level data
#                                             at the 00a/00b/00c stage) goes
#                                             here instead of the Nextcloud-
#                                             synced project, so no raw or
#                                             intermediate personal data ever
#                                             touches the synced tree.
#
# When SSD_DATA_ROOT is unset (e.g. developing/testing away from the SSD
# machine), both resolve to the old local analyse/inputs and analyse/outputs
# folders — unchanged behaviour.

import os
from pathlib import Path

PROJECT_NAME = "Booking_in_the_dark"


def resolve_dirs(base_dir: Path) -> tuple[Path, Path]:
    """Return (raw_dir, target_dir) for reading raw exports / writing
    pipeline output, given a script's own analyse/ BASE_DIR as the local
    fallback root.

    Always prints which branch it took and why — silently falling back to
    the local (Nextcloud-synced) folders is exactly the failure mode that
    writes real participant data into the synced project, so this must never
    be quiet about it.
    """
    # Windows env vars set via `setx`/System Properties can carry stray
    # quotes or trailing whitespace depending on how they were entered, and
    # won't be picked up by a terminal/IDE process that was already running
    # when they were set (that needs a fresh terminal or IDE restart) —
    # strip defensively, but a genuinely unset var still falls back below.
    ssd_root = os.environ.get("SSD_DATA_ROOT", "").strip().strip('"').strip("'")
    if ssd_root:
        project_root = Path(ssd_root) / PROJECT_NAME
        raw_dir, target_dir = project_root / "raw", project_root / "inputs"
        print(f"[paths] SSD_DATA_ROOT={ssd_root!r} -> raw={raw_dir}  target={target_dir}")
    else:
        raw_dir, target_dir = base_dir / "inputs", base_dir / "outputs"
        print(
            "[paths] WARNING: SSD_DATA_ROOT is not set in this process's environment "
            f"-> falling back to LOCAL folders (raw={raw_dir}  target={target_dir}). "
            "If you meant to read/write on the SSD, this run just used the "
            "Nextcloud-synced folders instead — set SSD_DATA_ROOT and open a "
            "NEW terminal/IDE window (env vars set via setx/System Properties "
            "do not apply to already-running processes) before re-running."
        )
    return raw_dir, target_dir
