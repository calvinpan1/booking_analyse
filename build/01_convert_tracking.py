"""Convert the extension's per-cell JSON exports into one event-level CSV.

Usage: python 01_convert_tracking.py RAW_DIR INPUTS_DIR
Reads every folder under RAW_DIR that holds cell JSONs (one folder per
participant, any depth); writes INPUTS_DIR/merged/<subject>_merged.json and
INPUTS_DIR/extension_converted.csv (one row per event, participant_code on
every row).
"""
import csv
import json
import os
import sys
from datetime import datetime
from pathlib import Path

raw_dir, inputs_dir = Path(sys.argv[1]), Path(sys.argv[2])
merged_dir = inputs_dir / "merged"
merged_dir.mkdir(parents=True, exist_ok=True)


def find_export_folders(root):
    found = []
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if d != "merged"]
        if Path(dirpath) != root and any(f.endswith(".json") for f in filenames):
            found.append(Path(dirpath))
    return sorted(found)


def merge_export_folder(folder):
    """One flat {participant, events} JSON per folder; every event is tagged with its cell."""
    events, participant, version, exported = [], None, None, None
    for jf in sorted(folder.glob("*.json")):
        with open(jf, encoding="utf-8") as f:
            data = json.load(f)
        participant = participant or data.get("participant")
        version = data.get("extensionVersion", version)
        exported = max(exported or "", data.get("exportedAt", "") or "")
        cell = data.get("cell", {})
        for e in data.get("events", []):
            events.append({**e, "_cellIndex": cell.get("cellIndex"), "_cityKey": cell.get("cityKey"),
                           "_checkin": cell.get("checkin"), "_listIndex": cell.get("listIndex"),
                           "_thumb": cell.get("thumb")})
    events.sort(key=lambda e: e["timestamp"])
    subject = (participant or {}).get("subjectId") or folder.name
    out = merged_dir / f"{subject}_merged.json"
    with open(out, "w", encoding="utf-8") as f:
        json.dump({"extensionVersion": version, "exportedAt": exported, "participant": participant,
                   "eventCount": len(events), "events": events}, f, ensure_ascii=False, indent=2)
    return subject, events


def dumps(event, key):
    return json.dumps(event[key], ensure_ascii=False) if key in event and event[key] is not None else None


JSON_FIELDS = ["visiblePropertyIds", "metadata", "values", "failures", "checks", "missingSlugs", "bookingOrder",
               "displayOrder", "priceGuard", "priceRejectedSlugs", "resultCountGuard", "noTargetGuard",
               "originalMissingSlugs", "effectiveWhitelist", "substitutions", "passDetails"]
PLAIN_FIELDS = ["referrer", "setupMs", "afterPreload", "scrollDepthPercent", "kind", "targetTestId",
                "targetPropertyId", "durationMs", "ratio", "isIntersecting", "state", "source", "tabId",
                "windowId", "trigger", "displayedPrice", "selectedRooms", "cellIndex", "page", "ok", "thumb",
                "visiblePouceCount", "totalPouceCount", "shownCardCount", "stopReason", "whitelistCount",
                "foundCount", "cardCount", "iterations", "clicks", "elapsedMs", "substitutionError", "passes",
                "finalDisplayedCount", "usedSubstitutesFallback", "groupsCovered"]


def event_to_row(e):
    row = {"type": e["type"], "timestamp": e["timestamp"], "url": e.get("url"),
           "isTargetSample": e.get("isTargetSample"),
           "cell_index": e.get("_cellIndex"), "cell_city_key": e.get("_cityKey"), "cell_checkin": e.get("_checkin"),
           "cell_list_index": e.get("_listIndex"), "cell_thumb": e.get("_thumb"),
           "targetText": " ".join((e.get("targetText") or "").split()) or None,
           "tab_event": e.get("event")}
    row.update({k: e.get(k) for k in PLAIN_FIELDS})
    row.update({k: dumps(e, k) for k in JSON_FIELDS})
    return row


COLUMNS = ["participant_code", "timestamp", "datetime_local", "elapsed", "type", "url", "isTargetSample",
           "cell_index", "cell_city_key", "cell_checkin", "cell_list_index", "cell_thumb",
           "targetPropertyId", "kind", "durationMs", "referrer", "setupMs", "afterPreload", "targetTestId",
           "targetText", "metadata", "scrollDepthPercent", "visiblePropertyIds", "ratio", "isIntersecting",
           "state", "source", "tab_event", "tabId", "windowId", "values", "trigger", "displayedPrice",
           "selectedRooms", "cellIndex", "page", "failures", "checks", "ok", "thumb", "visiblePouceCount",
           "totalPouceCount", "shownCardCount", "stopReason", "whitelistCount", "foundCount", "cardCount",
           "iterations", "clicks", "elapsedMs", "missingSlugs", "bookingOrder", "displayOrder", "priceGuard",
           "priceRejectedSlugs", "resultCountGuard", "noTargetGuard", "originalMissingSlugs",
           "effectiveWhitelist", "substitutions", "substitutionError", "passes", "passDetails",
           "finalDisplayedCount", "usedSubstitutesFallback", "groupsCovered"]

rows = []
n_sessions = 0
for folder in find_export_folders(raw_dir):
    subject, events = merge_export_folder(folder)
    events = sorted((e for e in events if e["type"] != "scraped_content"), key=lambda e: e["timestamp"])
    if not events:
        continue
    n_sessions += 1
    t0 = events[0]["timestamp"]
    for e in events:
        row = event_to_row(e)
        row["participant_code"] = subject
        dt = datetime.fromtimestamp(e["timestamp"] / 1000)
        row["datetime_local"] = dt.strftime("%Y-%m-%d %H:%M:%S.") + f"{dt.microsecond // 1000:03d}"
        m, rem = divmod(e["timestamp"] - t0, 60_000)
        s, ms = divmod(rem, 1000)
        row["elapsed"] = f"{int(m):02d}:{int(s):02d}.{int(ms):03d}"
        rows.append(row)

if not rows:
    sys.exit(f"No participant export folders found under {raw_dir}")

extra = sorted({k for r in rows for k in r if k not in COLUMNS})
with open(inputs_dir / "extension_converted.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=COLUMNS + extra, extrasaction="ignore")
    w.writeheader()
    w.writerows(rows)
print(f"{len(rows)} events, {n_sessions} participants -> extension_converted.csv")
