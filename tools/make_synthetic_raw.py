"""Fabricate a raw/ tree in the extension and oTree export formats, for smoke tests only.

Usage: python make_synthetic_raw.py OUT_DIR CHOICE_SETS_JSON [N_SUBJECTS]
Every value is random; nothing comes from real participants.
"""
import csv
import json
import random
import sys
from pathlib import Path

out, cs_path = Path(sys.argv[1]), Path(sys.argv[2])
n_subjects = int(sys.argv[3]) if len(sys.argv) > 3 else 60
rng = random.Random(1)
with open(cs_path, encoding="utf-8") as f:
    cs = json.load(f)
cities = {v["choice_sets"]["choice_set_1"][0]["city_slug"]: (k, v) for k, v in cs.items()}
checkins = ["2026-10-02", "2026-10-09", "2026-10-16", "2026-10-23", "2026-10-30", "2026-11-06", "2026-11-13", "2026-11-20", "2026-11-27"]
otree_rows, pagetimes_rows = [], []


def slug_url(slug, dest_id):
    return f"https://www.booking.com/hotel/fr/{slug}.fr.html?dest_id={dest_id}&checkin=x"


for s in range(n_subjects):
    code = f"s{s:03d}xyz"
    folder = out / "session_1" / code
    folder.mkdir(parents=True, exist_ok=True)
    my_cities = rng.sample(list(cities), 3)
    my_weekends = rng.sample(checkins, 4)
    clutter = rng.choice(["O", "N"])
    t = 1_780_000_000_000 + s * 7_200_000
    t_start = t / 1000
    pagetimes_rows += [(code, 1, "instructions_block", "Instructions", t_start + 60, 1),
                       (code, 2, "pretask_block", "WeekendAndCitySelection", t_start + 180, 1)]
    otree = {"participant.code": code, "participant.clutter_treatment": clutter, "participant.label": ""}
    taste = rng.choice([1, 2, 3])
    cell = 0
    for ci, city_slug in enumerate(my_cities):
        city_name, blob = cities[city_slug]
        thumbs = rng.sample(["A", "A", "P", "P"], 4)
        for wi, checkin in enumerate(my_weekends):
            cell += 1
            listing_set = blob["choice_sets"][f"choice_set_{wi + 1}"]
            slugs = [p["property_slug"] for p in listing_set]
            dest_id = listing_set[0]["dest_id"]
            results_url = f"https://www.booking.com/searchresults.fr.html?ss={city_name}&checkin={checkin}&dest_id={dest_id}"
            events = []
            t += 5000
            events.append({"type": "page_view", "timestamp": t, "url": results_url, "isTargetSample": True, "referrer": ""})
            loading = rng.randint(30_000, 200_000)
            t += loading
            events.append({"type": "preload", "timestamp": t, "url": results_url, "isTargetSample": True, "ok": True,
                           "stopReason": "complete", "elapsedMs": loading, "effectiveWhitelist": slugs, "substitutions": [],
                           "passes": 1, "finalDisplayedCount": 9})
            events.append({"type": "manip_check", "timestamp": t + 10, "url": results_url, "isTargetSample": True,
                           "thumb": thumbs[wi], "visiblePouceCount": 3 if thumbs[wi] == "P" else 0, "totalPouceCount": 3, "shownCardCount": 9, "ok": True})
            for slug in slugs:
                t += rng.randint(500, 4000)
                events.append({"type": "viewport", "timestamp": t, "url": results_url, "isTargetSample": True, "targetPropertyId": slug, "ratio": 0.5, "isIntersecting": True})
                t += rng.randint(500, 15000)
                events.append({"type": "viewport", "timestamp": t, "url": results_url, "isTargetSample": True, "targetPropertyId": slug, "ratio": 0, "isIntersecting": False})
                if rng.random() < 0.4:
                    events.append({"type": "hover", "timestamp": t + 100, "url": results_url, "isTargetSample": True, "kind": "product_card",
                                   "targetPropertyId": slug, "durationMs": rng.randint(200, 6000)})
            events.append({"type": "scroll", "timestamp": t + 200, "url": results_url, "isTargetSample": True,
                           "scrollDepthPercent": rng.randint(30, 100), "visiblePropertyIds": slugs[:3]})
            if thumbs[wi] == "P" and rng.random() < 0.1:
                events.append({"type": "pouce_explanation", "timestamp": t + 300, "url": results_url, "isTargetSample": True,
                               "durationMs": rng.randint(500, 5000), "trigger": "hover", "targetPropertyId": slugs[0]})
            for slug in rng.sample(slugs, rng.randint(0, 3)):
                t += rng.randint(1000, 5000)
                events.append({"type": "click", "timestamp": t, "url": results_url, "isTargetSample": True, "kind": "product_card",
                               "targetPropertyId": slug, "targetTestId": "property-card", "targetText": "Voir"})
                t += 300
                events.append({"type": "page_view", "timestamp": t, "url": slug_url(slug, dest_id), "isTargetSample": True, "referrer": results_url})
                t += rng.randint(2000, 60000)
                events.append({"type": "tab_focus", "timestamp": t, "url": results_url, "event": "activated", "tabId": 1, "windowId": 1})
            pref = [p for p in listing_set if p["cluster"] == taste] if rng.random() < 0.6 else listing_set
            chosen = rng.choice(pref)
            t += rng.randint(2000, 30000)
            events.append({"type": "click", "timestamp": t, "url": results_url, "isTargetSample": True, "kind": "product_card",
                           "targetPropertyId": chosen["property_slug"], "targetTestId": "property-card"})
            t += 300
            events.append({"type": "page_view", "timestamp": t, "url": slug_url(chosen["property_slug"], dest_id), "isTargetSample": True})
            t += rng.randint(3000, 40000)
            events.append({"type": "reserve", "timestamp": t, "url": slug_url(chosen["property_slug"], dest_id), "isTargetSample": True,
                           "targetPropertyId": chosen["property_slug"], "displayedPrice": "199 €", "selectedRooms": 1, "cellIndex": cell})
            with open(folder / f"cell{cell:02d}_{city_slug}_{checkin}.json", "w", encoding="utf-8") as f:
                json.dump({"extensionVersion": "0.0", "exportedAt": "2026-07-01T00:00:00Z", "participant": {"subjectId": code},
                           "cell": {"cellIndex": cell, "cityKey": city_slug, "checkin": checkin, "listIndex": wi, "thumb": thumbs[wi]},
                           "events": events}, f)
            otree[f"choice_task_block.{cell}.player.choice_cluster"] = chosen["cluster"]
            otree[f"choice_task_block.{cell}.player.choice_preferred"] = chosen["preferred"]
            pagetimes_rows.append((code, 2 + cell, "choice_task_block", "ChoiceTaskLoop", t / 1000 + 5, cell))
    q = 100 + cell
    pagetimes_rows.append((code, q, "postexperiment_block", "PostExperimentQuestionnaire1", t / 1000 + 400, 1))
    otree.update({
        "postexperiment_block.1.player.noticed_thumb": rng.random() < 0.6,
        "postexperiment_block.1.player.noticed_checkmark": rng.random() < 0.4,
        "postexperiment_block.1.player.noticed_badge": rng.random() < 0.4,
        "postexperiment_block.1.player.visual_complexity": rng.randint(1, 7),
        **{f"postexperiment_block.1.player.nasa_tlx_{d}": rng.randrange(0, 101, 5)
           for d in ["mental", "physical", "temporal", "performance", "effort", "frustration"]},
        "instructions_block.1.player.failed_comprehension_prize": rng.random() < 0.2,
        "instructions_block.1.player.failed_comprehension_choice_city_weekend": rng.random() < 0.2,
        "instructions_block.1.player.failed_comprehension_no_cancellation": rng.random() < 0.2,
        "postexperiment_block.1.player.gender": rng.choice(["Homme", "Femme"]),
        "postexperiment_block.1.player.age": rng.randint(19, 70),
        "postexperiment_block.1.player.student_status": rng.choice(["Étudiant", "Non étudiant"]),
        "postexperiment_block.1.player.household_structure": rng.choice(["J'habite seul", "J'habite en couple", "J'habite avec ma famille"]),
        "postexperiment_block.1.player.paris_resident": rng.random() < 0.8,
        "postexperiment_block.1.player.booking_familiarity": rng.randint(1, 4),
        "postexperiment_block.1.player.belief_thumb_quality": rng.randint(1, 4),
        "pretask_block.1.player.accommodation_preference_open": "le prix et la localisation",
        "postexperiment_block.1.player.choice_process_open": rng.choice(["proche de la plage et pas trop cher", "les photos et les avis", "la note et le prix"]),
        "postexperiment_block.1.player.belief_thumb_meaning_open": rng.choice(["recommandé par booking", "les avis des clients", "un bon hôtel", "aucune idée", "partenaire payant"]),
        "postexperiment_block.1.player.thumb_use_open": rng.choice(["oui, j'ai préféré ces hôtels", "non", "pas remarqué", "je ne m'en suis pas servi"]),
        "postexperiment_block.1.player.feedback_open": rng.choice(["chargement un peu long", "très bien", ""]),
    })
    otree_rows.append(otree)

cols = list(otree_rows[0].keys())
with open(out / "session_1" / "all_apps_wide_2026-07-01.csv", "w", newline="", encoding="utf-8-sig") as f:
    w = csv.DictWriter(f, fieldnames=cols)
    w.writeheader()
    w.writerows(otree_rows)
with open(out / "session_1" / "PageTimes-2026-07-01.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow(["session_code", "participant_id_in_session", "participant_code", "page_index", "app_name", "page_name",
                "epoch_time_completed", "round_number", "timeout_happened", "is_wait_page"])
    for code, idx, app, page, ts, rnd in pagetimes_rows:
        w.writerow(["sess1", 1, code, idx, app, page, ts, rnd, 0, 0])
print(f"synthetic raw tree for {n_subjects} subjects -> {out}")
