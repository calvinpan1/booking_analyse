
normalize_text <- function(x) {
  # chartr rather than iconv(..., "ASCII//TRANSLIT"): macOS iconv renders
  # "é" as "'e", which silently breaks every accented keyword; chartr is
  # identical on macOS, Windows and Linux.
  x <- ifelse(is.na(x), "", as.character(x))
  x <- tolower(x)
  x <- chartr("àâäéèêëîïôöùûüç’",
              "aaaeeeeiioouuuc'", x)
  gsub("\\s+", " ", trimws(x))
}

has <- function(x, pattern) grepl(pattern, x, perl = TRUE)

# --- Q: "Selon vous, que signifie l'icone pouce ?" -------------------------
code_meaning <- function(x) {
  x <- normalize_text(x)
  reviews  <- has(x, "\\bavis\\b|client|utilisateur|usager|voyageur|\\bnote|notation|commentaire|\\bpublic\\b|retour|reservant|feedback|j'?aime|experience|satisf|plebiscit")
  paid     <- has(x, "partenaire|sponsor|\\bpay|commission|mise en avant|publicit|promot")
  platform <- paid | has(x, "booking|\\bsite\\b|plateforme|\\bgenius\\b")
  value    <- has(x, "qualit[ea]?[ -]*prix|rapport|tarif|reduction|\\bprix\\b")
  dontknow <- has(x, "aucune idee|^non\\b|je ne sais|^pas vraiment|^\\?*$")
  generic  <- !(reviews | platform | value | dontknow) &
    has(x, "\\bbon|\\bbien\\b|qualit|recommand|approuv|conseill|confiance|fiable|super|valid|certif|meilleur|favori|\\bok\\b|appreci|positiv|impeccable|interessant|\\btop\\b|priorit|\\bplus\\b|agreable")
  other    <- !(reviews | platform | value | dontknow | generic)
  data.frame(meaning_reviews = reviews, meaning_platform = platform, meaning_paid = paid,
             meaning_value = value, meaning_dontknow = dontknow, meaning_generic = generic,
             meaning_other = other)
}

# --- Q: "Si vous aviez remarque l'icone pouce ..., vous en etes-vous servi ?" ---
code_use <- function(x) {
  x <- normalize_text(x)
  not_noticed <- has(x, "pas (du tout )?remarqu|pas vu|rien remarqu|ne sais plus|pas fait attention|pas systematiquement|jamais (vu|remarqu)")
  bare_no     <- has(x, "^(non|no|nan|non pas vraiment|pas vraiment)\\W*$")
  not_used    <- !not_noticed & !bare_no &
    has(x, "pas (specialement |du tout |vraiment )?(servi|utilis)|ne m'?en (suis|ser[st]|serai)|pas servi|pas utilis|connais mais|remarqu.*(pas|ne)|^non\\b")
  used_click  <- has(x, "cliqu|appuy|rentrer dans|voir les (commentaires|avis)|pour l'?avis|aux avis|les avis des gens")
  used_signal <- !not_noticed & !not_used & !bare_no &
    (has(x, "^oui|\\boui\\b|prefer|reserve|confiance|choix|fiable|bien servi|priorit|pour faire") | used_click)
  not_understood <- has(x, "pas compris|aucune information|signification|utilite|\\bsens\\b|sais pas (ce que|a quoi)")
  distrust    <- has(x, "aime pas|me fier|fie pas|a l'essentiel")
  used        <- (used_signal | used_click) & !not_noticed & !not_used & !bare_no
  data.frame(use_not_noticed = not_noticed, use_noticed_not_used = not_used, use_bare_no = bare_no,
             use_as_signal = used_signal, use_to_click = used_click, use_used = used,
             use_not_understood = not_understood, use_distrust = distrust,
             use_not_used_any = (not_noticed | not_used | bare_no) & !used)
}

# --- Q: "Comment avez-vous procede pour choisir les logements ?" -----------
code_choice <- function(x) {
  x <- normalize_text(x)
  data.frame(
    choice_location  = has(x, "centre|gare|transport|commerce|\\bville\\b|emplacement|situ|localis|\\blieu|distance|proxim|acces|plage|\\bmer\\b|bord|geograph|environnement|\\bkm\\b"),
    choice_price     = has(x, "\\bprix|tarif|\\bcher|budget|euro|400|abordable|cout|enveloppe|moins ch"),
    choice_rating    = has(x, "\\bnote|\\bavis\\b|commentaire|review|notation|satisf|reservations passees|experience|retour|qualification|points? de"),
    choice_photos    = has(x, "photo|image|esthet|beaut|deco|design|style|\\bvue\\b|cosy|atypique|agreable|joli|plai"),
    choice_amenities = has(x, "superficie|taille|espace|equipement|clim|piscine|cuisine|\\blits?\\b|confort|service|prestation|\\bspa\\b|jacuzzi|m2|\\boption|commodit|propre|\\betat\\b|amenagement|chambre|spacieux|\\bgrand|extras|installation|menage|restauration|annexe"),
    choice_badge     = has(x, "icone|pouce|\\blogo|badge")
  )
}

# --- Q: "Souhaitez-vous nous faire part de commentaires ?" ----------------
code_feedback <- function(x) {
  x <- normalize_text(x)
  data.frame(
    feedback_slow_or_long = has(x, "chargement|\\blong|attente|\\blent|fastidieux|plus rapide"),
    feedback_positive     = has(x, "\\bbien\\b|bonne|belle|agreable|interessant|super|parfait|merci|adore|facile|ludique|bravo|\\btop\\b|reflechir")
  )
}

code_open_text <- function(open_df) {
  stopifnot(all(c("belief_thumb_meaning_open", "thumb_use_open", "choice_process_open", "feedback_open") %in% names(open_df)))
  dplyr::bind_cols(
    open_df,
    code_meaning(open_df$belief_thumb_meaning_open),
    code_use(open_df$thumb_use_open),
    code_choice(open_df$choice_process_open),
    code_feedback(open_df$feedback_open)
  )
}
