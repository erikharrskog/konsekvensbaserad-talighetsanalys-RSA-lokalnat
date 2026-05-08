# Fil:                foundation.R
# Författare:         Erik Hårrskog
# Datum:              2026-05-08
#
# Innehåll:
#
# 3.1 Publika hjälpfunktioner
#     a) Loggning och felhantering (time_stamp, say, err)
#     b) Nodnycklar och koordinater (make_key, key_to_xy)
#     c) Inläsning och validering av data (read_input_file, load_inputs)

# ============================================================================ #
# 3.1 Publika hjälpfunktioner                                                  #
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 3.1 a) Loggning och felhantering (time_stamp, say, err)                      #
# ---------------------------------------------------------------------------- #

# Returnerar aktuell tid i standardiserat tidsformat
time_stamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

# Skriver tidsstämplade informationsmeddelanden till konsol om WRITE_LOG är TRUE
say <- function(...) {
  
  # Avbryter om loggning är avstängd
  if (!isTRUE(WRITE_LOG)) return(invisible(NULL))
  
  cat(crayon::blue(sprintf("[%s] ", time_stamp())), sprintf(...), "\n", sep = "")
}

# Skriver tidsstämplade felmeddelanden till konsol
err <- function(...) {
  cat(crayon::red(sprintf("[%s] ", time_stamp())), crayon::red(sprintf(...)), "\n", sep = "")
}

# ---------------------------------------------------------------------------- #
# 3.1 b) Nodnycklar och koordinater (make_key, key_to_xy)                      #
# ---------------------------------------------------------------------------- #

# Skapar nodnyckel baserat på koordinater
make_key <- function(x, y) {
  paste0(x, "_", y)
}

# Extraherar koordinater från nodnyckel
key_to_xy <- function(key) {
  
  # Tar bort |A eller |B om det ligger sist i strängen
  base_key <- sub("\\|[AB]$", "", as.character(key))
  
  # Delar upp nodnyckeln i en X- och en Y-del
  coord_parts <- strsplit(base_key, "_", fixed = TRUE)[[1]]
  
  # Stoppar om koordinaterna inte kan extraheras korrekt
  if (length(coord_parts) < 2) stop("Ogiltigt nodnyckelformat: ", key, call. = FALSE)
  
  # Returnerar koordinaterna som tal
  as.numeric(coord_parts[1:2])
}

# ---------------------------------------------------------------------------- #
# 3.1 c) Inläsning och validering av data (read_input_file, load_inputs)       #
# ---------------------------------------------------------------------------- #

# Läser in och validerar Excel-fil med angivna kolumner
read_input_file <- function(path, name, columns) {
  
  # Avbryter med fel om datafil saknas
  if (!file.exists(path)) {
    msg <- sprintf("Saknar datafil: %s", path)
    err(msg)
    stop(msg, call. = FALSE)
  }
  
  # Läser in hela Excel-arket och väljer ut angivna kolumner
  raw_data   <- readxl::read_excel(path, sheet = 1)
  input_data <- dplyr::select(raw_data, dplyr::any_of(columns))
  
  # Validerar att inläst data inte är tom
  if (nrow(input_data) == 0) {
    msg <- sprintf("Fel: Datan '%s' kunde inte laddas in korrekt", name)
    err(msg)
    stop(msg, call. = FALSE)
  }
  input_data
}

# Läser in och validerar samtliga indatafiler enligt specificerade kolumnlistor
load_inputs <- function(paths) {
  
  # Definierar vilka kolumner som ska läsas in per datakälla
  cols <- list(
    FEEDERPOINT   = c("X", "Y"),
    MVPART        = c("ID", "X1", "Y1", "X2", "Y2", "LENGTH"),
    BUSBARPART    = c("ID", "FATHERID", "X1", "Y1", "X2", "Y2"),
    DISCONNECTORS = c("ID", "FATHERID", "X", "Y", "STATE", "LABEL"),
    SCENARIOS     = c("ID", "FATHERID", "X", "Y", "STATE", "LABEL"),
    TRANSFORMER   = c("ID", "FATHERID", "X", "Y", "LABEL"),
    INTERRUPTIONS     = c("DISTRTRAFO.ID", "Antal_kunder_UT", "Eifs_KILEnergi_per_h",
                      "CPPOWER_UT", "CONTRACTPOWER_UT"),
    LINE_INFO      = c("NCMLFPART.ID = MVPART.ID", "Odefinierad [m]", "Friledning [m]",
                      "Hängkabel [m]", "Jordkabel [m]", "Sjökabel [m]", 
                      "Annan ledarkonstruktion [m]", "BLX-ledning [m]",
                      "Friledning i skog [m]", "Hängkabel i skog [m]",
                      "BLX-ledning i skog [m]", "Jordkabel i sjö [m]")
  )
  
  say("Läser in indatafiler...")
  
  # Returnerar samtliga inlästa data som en lista
  list(
    FEEDERPOINT   = read_input_file(paths$FEEDERPOINT,   "FEEDERPOINT_INPUT",   cols$FEEDERPOINT),
    MVPART        = read_input_file(paths$MVPART,        "MVPART_INPUT",        cols$MVPART),
    BUSBARPART    = read_input_file(paths$BUSBARPART,    "BUSBARPART_INPUT",    cols$BUSBARPART),
    DISCONNECTORS = read_input_file(paths$DISCONNECTORS, "DISCONNECTORS_INPUT", cols$DISCONNECTORS),
    SCENARIOS     = read_input_file(paths$SCENARIOS,     "SCENARIOS_INPUT",     cols$SCENARIOS),
    TRANSFORMER   = read_input_file(paths$TRANSFORMER,   "TRANSFORMER_INPUT",   cols$TRANSFORMER),
    INTERRUPTIONS = read_input_file(paths$INTERRUPTIONS, "INTERRUPTIONS_INPUT", cols$INTERRUPTIONS),
    LINE_INFO = {
      if (!is.null(paths$LINE_INFO) && file.exists(paths$LINE_INFO)) {
        read_input_file(paths$LINE_INFO, "LINE_INFO_INPUT", cols$LINE_INFO)
      } else {
        NULL
      }
    }
  )
}