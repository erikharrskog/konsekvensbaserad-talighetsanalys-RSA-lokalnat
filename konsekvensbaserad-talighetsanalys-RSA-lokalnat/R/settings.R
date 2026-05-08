# Fil:                settings.R
# Författare:         Erik Hårrskog
# Datum:              2026-05-08
#
# Innehåll:
#
# 2.1 Globala inställningar och körflaggor
# 2.2 Definition av indata
# 2.3 Initiering av exportstruktur och körnamn

# ============================================================================ #
# 2.1) Globala inställningar och körflaggor                                    #
# ============================================================================ #

# Definierar global flagga för logging
WRITE_LOG <- TRUE

# Definierar körflaggor som skickas som argument till funktioner
FLAGS <- list(
  REDUCED         = TRUE,
  OPEN_ID_LIMIT   = Inf,
  
  ASK_EXCLUDE     = TRUE,
  ALLOW_BB_FAULTS = FALSE,
  
  PLOT_CANDIDATES = TRUE,
  PLOT_SCENARIOS  = TRUE,
  
  EXPORT_PLOTS    = TRUE,
  EXPORT_DATA     = TRUE
)

# Definierar namn på nätet och exportmapp
NET_NAME <- "xxx"
EXPORT_FOLDER <- "export"

# ============================================================================ #
# 2.2) Definition av indata                                                    #
# ============================================================================ #

# Specificerar vilka input-filer som används
input_paths <- list(
  MVPART        = "data/xxx.xlsx",
  DISCONNECTORS = "data/xxx.xlsx",
  SCENARIOS     = "data/xxx.xlsx",
  BUSBARPART    = "data/xxx.xlsx",
  TRANSFORMER   = "data/xxx.xlsx",
  FEEDERPOINT   = "data/xxx.xlsx",
  INTERRUPTIONS = "data/xxx.xlsx",
  LINE_INFO     = "data/xxx.xlsx"
)

# ============================================================================ #
# 2.3) Initiering av exportstruktur och körnamn                                #
# ============================================================================ #

# Skapar tidsstämpel för namngivning av mapp och excelfil
RUN_TS <- gsub(":", "_", time_stamp())
RUN_NAME <- sprintf("Simuleringsresultat RSA %s %s", NET_NAME, RUN_TS)

# Definierar filväg för exporten
PLOT_FOLDER <- file.path(EXPORT_FOLDER, RUN_NAME)
EXPORT_PATH <- file.path(PLOT_FOLDER, paste0(RUN_NAME, ".xlsx"))

# Skapar exportmapp om någon av exportflaggorna är aktiverade
if ((isTRUE(FLAGS$EXPORT_PLOTS) || isTRUE(FLAGS$EXPORT_DATA)) && !dir.exists(PLOT_FOLDER)) {
  dir.create(PLOT_FOLDER, recursive = TRUE, showWarnings = FALSE)
}