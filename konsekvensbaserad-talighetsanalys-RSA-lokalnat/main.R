# Fil:                main.R
# Författare:         Erik Hårrskog
# Datum:              2026-05-08
#
# Innehåll:
#
# 1.1 Inladdning av paket
# 1.2 Inladdning av filer
# 1.3 Bygg grundtopologi och identifiera matningskandidater
# 1.4 Konfigurera tillåtna alternativa matningskandidater
# 1.5 Kör alternativ matning + export

# ============================================================================ #
# 1.1) Inladdning av paket                                                     #
# ============================================================================ #

# Listar nödvändiga paket
required_packages <- c("tidyverse", "readxl", "igraph", "crayon", "openxlsx")

# Kontrollerar att nödvändiga paket finns installerade, stoppar annars
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    error_message <- sprintf(
      "Paket saknas: '%s'. Kör install.packages('%s').",
      pkg, pkg
    )
    stop(error_message, call. = FALSE)
  }
}

# Laddar alla nödvändiga paket i sessionen
suppressPackageStartupMessages(
  invisible(lapply(required_packages, library, character.only = TRUE))
)

# ============================================================================ #
# 1.2) Inladdning av filer                                                     #
# ============================================================================ #

# Listar filer som ska sourcas
required_scripts <- c(
  "R/foundation.R",
  "R/settings.R",
  "R/build_topology.R",
  "R/reduce_topology.R",
  "R/segment_lengths.R",
  "R/flow_graph.R",
  "R/run_scenarios.R",
  "R/scenario_topology.R",
  "R/resupply.R",
  "R/plots.R",
  "R/export.R"
)

# Laddar in filerna, stoppar om någon saknas
for (script in required_scripts) {
  if (!file.exists(script)) {
    error_message <- sprintf("Fil saknas: %s", script)
    stop(error_message, call. = FALSE)
  }
  source(script, local = FALSE)
}

# Läser in samtliga inputdata
inputs <- load_inputs(input_paths)

# Avgör om längd- och exponeringsdata (LINE_INFO) finns tillgänglig
has_line_info <- !is.null(inputs$LINE_INFO) && nrow(inputs$LINE_INFO) > 0

# ============================================================================ #
# 1.3) Bygg grundtopologi och identifiera matningskandidater                   #
# ============================================================================ #

# Bygger nätets grundtopologi och returnerar subnätet utifrån startpunkten
base_topology <- build_topology_from_point(
  MVPARTS       = inputs$MVPART,
  BUSBARPARTS   = inputs$BUSBARPART,
  DISCONNECTORS = inputs$DISCONNECTORS,
  FEEDERPOINT   = inputs$FEEDERPOINT,
  TRANSFORMERS  = inputs$TRANSFORMER,
  flags         = FLAGS
)

# Förbereder tabell med total längd per MVPART för senare analys
inputs$MVPART_LENGTHS <- inputs$MVPART |>
  dplyr::transmute(
    MVPART_ID   = as.character(ID),
    LEN_TOTAL_M = as.numeric(LENGTH)
  )

# Kopplar MVPART-längd till MV-kanterna i subnätet
base_topology$mv_subedge_lengths <- build_mv_subedge_lengths(
  res    = base_topology,
  inputs = inputs,
  flags  = FLAGS
)

say("Grundnätet är konstruerat")

# Identifierar alla möjliga alternativa matningsvägar baserat på grundtopologin
resupply_candidates <- build_resupply_candidates(base_topology = base_topology, flags = FLAGS)

# Delar upp externa (BOUNDARY) och interna (INTERNAL) alternativa matningsvägar
boundary_candidates   <- resupply_candidates$boundary_candidates
internal_candidates   <- resupply_candidates$internal_candidates

# Plottar grundgrafen med matningskandidater om flaggan är aktiverad
if (isTRUE(FLAGS$PLOT_CANDIDATES)) {
  
  plot_candidates(
    GRAPH = base_topology$graph,
    start_x = base_topology$start_x,
    start_y = base_topology$start_y,
    disconnector_summary = base_topology$disconnector_summary,
    boundary_candidates = boundary_candidates,
    internal_candidates = internal_candidates,
    flags = FLAGS,
    plot_filename = file.path(PLOT_FOLDER, "Grundplot.png")
  )
}

# ============================================================================ #
# 1.4) Konfigurera tillåtna alternativa matningskandidater                     #
# ============================================================================ #

# Ber användaren exkludera externa matningskällor (BOUNDARY) om flaggan är aktiverad
boundary_candidates <- apply_boundary_user_exclusion(
  boundary_candidates = boundary_candidates,
  disconnector_summary = base_topology$disconnector_summary,
  flags = FLAGS
)

# Ber användaren exkludera interna matningsvägar (INTERNAL) om flaggan är aktiverad
internal_candidates <- apply_internal_user_exclusion(
  internal_candidates = internal_candidates,
  boundary_candidates  = boundary_candidates,
  disconnector_summary = base_topology$disconnector_summary,
  flags = FLAGS
)

# Samlar alternativa externa matningskällor som får användas
valid_boundary_candidates <- boundary_candidates |> dplyr::filter(!user_excluded)
say("Antal externa matningsvägar (BOUNDARY): %d", nrow(valid_boundary_candidates))

# Samlar alternativa interna matningsvägar som får användas
valid_internal_candidates <- internal_candidates |> dplyr::filter(!user_excluded)
say("Antal interna matningsvägar (INTERNAL): %d", nrow(valid_internal_candidates))

# ============================================================================ #
# 1.5) Kör alternativ matning + export                                         #
# ============================================================================ #

# Kör de olika scenarierna med alternativ matning och samlar resultaten
scenario_analysis <- run_outage_scenarios(
  base_topology = base_topology,
  flags         = FLAGS,
  inputs        = inputs,
  valid_boundary_candidates = valid_boundary_candidates,
  valid_internal_candidates = valid_internal_candidates,
  plot_folder = PLOT_FOLDER
)

# Exporterar resultatet till en Excelfil om flaggan är aktiverad
export_data(
  scenario_results            = scenario_analysis$scenario_results,
  scenarios                   = scenario_analysis$scenarios,
  transformer_vulnerability   = scenario_analysis$transformer_vulnerability,
  export_data_flag            = FLAGS$EXPORT_DATA,
  flags                       = FLAGS,
  valid_boundary_candidates   = valid_boundary_candidates,
  valid_internal_candidates   = valid_internal_candidates,
  base_topology               = base_topology,
  inputs                      = inputs,
  export_path                 = EXPORT_PATH
)

# Informerar att scenarioviktning ej genomförts om LINE_INFO saknas
if (!has_line_info) {
  err("OBS: LINE_INFO saknas – scenarioviktning har ej genomförts")
}
