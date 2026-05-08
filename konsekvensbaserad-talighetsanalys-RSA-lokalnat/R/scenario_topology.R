# Fil:                scenario_topology.R
# Författare:         Erik Hårrskog
# Datum:              2026-05-08
#
# Innehåll:
#
# 9.1 Publik hjälpfunktion
#     a) Bygger felscenarier (build_scenarios)
#
# 9.2 Huvudfunktion: build_scenario_topology
#     a) Normalisering av indata och förberedelser
#     b) Ombyggnad av topologi med öppnade frånskiljare
#     c) Konstruktion av flödesgraf för scenariot
#     d) Sammanställning av scenariodata

# ============================================================================ #
# 9.1) Publik hjälpfunktion                                                    #
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 9.1 a) Bygger felscenarier (build_scenarios)                                 #
# ---------------------------------------------------------------------------- #

# Genererar felscenarier baserat på frånskiljare och nätsegment
build_scenarios <- function(res, seed_ids, inputs, flags) {
  
  idx       <- build_segment_index(res, flags)
  seg_id    <- idx$seg_id
  start_seg <- idx$start_seg
  
  # Bygger tabell där varje frånskiljare kopplas till segmenten på respektive sida
  disc_tbl <- res$disconnector_summary |>
    dplyr::mutate(
      ID       = as.character(ID),
      a        = as.character(a),
      b        = as.character(b),
      STATE    = as.integer(STATE),
      FATHERID = as.character(FATHERID),
      seg_a    = as.character(seg_id[a]),
      seg_b    = as.character(seg_id[b])
    ) |>
    dplyr::filter(a %in% names(seg_id), b %in% names(seg_id))
  
  # Grafen används för att avgöra vad som är "nedströms" relativt matningen
  closed_segment_graph <- igraph::graph_from_data_frame(
    disc_tbl |>
      dplyr::filter(STATE == 1) |>
      dplyr::transmute(from = seg_a, to = seg_b, ID = ID),
    directed = FALSE
  )
  
  # Hjälpfunktion för att välja nedströms segment
  pick_downstream_seg <- function(seg_a, seg_b) {
    
    # Om ena sidan är startsegmentet är den andra per definition nedströms
    if (seg_a == as.character(start_seg)) return(seg_b)
    if (seg_b == as.character(start_seg)) return(seg_a)
    
    # Fallback om nedströms inte kan avgöras, väljer alltid seg_b
    if (igraph::vcount(closed_segment_graph) == 0 ||
        !(as.character(start_seg) %in% igraph::V(closed_segment_graph)$name)) {
      return(seg_b)
    }
    
    # Jämför topologiskt avstånd från startsegmentet till respektive sida
    dist_to_a <- suppressWarnings(igraph::distances(closed_segment_graph, v = as.character(start_seg), to = seg_a)[1, 1])
    dist_to_b <- suppressWarnings(igraph::distances(closed_segment_graph, v = as.character(start_seg), to = seg_b)[1, 1])
    
    # Nedströms definieras som "längre bort från matningen"
    if (is.finite(dist_to_a) && is.finite(dist_to_b) && dist_to_a != dist_to_b) {
      if (dist_to_a < dist_to_b) seg_b else seg_a
    } else {
      seg_b
    }
  }
  
  scenarios <- list()
  
  # Specialfall: fel direkt efter matningen (öppna alla första frånskiljare)
  open_start <- disc_tbl |>
    dplyr::filter(seg_a == as.character(start_seg) | seg_b == as.character(start_seg)) |>
    dplyr::pull(ID) |>
    unique()
  
  # Markerar att felet sker i startsegmentet
  fault_seg <- as.character(start_seg)
  
  # Beräknar segmentlängd för felzonen
  segment_length_m <- get_segment_length_m(target_seg_id = fault_seg, res = res, flags  = flags)
  
  # Skapar START_FAULT-scenario
  scenarios[["START_FAULT"]] <- list(
    label     = "START_FAULT",
    seed_id   = NA_character_,
    open_ids  = open_start,
    fault_seg = fault_seg,
    segment_length_m   = segment_length_m
  )
  
  # Scenarier initierade av seed-frånskiljare i subnätet
  for (sid in as.character(seed_ids)) {
    
    # Hämtar seed-frånskiljaren för scenariot
    row <- disc_tbl |>
      dplyr::filter(ID == sid) |>
      dplyr::slice(1)
    
    # Stoppar om seed_id inte finns i summary
    if (nrow(row) == 0) {
      stop("Seed-ID saknas i disconnector_summary: ", sid, call. = FALSE)
    }
    
    # Väljer nedströms segment relativt matningen
    downstream_seg <- pick_downstream_seg(row$seg_a[1], row$seg_b[1])
    
    # Alla stängda frånskiljare som utgör snittet runt nedströms-segmentet
    incident_downstream <- disc_tbl |>
      dplyr::filter(
        STATE == 1,
        xor(seg_a == downstream_seg, seg_b == downstream_seg)
      ) |>
      dplyr::pull(ID) |>
      unique()
    
    # Öppna seed + snittet runt nedströms-segmentet
    open_ids <- sort(unique(c(sid, incident_downstream)))
    
    # Skapar scenarioetikett baserat på seed-ID
    label    <- paste0("SEED_", sid)
    
    # Felsegmentet motsvarar nedströms-segmentet
    fault_seg <- as.character(downstream_seg)
    
    # Beräknar segmentlängd för felzonen
    segment_length_m <- get_segment_length_m(
      target_seg_id = fault_seg,
      res    = res,
      flags  = flags
    )
    
    # Skapar seed-scenario
    scenarios[[label]] <- list(
      label     = label,
      seed_id   = sid,
      open_ids  = open_ids,
      fault_seg = fault_seg,
      segment_length_m   = segment_length_m
    )
  }
  
  # Begränsar scenariomängden om OPEN_ID_LIMIT används som maxantal scenarier
  if (is.finite(flags$OPEN_ID_LIMIT) && flags$OPEN_ID_LIMIT > 0 && length(scenarios) > 1) {
    ordered_names <- setdiff(names(scenarios), "START_FAULT")
    ordered_names <- head(ordered_names, as.integer(flags$OPEN_ID_LIMIT))
    scenarios     <- scenarios[c("START_FAULT", ordered_names)]
  }
  
  # Returnerar lista över de scenarier som ska köras
  scenarios
}

# ============================================================================ #
# 9.2) Huvudfunktion: build_scenario_topology                                  #
# ============================================================================ #

# Definierar funktion "build_scenario_topology" som bygger topologi och graf
build_scenario_topology <- function(res,
                                    open_disconnector_ids,
                                    inputs,
                                    keep_unreachable = FALSE,
                                    flags) {
  
  # -------------------------------------------------------------------------- #
  # 9.2 a) Normalisering av indata och förberedelser                           #
  # -------------------------------------------------------------------------- #
  
  # Normaliserar frånskiljar-ID
  open_disconnector_ids <- as.numeric(open_disconnector_ids)
  
  # Hämtar indataobjekt som används vid ombyggnad av topologin
  mv_parts      <- inputs$MVPART
  busbar_parts  <- inputs$BUSBARPART
  disconnectors <- inputs$DISCONNECTORS
  transformers  <- inputs$TRANSFORMER
  
  # -------------------------------------------------------------------------- #
  # 9.2 b) Ombyggnad av topologi med öppnade frånskiljare                      #
  # -------------------------------------------------------------------------- #
  
  # Bygger ny topologi där angivna frånskiljare är öppnade
  scenario_topology <- build_topology_from_point(
    MVPARTS              = mv_parts,
    BUSBARPARTS          = busbar_parts,
    DISCONNECTORS        = disconnectors,
    FEEDERPOINT          = data.frame(X = res$start_x, Y = res$start_y),
    TRANSFORMERS         = transformers,
    opened_disconnectors = open_disconnector_ids,
    flags                = modifyList(flags, list(REDUCED=FALSE))
  )
  
  # -------------------------------------------------------------------------- #
  # 9.2 c) Konstruktion av flödesgraf för scenariot                            #
  # -------------------------------------------------------------------------- #
  
  # Väljer kantmängd i korrekt nodrymd (FULL/REDUCED)
  scenario_edges_all <- if (isTRUE(flags$REDUCED)) {
    
    # Normaliserar scenariots kantlista
    ef <- scenario_topology$all_edges_full |>
      dplyr::mutate(
        from  = as.character(from),
        to    = as.character(to),
        ID    = as.character(ID),
        SRC   = as.character(SRC)
      )
    
    # Begränsar till noder som ingår i bas-subnätets nodrymd
    full_nodes_in_subnet <- names(res$node_rep_map)
    ef <- ef |>
      dplyr::filter(from %in% full_nodes_in_subnet & to %in% full_nodes_in_subnet)
    
    # Mappar kanter till reducerad nodrymd
    ef |>
      dplyr::mutate(
        from = map_to_reduced(res, from, flags),
        to   = map_to_reduced(res, to, flags)
      ) |>
      dplyr::filter(!is.na(from), !is.na(to), from != to) |>
      dplyr::distinct(from, to, ID, SRC, .keep_all = TRUE)
    
  } else {
    scenario_topology$all_edges_full
  }
  
  # Normaliserar startnodens nyckel till korrekt nodrymd
  source_key <- map_to_reduced(res, scenario_topology$start_node_key, flags)
  
  # Bygger flödesgraf för scenariot
  flow_scenario <- build_flow_graph(
    edges_active     = scenario_edges_all,
    source_keys      = source_key,
    keep_unreachable = keep_unreachable
  )
  
  # -------------------------------------------------------------------------- #
  # 9.2 d) Sammanställning av scenariodata                                     #
  # -------------------------------------------------------------------------- #
  
  # Kopplar scenariometadata till flödesresultatet
  flow_scenario$open_disconnector_ids     <- open_disconnector_ids
  flow_scenario$disconnector_summary      <- scenario_topology$disconnector_summary
  flow_scenario$res_scenario_topology     <- scenario_topology
  flow_scenario$edges_scenario_undirected <- scenario_edges_all
  
  # Returnerar scenariots flödesrepresentation och metadata
  flow_scenario
}