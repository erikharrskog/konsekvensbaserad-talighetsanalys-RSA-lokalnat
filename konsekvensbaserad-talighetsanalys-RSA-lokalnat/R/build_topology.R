# Fil:                build_topology.R
# Författare:         Erik Hårrskog
# Datum:              2026-05-08
#
# Innehåll:
#
# 4.1 Huvudfunktion: build_topology_from_point
#     a) Fastställande av startpunkt
#     b) Topologiska grundobjekt
#     c) Geometriska hjälpobjekt
#     d) Förberedelse inför split av kanter
#     e) Infogning av frånskiljare
#     f) Koppling av transformatorer till BUSBAR-noder i nätstationer
#     g) Konstruktion av data för modifierad topologi
#     h) Konstruktion av nätverksgraf samt subnät utifrån startpunkt
#     i) Reducering av graf
#     j) Retur av resultatet


# ============================================================================ #
# 4.1) Huvudfunktion: build_topology_from_point                                #
# ============================================================================ #

# Definierar funktion "build_topology_from_point" för uppbyggnad av nättopologi
build_topology_from_point <- function(MVPARTS,
                                      BUSBARPARTS,
                                      DISCONNECTORS,
                                      TRANSFORMERS,
                                      FEEDERPOINT,
                                      opened_disconnectors = integer(0),
                                      flags) {
  
  # Säkerställer rätt datatyp i opened_disconnectors
  opened_disconnectors <- as.numeric(opened_disconnectors)
  
  # -------------------------------------------------------------------------- #
  # 4.1 a) Fastställande av startpunkt                                         #
  # -------------------------------------------------------------------------- #
  
  # Kontrollerar att startpunkten finns definierad
  if (is.null(FEEDERPOINT) || nrow(FEEDERPOINT) == 0) {
    err("Startpunkten saknas i indatan")
    stop("Startpunkten saknas i indatan", call. = FALSE)
  }
  
  # -------------------------------------------------------------------------- #
  # 4.1 b) Topologiska grundobjekt                                             #
  # -------------------------------------------------------------------------- #
  
  # Skapar en lista med MV-kanterna
  mv_edges <- dplyr::transmute(MVPARTS,
                               from           = make_key(X1, Y1),
                               to             = make_key(X2, Y2),
                               ID             = as.character(ID),
                               EDGE_KEY       = paste0(ID, "::", from, "::", to),
                               SRC            = "MVPART"
  )
  
  # Skapar en lista med BB-kanterna
  bb_edges <- dplyr::transmute(BUSBARPARTS,
                               from           = make_key(X1, Y1),
                               to             = make_key(X2, Y2),
                               ID             = as.character(ID),
                               STATIONS_ID    = as.numeric(FATHERID),
                               SRC            = "BUSBARPARTS"
  )
  
  # -------------------------------------------------------------------------- #
  # 4.1 c) Geometriska hjälpobjekt                                             #
  # -------------------------------------------------------------------------- #
  
  # Skapar en lista med MV-segmenten för ihopparning med frånskiljare
  mv_segments <- dplyr::transmute(MVPARTS,
                                  MV_ID       = as.character(ID),
                                  X1          = as.numeric(X1), Y1 = as.numeric(Y1),
                                  X2          = as.numeric(X2), Y2 = as.numeric(Y2),
                                  from        = make_key(X1, Y1),
                                  to          = make_key(X2, Y2)
  )
  
  # Skapar en lista med BB-segmenten för ihopparning med stationer
  bb_segments <- dplyr::transmute(BUSBARPARTS,
                                  BB_ID       = as.character(ID),
                                  X1          = as.numeric(X1), Y1 = as.numeric(Y1),
                                  X2          = as.numeric(X2), Y2 = as.numeric(Y2),
                                  from        = make_key(X1, Y1),
                                  to          = make_key(X2, Y2),
                                  STATIONS_ID = as.numeric(FATHERID)
  )
  
  # Skapar en lista med frånskiljare
  disconnectors <- dplyr::transmute(DISCONNECTORS,
                                    ID        = as.character(ID),
                                    FATHERID  = as.numeric(FATHERID),
                                    X         = as.numeric(X),
                                    Y         = as.numeric(Y),
                                    STATE     = as.integer(STATE),
                                    LABEL     = LABEL
  )
  # Rensar frånskiljare som har felaktiga koordinater
  disconnectors <- dplyr::filter(disconnectors, is.finite(X), is.finite(Y))
  
  # Skapar en lista med transformatorer för koppling till BUSBAR-noder
  transformers <- dplyr::transmute(TRANSFORMERS,
                                   ID         = as.character(ID),
                                   FATHERID   = as.numeric(FATHERID),
                                   X          = as.numeric(X),
                                   Y          = as.numeric(Y)
  )
  # Rensar transformatorer som har felaktiga koordinater
  transformers <- dplyr::filter(transformers, is.finite(X), is.finite(Y))
  
  # -------------------------------------------------------------------------- #
  # 4.1 d) Förberedelse inför split av kanter                                  #
  # -------------------------------------------------------------------------- #
  
  # Skapar temporära datastrukturer för kantuppdelning
  added_mv             <- list()
  added_bb             <- list()
  added_disconnectors  <- list()
  disconnector_info    <- list()
  added_transformers   <- list()
  
  # Skapar logiska vektorer för att hitta kanter som ska tas bort
  remove_mv <- logical(nrow(mv_edges))
  remove_bb <- logical(nrow(bb_edges))
  
  # -------------------------------------------------------------------------- #
  # 4.1 e) Infogning av frånskiljare                                           #
  # -------------------------------------------------------------------------- #
  
  # Loopar igenom samtliga frånskiljare och ersätter respektive segment
  for (k in seq_len(nrow(disconnectors))) {
    
    # Hämtar frånskiljarens koordinater
    x_k <- disconnectors$X[k]
    y_k <- disconnectors$Y[k]
    
    # Hämtar referens till segment som frånskiljaren tillhör
    father_id <- disconnectors$FATHERID[k]
    father_id_chr <- as.character(father_id)
    
    # Identifierar om father_id pekar på MV- eller BB-segment
    mv_seg_index <- match(father_id_chr, mv_segments$MV_ID)
    bb_seg_index <- match(father_id_chr, bb_segments$BB_ID)
    
    # Hanterar MV-segment
    if (!is.na(mv_seg_index)) {
      
      # Hämtar ändnoder och ID för MV-segmentet
      seg_from <- mv_segments$from[mv_seg_index]
      seg_to   <- mv_segments$to[mv_seg_index]
      seg_id   <- mv_segments$MV_ID[mv_seg_index]
      seg_type <- "MV"
      
      # Identifierar motsvarande MV-kant i kantlistan
      matched_edge_index <- which(
        mv_edges$ID == seg_id &
          ((mv_edges$from == seg_from & mv_edges$to == seg_to) |
             (mv_edges$from == seg_to   & mv_edges$to == seg_from))
      )
      
      # Stoppar om frånskiljare inte kan kopplas mot kant
      if (!length(matched_edge_index)) {
        stop(
          sprintf(
            "Frånskiljare %s kan inte matchas mot någon kant (segment-ID: %s)",
            disconnectors$ID[k], father_id_chr
          ),
          call. = FALSE
        )
      }
      
      # Markerar ursprunglig MV-kant för borttagning
      remove_mv[matched_edge_index] <- TRUE
      
      # Hanterar BB-segment
    } else if (!is.na(bb_seg_index)) {
      
      # Hämtar ändnoder, ID och stations-ID för BB-segmentet
      seg_from      <- bb_segments$from[bb_seg_index]
      seg_to        <- bb_segments$to[bb_seg_index]
      seg_id        <- bb_segments$BB_ID[bb_seg_index]
      bb_station_id <- bb_segments$STATIONS_ID[bb_seg_index]
      seg_type      <- "BB"
      
      # Identifierar motsvarande BB-kant i kantlistan
      matched_edge_index <- which(
        bb_edges$ID == seg_id &
          ((bb_edges$from == seg_from & bb_edges$to == seg_to) |
             (bb_edges$from == seg_to   & bb_edges$to == seg_from))
      )
      
      # Stoppar om frånskiljare inte kan kopplas mot kant
      if (!length(matched_edge_index)) {
        stop(
          sprintf(
            "Frånskiljare %s kan inte matchas mot någon BB-kant (segment-ID: %s)",
            disconnectors$ID[k], father_id_chr
          ),
          call. = FALSE
        )
      }
      
      # Markerar ursprunglig BB-kant för borttagning
      remove_bb[matched_edge_index] <- TRUE
    }
    
    # Stoppar om giltigt FATHERID saknas
    else {
      stop(
        sprintf(
          "Frånskiljare %s har ogiltig FATHERID (%s) – varken MV- eller BB-segment",
          disconnectors$ID[k], father_id_chr
        ),
        call. = FALSE
      )
    }
    
    # Beräknar närmaste ändnod på segmentet (snapping)
    xy_from <- key_to_xy(seg_from)
    xy_to   <- key_to_xy(seg_to)
    
    d_from <- (x_k - xy_from[1])^2 + (y_k - xy_from[2])^2
    d_to   <- (x_k - xy_to[1])^2   + (y_k - xy_to[2])^2
    
    # Väljer ändnod som bas för A/B-noder
    base_key <- if (d_from <= d_to) seg_from else seg_to
    key_disconnector_a <- paste0(base_key, "|A")
    key_disconnector_b <- paste0(base_key, "|B")
    
    seg_id_chr <- as.character(seg_id)
    
    # Skapar ersättningskanter för MV-segment
    if (seg_type == "MV") {
      
      # Skapar ersättningskant från segmentets första ändnod till A-noden
      added_mv[[length(added_mv)+1]] <- tibble::tibble(
        from = seg_from,
        to   = key_disconnector_a,
        ID   = seg_id_chr,
        EDGE_KEY = paste0(seg_id_chr, "::", seg_from, "::", key_disconnector_a),
        SRC  = "MVPART"
      )
      
      # Skapar ersättningskant från B-noden till segmentets andra ändnod
      added_mv[[length(added_mv)+1]] <- tibble::tibble(
        from = key_disconnector_b,
        to   = seg_to,
        ID   = seg_id_chr,
        EDGE_KEY = paste0(seg_id_chr, "::", key_disconnector_b, "::", seg_to),
        SRC  = "MVPART"
      )
      
      # Skapar ersättningskanter för BB-segment
    } else {
      
      # Skapar ersättningskant från segmentets första ändnod till A-noden
      added_bb[[length(added_bb)+1]] <- tibble::tibble(
        from = seg_from,
        to   = key_disconnector_a,
        ID   = seg_id,
        STATIONS_ID = bb_station_id,
        SRC  = "BUSBARPARTS"
      )
      
      # Skapar ersättningskant från B-noden till segmentets andra ändnod
      added_bb[[length(added_bb)+1]] <- tibble::tibble(
        from = key_disconnector_b,
        to   = seg_to,
        ID   = seg_id,
        STATIONS_ID = bb_station_id,
        SRC  = "BUSBARPARTS"
      )
    }
    
    # Skapar bryggkant om frånskiljaren är stängd
    if (disconnectors$STATE[k] == 1) {
      added_disconnectors[[length(added_disconnectors)+1]] <- tibble::tibble(
        from = key_disconnector_a,
        to   = key_disconnector_b,
        ID   = disconnectors$ID[k],
        SRC  = "FRÅNSKILJARE"
      )
    }
    
    # Sparar metadata om frånskiljaren
    disconnector_info[[length(disconnector_info)+1]] <- tibble::tibble(
      ID       = disconnectors$ID[k],
      STATE    = disconnectors$STATE[k],
      FATHERID = father_id,
      a        = key_disconnector_a,
      b        = key_disconnector_b,
      LABEL    = disconnectors$LABEL[k]
    )
  }
  
  # -------------------------------------------------------------------------- #
  # 4.1 f) Koppling av transformatorer till BUSBAR-noder i nätstationer        #
  # -------------------------------------------------------------------------- #
  
  # Sammanställer unika BUSBAR-noder per nätstation
  bb_nodes_per_station <- bb_edges |>
    dplyr::mutate(from = as.character(from), to = as.character(to)) |>
    dplyr::select(STATIONS_ID, from, to) |>
    tidyr::pivot_longer(cols = c(from, to), names_to = "which", values_to = "node") |>
    dplyr::distinct(STATIONS_ID, node)
  
  # Förberäknar koordinater för alla kandidatnoder
  bb_nodes_per_station <- bb_nodes_per_station |>
    dplyr::mutate(
      node_x = vapply(node, function(k) key_to_xy(k)[1], numeric(1)),
      node_y = vapply(node, function(k) key_to_xy(k)[2], numeric(1))
    )
  
  # Loopar igenom alla transformatorer och kopplar dem till närmaste BUSBAR-nod
  # i respektive nätstation
  if (nrow(transformers) > 0 && nrow(bb_nodes_per_station) > 0) {
    for (i in seq_len(nrow(transformers))) {
      
      # Hämtar transformatorns ID och koordinater, samt skapar nodnyckel
      transformer_id <- transformers$ID[i]
      transformer_x  <- transformers$X[i]
      transformer_y  <- transformers$Y[i]
      transformer_node_key <- make_key(transformer_x, transformer_y)
      
      # Identifierar kandidater: BUSBAR-noder i samma station som transformatorn
      station_id <- transformers$FATHERID[i]
      candidate_bb_nodes <- dplyr::filter(bb_nodes_per_station, STATIONS_ID == station_id)
      
      # Fallback om transformator inte kan matchas mot BUSBAR
      if (nrow(candidate_bb_nodes) == 0) {
    
        # Använder alla BUSBAR-noder istället
        candidate_bb_nodes <- bb_nodes_per_station
      }
      
      # Beräknar avstånd mellan transformatorn och kandidatnoder
      valid_node_coords <- is.finite(candidate_bb_nodes$node_x) & is.finite(candidate_bb_nodes$node_y)
      
      # Stoppar om det saknas giltiga koordinater
      if (!any(valid_node_coords)) {
        stop(
          sprintf(
            "Inga giltiga koordinater för BUSBAR-noder i station %s (transformator %s)",
            station_id, transformer_id
          ),
          call. = FALSE
        )
      }
      
      # Väljer närmaste BUSBAR-nod baserat på geometriskt avstånd
      dx <- candidate_bb_nodes$node_x[valid_node_coords] - transformer_x
      dy <- candidate_bb_nodes$node_y[valid_node_coords] - transformer_y
      nearest_index <- which.min(dx*dx + dy*dy)
      nearest_bb_node <- candidate_bb_nodes$node[which(valid_node_coords)[nearest_index]]
      
      # Skapar en kant mellan transformatorn och vald BUSBAR-nod
      added_transformers[[length(added_transformers)+1]] <- dplyr::tibble(
        from = nearest_bb_node,
        to   = transformer_node_key,
        ID   = transformer_id,
        SRC  = "TRANSFORMER"
      )
    }
  }
  
  # -------------------------------------------------------------------------- #
  # 4.1 g) Konstruktion av data för modifierad topologi                        #
  # -------------------------------------------------------------------------- #
  
  # Tar bort alla segment som ska ersättas
  mv_edges <- mv_edges[!remove_mv, , drop = FALSE]
  bb_edges <- bb_edges[!remove_bb, , drop = FALSE]
  
  # Skapar tabeller för senare analys av stängda respektive samtliga frånskiljare
  disconnector_edges <- dplyr::tibble(
    from = character(),
    to   = character(),
    ID   = character(),
    SRC  = character()
    )
  
  disconnector_summary <- tibble(
    ID = character(),
    STATE = integer(),
    FATHERID = numeric(),
    a = character(),
    b = character(),
    LABEL = character()
  )
  # Skapar tabell för senare analys av transformatorer
  transformer_edges <- dplyr::tibble(from=character(), to=character(), ID=character(), SRC=character())
  
  # Lägger till alla nya kanter och sammanställer tillhörande tabeller
  if (length(added_mv)) mv_edges <- dplyr::bind_rows(mv_edges, dplyr::bind_rows(added_mv))
  if (length(added_bb)) bb_edges <- dplyr::bind_rows(bb_edges, dplyr::bind_rows(added_bb))
  if (length(added_disconnectors)) disconnector_edges <- dplyr::bind_rows(added_disconnectors)
  if (length(disconnector_info))   disconnector_summary <- dplyr::bind_rows(disconnector_info)
  if (length(added_transformers))  transformer_edges <- dplyr::bind_rows(added_transformers)
  
  # Tar bort bryggkanten för frånskiljare som ska vara öppna i ett scenario
  disconnector_edges <- dplyr::filter(disconnector_edges, !(ID %in% opened_disconnectors))
  
  # Uppdaterar status för öppna frånskiljare i sammanställningen
  disconnector_summary <- disconnector_summary |>
    dplyr::mutate(STATE = dplyr::if_else(ID %in% opened_disconnectors, -1L, STATE))
  
  # Slår ihop alla kanter i nätet
  all_edges <- dplyr::bind_rows(mv_edges, bb_edges, disconnector_edges, transformer_edges)
  
  # -------------------------------------------------------------------------- #
  # 4.1 h) Konstruktion av nätverksgraf samt subnät utifrån startpunkt         #
  # -------------------------------------------------------------------------- #
  
  # Bygger en nätverksgraf (oriktad) utifrån kantsammanställningen
  g_full <- igraph::graph_from_data_frame(all_edges, directed = FALSE)
  
  # Definierar startnoden och kontrollerar att den ingår i grafen
  start_node <- make_key(FEEDERPOINT$X[1], FEEDERPOINT$Y[1])
  if (!(start_node %in% igraph::V(g_full)$name)) {
    msg <- "Startpunkten finns inte i grafen."
    err(msg)
    stop(msg, call. = FALSE) 
  }
  
  # Identifierar vilka noder som ingår i samma komponent som startnoden
  components <- igraph::components(g_full)
  start_component <- components$membership[igraph::V(g_full)$name == start_node]
  keep_nodes_base <- igraph::V(g_full)$name[components$membership == start_component]
  
  # Bygger en ny graf (subnät) som bara består av startnodens komponent
  g_sub <- igraph::induced_subgraph(g_full, vids = keep_nodes_base)
  
  # -------------------------------------------------------------------------- #
  # 4.1 i) Reducering av graf                                                  #
  # -------------------------------------------------------------------------- #
  
  # Reducerar grafen om flaggan är aktiverad
  if (isTRUE(flags$REDUCED)) {
    
    # Identifierar transformatornoder som ska bevaras
    transformer_nodes <- as.character(make_key(transformers$X, transformers$Y))
    
    # Reducerar grafen genom att ta bort noder som inte bidrar med värdefull information
    reduced <- reduce_topology_graph(
      edges = dplyr::filter(all_edges, from %in% keep_nodes_base & to %in% keep_nodes_base),
      disconnector_summary = disconnector_summary,
      start_node_key = start_node,
      transformer_nodes = transformer_nodes
    )
    
    # Hämtar relation mellan fullgrafens noder och den reducerade grafen
    node_rep_map_out     <- reduced$node_rep_map
    node_groups_out <- reduced$node_groups
    
    # Stoppar om reducering misslyckades
    if (is.null(node_rep_map_out)) {
      stop(
        "FLAGS$REDUCED = TRUE men node_rep_map saknas. Reducerad topologi är inkonsekvent.",
        call. = FALSE
      )
    }
    
    # Förbereder kanterna
    edges_out <- reduced$edges |>
      dplyr::mutate(from = as.character(from), to = as.character(to))
    
    # Samlar alla noder som ingår i reducerad graf
    keep_nodes_out <- unique(c(
      as.character(reduced$keep_nodes),
      as.character(edges_out$from),
      as.character(edges_out$to)
    ))
    
    # Konstruerar den nya, reducerade grafen
    g_out <- igraph::graph_from_data_frame(
      edges_out |> dplyr::select(from, to),
      directed = FALSE,
      vertices = data.frame(name = keep_nodes_out)
    )
    
    # Hämtar nodnycklar från reducerad graf
    nodes_out <- igraph::V(g_out)$name
    
    # Säkerställer rätt datatyp i sammanställningen
    disconnector_summary <- disconnector_summary |>
      dplyr::mutate(
        a  = as.character(a),
        b  = as.character(b),
        ID = as.character(ID)
      )
    
  } else {
    
    # Använder oreducerad graf om flaggan är FALSE
    g_out <- g_sub
    nodes_out <- igraph::V(g_sub)$name
    edges_out <- dplyr::filter(all_edges, from %in% keep_nodes_base & to %in% keep_nodes_base)
    keep_nodes_out <- keep_nodes_base
  }
  
  # -------------------------------------------------------------------------- #
  # 4.1 j) Retur av resultatet                                                 #
  # -------------------------------------------------------------------------- #

  # Returnerar resultat till anropande funktion
  list(
    # Subnätets topologi
    graph = g_out,
    nodes = data.frame(name = nodes_out),
    edges = edges_out,
    
    # Definition av subnätet                                                    
    start_node_key  = start_node,
    start_x         = FEEDERPOINT$X[1],
    start_y         = FEEDERPOINT$Y[1],
    keep_nodes_base = keep_nodes_out,
    all_edges_full  = all_edges,
    
    # Statistik och kontroll
    stats = list(
      num_of_nodes = igraph::vcount(g_out),
      num_of_edges = igraph::ecount(g_out),
      num_of_comps = igraph::components(g_out)$no
    ),
    
    # Koppling mellan full graf och reducerad representation
    node_rep_map     = if (isTRUE(flags$REDUCED)) node_rep_map_out else NULL,
    node_groups = if (isTRUE(flags$REDUCED)) node_groups_out else NULL,
    
    # Diagnostik och metadata
    disconnector_summary = disconnector_summary
  )
}