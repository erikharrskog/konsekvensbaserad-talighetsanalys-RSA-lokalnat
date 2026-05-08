# Fil:                flow_graph.R
# Författare:         Erik Hårrskog
# Datum:              2026-05-08
#
# Innehåll:
#
# 7.1 Publik hjälpfunktion
#     a) Segmentindelning av subnät (build_segment_index)
#
# 7.2 Huvudfunktion: build_flow_graph
#     a) Grundläggande validering och normalisering
#     b) Nåbarhetsanalys från matningspunkter
#     c) Skapande av artificiell virtuell startpunkt
#     d) Nåbarhetsanalys och beräkning av topologiskt avstånd
#     e) Urval av nåbara noder och kanter
#     f) Bestämning av flödesriktning
#     g) Hantering av ringmatning (tie-kanter)
#     h) Konstruktion av slutlig flödesgraf

# ============================================================================ #
# 7.1) Publik hjälpfunktion                                                    #
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 7.1 a) Segmentindelning av subnät (build_segment_index)                      #
# ---------------------------------------------------------------------------- #

# Delar upp subnätet i sammanhängande segment utan frånskiljare
build_segment_index <- function(res, flags) {
  
  # Samlar noder som ingår i subnätet, baserat på om REDUCED = TRUE/FALSE
  subnet_nodes <- if (isTRUE(flags$REDUCED)) {
    as.character(names(res$node_rep_map))
  } else {
    as.character(res$keep_nodes_base)
  }
  
  # Normaliserar kantlistan och begränsar den till subnätet
  edges_full <- res$all_edges_full |>
    dplyr::mutate(
      from = as.character(from),
      to   = as.character(to),
      SRC  = as.character(SRC),
      ID   = as.character(ID)
    ) |>
    dplyr::filter(from %in% subnet_nodes & to %in% subnet_nodes)
  
  # Exkluderar frånskiljare för att få sammanhängande nätsegment
  edges_no_switch <- edges_full |>
    dplyr::filter(SRC != "FRÅNSKILJARE")
  
  # Bygger en oriktad graf för segmentindelning
  segment_graph <- igraph::graph_from_data_frame(
    edges_no_switch,
    directed = FALSE,
    vertices = data.frame(name = subnet_nodes)
  )
  
  # Beräknar komponent-ID per nod
  comp   <- igraph::components(segment_graph)
  seg_id <- setNames(comp$membership, igraph::V(segment_graph)$name)
  
  # Identifierar segmentet där matningen finns
  start_key <- as.character(res$start_node_key)
  if (!(start_key %in% names(seg_id))) {
    stop("Startnod finns inte i segmentindex.", call. = FALSE)
  }
  
  # Returnerar segmentindex och startsegment
  list(
    seg_id    = seg_id,
    start_seg = unname(seg_id[[start_key]])
  )
}

# ============================================================================ #
# 7.2) Huvudfunktion: build_flow_graph                                         #
# ============================================================================ #

# Definierar funktion "build_flow_graph" som bygger flödesgraf
build_flow_graph <- function(edges_active,
                             source_keys,
                             keep_unreachable = FALSE) {
  
  # -------------------------------------------------------------------------- #
  # 7.2 a) Grundläggande validering och normalisering                          #
  # -------------------------------------------------------------------------- #
  
  # Säkerställer att minst en källnod finns angiven
  stopifnot(length(source_keys) > 0)
  
  # Normaliserar from/to till teckensträngar
  edges_scenario <- edges_active |>
    dplyr::mutate(
      from = as.character(from),
      to   = as.character(to)
    )
  
  # Normaliserar källnoder till teckensträngar
  source_node_keys <- as.character(source_keys)
  
  # -------------------------------------------------------------------------- #
  # 7.2 b) Nåbarhetsanalys från matningspunkter                                #
  # -------------------------------------------------------------------------- #
  
  # Bygger en oriktad graf för analys av nåbarhet
  g_reachability <- igraph::graph_from_data_frame(edges_scenario, directed = FALSE)
  
  # Filtrerar fram källnoder som faktiskt finns i grafen
  sources_in_graph <- source_node_keys[source_node_keys %in% igraph::V(g_reachability)$name]
  if (length(sources_in_graph) == 0) stop("Ingen angiven källnod finns i scenariografen.")
  
  # -------------------------------------------------------------------------- #
  # 7.2 c) Skapande av artificiell virtuell startpunkt                         #
  # -------------------------------------------------------------------------- #
  
  # Definierar virtuell startpunkt för gemensam BFS och avståndsdefinition från flera matningspunkter
  virtual_source_key <- "__VIRTUAL_SOURCE__"
  if (virtual_source_key %in% igraph::V(g_reachability)$name) {
    virtual_source_key <- paste0(virtual_source_key, sample.int(1e9, 1))
  }
  
  # Kopplar den virtuella startpunkten till samtliga källnoder
  g_reachability_with_source <- g_reachability |>
    igraph::add_vertices(1, name = virtual_source_key) |>
    igraph::add_edges(
      as.vector(
        rbind(
          rep(virtual_source_key, length(sources_in_graph)),
          sources_in_graph
        )
      )
    )
  
  # -------------------------------------------------------------------------- #
  # 7.2 d) Nåbarhetsanalys och beräkning av topologiskt avstånd                #
  # -------------------------------------------------------------------------- #
  
  # Beräknar topologiskt avstånd från matningspunkterna
  bfs_result <- igraph::bfs(
    g_reachability_with_source,
    root = which(igraph::V(g_reachability_with_source)$name == virtual_source_key),
    dist = TRUE,
    parent = TRUE,
    unreachable = keep_unreachable
  )
  
  # Extraherar avstånd per nod och tar bort den virtuella startpunkten
  dist_from_source <- bfs_result$dist
  names(dist_from_source) <- igraph::V(g_reachability_with_source)$name
  dist_from_source <- dist_from_source[names(dist_from_source) != virtual_source_key]
  
  # -------------------------------------------------------------------------- #
  # 7.2 e) Urval av nåbara noder och kanter                                    #
  # -------------------------------------------------------------------------- #
  
  # Identifierar nåbara noder
  reachable_node_keys <- names(dist_from_source)[!is.na(dist_from_source) & dist_from_source >= 0]
  
  # Filtrerar fram kanter inom den nåbara delgrafen
  edges_reachable <- edges_scenario |>
    dplyr::filter(from %in% reachable_node_keys & to %in% reachable_node_keys)
  
  # Hanterar fallet där inga nåbara kanter finns
  if (nrow(edges_reachable) == 0) {
    g_empty <- igraph::make_empty_graph(directed = TRUE) |>
      igraph::add_vertices(length(reachable_node_keys), name = reachable_node_keys)
    
    # Returnerar tom flödesgraf
    return(list(
      graph_flow     = g_empty,
      nodes          = tibble::tibble(name = reachable_node_keys,
                                      dist = dist_from_source[reachable_node_keys]),
      edges_directed = tibble::tibble()
    ))
  }
  
  # -------------------------------------------------------------------------- #
  # 7.2 f) Bestämning av flödesriktning                                        #
  # -------------------------------------------------------------------------- #
  
  # Hämtar topologiskt avstånd till respektive kantände
  dist_from <- dist_from_source[edges_reachable$from]
  dist_to   <- dist_from_source[edges_reachable$to]
  
  # Bestämmer flödesriktning bort från matningspunkterna
  flow_from <- ifelse(dist_from <= dist_to, edges_reachable$from, edges_reachable$to)
  flow_to   <- ifelse(dist_from <= dist_to, edges_reachable$to,   edges_reachable$from)
  
  # Skapar riktad kantlista
  edges_directed <- edges_reachable
  edges_directed$from <- flow_from
  edges_directed$to   <- flow_to
  
  # Sparar flödesavstånd för respektive riktad kant
  edges_directed$FLOW_DIST_FROM <- pmin(dist_from, dist_to)
  edges_directed$FLOW_DIST_TO   <- pmax(dist_from, dist_to)
  
  # -------------------------------------------------------------------------- #
  # 7.2 g) Hantering av ringmatning (tie-kanter)                               #
  # -------------------------------------------------------------------------- #
  
  # Initierar alla kanter som icke ringmatning
  edges_directed$FLOW_TIE <- FALSE
  
  # Identifierar kanter där flödesriktning inte kan avgöras (samma avstånd)
  tie_edge_idx <- which(
    is.finite(dist_from) &
      is.finite(dist_to) &
      dist_from == dist_to
  )
  
  if (length(tie_edge_idx) > 0) {
    
    # Skapar omvänd flödesriktning för att hantera ringmatning
    edges_directed_rev <- edges_directed[tie_edge_idx, , drop = FALSE]
    
    # Vänder flödesriktningen för ringmatningskanter
    tmp_from <- edges_directed_rev$from
    edges_directed_rev$from <- edges_directed_rev$to
    edges_directed_rev$to   <- tmp_from
    
    # Märker omvända kanter som ringmatning
    edges_directed_rev$FLOW_TIE <- TRUE
    
    # Slår samman ursprungliga och omvända kanter
    edges_directed <- dplyr::bind_rows(edges_directed, edges_directed_rev)
  }
  
  # -------------------------------------------------------------------------- #
  # 7.2 h) Konstruktion av slutlig flödesgraf                                  #
  # -------------------------------------------------------------------------- #
  
  # Bygger riktad flödesgraf
  g_flow <- igraph::graph_from_data_frame(
    edges_directed,
    directed = TRUE,
    vertices = tibble::tibble(
      name = reachable_node_keys,
      dist = dist_from_source[reachable_node_keys]
    )
  )
  
  # Returnerar flödesgraf, nodinformation och riktade kanter
  list(
    graph_flow     = g_flow,
    nodes          = tibble::tibble(name = reachable_node_keys,
                                    dist = dist_from_source[reachable_node_keys]),
    edges_directed = edges_directed
  )
}