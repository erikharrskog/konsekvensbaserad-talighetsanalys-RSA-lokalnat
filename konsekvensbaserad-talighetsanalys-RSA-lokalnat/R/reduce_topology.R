# Fil:                reduce_topology.R
# Författare:         Erik Hårrskog
# Datum:              2026-05-08
#
# Innehåll:
#
# 5.1 Publik hjälpfunktion
#     a) Mappning från full nodrymd till reducerad nodrymd (map_to_reduced)
#
# 5.2 Lokal hjälpfunktion
#     a) Hitta närmaste KEEP-nod
#
# 5.3 Huvudfunktion: reduce_topology_graph
#     a) Förberedelser
#     b) Definiera noder som måste behållas
#     c) Kontrahera linjära nodsträckor mellan strukturellt viktiga noder
#     d) Reducera frånskiljar-kanter till KEEP-noder
#     e) Säkerställ att startnoden inte blir isolerad
#     f) Slutliga KEEP-noder = endpoints i reduced_edges
#     g) Bygg mapping mellan originalnoder och reducerad topologi
#     h) Retur av reducerad topologi

# ============================================================================ #
# 5.1) Publik hjälpfunktion                                                    #
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 5.1 a) Mappning från full nodrymd till reducerad nodrymd (map_to_reduced)    #
# ---------------------------------------------------------------------------- #

# Mappar noder från full till reducerad graf beroende på flagga
map_to_reduced <- function(base_topology, nodes, flags) {
  
  # Normaliserar nodnycklar
  nodes <- as.character(nodes)
  
  # Använder reducerad graf om flaggan är aktiverad samt om mapping finns
  if (isTRUE(flags$REDUCED) && !is.null(base_topology$node_rep_map)) {
    
    # Identifierar reducerad representation från node_rep_map
    nodes_reduced <- unname(base_topology$node_rep_map[nodes])
    
    # Behåll originalnod om mapping saknas eller är tom
    nodes_reduced[is.na(nodes_reduced) | nodes_reduced == ""] <- nodes[is.na(nodes_reduced) | nodes_reduced == ""]
    
    nodes_reduced
  } 
  # Om full-läge, returnera noderna oförändrade
  else {
    nodes
  }
}

# ============================================================================ #
# 5.2) Lokal hjälpfunktion                                                     #
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 5.2 a) Hitta närmaste KEEP-nod                                               #
# ---------------------------------------------------------------------------- #

# Mappar en originalnod till närmaste nod (grafavstånd) i reducerad graf
map_to_keep <- function(start_node, graph, keep_nodes) {
  
  # Normaliserar startnoden
  start_node <- as.character(start_node)
  
  # Avbryter och returnerar NA om startnoden saknas eller är ogiltig
  if (length(start_node) == 0 || is.na(start_node) || start_node == "") return(NA_character_)
  if (!(start_node %in% igraph::V(graph)$name)) return(NA_character_)
  
  # Om startnoden redan är en KEEP-nod returneras den direkt
  if (start_node %in% keep_nodes) return(start_node)
  
  # Initierar datastrukturer
  visited <- character(0)
  queue   <- start_node
  
  # Söker tills en KEEP-nod hittas
  while (length(queue) > 0) {
    
    # Hämtar nästa nod i kön och markerar den som besökt
    current_node <- queue[1]
    queue        <- queue[-1]
    visited      <- c(visited, current_node)
    
    # Itererar över alla grannoder
    neighbors <- igraph::neighbors(graph, current_node)$name
    for (neighbor in neighbors) {
      
      # Ignorerar redan besökta noder
      if (neighbor %in% visited) next
      
      # Avslutar om grannen är en KEEP-nod
      if (neighbor %in% keep_nodes) return(neighbor)
      
      # Lägger annars till grannen i kön för vidare sökning
      queue <- c(queue, neighbor)
    }
  }
  
  # Returnerar NA om ingen KEEP-nod hittas
  NA_character_
}

# ============================================================================ #
# 5.3) Huvudfunktion: reduce_topology_graph                                    #
# ============================================================================ #

# Definierar funktion "reduce_topology_graph" för reducering av nättopologi
reduce_topology_graph <- function(
    edges,
    disconnector_summary,
    start_node_key,
    transformer_nodes
) {
  
  # -------------------------------------------------------------------------- #
  # 5.3 a) Förberedelser                                                       #
  # -------------------------------------------------------------------------- #
  
  # Normaliserar kantdata
  edges <- edges |>
    dplyr::mutate(
      from  = as.character(from),
      to    = as.character(to),
      ID    = as.character(ID),
      SRC   = as.character(SRC)
    )
  
  # Separat lista med frånskiljarkanter
  disconnector_edges_full <- edges |>
    dplyr::filter(SRC == "FRÅNSKILJARE")
  
  # Normaliserar frånskiljarsammanställningen
  disconnector_summary <- disconnector_summary |>
    dplyr::mutate(
      a     = as.character(a),
      b     = as.character(b),
      ID    = as.character(ID),
      STATE = as.integer(STATE)
    )
  
  # Normaliserar data
  start_node_key    <- as.character(start_node_key)
  transformer_nodes <- as.character(transformer_nodes)
  
  # Bygger en oriktad graf från edge-listan
  graph_full <- igraph::graph_from_data_frame(
    edges |>
      dplyr::select(from, to),
    directed = FALSE
  )
  
  # Hämtar noduppsättning och gradtal
  node_names <- igraph::V(graph_full)$name
  node_degree        <- igraph::degree(graph_full)
  
  # Samtliga noder som tillhör frånskiljare (A/B-sidor)
  disconnector_nodes <- unique(c(
    as.character(disconnector_summary$a),
    as.character(disconnector_summary$b)
  ))
  
  # -------------------------------------------------------------------------- #
  # 5.3 b) Definiera noder som måste behållas                                  #
  # -------------------------------------------------------------------------- #

  # Identifierar de noder som måste bevaras vid reducering:
  # matningsnoden, frånskiljarnoder, transformatornoder samt förgreningar/slutpunkter
  keep_nodes <- unique(c(as.character(start_node_key), disconnector_nodes,
                         transformer_nodes, names(node_degree)[node_degree != 2]))
  
  # Begränsar KEEP-noder till de noder som ingår i grafen
  keep_nodes <- intersect(keep_nodes, node_names)
  
  # -------------------------------------------------------------------------- #
  # 5.3 c) Kontrahera linjära nodsträckor mellan strukturellt viktiga noder    #
  # -------------------------------------------------------------------------- #
  
  # Samlar nya reducerade kanter som ersätter mellanliggande nodsträckor
  reduced_other_edges <- list()
  
  # Spårar redan hanterade KEEP-nodpar för att undvika dubbletter
  seen_node_pairs <- new.env(parent = emptyenv())
  
  # För varje KEEP-nod: följ sammanhängande nodsträckor tills nästa KEEP-nod nås
  for (keep_node in keep_nodes) {
    
    # Startar från alla direkt angränsande noder
    neighbor_nodes <- igraph::neighbors(graph_full, keep_node)$name
    if (length(neighbor_nodes) == 0) next
    
    # Startar sökning längs nodsträckan med utgångspunkt i KEEP-noden
    for (neighbor_node in neighbor_nodes) {
      previous_node <- keep_node
      current_node  <- neighbor_node
      
      # Följ nodsträckan så länge vi befinner oss på noder som inte är KEEP-noder 
      while (!(current_node %in% keep_nodes)) {
        
        # Identifierar nästa nod i sträckan
        current_neighbors <- igraph::neighbors(graph_full, current_node)$name
        next_node         <- setdiff(current_neighbors, previous_node)
        
        # Avbryt om sträckan inte längre är linjär
        if (length(next_node) != 1) break
        
        # Går vidare till nästa nod i samma sträcka
        previous_node <- current_node
        current_node  <- next_node
      }
      
      # Skydd mot själv-loop om ingen giltig sträcka hittades
      if (current_node == keep_node) next
      
      # Fastställer ändpunkter för den reducerade kanten
      from_keep_node <- keep_node
      to_keep_node   <- current_node
      
      # Undvik att skapa samma reducerade koppling flera gånger
      node_pair_key <- paste(sort(c(from_keep_node, to_keep_node)), collapse = "|")
      if (exists(node_pair_key, envir = seen_node_pairs, inherits = FALSE)) next
      assign(node_pair_key, TRUE, envir = seen_node_pairs)
      
      # Skapar en reducerad kant som ersätter hela nodsträckan mellan två KEEP-noder
      reduced_other_edges[[length(reduced_other_edges) + 1]] <- data.frame(
        from  = from_keep_node,
        to    = to_keep_node,
        ID    = NA_character_,
        SRC   = "REDUCED",
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Tom mall som säkerställer konsekvent struktur
  empty_edge_tbl <- tibble::tibble(
    from  = character(),
    to    = character(),
    ID    = character(),
    SRC   = character()
  )
  
  # Bygger tabell med reducerade kanter på deterministiskt sätt
  reduced_linear_edges <- dplyr::bind_rows(reduced_other_edges)
  
  # Säkerställer konsekvent struktur även om inga reducerade kanter skapats
  if (nrow(reduced_linear_edges) == 0) {
    reduced_linear_edges <- empty_edge_tbl
  } else {
    reduced_linear_edges <- reduced_linear_edges |>
      dplyr::select(from, to, ID, SRC)
  }
  
  # -------------------------------------------------------------------------- #
  # 5.3 d) Reducera frånskiljar-kanter till KEEP-noder                         #
  # -------------------------------------------------------------------------- #
  
  # Mappar frånskiljar-kanter till den reducerade topologin genom koppling till KEEP-noder
  disconnector_edges_reduced <- disconnector_edges_full |>
    dplyr::rowwise() |>
    
    # Identifierar vilka KEEP-noder respektive ändpunkt hör till 
    dplyr::mutate(
      from_keep = map_to_keep(from, graph_full, keep_nodes),
      to_keep   = map_to_keep(to,   graph_full, keep_nodes)
    ) |>
    dplyr::ungroup() |>
    
    # Tar bort ogiltiga kopplingar och självkopplingar efter reducering
    dplyr::filter(
      !is.na(from_keep),
      !is.na(to_keep),
      from_keep != to_keep
    ) |>
    dplyr::transmute(
      from  = from_keep,
      to    = to_keep,
      ID    = ID,
      SRC   = "FRÅNSKILJARE"
    )
  
  # Slår ihop reducerade frånskiljar-kanter med övriga reducerade kanter
  reduced_edges <- dplyr::bind_rows(
    disconnector_edges_reduced,
    reduced_linear_edges
  ) |>
    dplyr::distinct(from, to, ID, SRC, .keep_all = TRUE)
  
  # -------------------------------------------------------------------------- #
  # 5.3 e) Säkerställ att startnoden inte blir isolerad                        #
  # -------------------------------------------------------------------------- #
  
  # Normaliserar startnodens nyckel
  start_key <- as.character(start_node_key)
  
  # Kontrollerar om startnoden saknar kopplingar i den reducerade grafen
  if (!any(reduced_edges$from == start_key | reduced_edges$to == start_key)) {
    
    # Hämtar startnodens närmaste noder i originalgrafen
    start_neighbors <- igraph::neighbors(graph_full, start_key)$name
    
    # Fortsätter endast om startnoden har grannar i originalgrafen
    if (length(start_neighbors) > 0) {
      
      # Startar från startnoden och följer en nodsträcka ut i nätet
      previous_node <- start_key
      current_node <- start_neighbors[1]
      
      # Följer nodsträckan tills en KEEP-nod nås eller sträckan inte är linjär
      while (!(current_node %in% keep_nodes)) {
        current_neighbors <- igraph::neighbors(graph_full, current_node)$name
        next_node   <- setdiff(current_neighbors, previous_node)
        if (length(next_node) != 1) break
        previous_node <- current_node
        current_node <- next_node
      }
      
      # Lägger till en reducerad kant så att startnoden kopplas in i den reducerade topologin
      reduced_edges <- dplyr::bind_rows(
        reduced_edges,
        data.frame(
          from  = start_key,
          to    = current_node,
          ID    = NA_character_,
          SRC   = "REDUCED",
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  # -------------------------------------------------------------------------- #
  # 5.3 f) Slutliga KEEP-noder = endpoints i reduced_edges                     #
  # -------------------------------------------------------------------------- #
  
  # Samlar alla noder som utgör ändpunkter i den reducerade grafen
  keep_nodes_final <- unique(c(
    start_key,
    as.character(keep_nodes),
    as.character(reduced_edges$from),
    as.character(reduced_edges$to)
  ))
  keep_nodes_final <- intersect(keep_nodes_final, node_names)
  
  # Bygger reducerad graf
  g_reduced <- igraph::graph_from_data_frame(
    reduced_edges |>
      dplyr::select(from, to),
    directed = FALSE,
    vertices = data.frame(name = keep_nodes_final, stringsAsFactors = FALSE)
  )
  
  # -------------------------------------------------------------------------- #
  # 5.3 g) Bygg mapping mellan originalnoder och reducerad topologi             #
  # -------------------------------------------------------------------------- #
  
  # Säkerställer att kund- och flödeslogik blir identisk i FULL och REDUCED
  node_rep_map <- vapply(
    node_names,
    map_to_keep,
    character(1),
    graph = graph_full,
    keep_nodes = keep_nodes_final
  )
  
  # Mappar till sig själv om noden redan finns i reduced
  node_rep_map[is.na(node_rep_map) & node_names %in% keep_nodes_final] <-
    node_names[is.na(node_rep_map) & node_names %in% keep_nodes_final]
  
  node_groups <- split(node_names, node_rep_map)
  
  # -------------------------------------------------------------------------- #
  # 5.3 h) Retur av reducerad topologi                                         #
  # -------------------------------------------------------------------------- #
  
  # Returnerar den reducerade topologin
  list(
    graph       = g_reduced,
    edges       = reduced_edges,
    keep_nodes  = keep_nodes_final,
    node_rep_map     = node_rep_map,
    node_groups = node_groups
  )
}