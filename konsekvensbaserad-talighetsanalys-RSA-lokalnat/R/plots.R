# Fil:                plots.R
# Författare:         Erik Hårrskog
# Datum:              2026-05-08
#
# Innehåll:
#
# 11.1 Interna hjälpfunktioner för plots
#     a) Spara aktuell plot till PNG
#     b) Preparering av graf-layout
#
# 11.2 Huvudfunktion: plot_candidates
#     a) Förberedelse av graf och layout
#     b) Identifiering av stängda frånskiljare
#     c) Grafiska nodattribut och etiketter
#     d) BOUNDARY: Rita kandidatnummer vid noden på subnätsidan
#     e) INTERNAL: Rita kandidatnummer vid noden
#     f) Rita topologigraf och exportera till PNG
#
# 11.3 Huvudfunktion: plot_scenarios
#     a) Förberedelse av graf och layout
#     b) Markering av frånskiljare
#     c) Initiering av nodattribut
#     d) Etikett för startnoden
#     e) Grafiska kantattribut
#     f) Rita topologigraf och exportera till PNG

# ============================================================================ #
# 11.1) Interna hjälpfunktioner för plots                                      #
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 11.1 a) Spara aktuell plot till PNG                                          #
# ---------------------------------------------------------------------------- #

# Sparar aktuell plot till PNG-fil
save_current_plot_png <- function(filename,
                                  width = 2700,
                                  height = 1700,
                                  res = 200) {
  
  # Avbryter om inget giltigt filnamn angivits
  if (is.null(filename) || !nzchar(filename)) {
    return(invisible(NULL))
  }
  
  # Säkerställer att målkatalogen finns
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  
  # Kör följande kod om ingen aktiv device finns
  if (dev.cur() == 1) {

    # Öppnar ny PNG-device och förväntar omritning av aktuell plot
    png(filename = filename, width = width, height = height, res = res)
    on.exit(dev.off(), add = TRUE)
    return(invisible("PNG_ONLY"))
  }
  
  # Kopierar aktuell grafisk device till PNG
  dev.copy(
    png,
    filename = filename,
    width = width,
    height = height,
    res = res
  )
  dev.off()
  
  # Returnerar status för sparad plot
  invisible("COPIED_FROM_SCREEN")
}

# ---------------------------------------------------------------------------- #
# 11.1 b) Preparering av graf-layout                                           #
# ---------------------------------------------------------------------------- #

# Förbereder graf, koordinater och startnod för visualisering
plot_prep_graph_layout <- function(GRAPH,
                                   start_x, start_y) {
  
  
  # Skapar en lokal kopia av grafen
  G <- GRAPH
  
  # Identifierar transformatorkanter i grafen
  edge_type <- toupper(trimws(as.character(igraph::E(G)$SRC)))
  transformer_edge_index <- which(edge_type == "TRANSFORMER")
  
  # Tar bort alla transformatorkanter från G
  if (length(transformer_edge_index) > 0) {
    G <- igraph::delete_edges(G, igraph::E(G)[transformer_edge_index])
    
    # Markerar noder som blir isolerade och tar bort dessa
    isolated_vertices <- igraph::degree(G, mode = "all") == 0
    if (any(isolated_vertices)) {
      G <- igraph::delete_vertices(G, igraph::V(G)[isolated_vertices])
    }
  }
  
  # Hämtar ut nodnycklar från grafen och rensar onödig info
  node_keys <- igraph::V(G)$name
  node_keys_xy <- sub("\\|.*$", "", node_keys)
  
  # Hämtar ut koordinater för nodnycklarna och ersätter trasiga värden
  coords <- t(vapply(node_keys_xy, key_to_xy, numeric(2)))
  valid_coords <- is.finite(coords[,1]) & is.finite(coords[,2])
  
  # Ersätter ogiltiga koordinater med nollposition
  if (!all(valid_coords)) coords[!valid_coords, ] <- 0
  
  # Roterar och spegelvänder grafen för att stämma överens med verkligheten
  coords[,1] <- -coords[,1]
  coords <- cbind(coords[,2], -coords[,1])
  
  # Identifierar vilken nod som är startnod
  is_start <- (node_keys == make_key(start_x, start_y))
  
  # Returnerar de förberedda objekten för visualisering
  list(
    G = G,
    node_keys = node_keys,
    coords = coords,
    is_start = is_start
  )
}

# ============================================================================ #
# 11.2) Huvudfunktion: plot_candidates                                         #
# ============================================================================ #

# Definierar funktion "plot_candidates" som plottar grundgrafen med kandidater
plot_candidates <- function(GRAPH, start_x, start_y,
                       disconnector_summary,
                       boundary_candidates = NULL,
                       internal_candidates = NULL,
                       main_title = "Grundnät med BOUNDARY- och INTERNAL-kandidater",
                       flags,
                       plot_filename = NULL) {
  
  # -------------------------------------------------------------------------- #
  # 11.2 a) Förberedelse av graf och layout                                    #
  # -------------------------------------------------------------------------- #
  
  # Förbereder graf, noder och koordinater för visualisering
  prep <- plot_prep_graph_layout(GRAPH, start_x, start_y)
  
  # Tilldelar förberedelserna till variabler
  G <- prep$G
  node_keys <- prep$node_keys
  coords <- prep$coords
  is_start <- prep$is_start
  
  # -------------------------------------------------------------------------- #
  # 11.2 b) Identifiering av stängda frånskiljare                              #
  # -------------------------------------------------------------------------- #
  
  # Initierar tom lista för noder med stängda frånskiljare
  closed_node_index <- integer(0)
  
  # Kör följande kod om det finns frånskiljardata
  if (nrow(disconnector_summary) > 0) {
    
    # Identifierar frånskiljare som är stängda i scenariot
    closed_rows <- disconnector_summary[
      !is.na(disconnector_summary$STATE) & disconnector_summary$STATE == 1,
      , drop = FALSE
    ]
    # Kör följande kod om det finns stängda frånskiljare
    if (nrow(closed_rows) > 0) {
      closed_keys <- unique(as.character(closed_rows$a))
      
      # Mappar stängda frånskiljare förutom startnoden till nodindex i grafen
      closed_node_index <- which(node_keys %in% closed_keys)
      closed_node_index <- setdiff(closed_node_index, which(is_start))
    }
  }
  
  # -------------------------------------------------------------------------- #
  # 11.2 c) Grafiska nodattribut och etiketter                                 #
  # -------------------------------------------------------------------------- #
  
  # Intierar grafiska attribut för alla noder
  n <- igraph::vcount(G)
  node_size  <- rep(0, n)
  node_color <- rep(NA, n)
  node_shape <- rep("none", n)
  
  # Markerar startnoden
  node_size[is_start]  <- 4
  node_color[is_start] <- "black"
  node_shape[is_start] <- "circle"
  
  # Markerar stängda frånskiljare
  node_size[closed_node_index]  <- 2
  node_color[closed_node_index] <- "grey50"
  node_shape[closed_node_index] <- "circle"

  # Sätter etikett för startnoden
  vertex_label <- rep(NA_character_, n)
  vertex_label[is_start] <- "START"
  
  # Justerar etikettens placering
  vertex_label_dist <- rep(1, n)
  vertex_label_angle <- rep(0, n)
  vertex_label_dist[is_start] <- 1
  vertex_label_angle[is_start] <- 3*pi/2
  
  # Sätter färg på etiketten
  vertex_label_color <- rep(NA, n)
  vertex_label_color[is_start] <- "black"
  
  # -------------------------------------------------------------------------- #
  # 11.2 d) BOUNDARY: Rita kandidatnummer vid noden på subnätsidan             #
  # -------------------------------------------------------------------------- #
  
  # Kör följande kod om det finns BOUNDARY-kandidater att visualisera
  if (!is.null(boundary_candidates) && nrow(boundary_candidates) > 0) {
    
    # Identifierar noden på subnätssidan för varje BOUNDARY-kandidat
    cand <- boundary_candidates |>
      dplyr::mutate(
        plot_node = ifelse(a_in, a, b),
        plot_node = trimws(as.character(plot_node))
      )
    
    # Mappar BOUNDARY-kandidater till nodindex i grafen
    node_keys_trim <- trimws(as.character(node_keys))
    plot_node_index <- match(cand$plot_node, node_keys_trim)
    valid_node_index <- !is.na(plot_node_index)
    
    # Markerar BOUNDARY-kandidater grafiskt och visar kandidatnummer
    if (any(valid_node_index)) {
      node_size[plot_node_index[valid_node_index]]  <- 4
      node_color[plot_node_index[valid_node_index]] <- "dodgerblue"
      node_shape[plot_node_index[valid_node_index]] <- "circle"
      
      vertex_label[plot_node_index[valid_node_index]] <- as.character(cand$candidate_no[valid_node_index])
      vertex_label_color[plot_node_index[valid_node_index]] <- "dodgerblue"
    }
  }
  
  # Initierar och identifierar högsta kandidatnummer för BOUNDARY-alternativ
  boundary_max_no <- 0
  if (!is.null(boundary_candidates) && nrow(boundary_candidates) > 0) {
    boundary_max_no <- max(boundary_candidates$candidate_no, na.rm = TRUE)
  }
  
  # -------------------------------------------------------------------------- #
  # 11.2 e) INTERNAL: Rita kandidatnummer vid noden                            #
  # -------------------------------------------------------------------------- #
  
  # Kör följande kod om det finns INTERNAL-kandidater att visualisera
  if (!is.null(internal_candidates) && nrow(internal_candidates) > 0) {
    
    # Identifierar noden för varje INTERNAL-kandidat
    internal_plot_candidates <- internal_candidates |>
      dplyr::mutate(
        plot_node = trimws(as.character(a))
      )
    
    # Mappar INTERNAL-kandidater till nodindex i grafen
    node_keys_trim <- trimws(as.character(node_keys))
    plot_node_index  <- match(internal_plot_candidates$plot_node, node_keys_trim)
    valid_node_index <- !is.na(plot_node_index)
    
    # Markerar INTERNAL-kandidater grafiskt och visar kandidatnummer
    if (any(valid_node_index)) {
      node_size[plot_node_index[valid_node_index]]  <- 4
      node_color[plot_node_index[valid_node_index]] <- "darkorange"
      node_shape[plot_node_index[valid_node_index]] <- "circle"
      
      # Behåller eventuella BOUNDARY-etiketter och sätter INTERNAL-etiketter där det är tomt
      empty_label <- is.na(vertex_label[plot_node_index[valid_node_index]])
      if (any(empty_label)) {
        internal_candidate_no <- boundary_max_no + internal_plot_candidates$candidate_no
        vertex_label[plot_node_index[valid_node_index][empty_label]] <-
          as.character(internal_candidate_no[valid_node_index][empty_label])
        vertex_label_color[plot_node_index[valid_node_index][empty_label]] <- "darkorange"
      }
    }
  }
  
  # -------------------------------------------------------------------------- #
  # 11.2 f) Rita topologigraf och exportera till PNG                           #
  # -------------------------------------------------------------------------- #
  
  # Ritar topologigrafen
  plot(
    G,
    layout = coords,
    vertex.size = node_size,
    vertex.shape = node_shape,
    vertex.color = node_color,
    vertex.frame.color = NA,
    vertex.label = vertex_label,
    vertex.label.color = vertex_label_color,
    vertex.label.dist = vertex_label_dist,
    vertex.label.degree = vertex_label_angle,
    edge.color = "grey70",
    edge.width = 2,
    cex = 0.7,
    main = main_title
  )
  
  # Ritar legend för nod- och kandidattyper
  legend(
    "topright",
    legend = c(
      "Startnod",
      "Stängd frånskiljare",
      "BOUNDARY-kandidat",
      "INTERNAL-kandidat"
    ),
    pch = 21,
    pt.cex = c(1.4, 1.0, 1.2, 1.2),
    pt.bg = c(
      "black",        # Startnod
      "gray50",       # Stängd frånskiljare
      "dodgerblue",   # BOUNDARY
      "darkorange"    # INTERNAL
    ),
    col = NA,
    bty = "n",
    cex = 1.2
  )
  
  # Exporterar plot till PNG om export är aktiverad
  if (isTRUE(flags$EXPORT_PLOTS) && !is.null(plot_filename)) {
    save_current_plot_png(plot_filename)
  }
}

# ============================================================================ #
# 11.3) Huvudfunktion: plot_scenarios                                          #
# ============================================================================ #

# Definierar funktion "plot_scenarios" som plottar flödesgraf
plot_scenarios <- function(GRAPH_LAYOUT,
                            start_x, start_y,
                            main_title = "Flödesgraf",
                            energized_nodes,
                            alt_supply_nodes = character(0),
                            failed_disconnector_id = NULL,
                            opened_disconnector_ids = character(0),
                            disconnector_summary = NULL,
                            feeding_disconnector_ids = character(0),
                            fault_zone_nodes = character(0),
                            disconnected_transformer_nodes = character(0),
                            flags,
                            plot_filename = NULL) {
  
  # -------------------------------------------------------------------------- #
  # 11.3 a) Förberedelse av graf och layout                                    #
  # -------------------------------------------------------------------------- #

  # Förbereder graf, noder och koordinater för visualisering
  prep <- plot_prep_graph_layout(GRAPH_LAYOUT, start_x, start_y)
  
  # Tilldelar förberedelserna till variabler
  G <- prep$G
  node_keys <- prep$node_keys
  coords <- prep$coords
  is_start <- prep$is_start
  
  # Normaliserar indata för energiserade och alternativa noder
  energized_nodes <- as.character(energized_nodes)
  alt_supply_nodes <- intersect(as.character(alt_supply_nodes), energized_nodes)
  fault_zone_nodes <- as.character(fault_zone_nodes)
  
  # -------------------------------------------------------------------------- #
  # 11.3 b) Markering av frånskiljare                                          #
  # -------------------------------------------------------------------------- #
  
  # Initierar index för felande och matande frånskiljare
  primary_disconnector_index <- integer(0)
  feed_node_index <- integer(0)
  secondary_disconnector_index <- integer(0)
  
  # Identifierar och markerar felande frånskiljare
  if (!is.null(failed_disconnector_id) && !is.null(disconnector_summary)) {
    
    # Identifierar felande frånskiljare
    fail_rows <- disconnector_summary[
      as.character(disconnector_summary$ID) == as.character(failed_disconnector_id),
      , drop = FALSE
    ]
    
    # Markerar felande frånskiljare
    if (nrow(fail_rows) > 0) {
      fail_nodes <- unique(c(as.character(fail_rows$a),
                             as.character(fail_rows$b)))
      primary_disconnector_index <- which(node_keys %in% fail_nodes)
      primary_disconnector_index <- setdiff(primary_disconnector_index, which(is_start))
    }
  }
  
  # Identifierar och markerar matande frånskiljare
  if (!is.null(disconnector_summary) && length(feeding_disconnector_ids) > 0) {
    
    # Identifierar matande frånskiljare
    feed_rows <- disconnector_summary[
      as.character(disconnector_summary$ID) %in% as.character(feeding_disconnector_ids),
      , drop = FALSE
    ]
    
    # Markerar matande frånskiljare
    if (nrow(feed_rows) > 0) {
      feed_nodes <- unique(c(as.character(feed_rows$a),
                             as.character(feed_rows$b)))
      feed_node_index <- which(node_keys %in% feed_nodes)
      feed_node_index <- setdiff(feed_node_index, which(is_start))
    }
  }
  
  # Markerar öppna frånskiljare som ingår i scenariot
  if (!is.null(disconnector_summary) && length(opened_disconnector_ids) > 0) {
    
    # Väljer ut de rader som motsvarar öppna frånskiljare
    opened_rows <- disconnector_summary[
      as.character(disconnector_summary$ID) %in% as.character(opened_disconnector_ids),
      , drop = FALSE
    ]
    
    # Om det finns öppna frånskiljare körs denna kod
    if (nrow(opened_rows) > 0) {
      # Samlar nodnycklar, identifierar index och exkluderar startnoden
      opened_nodes <- unique(c(as.character(opened_rows$a),
                               as.character(opened_rows$b)))
      secondary_disconnector_index <- which(node_keys %in% opened_nodes)
      secondary_disconnector_index <- setdiff(secondary_disconnector_index, which(is_start))
    }
  }
  
  # Identifierar noder som tillhör frånkopplade transformatorer
  disconnected_tr_index <- which(node_keys %in% as.character(disconnected_transformer_nodes))
  
  # -------------------------------------------------------------------------- #
  # 11.3 c) Initiering av nodattribut                                          #
  # -------------------------------------------------------------------------- #
  
  # Skapar vektorer för grafiska nodattribut för grafens olika objekt
  node_size  <- rep(0, igraph::vcount(G))
  node_color <- rep(NA, igraph::vcount(G))
  node_shape <- rep("none", igraph::vcount(G))
  
  # Definierar grafiska nodattribut för startnoden
  node_size[is_start]  <- 4
  node_color[is_start] <- "black"
  node_shape[is_start] <- "circle"
  
  # Definierar grafiska nodattribut för sekundära frånskiljare
  node_size[secondary_disconnector_index]  <- 3
  node_color[secondary_disconnector_index] <- "red"
  node_shape[secondary_disconnector_index] <- "circle"
  
  # Definierar grafiska nodattribut för primär frånskiljare
  node_size[primary_disconnector_index]  <- 4
  node_color[primary_disconnector_index] <- "red"
  node_shape[primary_disconnector_index] <- "circle"
  
  # Definierar grafiska nodattribut för matande frånskiljare
  node_size[feed_node_index]  <- 4
  node_color[feed_node_index] <- "green3"
  node_shape[feed_node_index] <- "circle"
  
  # Definierar grafiska nodattribut för bortkopplade transformatorer (efter alt-matning)
  node_size[disconnected_tr_index]  <- 4
  node_color[disconnected_tr_index] <- "purple"
  node_shape[disconnected_tr_index] <- "circle"
  
  # -------------------------------------------------------------------------- #
  # 11.3 d) Etikett för startnoden                                             #
  # -------------------------------------------------------------------------- #
  
  # Sätter etikett för startnoden
  vertex_label <- rep(NA_character_, igraph::vcount(G))
  vertex_label[is_start] <- "START"
  
  # Justerar etikettens placering
  vertex_label_dist <- rep(1, igraph::vcount(G))
  vertex_label_angle <- rep(0, igraph::vcount(G))
  
  vertex_label_dist[is_start] <- 1
  vertex_label_angle[is_start] <- 3*pi/2
  
  # Sätter färg på etiketten
  vertex_label_color <- rep(NA, igraph::vcount(G))
  vertex_label_color[is_start] <- "black"
  
  # -------------------------------------------------------------------------- #
  # 11.3 e) Grafiska kantattribut                                              #
  # -------------------------------------------------------------------------- #
  
  # Hämtar grundläggande kantinformation
  base_edges <- igraph::as_data_frame(G, what = "edges")
  edge_from_nodes <- as.character(base_edges$from)
  edge_to_nodes   <- as.character(base_edges$to)
  
  # Definierar grafiska kantattribut för grundnätet
  edge_color <- rep("grey50", nrow(base_edges))
  edge_width <- rep(2, nrow(base_edges))
  
  # Identifierar om kantens ändpunkter är energiserade
  from_node_is_energized <- edge_from_nodes %in% energized_nodes
  to_node_is_energized   <- edge_to_nodes   %in% energized_nodes
  
  # Definierar grafiska kantattribut för alternativ matning
  alt_supply_edge_index <- edge_from_nodes %in% alt_supply_nodes &
    edge_to_nodes   %in% alt_supply_nodes
  edge_color[alt_supply_edge_index] <- "green3"
  edge_width[alt_supply_edge_index] <- 3
  
  # Identifierar kanter där båda ändpunkter är strömlösa
  both_deenergized <- (!from_node_is_energized & !to_node_is_energized)
  
  # Identifierar kanter där båda ändpunkter ligger i felzonen
  both_in_fault_zone <- (edge_from_nodes %in% fault_zone_nodes) &
    (edge_to_nodes   %in% fault_zone_nodes)
  
  # Indirekt: strömlösa kanter som inte ligger helt inom felzonen
  edge_color[both_deenergized & !both_in_fault_zone] <- "darkorange"
  edge_color[both_deenergized &  both_in_fault_zone] <- "red"
  
  # -------------------------------------------------------------------------- #
  # 11.3 f) Rita topologigraf och exportera till PNG                           #
  # -------------------------------------------------------------------------- #
  
  # Plottar flödesgrafen
  plot(
    G,
    layout = coords,
    vertex.size = node_size,
    vertex.shape = node_shape,
    vertex.color = node_color,
    vertex.frame.color = NA,
    vertex.label = vertex_label,
    vertex.label.color = vertex_label_color,
    vertex.label.dist = vertex_label_dist,
    vertex.label.degree = vertex_label_angle,
    edge.color = edge_color,
    edge.width = edge_width,
    edge.arrow.size = 0,
    edge.arrow.width = 0,
    cex = 0.7,
    main = main_title
  )
  legend(
    "topright",
    legend = c(
      "Startnod",
      "Primär frånskiljare",
      "Sekundär frånskiljare",
      "Matande frånskiljare",
      "Bortkopplad transformator",
      "Alternativ matning",
      "Felzon",
      "Isolerad zon",
      "Normalt nät"
    ),
    pch = c(21, 21, 21, 21, 21, NA, NA, NA, NA),
    pt.cex = c(1.4, 1.3, 1.1, 1.3, 1.3, NA, NA, NA, NA),
    pt.bg = c(
      "black",    # Start
      "red",      # Primär Frånskiljare
      "red",      # Sekundär Frånskiljare
      "green3",   # Matande Frånskiljare
      "purple",   # Transformator
      NA, NA, NA, NA
    ),
    lty = c(
      NA, NA, NA, NA, NA,
      1, 1, 1, 1
    ),
    lwd = c(
      NA, NA, NA, NA, NA,
      3, 2, 2, 2
    ),
    col = c(
      NA, NA, NA, NA, NA,
      "green3",   # Alternativ matning
      "red",      # Felzon
      "darkorange", # Indirekt strömlös
      "grey50"    # Normalt nät
    ),
    bty = "n",
    cex = 1.2
  )
  
  if (isTRUE(flags$EXPORT_PLOTS) && !is.null(plot_filename)) {
    save_current_plot_png(plot_filename)
  }
}
