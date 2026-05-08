# Fil:                resupply.R
# Författare:         Erik Hårrskog
# Datum:              2026-05-08
#
# Innehåll:
#
# 10.1 Publika hjälpfunktioner
#     a) Skapande av matningskandidater (build_resupply_candidates)
#     b) Exkludering av BOUNDARY (apply_boundary_user_exclusion)
#     c) Exkludering av INTERNAL (apply_internal_user_exclusion)
#     d) Klassning av öppna frånskiljare (classify_open_disconnectors)
#
# 10.2 Huvudfunktion: evaluate_resupply_options
#     a) Bygg simulerad nättopologi
#     b) Begränsa analysen till subnätet
#     c) Basfall: matning endast från ordinarie startpunkt
#     d) Identifiera felzon
#     e) Identifiera och filtrera BOUNDARY-kandidater
#     f) Utvärdera BOUNDARY-alternativ
#     g) Identifiera och filtrera INTERNAL-kandidater
#     h) Utvärdera INTERNAL-alternativ
#     i) Val av bästa återställningsalternativ
#     j) Sammanställ och returnera resultat

# ============================================================================ #
# 10.1) Publika hjälpfunktioner                                                #
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 10.1 a) Skapande av matningskandidater (build_resupply_candidates)           #
# ---------------------------------------------------------------------------- #

# Identifierar alternativa interna och externa matningskandidater
build_resupply_candidates <- function(base_topology, flags) {
  
  # Identifierar noder som ingår i grundsubnätet
  subnet_nodes <- as.character(base_topology$keep_nodes_base)
  
  # Identifierar frånskiljare som är stängda i grundtopologin
  disconnector_ids_base <- unique(
    base_topology$all_edges_full$ID[
      base_topology$all_edges_full$SRC == "FRÅNSKILJARE"
    ]
  ) |> as.character()
  
  # Kör följande kod om REDUCED-flaggan är aktiverad
  if (isTRUE(flags$REDUCED)) {
    
    # Hämtar normalt öppnade frånskiljare
    ds_open <- classify_open_disconnectors(base_topology, flags)
    
    # Identifierar gränskandidater i REDUCED-läge
    boundary_candidates <- ds_open |>
      dplyr::filter(is_boundary) |>
      dplyr::arrange(ID) |>
      dplyr::mutate(candidate_no = dplyr::row_number()) |>
      dplyr::left_join(
        base_topology$disconnector_summary |>
          dplyr::mutate(
            ID = as.character(ID),
            a  = as.character(a),
            b  = as.character(b)
          ),
        by = "ID"
      ) |>
      dplyr::transmute(
        ID,
        a = dplyr::if_else(a %in% names(base_topology$node_rep_map), a, b),
        b = dplyr::if_else(a %in% names(base_topology$node_rep_map), b, a),
        open_in_base = TRUE,
        a_in = TRUE,
        b_in = FALSE,
        candidate_no
      )
    
    # Identifierar interna kandidater i REDUCED-läge
    internal_candidates <- ds_open |>
      dplyr::filter(is_internal) |>
      dplyr::arrange(ID) |>
      dplyr::mutate(candidate_no = dplyr::row_number()) |>
      dplyr::transmute(
        ID,
        a = map_to_reduced(base_topology, disconnector_base_key, flags),
        b = map_to_reduced(base_topology, disconnector_base_key, flags),
        open_in_base = TRUE,
        a_in = TRUE,
        b_in = TRUE,
        candidate_no
      )
  } 
  # Annars körs följande kod
  else {
    
    # Identifierar gränskandidater i FULL-läge
    boundary_candidates <- base_topology$disconnector_summary |>
      dplyr::mutate(
        ID = as.character(ID),
        a  = as.character(a),
        b  = as.character(b),
        open_in_base = !(ID %in% disconnector_ids_base),
        a_in = a %in% subnet_nodes,
        b_in = b %in% subnet_nodes
      ) |>
      dplyr::filter(open_in_base, xor(a_in, b_in)) |>
      dplyr::arrange(ID) |>
      dplyr::mutate(candidate_no = dplyr::row_number())
    
    # Identifierar interna kandidater i FULL-läge
    internal_candidates <- base_topology$disconnector_summary |>
      dplyr::mutate(
        ID = as.character(ID),
        a  = as.character(a),
        b  = as.character(b),
        open_in_base = !(ID %in% disconnector_ids_base),
        a_in = a %in% subnet_nodes,
        b_in = b %in% subnet_nodes
      ) |>
      dplyr::filter(open_in_base, a_in, b_in) |>
      dplyr::arrange(ID) |>
      dplyr::mutate(candidate_no = dplyr::row_number())
  }
  
  # Returnerar alternativa matningskandidater
  list(
    boundary_candidates = boundary_candidates,
    internal_candidates = internal_candidates
  )
}

# ---------------------------------------------------------------------------- #
# 10.1 b) Exkludering av BOUNDARY (apply_boundary_user_exclusion)              #
# ---------------------------------------------------------------------------- #

# Tillämpar användarstyrd exkludering av externa matningskandidater
apply_boundary_user_exclusion <- function(boundary_candidates, disconnector_summary, flags) {
  
  # Avbryter direkt och returnerar alla kandidater om ASK_EXCLUDE = FALSE
  if (!isTRUE(flags$ASK_EXCLUDE) || nrow(boundary_candidates) == 0) {
    return(
      boundary_candidates |>
        dplyr::mutate(user_excluded = FALSE)
    )
  }
  
  # Hämtar etiketter för boundary-kandidater
  labels <- disconnector_summary$LABEL[
    match(boundary_candidates$ID, disconnector_summary$ID)
  ]
  
  # Skapar valalternativ för användarinteraktion
  choices <- sprintf("Frånskiljare %s", crayon::bold(labels))
  
  # Hämtar användarens val av exkluderade boundary-kandidater
  selected <- utils::select.list(choices, multiple = TRUE,
    title = "\nVälj frånskiljare (BOUNDARY) som INTE kan användas som alternativ matning")
  
  # Identifierar valda kandidater i ursprunglig ordning
  excluded_index <- match(selected, choices)
  
  # Identifierar frånskiljare som exkluderats av användaren
  excluded_ids <- boundary_candidates$ID[excluded_index]
  excluded_ids <- excluded_ids[!is.na(excluded_ids)]
  
  # Märker boundary-kandidater som exkluderats enligt val
  boundary_candidates_marked <- boundary_candidates |>
    dplyr::mutate(user_excluded = ID %in% excluded_ids)
  
  # Identifierar etiketter för frånskiljare som exkluderats av användaren
  excluded_labels <- disconnector_summary$LABEL[
    match(excluded_ids, disconnector_summary$ID)
  ]
  
  # Loggar användarens exkluderingsval
  if (length(excluded_ids) == 0) {
    say(
      "Alternativa matningar (BOUNDARY) exkluderade av användaren: %s\n",
      crayon::bold("inga")
    )
  } else {
    say(
      "Alternativa matningar (BOUNDARY) exkluderade av användaren: %s\n",
      crayon::bold(paste(excluded_labels, collapse = ", "))
    )
  }
  # Returnerar uppdaterade boundary-kandidater
  boundary_candidates_marked
}

# ---------------------------------------------------------------------------- #
# 10.1 c) Exkludering av INTERNAL (apply_internal_user_exclusion)              #
# ---------------------------------------------------------------------------- #

# Tillämpar användarstyrd exkludering av interna matningskandidater
apply_internal_user_exclusion <- function(
    internal_candidates,
    boundary_candidates,
    disconnector_summary,
    flags
) {
  
  # Avbryter direkt och returnerar alla kandidater om ASK_EXCLUDE = FALSE
  if (!isTRUE(flags$ASK_EXCLUDE) || nrow(internal_candidates) == 0) {
    return(
      internal_candidates |>
        dplyr::mutate(user_excluded = FALSE)
    )
  }
  
  # Identifierar högsta kandidatnummer för boundary-kandidater
  boundary_max_no <- if (nrow(boundary_candidates) > 0) {
    max(boundary_candidates$candidate_no, na.rm = TRUE)
  } else {
    0
  }
  
  # Beräknar löpnummer för interna kandidater fortsättande efter boundary
  internal_numbers <- boundary_max_no + internal_candidates$candidate_no
  
  # Hämtar etiketter för interna kandidater
  labels <- disconnector_summary$LABEL[
    match(internal_candidates$ID, disconnector_summary$ID)
  ]
  
  # Skapar valalternativ för användarinteraktion
  choices <- sprintf(
    "Nr %d – Intern frånskiljare %s",
    internal_numbers,
    crayon::bold(labels)
  )
  
  # Hämtar användarens val av exkluderade interna kandidater
  selected <- utils::select.list(
    choices,
    multiple = TRUE,
    title = "\nVälj interna frånskiljare (INTERNAL) som INTE kan användas"
  )
  
  # Identifierar valda kandidater i ursprunglig ordning
  selected_index <- match(selected, choices)
  
  # Identifierar frånskiljare som exkluderats av användaren
  excluded_ids <- if (length(selected_index) == 0) {
    character(0)
  } else {
    internal_candidates$ID[selected_index]
  }
  
  # Märker interna kandidater som exkluderats enligt användarval
  internal_candidates_marked <- internal_candidates |>
    dplyr::mutate(user_excluded = ID %in% excluded_ids)
  
  # Identifierar etiketter för frånskiljare som exkluderats av användaren
  excluded_labels <- disconnector_summary$LABEL[
    match(excluded_ids, disconnector_summary$ID)
  ]
  
  # Loggar användarens exkluderingsval
  if (length(excluded_ids) == 0) {
    say(
      "Alternativa matningar (INTERNAL) exkluderade av användaren: %s\n",
      crayon::bold("inga")
    )
  } else {
    say(
      "Alternativa matningar (INTERNAL) exkluderade av användaren: %s\n",
      crayon::bold(paste(excluded_labels, collapse = ", "))
    )
  }
  
  # Returnerar uppdaterade interna kandidater
  internal_candidates_marked
}

# ---------------------------------------------------------------------------- #
# 10.1 d) Klassning av öppna frånskiljare (classify_open_disconnectors)        #
# ---------------------------------------------------------------------------- #

# Klassificerar normalt öppna frånskiljare relativt subnätets startkomponent
classify_open_disconnectors <- function(base_topology, flags) {
  
  # Identifierar startnod för subnätet
  start_node <- as.character(base_topology$start_node_key)
  
  # Bygger graf från hela subnätets alla kanter
  edges_full <- base_topology$all_edges_full |> dplyr::mutate(from = as.character(from), to = as.character(to))
  
  # Bygger oriktad graf för komponentanalys
  g_full <- igraph::graph_from_data_frame(edges_full |> dplyr::select(from, to), directed = FALSE)
  
  # Beräknar sammanhängande komponenter för subnätet
  all_nodes <- igraph::V(g_full)$name
  components <- igraph::components(g_full)
  node_component_map <- components$membership
  names(node_component_map) <- all_nodes
  
  # Säkerställer att startnoden kan mappas till en komponent
  if (!start_node %in% names(node_component_map)) {
    if (isTRUE(flags$REDUCED)) {
      start_node_mapped <- unname(base_topology$node_rep_map[start_node])
      if (!is.na(start_node_mapped) && start_node_mapped %in% names(node_component_map)) {
        start_node <- start_node_mapped
      }
    }
  }
  if (!start_node %in% names(node_component_map)) {
    stop(
      "classify_open_disconnectors: start_node_key finns inte i g_full (varken direkt eller via node_rep_map)."
    )
  }
  
  # Identifierar komponenten som innehåller startnoden
  start_component <- node_component_map[[start_node]]
  
  # Bygger tabell med öppna frånskiljare och deras komponenttillhörighet
  classified_open_disconnectors <- base_topology$disconnector_summary |>
    dplyr::mutate(
      ID          = as.character(ID),
      STATE       = as.integer(STATE),
      a_original  = as.character(a),
      b_original  = as.character(b)
    ) |>
    dplyr::distinct(ID, a_original, b_original, STATE) |>
    dplyr::filter(STATE != 1) |>
    dplyr::mutate(
      disconnector_base_key =
        sub("(\\n[AB]$)", "", sub("(\\|[AB]$)", "", a_original)),
      nodeA_fallback = paste0(disconnector_base_key, "|A"),
      nodeB_fallback = paste0(disconnector_base_key, "|B"),
      
      disconnector_node_a =
        dplyr::if_else(a_original %in% names(node_component_map), a_original, nodeA_fallback),
      disconnector_node_b =
        dplyr::if_else(b_original %in% names(node_component_map), b_original, nodeB_fallback),
      
      component_a = unname(node_component_map[disconnector_node_a]),
      component_b = unname(node_component_map[disconnector_node_b]),
      
      # Klassar öppna frånskiljare relativt startkomponenten
      is_boundary = (!is.na(component_a) & !is.na(component_b)) &
        xor(component_a == start_component, component_b == start_component),
      is_internal = (!is.na(component_a) & !is.na(component_b)) &
        (component_a == start_component & component_b == start_component),
      is_outside  = (!is.na(component_a) & !is.na(component_b)) &
        (component_a != start_component & component_b != start_component)
    )
  
  # Returnerar klassade öppna frånskiljare
  classified_open_disconnectors
}

# ============================================================================ #
# 10.2) Huvudfunktion: evaluate_resupply_options                               #
# ============================================================================ #

# Definierar funktion "evaluate_resupply_options" som utvärderar alternativa matningsvägar
evaluate_resupply_options <- function(base_topology,
                                      open_disconnector_ids,
                                      fault_seed_id = NULL,
                                      top_n = 5L,
                                      inputs,
                                      flags,
                                      valid_boundary_candidates,
                                      valid_internal_candidates) {
  
  # -------------------------------------------------------------------------- #
  # 10.2 a) Bygg simulerad nättopologi                                         #
  # -------------------------------------------------------------------------- #
  
  # Identifierar scenariots topologi efter bortkoppling
  outage_scenario <- build_scenario_topology(
    base_topology,
    open_disconnector_ids = open_disconnector_ids,
    inputs = inputs,
    flags = flags
  )
  
  # Hämtar scenariots kantlista
  scenario_edges_all <- outage_scenario$edges_scenario_undirected
  
  # -------------------------------------------------------------------------- #
  # 10.2 b) Begränsa analysen till subnätet                                    #
  # -------------------------------------------------------------------------- #
  
  # Begränsar analysen till relevant nodrymd (FULL vs REDUCED)
  subnet_nodes <- if (isTRUE(flags$REDUCED)) {
    unique(c(
      as.character(scenario_edges_all$from),
      as.character(scenario_edges_all$to)
    ))
  } else {
    as.character(base_topology$keep_nodes_base)
  }
  
  # Filtrerar scenariots kantlista till noder som ingår i subnätet
  scenario_edges_subnet <- scenario_edges_all |>
    dplyr::mutate(
      from = as.character(from),
      to   = as.character(to)
    ) |>
    dplyr::filter(from %in% subnet_nodes & to %in% subnet_nodes)
  
  # Normaliserar startnodens nyckel till aktuell nodrymd
  start_key_edges <- map_to_reduced(base_topology, base_topology$start_node_key, flags)
  
  # Säkerställer att startnoden finns i kantlistan
  if (!any(scenario_edges_subnet$from == start_key_edges | scenario_edges_subnet$to == start_key_edges)) {
    
    # Identifierar kanter som är direkt kopplade till startnoden
    start_node_edges <- scenario_edges_all |>
      dplyr::mutate(from = as.character(from), to = as.character(to)) |>
      dplyr::filter(from == start_key_edges | to == start_key_edges)
    
    # Säkerställer att startnoden ingår genom att lägga till dess kanter
    if (nrow(start_node_edges) > 0) {
      scenario_edges_subnet <- dplyr::bind_rows(scenario_edges_subnet, start_node_edges) |>
        dplyr::distinct(from, to, ID, SRC, STATE, .keep_all = TRUE)
    }
  }
  
  # Uppdatera subnet_nodes så att det matchar det vi faktiskt analyserar
  subnet_nodes <- unique(c(as.character(scenario_edges_subnet$from), as.character(scenario_edges_subnet$to)))
  
  # -------------------------------------------------------------------------- #
  # 10.2 c) Basfall: matning endast från ordinarie startpunkt                  #
  # -------------------------------------------------------------------------- #
  
  # Normaliserar startnodens nyckel till aktuell nodrymd
  start_key <- map_to_reduced(base_topology, base_topology$start_node_key, flags)
  
  # Bygger flödesgraf för basfall med endast ordinarie matning
  flow_from_start_only <- build_flow_graph(
    edges_active = scenario_edges_subnet,
    source_keys  = start_key
  )
  
  # Identifierar noder som är spänningssatta respektive strömlösa i basfallet
  energized_baseline_nodes   <- as.character(flow_from_start_only$nodes$name)
  deenergized_baseline_nodes <- setdiff(subnet_nodes, energized_baseline_nodes)
  
  # Beräknar kund- och KILE-påverkan för basfallet
  outage_metrics <- summarize_impact_in_nodes(
    deenergized_baseline_nodes,
    inputs
  )
  
  # -------------------------------------------------------------------------- #
  # 10.2 d) Identifiera felzon                                                 #
  # -------------------------------------------------------------------------- #
  
  # Initierar tom felzon
  fault_zone_nodes <- character(0)
  
  # Säkerställer att fault_seed_id är satt
  if (is.null(fault_seed_id) || length(fault_seed_id) == 0 || is.na(fault_seed_id)) {
    if (length(open_disconnector_ids) > 0)
      fault_seed_id <- as.character(open_disconnector_ids[1])
  }
  
  # Kör följande kod om ett giltigt fault_seed_id finns
  if (!is.null(fault_seed_id) && !is.na(fault_seed_id)) {
    
    # Förbereder scenariots kantlista för komponentanalys i felzonsberäkning
    component_edges <- scenario_edges_subnet |>
      dplyr::mutate(from = as.character(from), to = as.character(to)) |>
      dplyr::select(from, to)
    
    # Bygger oriktad graf för komponentanalys av scenariot
    scenario_graph <- igraph::graph_from_data_frame(
      component_edges,
      directed = FALSE,
      vertices = data.frame(name = subnet_nodes)
    )
    
    # Beräknar sammanhängande komponenter i scenariografen
    scenario_components <- igraph::components(scenario_graph)$membership
    names(scenario_components) <- igraph::V(scenario_graph)$name
    
    # Normaliserar startnodens nyckel till aktuell nodrymd
    start_key_all <- map_to_reduced(base_topology, base_topology$start_node_key, flags)
    
    # Identifierar komponenten som innehåller startnoden
    start_comp <- if (start_key_all %in% names(scenario_components)) {
      unname(scenario_components[[start_key_all]])
    } else {
      NA_integer_
    }

    # Avgör om felet är kopplat direkt till startnoden
    is_start_fault <-
      !is.na(start_comp) &&
      as.character(fault_seed_id) == as.character(start_key_all)
    
    # Hanterar specialfall där felet är direkt kopplat till startnoden i basfallet
    if (is_start_fault) {
      
      # Justerar basfallsresultat vid fel i startnoden
      flow_from_start_only <- list(
        nodes = data.frame(name = character(0), stringsAsFactors = FALSE))
      energized_baseline_nodes   <- character(0)
      deenergized_baseline_nodes <- subnet_nodes
    }
    
    # Identifierar felzon när felet sitter i startkomponenten 
    if (!is.na(start_comp) &&
        as.character(fault_seed_id) == as.character(start_key_all)) {
      
      fault_zone_nodes <- names(scenario_components)[scenario_components == start_comp]
    }
    
    # Identifierar felzon där felet sitter nedströms start-komponenten
    else {
      
      # Hämtar information om seed-frånskiljarens två sidor
      seed_row <- base_topology$disconnector_summary |>
        dplyr::mutate(ID = as.character(ID), a = as.character(a), b = as.character(b)) |>
        dplyr::filter(ID == as.character(fault_seed_id)) |>
        dplyr::slice(1)
      
      # Säkerställer att seed-frånskiljare kan kopplas till en komponent
      if (nrow(seed_row) > 0 && !is.na(start_comp)) {
        
        # Identifierar komponenttillhörighet för seed-frånskiljarens båda sidor
        comp_a <- if (seed_row$a[1] %in% names(scenario_components)) {
          unname(scenario_components[[seed_row$a[1]]])} 
        else NA_integer_
        
        comp_b <- if (seed_row$b[1] %in% names(scenario_components)) {
          unname(scenario_components[[seed_row$b[1]]])} 
        else NA_integer_
        
        # Identifierar felkomponenten som inte sammanfaller med start-komponenten
        fault_comp <- NA_integer_
        if (!is.na(comp_a) && comp_a != start_comp) fault_comp <- comp_a
        if (is.na(fault_comp) && !is.na(comp_b) && comp_b != start_comp) fault_comp <- comp_b
        if (!is.na(fault_comp)) {
          fault_zone_nodes <- names(scenario_components)[scenario_components == fault_comp]
        }
    
      }
    
    }
  }
  
  # -------------------------------------------------------------------------- #
  # 10.2 e) Identifiera och filtrera BOUNDARY-kandidater                       #
  # -------------------------------------------------------------------------- #
  
  # Identifierar frånskiljare som är stängda i grundtopologin
  disconnector_ids_base <- unique(
    base_topology$all_edges_full$ID[base_topology$all_edges_full$SRC == "FRÅNSKILJARE"]
  ) |> as.character()
  
  # Initierar lista över tillåtna BOUNDARY-kandidater
  allowed_boundary_ids <- NULL
  
  # Begränsar BOUNDARY-kandidater baserat på giltiga användarval
  if (!is.null(valid_boundary_candidates)) {
    if (nrow(valid_boundary_candidates) > 0 && "ID" %in% names(valid_boundary_candidates)) {
      allowed_boundary_ids <- as.character(valid_boundary_candidates$ID)
    } else {
      allowed_boundary_ids <- character(0)
    }
  }
  
  # Initierar lista över tillåtna INTERNAL-kandidater
  allowed_internal_ids <- NULL
  
  # Begränsar INTERNAL-kandidater baserat på giltiga användarval
  if (!is.null(valid_internal_candidates)) {
    if (nrow(valid_internal_candidates) > 0 && "ID" %in% names(valid_internal_candidates)) {
      allowed_internal_ids <- as.character(valid_internal_candidates$ID)
    } else {
      allowed_internal_ids <- character(0)
    }
  }
  
  # Kör följande kod om analysen sker i REDUCED-läge
  if (isTRUE(flags$REDUCED)) {
    
    # Bygger inverterad nodmapping baserat på node_groups
    if (!is.null(base_topology$node_groups)) {
      
      # Skapar mapping från fullnoder till reducerade noder
      inv_rep <- setNames(
        rep(names(base_topology$node_groups), lengths(base_topology$node_groups)),
        unlist(base_topology$node_groups, use.names = FALSE)
      )
      inv_rep[names(base_topology$node_groups)] <- names(base_topology$node_groups)
    } 
    
    # Använder befintlig nodmapping från node_rep_map
    else if (!is.null(base_topology$node_rep_map)) {
      inv_rep <- base_topology$node_rep_map
    } 
    
    # Stoppar om ingen nodmapping finns tillgänglig
    else {
      stop("REDUCED: varken node_groups eller node_rep_map finns, kan inte mappa boundary till reducerad graf.")
    }
    
    # Identifierar noder som ingår i subnätet i REDUCED-läge
    subnet_nodes_red <- as.character(base_topology$keep_nodes_base)
    
    # Förbereder tabell över öppna frånskiljare för REDUCED-analys
    open_switches_classified <- base_topology$disconnector_summary |>
      dplyr::mutate(
        ID = as.character(ID),
        a  = as.character(a),
        b  = as.character(b)
      )
    
    # Begränsar BOUNDARY-kandidater till tillåtna ID:n
    if (!is.null(allowed_boundary_ids)) {
      open_switches_classified <- open_switches_classified |>
        dplyr::filter(ID %in% allowed_boundary_ids)
    }
    
    # Identifierar BOUNDARY-frånskiljare som normalt är öppna i REDUCED-läge
    boundary_disconnectors_normally_open <- open_switches_classified |>
      dplyr::mutate(
        a_rep = unname(inv_rep[a]),
        b_rep = unname(inv_rep[b]),
        in_node = dplyr::case_when(
          !is.na(a_rep) & a_rep %in% subnet_nodes_red ~ a_rep,
          !is.na(b_rep) & b_rep %in% subnet_nodes_red ~ b_rep,
          TRUE ~ NA_character_
        ),
        open_in_base = TRUE,
        a_in = TRUE,
        b_in = FALSE
      ) |>
      dplyr::filter(!is.na(in_node)) |>
      dplyr::select(-a_rep, -b_rep)
    
  } else {
    
    # Identitiferar BOUNDARY-kandidater som normalt är öppna i FULL-läge
    boundary_disconnectors_normally_open <- base_topology$disconnector_summary |>
      dplyr::mutate(
        ID           = as.character(ID),
        a            = as.character(a),
        b            = as.character(b),
        open_in_base = !(ID %in% disconnector_ids_base),
        a_in         = a %in% subnet_nodes,
        b_in         = b %in% subnet_nodes
      ) |>
      dplyr::filter(open_in_base, xor(a_in, b_in)) |>
      dplyr::mutate(in_node = ifelse(a_in, a, b))
    
    # Respektera användarens bortval för BOUNDARY
    if (!is.null(allowed_boundary_ids)) {
      boundary_disconnectors_normally_open <- boundary_disconnectors_normally_open |>
        dplyr::filter(ID %in% allowed_boundary_ids)
    }
  }
  
  # -------------------------------------------------------------------------- #
  # 10.2 f) Utvärdera BOUNDARY-alternativ                                      #
  # -------------------------------------------------------------------------- #
  
  if (nrow(boundary_disconnectors_normally_open) == 0) {
    restore_eval_boundary <- tibble::tibble(
      ID                     = character(0),
      in_node                = character(0),
      reachable_from_alt     = list(),
      restored_nodes         = list(),
      restored_customers     = numeric(0),
      alt_type               = character(0),
      edges_with_tie         = list()
    )
  } else {
    restore_eval_boundary <- boundary_disconnectors_normally_open |>
      dplyr::rowwise() |>
      dplyr::mutate(
        reachable_from_alt = list(
          as.character(
            build_flow_graph(
              edges_active = scenario_edges_subnet,
              source_keys  = in_node
            )$nodes$name
          )
        ),
        restored_nodes = list(intersect(reachable_from_alt, deenergized_baseline_nodes)),
        restored_customers = summarize_impact_in_nodes(
          if (isTRUE(flags$REDUCED)) {
            unique(unlist(base_topology$node_groups[restored_nodes], use.names = FALSE))
          } else {
            restored_nodes
          },
          inputs
        )$customers,
        alt_type = "BOUNDARY",
        edges_with_tie = list(NULL)
      ) |>
      dplyr::ungroup()
  }
  
  # Identifierar noder som ingår i scenariots analysgraf
  graph_nodes <- unique(c(
    as.character(scenario_edges_subnet$from),
    as.character(scenario_edges_subnet$to)
  ))
  
  # Utvärderar varje BOUNDARY-kandidat som ingår i analysgrafen
  restore_eval_boundary <- restore_eval_boundary |>
    dplyr::filter(in_node %in% graph_nodes)
  
  
  # -------------------------------------------------------------------------- #
  # 10.2 g) Identifiera och filtrera INTERNAL-kandidater                       #
  # -------------------------------------------------------------------------- #
  
  # Identifierar noder som interna frånskiljare får anslutas till
  subnet_nodes_for_disc <- if (isTRUE(flags$REDUCED)) {
    as.character(names(base_topology$node_rep_map))
  } else {
    subnet_nodes
  }
  
  # Identifierar interna frånskiljare som normalt är öppna i grundtopologin
  internal_candidates <- base_topology$disconnector_summary |>
    dplyr::mutate(
      ID           = as.character(ID),
      a            = as.character(a),
      b            = as.character(b),
      open_in_base = !(ID %in% disconnector_ids_base),
      a_in         = a %in% subnet_nodes_for_disc,
      b_in         = b %in% subnet_nodes_for_disc
    ) |>
    dplyr::filter(
      open_in_base,
      a_in,
      b_in,
      !(ID %in% as.character(open_disconnector_ids))
    )
  
  # -------------------------------------------------------------------------- #
  # 10.2 h) Utvärdera INTERNAL-alternativ                                      #
  # -------------------------------------------------------------------------- #
  
  # Begränsar INTERNAL-kandidater till tillåtna ID:n
  if (!is.null(allowed_internal_ids)) {
    internal_candidates <- internal_candidates |>
      dplyr::filter(ID %in% allowed_internal_ids)
  }
  
  # Förbereder scenariots kantlista i tabellform
  scenario_edges_subnet_tbl <- scenario_edges_subnet |>
    dplyr::mutate(
      from = as.character(from),
      to   = as.character(to),
      ID   = as.character(ID)
    )
  
  # Normaliserar startnodens nyckel
  start_key <- map_to_reduced(base_topology, base_topology$start_node_key, flags)
  
  # Utvärderar varje INTERNAL-alternativ
  rows <- lapply(seq_len(nrow(internal_candidates)), function(i) {
    
    # Hämtar data för aktuell intern kandidat
    internal_id <- internal_candidates$ID[i]
    node_a      <- internal_candidates$a[i]
    node_b      <- internal_candidates$b[i]
    
    # Skapar scenariots kantlista
    edges_with_tie <- dplyr::bind_rows(
      scenario_edges_subnet_tbl,
      tibble::tibble(
        from  = node_a,
        to    = node_b,
        ID    = as.character(internal_id),
        SRC   = "FRÅNSKILJARE",
        STATE = 1L
      )
    )
    
    # Beräknar noder som blir nåbara via INTERNAL-alternativet
    reachable <- as.character(
      build_flow_graph(
        edges_active = edges_with_tie,
        source_keys  = start_key
      )$nodes$name
    )
    
    # Utesluter alternativ som leder in i felzonen
    if (length(fault_zone_nodes) > 0 && any(reachable %in% fault_zone_nodes)) {
      return(NULL)
    }
    
    # Identifierar noder som återfås jämfört med basfallet
    restored_nodes <- intersect(reachable, deenergized_baseline_nodes)
    
    # Mappar återställda noder till kundnoder vid reducerad topologi
    restored_nodes_for_customers <- if (isTRUE(flags$REDUCED)) {
      unique(
        unlist(
          base_topology$node_groups[restored_nodes],
          use.names = FALSE
        )
      )
    } else {
      restored_nodes
    }
    
    # Sammanställer utvärderingsresultat för INTERNAL-alternativet
    tibble::tibble(
      ID                 = internal_id,
      a                  = node_a,
      b                  = node_b,
      open_in_base       = TRUE,
      a_in               = TRUE,
      b_in               = TRUE,
      in_node            = NA_character_,
      reachable_from_alt = list(reachable),
      restored_nodes     = list(restored_nodes),
      restored_customers = summarize_impact_in_nodes(
        restored_nodes_for_customers,
        inputs
      )$customers,
      alt_type           = "INTERNAL_TIE",
      edges_with_tie     = list(edges_with_tie)
    )
  })
  
  # Sammanställer utvärdering av alla INTERNAL-alternativ
  rows <- Filter(Negate(is.null), rows)
  restore_eval_internal <- dplyr::bind_rows(rows)
  
  # -------------------------------------------------------------------------- #
  # 10.2 i) Val av bästa återställningsalternativ                              #
  # -------------------------------------------------------------------------- #
  
  # Filtrerar externa matningsalternativ som återställer minst en kund
  boundary_candidates_pos <- restore_eval_boundary |>
    dplyr::filter(restored_customers > 0)
  
  # Initierar mängd av redan täckta återställda noder
  covered_restored_nodes <- character(0)
  
  # Initierar vektorer för valda BOUNDARY-alternativ
  selected_boundary_ids      <- character(0)
  selected_boundary_in_nodes <- character(0)
  
  repeat {
    
    # Avbryter om inga återstående kandidater finns
    if (nrow(boundary_candidates_pos) == 0) break
    
    # Beräknar marginal nytta i antal kunder för varje kandidat
    marginal_gains <- vapply(seq_len(nrow(boundary_candidates_pos)), function(i) {
      
      # Identifierar noder som tillförs utöver redan täckta noder
      added_nodes <- setdiff(
        boundary_candidates_pos$restored_nodes[[i]],
        covered_restored_nodes
      )
      
      # Mappar noder till kundnoder vid reducerad topologi
      added_nodes_for_customers <- if (isTRUE(flags$REDUCED)) {
        unique(unlist(
          base_topology$node_groups[added_nodes],
          use.names = FALSE
        ))
      } else {
        added_nodes
      }
      
      # Beräknar antal kunder som tillkommer
      summarize_impact_in_nodes(
        added_nodes_for_customers,
        inputs
      )$customers
      
    }, numeric(1))
    
    # Identifierar kandidat med största marginalnytta
    best_idx <- which.max(marginal_gains)
    
    # Avbryter om ingen ytterligare förbättring uppnås
    if (marginal_gains[best_idx] <= 0) break
    
    # Lägger till vald BOUNDARY i urvalet
    selected_boundary_ids <- c(selected_boundary_ids, boundary_candidates_pos$ID[[best_idx]])
    selected_boundary_in_nodes <- c(selected_boundary_in_nodes, as.character(boundary_candidates_pos$in_node[[best_idx]]))
    
    # Uppdaterar uppsättning täckta noder
    covered_restored_nodes <- union(
      covered_restored_nodes,
      boundary_candidates_pos$restored_nodes[[best_idx]]
    )
    
    # Tar bort vald kandidat från vidare urval
    boundary_candidates_pos <- boundary_candidates_pos[-best_idx, , drop = FALSE]
  }
  
  # Normaliserar startnodens nyckel
  start_key <- map_to_reduced(
    base_topology,
    base_topology$start_node_key,
    flags
  )
  
  # Identifierar inre noder för valda BOUNDARY-alternativ
  boundary_in_nodes <- restore_eval_boundary |>
    dplyr::filter(ID %in% selected_boundary_ids) |>
    dplyr::pull(in_node) |>
    as.character() |>
    unique()
  
  # Beräknar noder som blir nåbara vid samtidig matning från ordinarie startpunkt
  # och samtliga valda BOUNDARY-källor
  reachable_nodes <- as.character(
    build_flow_graph(
      edges_active = scenario_edges_subnet,
      source_keys  = unique(c(as.character(start_key), boundary_in_nodes))
    )$nodes$name
  )
  
  # Identifierar noder som återställs jämfört med basfallet
  restored_nodes <- intersect(
    reachable_nodes,
    deenergized_baseline_nodes
  )
  
  # Identifierar noder som fortfarande är strömlösa efter bästa återställning
  deenergized_after_nodes <- setdiff(
    deenergized_baseline_nodes,
    restored_nodes
  )
  
  # Mappar återställda noder till kundnoder vid reducerad topologi
  restored_nodes_for_customers <- if (isTRUE(flags$REDUCED)) {
    unique(unlist(
      base_topology$node_groups[restored_nodes],
      use.names = FALSE
    ))
  } else {
    restored_nodes
  }
  
  # Beräknar totalt antal återställda kunder för valda BOUNDARY-alternativ
  boundary_total_customers <- summarize_impact_in_nodes(
    restored_nodes_for_customers,
    inputs
  )$customers
  
  
  # Testar om någon BOUNDARY kan tas bort utan försämrat kundutfall
  if (length(selected_boundary_ids) > 1 && boundary_total_customers > 0) {
    
    # Itererar över varje vald BOUNDARY 
    for (remove_id in selected_boundary_ids) {
      
      # Skapar ett kandidatset där aktuell BOUNDARY exkluderas
      candidate_ids <- setdiff(selected_boundary_ids, remove_id)
      
      # Identifierar inre noder för kandidatset utan aktuell BOUNDARY
      candidate_in_nodes <- restore_eval_boundary |>
        dplyr::filter(ID %in% candidate_ids) |>
        dplyr::pull(in_node) |>
        as.character() |>
        unique()
      
      # Beräknar noder som blir nåbara vid matning från ordinarie startpunkt
      # och återstående BOUNDARY-källor
      reachable_nodes_candidate <- as.character(
        build_flow_graph(
          edges_active = scenario_edges_subnet,
          source_keys  = unique(c(as.character(start_key), candidate_in_nodes))
        )$nodes$name
      )
      
      # Identifierar noder som återställs jämfört med basfallet
      restored_nodes_candidate <- intersect(
        reachable_nodes_candidate,
        deenergized_baseline_nodes
      )
      
      # Mappar återställda noder till kundnoder vid reducerad topologi
      restored_nodes_for_customers_candidate <- if (isTRUE(flags$REDUCED)) {
        unique(unlist(
          base_topology$node_groups[restored_nodes_candidate],
          use.names = FALSE
        ))
      } else {
        restored_nodes_candidate
      }
      
      # Beräknar antal återställda kunder för kandidatset
      candidate_cust <- summarize_impact_in_nodes(
        restored_nodes_for_customers_candidate,
        inputs
      )$customers
      
      # Uppdaterar urvalet om borttag av aktuell BOUNDARY inte försämrar utfallet
      if (candidate_cust >= boundary_total_customers) {
        selected_boundary_ids    <- candidate_ids
        boundary_total_customers <- candidate_cust
      }
    }
  }
  
  # Uppdaterar lista med inre noder för slutligt valda BOUNDARY
  selected_boundary_in_nodes <- restore_eval_boundary |>
    dplyr::filter(ID %in% selected_boundary_ids) |>
    dplyr::pull(in_node) |>
    as.character() |>
    unique()
  
  # Hanterar fallet där inga giltiga INTERNAL-alternativ återstår
  if (nrow(restore_eval_internal) == 0 || 
      !"restored_customers" %in% names(restore_eval_internal)) {
    
    restore_eval_internal <- tibble::tibble(
      ID                 = character(0),
      a                  = character(0),
      b                  = character(0),
      open_in_base       = logical(0),
      a_in               = logical(0),
      b_in               = logical(0),
      in_node            = character(0),
      reachable_from_alt = list(),
      restored_nodes     = list(),
      restored_customers = numeric(0),
      alt_type           = character(0),
      edges_with_tie     = list()
    )
  }
  
  # Väljer INTERNAL-alternativ med störst kundåterställning
  best_internal <- restore_eval_internal |>
    dplyr::arrange(dplyr::desc(restored_customers)) |>
    dplyr::slice(1)
  
  # Hämtar kundutfall för bästa INTERNAL
  internal_best_customers <- if (nrow(best_internal) > 0)
    best_internal$restored_customers
  else
    0
  
  # Väljer mellan extern och intern återställning baserat på kundutfall
  selected_alt_type <- if (
    boundary_total_customers > 0 &&
    boundary_total_customers >= internal_best_customers
  ) {
    "BOUNDARY_MULTI"
  } else if (internal_best_customers > 0) {
    "INTERNAL_TIE"
  } else {
    NA_character_
  }
  
  # -------------------------------------------------------------------------- #
  # 10.2 j) Sammanställ och returnera resultat                                 #
  # -------------------------------------------------------------------------- #
  
  # Returnerar resultat från återställningsanalysen
  list(
    open_disconnector_ids            = open_disconnector_ids,
    edges_fault_in                   = scenario_edges_subnet,
    flow_outage                      = flow_from_start_only,
    outage_nodes_before              = deenergized_baseline_nodes,
    outage_nodes_after               = deenergized_after_nodes,
    outage_customers                 = outage_metrics$customers,
    outage_kile_energy_h             = outage_metrics$kile_energy_h,
    fault_zone_nodes                 = fault_zone_nodes,
    selected_alt_type                = selected_alt_type,
    selected_boundary_ids            = selected_boundary_ids,
    selected_boundary_in_nodes       = unique(selected_boundary_in_nodes),
    selected_internal_id             = if (nrow(best_internal) > 0) best_internal$ID else NA_character_,
    selected_internal_edges_with_tie = if (nrow(best_internal) > 0) best_internal$edges_with_tie[[1]] else NULL
  )
}