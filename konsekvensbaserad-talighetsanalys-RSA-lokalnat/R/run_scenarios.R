# Fil:                run_scenarios.R
# Författare:         Erik Hårrskog
# Datum:              2026-05-08
#
# Innehåll:
#
# 8.1 Publika hjälpfunktioner
#     a) Beräkning av maximal kundeffekt (compute_max_customer_power)
#     b) Summering av kund- och KILE-påverkan per nod (summarize_impact_in_nodes)
#
# 8.2 Huvudfunktion: run_outage_scenarios
#     a) Urval och validering av felscenarier
#     b) Initiering av datastrukturer och uppslag
#     c) Scenarioanalys: felsimulering, återställning och flödesanalys
#     d) Scenarioanalys: konsekvens- och påverkansberäkning
#     e) Scenarioanalys: export och visualisering
#     f) Sammanställning av resultat och utsatthetsanalys

# ============================================================================ #
# 8.1) Publika hjälpfunktioner                                                 #
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 8.1 a) Beräkning av max kundeffekt (compute_max_customer_power)              #
# ---------------------------------------------------------------------------- #

# Beräknar maximal kundeffekt för kunder kopplade till angivna noder
compute_max_customer_power <- function(candidate_nodes, inputs) {
  
  # Avslutar tidigt om inga noder är angivna
  if (length(candidate_nodes) == 0) return(0)
  
  # Identifierar transformatorer som är kopplade till aktuella noder
  connected_transformer_ids <- inputs$TRANSFORMER |>
    dplyr::mutate(
      node = as.character(make_key(X, Y)),
      `Transformator-ID` = as.character(ID)
    ) |>
    dplyr::filter(node %in% candidate_nodes) |>
    dplyr::pull(`Transformator-ID`)
  
  # Avslutar om inga transformatorer matchar noderna
  if (length(connected_transformer_ids) == 0) return(0)
  
  # Beräknar maximal kundeffekt för kunder kopplade till transformatorerna
  max_customer_power <- inputs$INTERRUPTIONS |>
    dplyr::mutate(
      `Transformator-ID` = as.character(`DISTRTRAFO.ID`),
      cppower        = as.numeric(CPPOWER_UT),
      contractpower  = as.numeric(CONTRACTPOWER_UT),
      
      # Maxeffekten bestäms från både använd och abonnerad effekt
      customer_power = pmax(cppower, contractpower, na.rm = TRUE)
    ) |>
    dplyr::filter(`Transformator-ID` %in% connected_transformer_ids) |>
    dplyr::summarise(
      max_customer_power =
        if (all(is.na(customer_power))) 0
      else max(customer_power, na.rm = TRUE)
    ) |>
    dplyr::pull(max_customer_power)
  
  # Returnerar 0 vid ogiltigt eller saknat värde
  ifelse(is.finite(max_customer_power), max_customer_power, 0)
}

# ---------------------------------------------------------------------------- #
# 8.1 b) Summering av kund- och KILE-påverkan (summarize_impact_in_nodes)            #
# ---------------------------------------------------------------------------- #

# Summerar kund- och KILE-påverkan för transformatorer kopplade till angivna noder
summarize_impact_in_nodes <- function(candidate_nodes, inputs) {
  
  # Normaliserar nodnycklar genom att ta bort ev. A/B-suffix
  base_node_keys <- sub("\\|[AB]$", "", as.character(candidate_nodes))
  
  # Identifierar transformatorer kopplade till de angivna noderna
  connected_transformer_ids <- inputs$TRANSFORMER |>
    dplyr::mutate(
      node_key       = sub("\\|[AB]$", "", as.character(make_key(X, Y))),
      transformer_id = as.character(ID)
    ) |>
    dplyr::filter(node_key %in% base_node_keys) |>
    dplyr::pull(transformer_id)
  
  # Summerar kunder och KILE för matchande transformatorer
  inputs$INTERRUPTIONS |>
    dplyr::mutate(
      TRANSFORMER_ID = as.character(`DISTRTRAFO.ID`),
      customers      = as.numeric(Antal_kunder_UT),
      kile_energy_h  = as.numeric(Eifs_KILEnergi_per_h),
    ) |>
    dplyr::filter(TRANSFORMER_ID %in% connected_transformer_ids) |>
    dplyr::summarise(
      customers      = sum(customers,     na.rm = TRUE),
      kile_energy_h  = sum(kile_energy_h,  na.rm = TRUE),
    )
}

# ============================================================================ #
# 8.2) Huvudfunktion: run_outage_scenarios                                     #
# ============================================================================ #

# Definierar funktion "run_outage_scenarios" som kör samtliga felscenarier
run_outage_scenarios <- function(base_topology,
                                 flags,
                                 inputs,
                                 valid_boundary_candidates = NULL,
                                 valid_internal_candidates = NULL,
                                 plot_folder) {
  
  # -------------------------------------------------------------------------- #
  # 8.2 a) Urval av felscenarier                                               #
  # -------------------------------------------------------------------------- #
  
  # Lokal hjälpfunktion som normaliserar nodnycklar till aktuell nodrymd (FULL/REDUCED)
  make_scenario_node_key <- function(x, y, base_topology, flags) {
    key_raw <- as.character(make_key(x, y))
    if (isTRUE(flags$REDUCED)) {
      as.character(map_to_reduced(base_topology, key_raw, flags))
    } else {
      key_raw
    }
  }
  
  # Sammanställer tabell med frånskiljare i samma nodrymd som base_topology
  scenario_disconnectors <- inputs$SCENARIOS |>
    dplyr::mutate(
      ID       = as.character(ID),
      STATE    = as.integer(STATE),
      FATHERID = as.character(FATHERID),
      a        = make_scenario_node_key(X, Y, base_topology, flags),
      b        = make_scenario_node_key(X, Y, base_topology, flags)
    )
  
  # Kontroll: alla scenarier måste finnas i full disconnector-uppsättning
  missing_ids <- setdiff(
    scenario_disconnectors$ID,
    base_topology$disconnector_summary$ID
  )
  
  # Stoppar om det saknar scenariofrånskiljare i den kompletta frånskiljarlistan
  if (length(missing_ids) > 0) {
    stop(
      sprintf(
        "SCENARIOS innehåller ID som saknas i full DISCONNECTORS: %s",
        paste(missing_ids, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  # Väljer stängda frånskiljare i subnätet som primära felkandidater
  primary_fault_candidates <- scenario_disconnectors |>
    dplyr::filter(
      STATE == 1,
      a %in% base_topology$keep_nodes_base,
      b %in% base_topology$keep_nodes_base
    )
  
  # Begränsar antalet felkandidater utifrån flaggan OPEN_ID_LIMIT
  failed_disconnector_ids <- primary_fault_candidates |>
    dplyr::arrange(ID) |>
    dplyr::slice_head(n = flags$OPEN_ID_LIMIT) |>
    dplyr::pull(ID)
  
  say("Antal scenarier (primära frånskiljare): %d\n", length(failed_disconnector_ids)+1)
  
  # Bygger scenarier (mängder av öppna frånskiljare) från seed-listan
  scenarios <- build_scenarios(
    res      = base_topology,
    seed_ids = failed_disconnector_ids,
    inputs   = inputs,
    flags    = flags
  )
  
  # Filtrerar bort scenarier där felzonen saknar MVPART
  if (!isTRUE(flags$ALLOW_BB_FAULTS)) {
    
    scenarios <- scenarios[
      vapply(
        scenarios,
        function(sc) {
          seg_len <- as.numeric(sc$segment_length_m)
          is.finite(seg_len) && seg_len > 0
        },
        logical(1)
      )
    ]
  }
  
  # Säkerställ att START_FAULT alltid körs först
  if ("START_FAULT" %in% names(scenarios)) {
    scenarios <- c(
      scenarios["START_FAULT"],
      scenarios[setdiff(names(scenarios), "START_FAULT")]
    )
  }
  
  # -------------------------------------------------------------------------- #
  # 8.2 b) Initiering av datastrukturer och uppslag                            #
  # -------------------------------------------------------------------------- #
  
  # Initierar datastrukturer för scenarioexport och sårbarhetsanalys
  scenario_export_rows <- list()
  outage_nodes_per_disconnector_before <- list()
  outage_nodes_per_disconnector_after  <- list()
  
  # Slår upp transformatorer per nod
  transformer_lookup <- inputs$TRANSFORMER |>
    dplyr::transmute(
      node = as.character(make_key(X, Y)),
      `Stations-ID` = as.character(FATHERID),
      `Transformator-ID` = as.character(ID)
    ) |>
    dplyr::distinct()
  
  # Slår upp transformatornamn per ID
  transformer_label_lookup <- inputs$TRANSFORMER %>%
    dplyr::transmute(
      `Transformator-ID` = as.character(ID),
      `Transformator-LABEL` = as.character(LABEL)
    )
  
  # -------------------------------------------------------------------------- #
  # 8.2 c) Scenarioanalys: felsimulering, återställning och flödesanalys       #
  # -------------------------------------------------------------------------- #
  
  # Kör felsimulering och analys för varje scenario
  for (sc_name in names(scenarios)) {
    
    # Hämtar scenariots öppna frånskiljare och ev. seed-ID
    sc       <- scenarios[[sc_name]]
    open_ids <- sc$open_ids
    seed_id  <- sc$seed_id
    
    # Hämtar LABEL för samtliga öppna frånskiljare i scenariot
    open_labels <- base_topology$disconnector_summary$LABEL[
      match(open_ids, base_topology$disconnector_summary$ID)
    ]
    
    # Hämtar primär frånskiljare (eller START)
    primary_labels <- if (identical(sc_name, "START_FAULT")) {
      "START"
    } else {
      base_topology$disconnector_summary$LABEL[
        match(seed_id, base_topology$disconnector_summary$ID)
      ]
    }
    
    # Identifierar sekundära frånskiljare (alla öppna utom primär)
    secondary_ids <- setdiff(
      as.character(open_ids),
      if (!is.na(seed_id)) as.character(seed_id) else character(0)
    )
    
    secondary_labels <- base_topology$disconnector_summary$LABEL[
      match(secondary_ids, base_topology$disconnector_summary$ID)
    ]
    
    say(
      "Öppnar frånskiljare: %s || %s",
      crayon::bold(primary_labels),
      crayon::bold(
        if (length(secondary_labels) > 0)
          paste(secondary_labels, collapse = ", ")
        else
          ""
      )
    )
    
    # Kör baseline-analys och identifierar alternativa matningskällor
    scenario_result <- evaluate_resupply_options(
      base_topology                   = base_topology,
      open_disconnector_ids = open_ids,
      fault_seed_id =
        if (identical(sc_name, "START_FAULT")) {
          as.character(base_topology$start_node_key)
        } else {
          as.character(seed_id)
        },
      inputs                = inputs,
      flags = flags,
      valid_boundary_candidates = valid_boundary_candidates,
      valid_internal_candidates = valid_internal_candidates
    )
    
    say(
      "Strömlösa kunder innan alternativ matning: %s",
      scenario_result$outage_customers
    )
    
    # Hämtar valt återställningssätt från resupply
    selected_alt_type <- scenario_result$selected_alt_type
    
    # Identifierar inre noder för valda BOUNDARY-alternativ
    restore_nodes <- scenario_result$selected_boundary_in_nodes
        
    # Normaliserar startnod till korrekt nodrymd (FULL/REDUCED)
    start_key <- map_to_reduced(base_topology, base_topology$start_node_key, flags)
    
    # Bygger flödesgraf för scenariot baserat på valt återställningssätt
    if (identical(selected_alt_type, "INTERNAL_TIE")) {
      
      # Bygger flödesgraf med intern koppling som alternativ matning
      flow_with_alt <- build_flow_graph(
        edges_active = scenario_result$selected_internal_edges_with_tie,
        source_keys  = as.character(start_key)
      )
      
    } else if (identical(selected_alt_type, "BOUNDARY_MULTI")) {
      
      # Bygger flödesgraf med extern matning via valda BOUNDARY-källor
      flow_with_alt <- build_flow_graph(
        edges_active = scenario_result$edges_fault_in,
        source_keys  = unique(c(
          as.character(start_key),
          as.character(restore_nodes)
        ))
      )
      
    } else {
      
      # Basfall: ingen alternativ matning möjlig, använder basflödets resultat
      flow_with_alt <- scenario_result$flow_outage
    }
    
    # Identifierar energiserade noder efter alternativ matning
    energized_alt_nodes <- as.character(flow_with_alt$nodes$name)
    
    # Identifierar noder som fortfarande är strömlösa i grundsubnätet
    deenergized_alt_nodes <- setdiff(
      as.character(base_topology$keep_nodes_base),
      energized_alt_nodes
    )
    
    # ------------------------------------------------------------------------ #
    # 8.2 d) Scenarioanalys: konsekvens- och påverkansberäkning                #
    # ------------------------------------------------------------------------ #
    
    # Identifierar bortkopplade transformatorer före alternativ matning
    transformers_before <- transformer_lookup |>
      dplyr::filter(node %in% as.character(scenario_result$outage_nodes_before)) |>
      dplyr::arrange(`Stations-ID`, `Transformator-ID`) |>
      dplyr::pull(`Transformator-ID`) |>
      unique()
    
    # Identifierar bortkopplade transformatorer efter alternativ matning
    transformers_after <- transformer_lookup |>
      dplyr::filter(node %in% as.character(deenergized_alt_nodes)) |>
      dplyr::arrange(`Stations-ID`, `Transformator-ID`) |>
      dplyr::pull(`Transformator-ID`) |>
      unique()
    
    # Hämtar LABEL för bortkopplade transformatorer
    transformers_before_labels <- transformer_label_lookup$`Transformator-LABEL`[
      match(transformers_before, transformer_label_lookup$`Transformator-ID`)]
    transformers_after_labels <- transformer_label_lookup$`Transformator-LABEL`[
      match(transformers_after, transformer_label_lookup$`Transformator-ID`)]
    
    # Sammanställer texter för export
    transformers_before_text <- if (length(transformers_before_labels) == 0) "" 
    else paste(transformers_before_labels, collapse = ", ")
    transformers_after_text  <- if (length(transformers_after_labels)  == 0) "" 
    else paste(transformers_after_labels,  collapse = ", ")
    
    # Beräknar maximal kundeffekt före alternativ matning
    max_customer_power_before <- compute_max_customer_power(
      candidate_nodes  = as.character(scenario_result$outage_nodes_before),
      inputs = inputs
    )
    
    # Beräknar maximal kundeffekt efter alternativ matning
    max_customer_power_after <- compute_max_customer_power(
      candidate_nodes  = as.character(deenergized_alt_nodes),
      inputs = inputs
    )
    
    # Identifierar direkt bortsektionerade noder (i felzon)
    fault_zone_nodes <- as.character(scenario_result$fault_zone_nodes)
    deenergized_fault_zone_nodes <- intersect(
      deenergized_alt_nodes, fault_zone_nodes
    )
    
    # Identifierar indirekt isolerade noder (utanför felzon)
    deenergized_isolated_nodes <- setdiff(
      deenergized_alt_nodes, fault_zone_nodes
    )
    
    # Summerar kund- och KILE-påverkan i felzon
    impact_fault_zone <- summarize_impact_in_nodes(
      deenergized_fault_zone_nodes, inputs
    )
    
    # Summerar kund- och KILE-påverkan för indirekt isolerade noder
    impact_isolated <- summarize_impact_in_nodes(
      deenergized_isolated_nodes, inputs
    )
    
    # Antal strömlösa kunder per delmängd
    direct_outage_customers_after <- impact_fault_zone$customers
    isolated_outage_customers_after <- impact_isolated$customers
    
    # KILE energi per timme per delmängd
    direct_outage_kile_energy_h <- impact_fault_zone$kile_energy_h
    isolated_outage_kile_energy_h <- impact_isolated$kile_energy_h   
    
    # Sparar strömlösa noder före och efter alternativ matning
    outage_nodes_per_disconnector_before[[sc_name]] <- as.character(scenario_result$outage_nodes_before)
    outage_nodes_per_disconnector_after[[sc_name]]  <- deenergized_alt_nodes
    
    # Summerar total kund- och KILE-påverkan efter alternativ matning
    after_metrics <- summarize_impact_in_nodes(
      deenergized_alt_nodes,
      inputs
    )
    
    say(
      "Strömlösa kunder efter alternativ matning: %s\n",
      after_metrics$customers
    )
    
    # Etikett för alternativ matning via intern koppling
    internal_restore_option_label <- base_topology$disconnector_summary$LABEL[
      match(
        scenario_result$selected_internal_id,
        base_topology$disconnector_summary$ID
      )
    ]
    
    if (is.na(internal_restore_option_label)) {
      internal_restore_option_label <- ""
    }
    
    
    # Etikett för använd alternativ matning
    best_restore_option_label <- switch(
      selected_alt_type,
      
      "BOUNDARY_MULTI" = paste(
        base_topology$disconnector_summary$LABEL[
          match(
            scenario_result$selected_boundary_ids,
            base_topology$disconnector_summary$ID
          )
        ],
        collapse = ", "
      ),
      
      "INTERNAL_TIE" = internal_restore_option_label,
      ""
    )
    
    # Kontrollerar att alternativ matning ger förbättring
    alt_used <- scenario_result$outage_customers > after_metrics$customers
    
    # ------------------------------------------------------------------------ #
    # 8.2 e) Scenarioanalys: export och visualisering                          #
    # ------------------------------------------------------------------------ #
    
    # Sammanställer exportdata för scenariot
    scenario_export_rows[[sc_name]] <- tibble::tibble(
      `Primär frånskiljare` =
        if (!is.na(seed_id)) {
          base_topology$disconnector_summary$LABEL[
            match(seed_id, base_topology$disconnector_summary$ID)]} 
      else if (identical(sc_name, "START_FAULT")) {"START"} 
      else {NA_character_},
      
      `Sekundär frånskiljare` =
        if (length(secondary_ids) == 0) {""} 
      else {paste(base_topology$disconnector_summary$LABEL[
              match(secondary_ids, base_topology$disconnector_summary$ID)],
            collapse = ", ")},
      `Segmentlängd (m)`                   = sc$segment_length_m,
      `Helt återställt?`                   = after_metrics$customers == 0,
      `Alternativ matning` = if (alt_used) best_restore_option_label else "",
      `Alternativ matningstyp` = if (alt_used) switch(selected_alt_type,
                                    "BOUNDARY_MULTI" = "BOUNDARY",
                                    "INTERNAL_TIE"   = "INTERNAL",
                                    "") else "",
      `Bortkopplade transformatorer före`  = transformers_before_text,
      `Bortkopplade transformatorer efter` = transformers_after_text,
      `Antal bortkopplade transformatorer före`  = length(transformers_before),
      `Antal bortkopplade transformatorer efter` = length(transformers_after),
      `Berörda kunder före`                = scenario_result$outage_customers,
      `Berörda kunder efter`               = after_metrics$customers,
      `Bortsektionerade kunder efter`      = direct_outage_customers_after,
      `Isolerade kunder efter`             = isolated_outage_customers_after,
      `KILE energi per h före`             = scenario_result$outage_kile_energy_h,
      `KILE energi per h efter`            = after_metrics$kile_energy_h,
      `Bortsektionerad KILE energi per h`  = direct_outage_kile_energy_h,
      `Isolerad KILE energi per h`         = isolated_outage_kile_energy_h,
      `Max kundeffekt före (kW)`           = max_customer_power_before,
      `Max kundeffekt efter (kW)`          = max_customer_power_after,
    )
    
    # Kör följande kod om PLOT_SCENARIOS-flaggan är aktiverad
    if (isTRUE(flags$PLOT_SCENARIOS)) {
      
      # Identifierar energiserade noder före alternativ matning
      energized_before_alt <- as.character(scenario_result$flow_outage$nodes$name)
      
      # Identifierar energiserade noder som tillkommit via alternativ matning
      alt_nodes <- setdiff(energized_alt_nodes, energized_before_alt)
      
      # Identifierar alternativ matningskällor i scenariot
      selected_alternative_ids <- switch(
        selected_alt_type,
        "BOUNDARY_MULTI" = as.character(scenario_result$selected_boundary_ids),
        "INTERNAL_TIE"   = as.character(scenario_result$selected_internal_id),
        character(0)
      )
      
      # Om REDUCED-flaggan är aktiverad visualiseras reducerad graf
      if (isTRUE(flags$REDUCED)) {
        
        graph_for_plot <- igraph::graph_from_data_frame(
          scenario_result$edges_fault_in |>
            dplyr::mutate(from = as.character(from), to = as.character(to)) |>
            dplyr::select(from, to),
          directed = FALSE,
          vertices = data.frame(
            name = unique(c(
              as.character(base_topology$keep_nodes_base),
              as.character(start_key)
            )),
            stringsAsFactors = FALSE
          )
        )
      }
      
      # Annars visualiseras full graf
      else {
        graph_for_plot <- igraph::induced_subgraph(
          base_topology$graph,
          vids = igraph::V(base_topology$graph)$name
        )
      }
      
      # Identifierar alla transformatornoder i nätet
      transformer_nodes <- inputs$TRANSFORMER |>
        dplyr::transmute(node = as.character(make_key(X, Y))) |>
        dplyr::pull(node)
      
      # Identifierar strömlösa transformatornoder
      disconnected_transformer_nodes <- intersect(
        transformer_nodes,
        deenergized_alt_nodes
      )
      
      # Förbereder filnamn för plot-export
      plot_filename <- NULL
      
      # Bygger titeltext till plot baserat på öppna frånskiljare
      title_open_text <- paste0(
        primary_labels,
        " || ",
        if (length(secondary_labels) > 0)
          paste(secondary_labels, collapse = ", ")
        else
          ""
      )
      
      # Förbereder för export av plot om aktiverad flagga
      if (isTRUE(flags$EXPORT_PLOTS) && !is.null(plot_folder)) {
        
        plot_filename <- file.path(
          plot_folder,
          paste0(
            "Scenario_",
            gsub("[|]", "_", title_open_text),
            ".png"
          )
        )
      }
      
      # Rensar bort felzon från energiserade noder för visualisering
      energized_nodes_for_plot <- setdiff(
        energized_alt_nodes,
        scenario_result$fault_zone_nodes
      )
      
      # Anropar plotfunktion för att plotta det aktuella scenariet
      plot_scenarios(
        GRAPH_LAYOUT              = graph_for_plot,
        start_x                   = base_topology$start_x,
        start_y                   = base_topology$start_y,
        main_title = sprintf(
          "Scenarioplot efter alternativ matning\nÖppna frånskiljare: %s",
          title_open_text
        ),
        energized_nodes           = energized_nodes_for_plot,
        alt_supply_nodes          = alt_nodes,
        failed_disconnector_id =
          if (identical(sc_name, "START_FAULT")) {
            NULL
          } else {
            as.character(seed_id)
          },
        disconnector_summary      = base_topology$disconnector_summary,
        opened_disconnector_ids   = open_ids,
        feeding_disconnector_ids  = selected_alternative_ids,
        fault_zone_nodes = scenario_result$fault_zone_nodes,
        disconnected_transformer_nodes = disconnected_transformer_nodes,
        flags = flags,
        plot_filename = plot_filename
      )
    }
  }
  
  # Skriver ut namn på exportmapp om EXPORT_PLOTS-flaggan är aktiverad
  if (flags$EXPORT_PLOTS) message(sprintf("Plottar exporterade till: %s", plot_folder))
  
  # -------------------------------------------------------------------------- #
  # 8.2 f) Sammanställning av resultat och utsatthetsanalys                    #
  # -------------------------------------------------------------------------- #
  
  # Slår samman scenarioresultat till en gemensam tabell
  scenario_results <- dplyr::bind_rows(scenario_export_rows)
  
  # Samlar unika transformatornoder
  transformer_nodes <- inputs$TRANSFORMER |>
    dplyr::transmute(node = as.character(make_key(X, Y))) |>
    dplyr::distinct()
  
  # Räknar hur ofta varje transformatornod blir strömlös över scenarierna
  vuln_by_node <- tibble::tibble(
    node = transformer_nodes$node,
    
    # Före alternativ matning
    scen_before = vapply(
      transformer_nodes$node,
      function(n)
        sum(vapply(
          outage_nodes_per_disconnector_before,
          function(ns) n %in% ns,
          logical(1)
        )),
      integer(1)
    ),
    
    # Efter alternativ matning
    scen_after = vapply(
      transformer_nodes$node,
      function(n)
        sum(vapply(
          outage_nodes_per_disconnector_after,
          function(ns) n %in% ns,
          logical(1)
        )),
      integer(1)
    )
  )
  
  # Sammanställer slutlig utsatthet per transformator
  transformer_vulnerability <- inputs$TRANSFORMER |>
    dplyr::transmute(
      `Stations-ID`      = as.character(FATHERID),
      `Transformator-ID` = as.character(ID),
      node               = as.character(make_key(X, Y))
    ) |>
    dplyr::left_join(vuln_by_node, by = "node") |>
    dplyr::mutate(
      `Påverkande scenarier före`  = dplyr::coalesce(scen_before, 0L),
      `Påverkande scenarier efter` = dplyr::coalesce(scen_after, 0L)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `Berörda kunder` = summarize_impact_in_nodes(node, inputs)$customers
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      inputs$INTERRUPTIONS |>
        dplyr::transmute(
          `Transformator-ID`  = as.character(`DISTRTRAFO.ID`),
          `KILE energi per h` = as.numeric(Eifs_KILEnergi_per_h),

          cppower             = as.numeric(CPPOWER_UT),
          contractpower       = as.numeric(CONTRACTPOWER_UT),
          max_customer_power  = pmax(cppower, contractpower, na.rm = TRUE)
        ) |>
        dplyr::group_by(`Transformator-ID`) |>
        dplyr::summarise(
          `KILE energi per h` = sum(`KILE energi per h`, na.rm = TRUE),
          `Max kundeffekt [kW]`    = if (all(is.na(max_customer_power))) 0
          else max(max_customer_power, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "Transformator-ID"
    ) |>
    dplyr::select(
      `Stations-ID`,
      `Transformator-ID`,
      `Påverkande scenarier före`,
      `Påverkande scenarier efter`,
      `Berörda kunder`,
      `KILE energi per h`,
      `Max kundeffekt [kW]`,
    )
  
  # Returnerar sammanställning av resultatet
  list(
    scenario_results                        = scenario_results,
    scenarios                               = scenarios,
    transformer_vulnerability               = transformer_vulnerability
  )
}