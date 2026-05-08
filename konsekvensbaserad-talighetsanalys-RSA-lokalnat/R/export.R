# Fil:                export.R
# Författare:         Erik Hårrskog
# Datum:              2026-05-08
#
# Innehåll:
#
# 12.1 Interna hjälpfunktioner för export
#     a) Grundformat för tabellblad
#     b) Kolumnbredder
#     c) Tomma kolumner
#     d) Villkorsstyrd formattering
#
# 12.2 Huvudfunktion: export_data
#     a) Initiering av arbetsbok och stilar
#     b) Förberäkningar: scenariotabeller och nyckeltal
#     c) Förberäkningar: transformatorer och inställningar
#     d) Förberäkningar: Scenariovikt
#     e) Blad 1: Nyckeltal
#     f) Blad 2: Scenariovikt
#     g) Blad 3: Scenarier
#     h) Blad 4: Transformatorer
#     i) Blad 5: Inställningar
#     j) Slutlig lagring

# ============================================================================ #
# 12.1) Interna hjälpfunktioner för export                                     #
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 12.1 a) Grundformat för tabellblad                                           #
# ---------------------------------------------------------------------------- #

# Applicerar grundläggande formatering på ett tabellblad i Excel
apply_basic_sheet_formatting <- function(wb, sheet, n_rows, n_cols, center_style, header_style, border_style, no_wrap_style, top_align_style) {
  
  # Applicerar grundläggande cellformattering på hela tabellen
  openxlsx::addStyle( wb, sheet, center_style, rows = 1:n_rows, cols = 1:n_cols, gridExpand = TRUE, stack = TRUE)
  
  # Applicerar rubrikstil på första raden
  openxlsx::addStyle(wb, sheet, header_style, rows = 1, cols = 1:n_cols, gridExpand = TRUE, stack = TRUE)
  
  # Applicerar toppjustering från rad 2
  if (n_rows >= 2) openxlsx::addStyle(wb, sheet, top_align_style, rows = 2:n_rows, cols = 1:n_cols, gridExpand = TRUE, stack = TRUE)
  
  # Inaktiverar radbrytning från rad 2
  if (!is.null(no_wrap_style) && n_rows >= 2) {
    openxlsx::addStyle(wb, sheet, no_wrap_style, rows = 2:n_rows, cols = 1:n_cols, gridExpand = TRUE, stack = TRUE)
  }
  
  # Applicerar kantstil om den är angiven som argument
  if (!is.null(border_style)) {
    openxlsx::addStyle(wb, sheet, border_style, rows = 1:n_rows, cols = 1:n_cols, gridExpand = TRUE, stack = TRUE)
  }
}

# ---------------------------------------------------------------------------- #
# 12.1 b) Kolumnbredder                                                        #
# ---------------------------------------------------------------------------- #

# Sätter kolumnbredder baserat på fasta och automatiska regler
apply_column_widths <- function(wb, sheet, colnames_table, fixed_widths) {
  
  # Identifierar kolumner med fasta bredder
  cols_fixed <- match(names(fixed_widths), colnames_table)
  valid_idx  <- !is.na(cols_fixed)
  
  # Tillämpar fasta bredder för angivna kolumner
  if (any(valid_idx)) openxlsx::setColWidths(wb, sheet, cols = cols_fixed[valid_idx], widths = unname(fixed_widths[valid_idx]))
  
  # Identifierar övriga kolumner
  other_cols <- setdiff(seq_along(colnames_table), cols_fixed[valid_idx])
  
  # Tillämpar automatisk bredd på övriga kolumner
  if (length(other_cols) > 0) openxlsx::setColWidths(wb, sheet, cols = other_cols, widths = "auto")
}

# ---------------------------------------------------------------------------- #
# 12.1 c) Tomma kolumner                                                       #
# ---------------------------------------------------------------------------- #

# Lägger in tomma kolumner för visuell separation i tabellen
add_spacer_columns <- function(table_df) {
  table_df |>
    tibble::add_column(` `   = NA, .after = "Alternativ matningstyp") |>
    tibble::add_column(`  `  = NA, .after = "Antal bortkopplade transformatorer efter") |>
    tibble::add_column(`   ` = NA, .after = "Isolerade kunder efter") |>
    tibble::add_column(`    ` = NA, .after = "Isolerad KILE energi per h")
}

# ---------------------------------------------------------------------------- #
# 12.1 d) Villkorsstyrd formattering                                           #
# ---------------------------------------------------------------------------- #

# Tillämpar villkorsstyrd formattering baserat på återställningsstatus
apply_status_conditional_formatting <- function(wb, sheet, k_helt, data_rows, data_cols, style_green, style_yellow, style_red, 
                            levels = c("JA", "DELVIS", "NEJ")) {
  
  # Bygger mapping mellan värde och stilar
  rule_map <- list(
    JA = list(rule  = sprintf('$%s%d="JA"',   k_helt, min(data_rows)), style = style_green),
    DELVIS = list(rule  = sprintf('$%s%d="DELVIS"', k_helt, min(data_rows)), style = style_yellow),
    NEJ = list(rule  = sprintf('$%s%d="NEJ"',  k_helt, min(data_rows)), style = style_red)
  )
  
  # Applicerar villkorsstyrd formattering för varje värde
  for (lvl in levels) {
    for (col in data_cols) {
      openxlsx::conditionalFormatting(
        wb, sheet,
        cols = col, rows = data_rows,
        type = "expression",
        rule = rule_map[[lvl]]$rule,
        style = rule_map[[lvl]]$style
      )
    }
  }
  
  # Ingen retur
  invisible(NULL)
}

# ============================================================================ #
# 12.2) Huvudfunktion: export_data                                             #
# ============================================================================ #
# Definierar funktion "export_data" som sammanställer och exporterar resultat till Excel
export_data <- function(
    scenario_results, scenarios, transformer_vulnerability = NULL,
    export_data_flag = FALSE, flags,
    valid_boundary_candidates, valid_internal_candidates,
    base_topology, inputs, export_path
) {
  
  # Avbryter direkt om export inte är aktiverad
  if (!isTRUE(export_data_flag)) return(invisible(NULL))
  
  # Säkerställer att exportkatalog finns och skapar ny Excel-fil
  dir.create(dirname(export_path), recursive = TRUE, showWarnings = FALSE)
  wb <- openxlsx::createWorkbook()
  
  # -------------------------------------------------------------------------- #
  # 12.2 a) Initiering av arbetsbok och stilar                                 #
  # -------------------------------------------------------------------------- #
  
  # Definierar grundläggande cell- och rubrikstilar
  center_style         <- openxlsx::createStyle(halign = "center",valign = "center", wrapText = TRUE)
  header_style         <- openxlsx::createStyle(textDecoration = "bold", valign = "center")
  section_header_style <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center")
  
  # Definierar ram- och kantstilar för tabellstruktur
  border_style         <- openxlsx::createStyle(border = "TopBottom", borderStyle = "thin")
  no_border_style      <- openxlsx::createStyle(border = c("top","bottom","left","right"),borderStyle = "thin",borderColour = "white")
  style_left_border    <- openxlsx::createStyle(border = "left", borderStyle = "thin")

  # Definierar formatstilar för celljustering och numeriska värden
  no_wrap_style        <- openxlsx::createStyle(wrapText = FALSE)
  top_align_style      <- openxlsx::createStyle(valign = "top")
  pct_style            <- openxlsx::createStyle(numFmt = "0.0%")
  num2_style           <- openxlsx::createStyle(numFmt = "0.00")
  num1_style           <- openxlsx::createStyle(numFmt = "0.0")
  num0_style           <- openxlsx::createStyle(numFmt = "0")
  
  # Definierar stilar för status- och specialmarkering
  style_green          <- openxlsx::createStyle(bgFill = "#C6EFCE")
  style_yellow         <- openxlsx::createStyle(bgFill = "#FFEB9C")
  style_red            <- openxlsx::createStyle(bgFill = "#FFC7CE")
  style_traf_orange    <- openxlsx::createStyle(bgFill = "#F4B183")
  
  # Definierar kolumnnamn för visuella avstånd i tabellen
  SPACER_COLS          <- c(" ", "  ", "   ", "    ")
  
  # Definierar fasta kolumnbredder för exporttabellen
  fixed_widths <- c(
    "Primär frånskiljare" = 10,
    "Sekundär frånskiljare" = 10,
    "Bortkopplade transformatorer före" = 19,
    "Bortkopplade transformatorer efter" = 19,
    "Antal bortkopplade transformatorer före"  = 19,
    "Antal bortkopplade transformatorer efter" = 19,
    "Berörda kunder före" = 11,
    "Berörda kunder efter" = 12,
    "Bortsektionerade kunder efter" = 16,
    "Isolerade kunder efter" = 12,
    "Helt återställt?" = 9,
    "Alternativ matning" = 10,
    "Alternativ matningstyp" = 11,
    "KILE energi per h före" = 10,
    "KILE energi per h efter" = 10,
    "Bortsektionerad KILE energi per h" = 16,
    "Isolerad KILE energi per h" = 12,
    "Max kundeffekt före (kW)" = 14,
    "Max kundeffekt efter (kW)" = 14,
    "Viktad ledningslängd (m)" = 16,
    "Scenariovikt" = 15,
    "Friledning ej i skog (m)" = 11.5,
    "Friledning i skog (m)" = 11.5,
    "Hängkabel ej i skog (m)" = 11,
    "Hängkabel i skog (m)" = 9.5,
    "BLX-ledning ej i skog (m)" = 12,
    "BLX-ledning i skog (m)" = 11,
    "Jordkabel (m)" = 9,
    "Jordkabel i sjö (m)" = 9,
    "Sjökabel (m)" = 9,
    "Annan ledarkonstruktion (m)" = 20,
    "Odefinierad (m)" = 13,
    "Segmentlängd (m)" = 13,
    "Saknas i LINE_INFO (m)" = 14,
    "Påverkande scenarier före" = 13,
    "Påverkande scenarier efter" = 13,
    " " = 2, "  " = 2, "   " = 2, "    " = 2
  )
  
  # Avgör om scenarioviktning kan beräknas (kräver LINE_INFO och mv_subedge_lengths)
  use_scenario_weights <- !is.null(inputs$LINE_INFO) &&
    nrow(inputs$LINE_INFO) > 0 &&
    !is.null(base_topology$mv_subedge_lengths)
  
  # -------------------------------------------------------------------------- #
  # 12.2 b) Förberäkningar: scenariotabeller och nyckeltal                     #
  # -------------------------------------------------------------------------- #
  
  # Klassar scenarier efter återställningsgrad
  scenario_results_enriched <- scenario_results |>
    dplyr::mutate(
      `Helt återställt?` = dplyr::case_when(
        `Berörda kunder före` > 0 & `Berörda kunder efter` == 0 ~ "JA",
        `Berörda kunder före` > 0 & `Berörda kunder efter` > 0 &
          `Berörda kunder efter` < `Berörda kunder före` ~ "DELVIS",
        `Berörda kunder före` > 0 &
          `Berörda kunder efter` == `Berörda kunder före` ~ "NEJ",
        TRUE ~ NA_character_
      )
    )
  
  # Förbereder exporttabell för samtliga scenarier
  scenario_results_sorted <- scenario_results_enriched |>
    dplyr::arrange(
      dplyr::desc(`Berörda kunder före`),
      dplyr::desc(`Berörda kunder efter`),
      `Primär frånskiljare`
    ) |>
    add_spacer_columns()
  
  # -------------------------------------------------------------------------- #
  # 12.2 c) Förberäkningar: transformatorer och inställningar                  #
  # -------------------------------------------------------------------------- #
  
  # Förbereder transformatorsammanställningar
  transformer_vulnerability_sorted <- NULL

  # Sorterar och filtrerar transformatorer med faktisk påverkan
  if (!is.null(transformer_vulnerability) && nrow(transformer_vulnerability) > 0) {
    transformer_vulnerability_sorted <- transformer_vulnerability |>
      dplyr::arrange(
        dplyr::desc(`Påverkande scenarier före`),
        dplyr::desc(`Påverkande scenarier efter`),
        dplyr::desc(`Berörda kunder`)
      )
  }
  
  # Förbereder tabell med inställningar för export
  settings_rows <- list()
  
  # Definierar vilka flaggor som ska presenteras
  flags_keep <- setdiff(
    names(flags),
    c("EXPORT_DATA", "EXPORT_PLOTS", "PLOT_CANDIDATES", "PLOT_SCENARIOS")
  )
  
  # Tar med plot-flaggor om exportflaggan är aktiverad
  flags_keep <- c(
    flags_keep,
    if (isTRUE(flags$EXPORT_PLOTS))
      c("EXPORT_PLOTS", "PLOT_CANDIDATES", "PLOT_SCENARIOS")
    else
      "EXPORT_PLOTS"
  )
  
  # Samlar aktiva flaggor
  settings_rows[["flags"]] <- tibble::tibble(
    Inställning = paste0("FLAG: ", flags_keep),
    Värde = vapply(
      flags[flags_keep],
      function(x) if (is.logical(x)) ifelse(x, "TRUE", "FALSE") else as.character(x),
      character(1)
    )
  )
  
  # Samlar valda BOUNDARY-alternativ
  settings_rows[["boundary"]] <- tibble::tibble(
    Inställning = "BOUNDARY – använda frånskiljare",
    Värde = if (nrow(valid_boundary_candidates) == 0) "inga"
    else paste(
      base_topology$disconnector_summary$LABEL[
        match(valid_boundary_candidates$ID,
              base_topology$disconnector_summary$ID)
      ],
      collapse = ", "
    )
  )
  
  # Samlar valda INTERNAL-alternativ
  settings_rows[["internal"]] <- tibble::tibble(
    Inställning = "INTERNAL – använda frånskiljare",
    Värde = if (nrow(valid_internal_candidates) == 0) "inga"
    else paste(
      base_topology$disconnector_summary$LABEL[
        match(valid_internal_candidates$ID,
              base_topology$disconnector_summary$ID)
      ],
      collapse = ", "
    )
  )
  
  # Slår samman inställningar till exportklar tabell
  settings_table <- dplyr::bind_rows(settings_rows)
  
  # -------------------------------------------------------------------------- #
  # 12.2 d) Förberäkningar: Scenariovikt                                       #
  # -------------------------------------------------------------------------- #
  
  # Körs endast om LINE_INFO finns
  if (use_scenario_weights) {
  
    # Bygger segmentindex för grundtopologin
    seg <- build_segment_index(base_topology, flags)
    
    # Mappar MVPART till segment baserat på delkanter
    mv_seg_map <- base_topology$mv_subedge_lengths |>
      dplyr::filter(length_m > 0) |>
      dplyr::mutate(seg_id = dplyr::coalesce(seg$seg_id[from], seg$seg_id[to])) |>
      dplyr::select(MVPART_ID, seg_id) |>
      dplyr::distinct()
    
    # Förbereder längder per MVPART och ledningstyp från exponeringsdata
    mv_type_len <- inputs$LINE_INFO |>
      dplyr::transmute(
        MVPART_ID                     = as.character(`NCMLFPART.ID = MVPART.ID`),
        `Odefinierad [m]`             = as.numeric(`Odefinierad [m]`),
        `Friledning [m]`              = as.numeric(`Friledning [m]`),
        `Friledning i skog [m]`       = as.numeric(`Friledning i skog [m]`),
        `Hängkabel [m]`               = as.numeric(`Hängkabel [m]`),
        `Hängkabel i skog [m]`        = as.numeric(`Hängkabel i skog [m]`),
        `BLX-ledning [m]`             = as.numeric(`BLX-ledning [m]`),
        `BLX-ledning i skog [m]`      = as.numeric(`BLX-ledning i skog [m]`),
        `Jordkabel [m]`               = as.numeric(`Jordkabel [m]`),
        `Jordkabel i sjö [m]`         = as.numeric(`Jordkabel i sjö [m]`),
        `Sjökabel [m]`                = as.numeric(`Sjökabel [m]`),
        `Annan ledarkonstruktion [m]` = as.numeric(`Annan ledarkonstruktion [m]`)
      )
    
    # Definierar koppling mellan exportkolumner, Elforsk-klass och exponeringsvärde
    elforsk_exposure_lookup <- tibble::tibble(
      Kolumn = c(
        "Odefinierad (m)", "Friledning ej i skog (m)", "Friledning i skog (m)",
        "Hängkabel ej i skog (m)", "Hängkabel i skog (m)",
        "BLX-ledning ej i skog (m)", "BLX-ledning i skog (m)",
        "Jordkabel (m)", "Jordkabel i sjö (m)", "Sjökabel (m)",
        "Annan ledarkonstruktion (m)"
      ),
      `Elforsk-klass` = c(
        "Oisolerad luft", "Oisolerad luft", "Oisolerad luft",
        "Hängkabel", "Hängkabel", "Belagd luft", "Belagd luft",
        "Jordkabel", "Jordkabel", "Jordkabel", "Oisolerad luft"
      ),
      Exponeringsvärde = c(20, 2, 20, 1, 5, 1, 8, 1, 1, 1, 20)
    )
    
    # Initierar lista för exponeringsgrader per scenario
    exposure_rows <- list()
    
    # Itererar över scenarier för att beräkna exponeringsdata
    for (sc_name in names(scenarios)) {
      sc <- scenarios[[sc_name]]
      
      # Identifierar primär frånskiljare
      primary_label <- if (!is.na(sc$seed_id)) {
        base_topology$disconnector_summary$LABEL[
          match(sc$seed_id, base_topology$disconnector_summary$ID)
        ]
      } else if (identical(sc_name, "START_FAULT")) "START" else NA_character_
      
      # Identifierar sekundära frånskiljare
      secondary_ids <- setdiff(sc$open_ids, sc$seed_id)
      secondary_label <- if (length(secondary_ids) == 0) "" else paste(
        base_topology$disconnector_summary$LABEL[
          match(secondary_ids, base_topology$disconnector_summary$ID)
        ],
        collapse = ", "
      )
      
      # Identifierar MVPART som ingår i felande segment
      mv_ids_in_segment <- mv_seg_map |>
        dplyr::filter(seg_id == as.character(sc$fault_seg)) |>
        dplyr::pull(MVPART_ID) |>
        unique()
      
      # Summerar längder per MVPART inom felande segment
      mv_len_in_segment <- base_topology$mv_subedge_lengths |>
        dplyr::filter(length_m > 0) |>
        dplyr::mutate(seg_id = dplyr::coalesce(seg$seg_id[from], seg$seg_id[to])) |>
        dplyr::filter(seg_id == as.character(sc$fault_seg), MVPART_ID %in% mv_ids_in_segment) |>
        dplyr::group_by(MVPART_ID) |>
        dplyr::summarise(length_m = sum(length_m, na.rm = TRUE), .groups = "drop")
      
      # Kopplar längder per MVPART till exponeringsdata
      mv_in_segment <- mv_len_in_segment |>
        dplyr::left_join(mv_type_len, by = "MVPART_ID")
      
      # Summerar exponeringslängder per ledningstyp inom scenariot
      overhead_total_length   <- sum(mv_in_segment$`Friledning [m]`, na.rm = TRUE)
      aerial_total_length     <- sum(mv_in_segment$`Hängkabel [m]`, na.rm = TRUE)
      underground_length      <- sum(mv_in_segment$`Jordkabel [m]`, na.rm = TRUE)
      lake_cable_length       <- sum(mv_in_segment$`Sjökabel [m]`, na.rm = TRUE)
      other_construction_len  <- sum(mv_in_segment$`Annan ledarkonstruktion [m]`, na.rm = TRUE)
      blx_total_length        <- sum(mv_in_segment$`BLX-ledning [m]`, na.rm = TRUE)
      overhead_forest_length  <- sum(mv_in_segment$`Friledning i skog [m]`, na.rm = TRUE)
      aerial_forest_length    <- sum(mv_in_segment$`Hängkabel i skog [m]`, na.rm = TRUE)
      blx_forest_length       <- sum(mv_in_segment$`BLX-ledning i skog [m]`, na.rm = TRUE)
      underground_water_len   <- sum(mv_in_segment$`Jordkabel i sjö [m]`, na.rm = TRUE)
      undef_length            <- sum(mv_in_segment$`Odefinierad [m]`, na.rm = TRUE)
      
      # Skapar exponeringsrad för scenariot i exportformat
      exposure_rows[[sc_name]] <- tibble::tibble(
        `Primär frånskiljare`              = primary_label,
        `Sekundär frånskiljare`            = secondary_label,
        `Friledning ej i skog (m)`         = overhead_total_length - overhead_forest_length,
        `Friledning i skog (m)`            = overhead_forest_length,
        `Hängkabel ej i skog (m)`          = aerial_total_length - aerial_forest_length,
        `Hängkabel i skog (m)`             = aerial_forest_length,
        `BLX-ledning ej i skog (m)`        = blx_total_length - blx_forest_length,
        `BLX-ledning i skog (m)`           = blx_forest_length,
        `Jordkabel (m)`                    = underground_length,
        `Jordkabel i sjö (m)`              = underground_water_len,
        `Sjökabel (m)`                     = lake_cable_length,
        `Annan ledarkonstruktion (m)`      = other_construction_len,
        `Odefinierad (m)`                  = undef_length
      )
    }
    
    # Slår samman exponeringsdata för samtliga scenarier
    scenario_exposure <- dplyr::bind_rows(exposure_rows, .id = "scenario_key")
    
    # Fastställer radordning baserat på scenarier-tabeller
    order_key_vec <- paste(
      scenario_results_sorted$`Primär frånskiljare`,
      scenario_results_sorted$`Sekundär frånskiljare`
    )
    
    # Anpassar exponeringsdata till samma radordning som scenarier
    scenario_exposure <- scenario_exposure |>
      dplyr::mutate(row_key = paste(`Primär frånskiljare`, `Sekundär frånskiljare`))
    
    # Flyttar om rader baserat på beräknad nyckel
    idx <- match(order_key_vec, scenario_exposure$row_key)
    scenario_exposure <- scenario_exposure[idx, , drop = FALSE] |>
      dplyr::select(-row_key)
    
    # Hämtar segmentlängd per scenario
    segment_length_by_scenario <- vapply(scenarios, function(sc) sc$segment_length_m, numeric(1))
    seglen_vec <- unname(segment_length_by_scenario[scenario_exposure$scenario_key])
    seglen_vec[is.na(seglen_vec)] <- 0
    
    # Bygger slutgiltig exponeringstabell med korrekt kolumnordning
    scenario_exposure_out <- scenario_exposure |>
      dplyr::select(-scenario_key) |>
      tibble::add_column(`Viktad ledningslängd (m)` = NA_real_, .after = "Sekundär frånskiljare") |>
      tibble::add_column(`Scenariovikt` = NA_real_, .after = "Viktad ledningslängd (m)") |>
      tibble::add_column(`Segmentlängd (m)` = seglen_vec) |>
      tibble::add_column(`Saknas i LINE_INFO (m)` = NA_real_)
    
    # Identifierar kolumner som innehåller exponeringslängder
    scenario_length_cols <- colnames(scenario_exposure)[3:ncol(scenario_exposure)]
    
    # Förbereder Elforsk-värden i samma ordning som exponeringskolumnerna
    elforsk_header <- elforsk_exposure_lookup |>
      dplyr::filter(Kolumn %in% scenario_length_cols) |>
      dplyr::slice(match(scenario_length_cols, Kolumn)) |>
      dplyr::select(Kolumn, `Elforsk-klass`, Exponeringsvärde) |>
      tibble::column_to_rownames("Kolumn") |>
      t() |>
      as.data.frame()
  }
  
  # -------------------------------------------------------------------------- #
  # 12.2 e) Blad 1: Nyckeltal                                                  #
  # -------------------------------------------------------------------------- #
  
  openxlsx::addWorksheet(wb,"Nyckeltal")
  
  # -------------------------------------------------------------------------- #
  # Rubriker för Nyckeltal                                                     #
  # -------------------------------------------------------------------------- #
  
  labels_rows_main <- c("Kunder [st]","KILE [tkr/h]","Maxeffekt [MW]","","Viktade nyckeltal efter","Kundtillgänglighet [%]","KILE-tillgänglighet [%]","Storkundstillgänglighet [%]")
  labels_rows_scenarios <- c("Totalt","Påverkande","","Ej återställda","Delvis återställda","","Fullt återställda","Opåverkande scenarier")
  labels_rows_other <- c("Kundbortfall [st]","Kundtillgänglighet","","KILE-bortfall [kr/h]","KILE-tillgänglighet","","Maxeffektbortfall [kW]","Storkundstillgänglighet")

  # -------------------------------------------------------------------------- #
  # Sektionsrubriker i Nyckeltal                                               #
  # -------------------------------------------------------------------------- #
  
  openxlsx::writeData(wb,"Nyckeltal","Huvudresultat",startCol=2,startRow=2,colNames=FALSE)
  openxlsx::mergeCells(wb,"Nyckeltal",cols=2:3,rows=2)
  openxlsx::writeData(wb,"Nyckeltal","Scenarier",startCol=5,startRow=2,colNames=FALSE)
  openxlsx::mergeCells(wb,"Nyckeltal",cols=5:7,rows=2)
  openxlsx::writeData(wb,"Nyckeltal","Före alternativ matning",startCol=9,startRow=2,colNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal","Efter alternativ matning",startCol=12,startRow=2,colNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal","Nettoförändring (diff)",startCol=15,startRow=2,colNames=FALSE)
  openxlsx::mergeCells(wb,"Nyckeltal",cols=9:10,rows=2)
  openxlsx::mergeCells(wb,"Nyckeltal",cols=12:13,rows=2)
  openxlsx::mergeCells(wb,"Nyckeltal",cols=15:16,rows=2)
  openxlsx::addStyle(wb,"Nyckeltal",section_header_style,rows=2,cols=c(2:3,5:7,9:10,12:13,15:16),gridExpand=TRUE,stack=TRUE)
  
  # -------------------------------------------------------------------------- #
  # Kolumnrubriker i Nyckeltal                                                 #
  # -------------------------------------------------------------------------- #
  
  openxlsx::writeData(wb,"Nyckeltal",t(as.matrix(c("Totala nyckeltal","Värde"))),startCol=2,startRow=3,colNames=FALSE,rowNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal",t(as.matrix(c("Nyckeltal","Värde","Andel"))),startCol=5,startRow=3,colNames=FALSE,rowNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal",t(as.matrix(c("Viktade nyckeltal","Värde"))),startCol=9,startRow=3,colNames=FALSE,rowNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal",t(as.matrix(c("Viktade nyckeltal","Värde"))),startCol=12,startRow=3,colNames=FALSE,rowNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal",t(as.matrix(c("Viktade nyckeltal","Värde"))),startCol=15,startRow=3,colNames=FALSE,rowNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal",t(as.matrix(c("Oviktade nyckeltal","Värde"))),startCol=9,startRow=14,colNames=FALSE,rowNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal",t(as.matrix(c("Oviktade nyckeltal","Värde"))),startCol=12,startRow=14,colNames=FALSE,rowNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal",t(as.matrix(c("Oviktade nyckeltal","Värde"))),startCol=15,startRow=14,colNames=FALSE,rowNames=FALSE)
  openxlsx::addStyle(wb,"Nyckeltal",header_style,rows=c(3,14),cols=c(9:10,12:13,15:16),gridExpand=TRUE,stack=TRUE)
  
  openxlsx::writeData(wb,"Nyckeltal","Före alternativ matning",startCol=9,startRow=13,colNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal","Efter alternativ matning",startCol=12,startRow=13,colNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal","Nettoförändring (diff)",startCol=15,startRow=13,colNames=FALSE)
  openxlsx::mergeCells(wb,"Nyckeltal",cols=9:10,rows=13)
  openxlsx::mergeCells(wb,"Nyckeltal",cols=12:13,rows=13)
  openxlsx::mergeCells(wb,"Nyckeltal",cols=15:16,rows=13)
  openxlsx::addStyle(wb,"Nyckeltal",section_header_style,rows=13,cols=c(9:10,12:13,15:16),gridExpand=TRUE,stack=TRUE)
  
  # -------------------------------------------------------------------------- #
  # Radrubriker i Nyckeltal                                                    #
  # -------------------------------------------------------------------------- #
  
  openxlsx::writeData(wb,"Nyckeltal",data.frame(Nyckeltal=labels_rows_main,Värde=NA),startCol=2,startRow=4,colNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal",data.frame(Nyckeltal=labels_rows_scenarios,Värde=NA,Andel=NA),startCol=5,startRow=4,colNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal",data.frame(Nyckeltal=labels_rows_other,Värde=NA),startCol=9,startRow=4,colNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal",data.frame(Nyckeltal=labels_rows_other,Värde=NA),startCol=12,startRow=4,colNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal",data.frame(Nyckeltal=labels_rows_other,Värde=NA),startCol=15,startRow=4,colNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal",data.frame(Nyckeltal=labels_rows_other,Värde=NA),startCol=9,startRow=15,colNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal",data.frame(Nyckeltal=labels_rows_other,Värde=NA),startCol=12,startRow=15,colNames=FALSE)
  openxlsx::writeData(wb,"Nyckeltal",data.frame(Nyckeltal=labels_rows_other,Värde=NA),startCol=15,startRow=15,colNames=FALSE)
  openxlsx::addStyle(wb,"Nyckeltal",openxlsx::createStyle(textDecoration="bold",halign="left",valign="center"),rows=8,cols=c(2,3),stack=TRUE)
  
  # -------------------------------------------------------------------------- #
  # Formler och format                                                         #
  # -------------------------------------------------------------------------- #
  
  openxlsx::writeFormula(wb,"Nyckeltal","=MAX(Scenarier!M:M)",startCol=3,startRow=4)
  openxlsx::writeFormula(wb,"Nyckeltal","=MAX(Scenarier!R:R)/1000",startCol=3,startRow=5)
  openxlsx::writeFormula(wb,"Nyckeltal","=MAX(Scenarier!W:W)/1000",startCol=3,startRow=6)
  
  openxlsx::writeData(wb,"Nyckeltal","Värde",startCol=3,startRow=8)
  openxlsx::writeFormula(wb,"Nyckeltal","=M5*100",startCol=3,startRow=9)
  openxlsx::writeFormula(wb,"Nyckeltal","=M8*100",startCol=3,startRow=10)
  openxlsx::writeFormula(wb,"Nyckeltal","=M11*100",startCol=3,startRow=11)
  
  openxlsx::writeFormula(wb,"Nyckeltal","=COUNTA(Scenarier!A2:A1000)",startCol=6,startRow=4)
  openxlsx::writeFormula(wb,"Nyckeltal","=F4-F10-F11",startCol=6,startRow=5)
  openxlsx::writeFormula(wb,"Nyckeltal","=COUNTIF(Scenarier!D:D,\"NEJ\")",startCol=6,startRow=7)
  openxlsx::writeFormula(wb,"Nyckeltal","=COUNTIF(Scenarier!D:D,\"DELVIS\")",startCol=6,startRow=8)
  openxlsx::writeFormula(wb,"Nyckeltal","=COUNTIF(Scenarier!D:D,\"JA\")",startCol=6,startRow=10)
  openxlsx::writeFormula(wb,"Nyckeltal","=COUNTIF(Scenarier!M:M,0)",startCol=6,startRow=11)
  
  openxlsx::writeFormula(wb,"Nyckeltal","=F5/F4",startCol=7,startRow=5)
  openxlsx::writeFormula(wb,"Nyckeltal","=F7/F4",startCol=7,startRow=7)
  openxlsx::writeFormula(wb,"Nyckeltal","=F8/F4",startCol=7,startRow=8)
  openxlsx::writeFormula(wb,"Nyckeltal","=F10/F4",startCol=7,startRow=10)
  openxlsx::writeFormula(wb,"Nyckeltal","=F11/F4",startCol=7,startRow=11)
  
  if (use_scenario_weights) {
    openxlsx::writeFormula(wb,"Nyckeltal","=SUMPRODUCT('Scenariovikt'!D6:D1004,Scenarier!M2:M1000)",startCol=10,startRow=4)
    openxlsx::writeFormula(wb,"Nyckeltal","=SUMPRODUCT('Scenariovikt'!D6:D1004,Scenarier!N2:N1000)",startCol=13,startRow=4)
    openxlsx::writeFormula(wb,"Nyckeltal","=M4-J4",startCol=16,startRow=4)
    
    openxlsx::writeFormula(wb,"Nyckeltal","=1-J4/C4",startCol=10,startRow=5)
    openxlsx::writeFormula(wb,"Nyckeltal","=1-M4/C4",startCol=13,startRow=5)
    openxlsx::writeFormula(wb,"Nyckeltal","=M5-J5",startCol=16,startRow=5)
    
    openxlsx::writeFormula(wb,"Nyckeltal","=SUMPRODUCT('Scenariovikt'!D6:D1004,Scenarier!R2:R1000)",startCol=10,startRow=7)
    openxlsx::writeFormula(wb,"Nyckeltal","=SUMPRODUCT('Scenariovikt'!D6:D1004,Scenarier!S2:S1000)",startCol=13,startRow=7)
    openxlsx::writeFormula(wb,"Nyckeltal","=M7-J7",startCol=16,startRow=7)
    
    openxlsx::writeFormula(wb,"Nyckeltal","=1-J7/C5/1000",startCol=10,startRow=8)
    openxlsx::writeFormula(wb,"Nyckeltal","=1-M7/C5/1000",startCol=13,startRow=8)
    openxlsx::writeFormula(wb,"Nyckeltal","=M8-J8",startCol=16,startRow=8)
    
    openxlsx::writeFormula(wb,"Nyckeltal","=SUMPRODUCT('Scenariovikt'!D6:D1004,Scenarier!W2:W1000)",startCol=10,startRow=10)
    openxlsx::writeFormula(wb,"Nyckeltal","=SUMPRODUCT('Scenariovikt'!D6:D1004,Scenarier!X2:X1000)",startCol=13,startRow=10)
    openxlsx::writeFormula(wb,"Nyckeltal","=M10-J10",startCol=16,startRow=10)
    
    openxlsx::writeFormula(wb,"Nyckeltal","=1-J10/C6/1000",startCol=10,startRow=11)
    openxlsx::writeFormula(wb,"Nyckeltal","=1-M10/C6/1000",startCol=13,startRow=11)
    openxlsx::writeFormula(wb,"Nyckeltal","=M11-J11",startCol=16,startRow=11)
  }
  
  openxlsx::writeFormula(wb,"Nyckeltal","=AVERAGE(Scenarier!M:M)",startCol=10,startRow=15)
  openxlsx::writeFormula(wb,"Nyckeltal","=AVERAGE(Scenarier!N:N)",startCol=13,startRow=15)
  openxlsx::writeFormula(wb,"Nyckeltal","=M15-J15",startCol=16,startRow=15)
  
  openxlsx::writeFormula(wb,"Nyckeltal","=1-J15/C4",startCol=10,startRow=16)
  openxlsx::writeFormula(wb,"Nyckeltal","=1-M15/C4",startCol=13,startRow=16)
  openxlsx::writeFormula(wb,"Nyckeltal","=M16-J16",startCol=16,startRow=16)
  
  openxlsx::writeFormula(wb,"Nyckeltal","=AVERAGE(Scenarier!R:R)",startCol=10,startRow=18)
  openxlsx::writeFormula(wb,"Nyckeltal","=AVERAGE(Scenarier!S:S)",startCol=13,startRow=18)
  openxlsx::writeFormula(wb,"Nyckeltal","=M18-J18",startCol=16,startRow=18)
  
  openxlsx::writeFormula(wb,"Nyckeltal","=1-J18/C5/1000",startCol=10,startRow=19)
  openxlsx::writeFormula(wb,"Nyckeltal","=1-M18/C5/1000",startCol=13,startRow=19)
  openxlsx::writeFormula(wb,"Nyckeltal","=M19-J19",startCol=16,startRow=19)
  
  openxlsx::writeFormula(wb,"Nyckeltal","=AVERAGE(Scenarier!W:W)",startCol=10,startRow=21)
  openxlsx::writeFormula(wb,"Nyckeltal","=AVERAGE(Scenarier!X:X)",startCol=13,startRow=21)
  openxlsx::writeFormula(wb,"Nyckeltal","=M21-J21",startCol=16,startRow=21)
  
  openxlsx::writeFormula(wb,"Nyckeltal","=1-J21/C6/1000",startCol=10,startRow=22)
  openxlsx::writeFormula(wb,"Nyckeltal","=1-M21/C6/1000",startCol=13,startRow=22)
  openxlsx::writeFormula(wb,"Nyckeltal","=M22-J22",startCol=16,startRow=22)
  
  # -------------------------------------------------------------------------- #
  # Grundformat                                                                #
  # -------------------------------------------------------------------------- #
  
  openxlsx::addStyle(wb,"Nyckeltal",no_border_style,rows=1:1000,cols=1:30,gridExpand=TRUE,stack=TRUE)
  openxlsx::setColWidths(wb,"Nyckeltal",cols=2,widths=25)
  openxlsx::setColWidths(wb,"Nyckeltal",cols=c(5,9,12,15),widths=21)
  openxlsx::setColWidths(wb,"Nyckeltal",cols=c(3,6,7,10,13,16),widths=8)
  openxlsx::setColWidths(wb,"Nyckeltal",cols=c(1,4,8,11,14),widths=2)
  openxlsx::setRowHeights(wb,"Nyckeltal",rows=1:1000,heights=18)
  openxlsx::addStyle(wb,"Nyckeltal",center_style,rows=1:22,cols=1:16,gridExpand=TRUE,stack=TRUE)
  
  openxlsx::conditionalFormatting(wb,"Nyckeltal",cols=2:3,rows=2:11,type="expression",rule="TRUE",style=openxlsx::createStyle(bgFill="#EEF2F7"))
  openxlsx::conditionalFormatting(wb,"Nyckeltal",cols=5:7,rows=2:11,type="expression",rule="TRUE",style=openxlsx::createStyle(bgFill="#F4F4F4"))
  openxlsx::conditionalFormatting(wb,"Nyckeltal",cols=9:10,rows=2:11,type="expression",rule="TRUE",style=openxlsx::createStyle(bgFill="#E9F2EC"))
  openxlsx::conditionalFormatting(wb,"Nyckeltal",cols=12:13,rows=2:11,type="expression",rule="TRUE",style=openxlsx::createStyle(bgFill="#E9F2EC"))
  openxlsx::conditionalFormatting(wb,"Nyckeltal",cols=15:16,rows=2:11,type="expression",rule="TRUE",style=openxlsx::createStyle(bgFill="#E9F2EC"))
  openxlsx::conditionalFormatting(wb,"Nyckeltal",cols=9:10,rows=13:22,type="expression",rule="TRUE",style=openxlsx::createStyle(bgFill="#F2F0E9"))
  openxlsx::conditionalFormatting(wb,"Nyckeltal",cols=12:13,rows=13:22,type="expression",rule="TRUE",style=openxlsx::createStyle(bgFill="#F2F0E9"))
  openxlsx::conditionalFormatting(wb,"Nyckeltal",cols=15:16,rows=13:22,type="expression",rule="TRUE",style=openxlsx::createStyle(bgFill="#F2F0E9"))
  
  # -------------------------------------------------------------------------- #
  # Kanter och linjer                                                          #
  # -------------------------------------------------------------------------- #
  
  openxlsx::addStyle(wb,"Nyckeltal",header_style,rows=3,cols=c(2:3,5:7),gridExpand=TRUE,stack=TRUE)
  openxlsx::addStyle(wb,"Nyckeltal",style_left_border,rows=c(2:11),cols=c(2,4,5,8),gridExpand=TRUE,stack=TRUE)
  openxlsx::addStyle(wb,"Nyckeltal",style_left_border,rows=c(2:11,13:22),cols=c(9,11,12,14,15,17),gridExpand=TRUE,stack=TRUE)
  openxlsx::addStyle(wb,"Nyckeltal",border_style,rows=1:11,cols=c(2,3,5:7),gridExpand=TRUE,stack=TRUE)
  openxlsx::addStyle(wb,"Nyckeltal",border_style,rows=1:22,cols=c(9:10,12:13,15:16),gridExpand=TRUE,stack=TRUE)
  openxlsx::addStyle(wb,"Nyckeltal",openxlsx::createStyle(border="left",borderStyle="thick",borderColour="black"),rows=2:11,cols=c(2,4),gridExpand=TRUE,stack=TRUE)
  openxlsx::addStyle(wb,"Nyckeltal",openxlsx::createStyle(border="bottom",borderStyle="thick",borderColour="black"),rows=c(1,11),cols=2:3,gridExpand=TRUE,stack=TRUE)
  
  openxlsx::addStyle(wb,"Nyckeltal",pct_style,rows=c(5,7,8,10,11),cols=7,gridExpand=TRUE,stack=TRUE)
  openxlsx::addStyle(wb,"Nyckeltal",pct_style,rows=c(5,8,11,16,19,22),cols=c(10,13,16),gridExpand=TRUE,stack=TRUE)
  openxlsx::addStyle(wb,"Nyckeltal",num1_style,rows=c(5:6,9:11),cols=3,gridExpand=TRUE,stack=TRUE)
  openxlsx::addStyle(wb,"Nyckeltal",num1_style,rows=c(4,10,15,21),cols=c(10,13,16),gridExpand=TRUE,stack=TRUE)
  openxlsx::addStyle(wb,"Nyckeltal",num0_style,rows=c(7,18),cols=c(10,13,16),gridExpand=TRUE,stack=TRUE)

  # -------------------------------------------------------------------------- #
  # 12.2 f) Blad 2: Scenariovikt                                               #
  # -------------------------------------------------------------------------- #
  
  if (use_scenario_weights) {
    openxlsx::addWorksheet(wb,"Scenariovikt")
    
    # ------------------------------------------------------------------------ #
    # Rubrik och metadata                                                      #
    # ------------------------------------------------------------------------ #
    
    openxlsx::writeData(wb,"Scenariovikt",elforsk_header,startRow=1,startCol=4,rowNames=TRUE)
    openxlsx::writeData(wb,"Scenariovikt",x=t(as.matrix(as.numeric(elforsk_exposure_lookup$Exponeringsvärde))),startRow=3,startCol=5,colNames=FALSE,rowNames=FALSE)
    scenario_start_row <- nrow(elforsk_header) + 3
    
    # ------------------------------------------------------------------------ #
    # Scenario-data                                                            #
    # ------------------------------------------------------------------------ #
    
    openxlsx::writeData(wb,"Scenariovikt",scenario_exposure_out,startRow=scenario_start_row,startCol=1)
    first_row <- scenario_start_row + 1
    last_row <- scenario_start_row + nrow(scenario_exposure_out)
    openxlsx::setRowHeights(wb, sheet = "Scenariovikt", rows  = c(2:4, 6:last_row), heights = 15)  
    
    # ------------------------------------------------------------------------ #
    # Beräknade kolumner                                                       #
    # ------------------------------------------------------------------------ #
    
    for (r in seq(first_row,last_row)) openxlsx::writeFormula(wb,"Scenariovikt",x=sprintf("SUMPRODUCT($E$3:$O$3,E%d:O%d)",r,r),startRow=r,startCol=3)
    openxlsx::writeFormula(wb,"Scenariovikt",x=sprintf("SUM(C%d:C%d)",first_row,last_row),startRow=4,startCol=3)
    for (r in seq(first_row,last_row)) openxlsx::writeFormula(wb,"Scenariovikt",x=sprintf("C%d/$C$4",r),startRow=r,startCol=4)
    openxlsx::writeFormula(wb,"Scenariovikt",x=sprintf("SUM(D%d:D%d)",first_row,last_row),startRow=4,startCol=4)
    for (r in seq(first_row,last_row)) openxlsx::writeFormula(wb,"Scenariovikt",x=sprintf("P%d-SUM(E%d:O%d)",r,r,r),startRow=r,startCol=17)
    
    # ------------------------------------------------------------------------ #
    # Grundformat                                                              #
    # ------------------------------------------------------------------------ #
    
    total_cols <- max(ncol(scenario_exposure_out),ncol(elforsk_header)+4)
    apply_column_widths(wb,sheet="Scenariovikt",colnames_table=colnames(scenario_exposure_out),fixed_widths=fixed_widths)
    apply_basic_sheet_formatting(wb,sheet="Scenariovikt",n_rows=last_row,n_cols=total_cols,center_style=center_style,header_style=header_style,border_style=border_style,no_wrap_style=NULL,top_align_style=top_align_style)
    openxlsx::addStyle(wb,"Scenariovikt",style=openxlsx::createStyle(wrapText=TRUE),rows=c(2:4,6:last_row),cols=1:total_cols,gridExpand=TRUE,stack=TRUE)
    border_clear_style <- openxlsx::createStyle(border=c("top","bottom"),borderStyle="none")
    openxlsx::addStyle(wb,"Scenariovikt",style=border_clear_style,rows=1:4,cols=c(1:3,16:17),gridExpand=TRUE,stack=TRUE)
    openxlsx::setRowHeights(wb,"Scenariovikt",rows=c(2:4,6:(nrow(scenario_exposure_out)+1)),heights=15)
    
    # ------------------------------------------------------------------------ #
    # Kanter och rubriker                                                      #
    # ------------------------------------------------------------------------ #
    
    openxlsx::addStyle(wb,"Scenariovikt",style_left_border,rows=1:3,cols=4,gridExpand=TRUE,stack=TRUE)
    openxlsx::addStyle(wb,"Scenariovikt",style_left_border,rows=1:3,cols=16,gridExpand=TRUE,stack=TRUE)
    openxlsx::addStyle(wb,"Scenariovikt",style_left_border,rows=5:last_row,cols=18,gridExpand=TRUE,stack=TRUE)
    openxlsx::addStyle(wb,"Scenariovikt",style=openxlsx::createStyle(textDecoration="bold",halign="center",valign="center",wrapText=TRUE),rows=5,cols=1:total_cols,gridExpand=TRUE,stack=TRUE)
  }
  
  # -------------------------------------------------------------------------- #
  # 12.2 g) Blad 3: Scenarier                                                  #
  # -------------------------------------------------------------------------- #
  
  openxlsx::addWorksheet(wb,"Scenarier")
  openxlsx::writeData(wb,"Scenarier",scenario_results_sorted)
  
  # -------------------------------------------------------------------------- #
  # Grundformat                                                                #
  # -------------------------------------------------------------------------- #
  
  apply_basic_sheet_formatting(wb,sheet="Scenarier",n_rows=nrow(scenario_results_sorted)+1,n_cols=ncol(scenario_results_sorted),center_style=center_style,header_style=header_style,border_style=border_style,no_wrap_style=no_wrap_style,top_align_style=top_align_style)
  apply_column_widths(wb,sheet="Scenarier",colnames_table=colnames(scenario_results_sorted),fixed_widths=fixed_widths)
  openxlsx::addStyle(wb,sheet = "Scenarier",style = num0_style,rows = 2:(nrow(scenario_results_sorted) + 1),cols = 3,gridExpand = TRUE,stack = TRUE)
  
  # -------------------------------------------------------------------------- #
  # Villkorsstyrd formattering                                                 #
  # -------------------------------------------------------------------------- #
  
  col_helt <- match("Helt återställt?",colnames(scenario_results_sorted))
  k_helt <- openxlsx::int2col(col_helt); data_rows <- 2:(nrow(scenario_results_sorted)+1); data_cols <- which(!colnames(scenario_results_sorted)%in%SPACER_COLS)
  apply_status_conditional_formatting(wb,sheet="Scenarier",k_helt=k_helt,data_rows=data_rows,data_cols=data_cols,style_green=style_green,style_yellow=style_yellow,style_red=style_red,levels=c("JA","DELVIS","NEJ"))
  
  # -------------------------------------------------------------------------- #
  # Radlayout                                                                  #
  # -------------------------------------------------------------------------- #
  
  openxlsx::setRowHeights(wb,"Scenarier",rows=2:(nrow(scenario_results_sorted)+1),heights=15)
  
  # -------------------------------------------------------------------------- #
  # 12.2 h) Blad 4: Transformatorer                                            #
  # -------------------------------------------------------------------------- #
  
  openxlsx::addWorksheet(wb,"Transformatorer")
  openxlsx::writeData(wb,"Transformatorer",transformer_vulnerability_sorted)
  
  # -------------------------------------------------------------------------- #
  # Grundformat                                                                #
  # -------------------------------------------------------------------------- #
  
  apply_basic_sheet_formatting(wb,sheet="Transformatorer",n_rows=nrow(transformer_vulnerability_sorted)+1,n_cols=ncol(transformer_vulnerability_sorted),center_style=center_style,header_style=header_style,border_style=border_style,no_wrap_style=no_wrap_style,top_align_style=top_align_style)
  apply_column_widths(wb,sheet="Transformatorer",colnames_table=colnames(transformer_vulnerability_sorted),fixed_widths=fixed_widths)
  # Låser radhöjd för alla datarader (ingen automatisk höjdanpassning)
  openxlsx::setRowHeights(wb, sheet = "Transformatorer", rows  = 2:(nrow(transformer_vulnerability_sorted) + 1), heights = 15)
  
  # -------------------------------------------------------------------------- #
  # Villkorsstyrd formattering                                                 #
  # -------------------------------------------------------------------------- #
  
  col_kunder <- match("Berörda kunder",colnames(transformer_vulnerability_sorted))
  col_kile_energy <- match("KILE energi per h",colnames(transformer_vulnerability_sorted))
  k_kunder <- openxlsx::int2col(col_kunder); data_rows <- 2:(nrow(transformer_vulnerability_sorted)+1); data_cols <- 1:ncol(transformer_vulnerability_sorted)
  openxlsx::conditionalFormatting(wb,sheet="Transformatorer",cols=data_cols,rows=data_rows,type="expression",rule=sprintf("$%s2=0",k_kunder),style=style_yellow)
  k_kile_energy <- openxlsx::int2col(col_kile_energy)
  openxlsx::conditionalFormatting(wb,sheet = "Transformatorer",cols  = data_cols,rows  = data_rows,type  = "expression",rule  = sprintf("AND($%s2>0,$%s2=0)", k_kunder, k_kile_energy),style = style_traf_orange)
  
  # -------------------------------------------------------------------------- #
  # 12.2 i) Blad 5: Inställningar                                              #
  # -------------------------------------------------------------------------- #
  
  openxlsx::addWorksheet(wb,"Inställningar")
  openxlsx::writeData(wb,"Inställningar",settings_table)
  
  # -------------------------------------------------------------------------- #
  # Grundformat                                                                #
  # -------------------------------------------------------------------------- #
  
  apply_basic_sheet_formatting(wb,sheet="Inställningar",n_rows=nrow(settings_table)+1,n_cols=ncol(settings_table),center_style=center_style,header_style=header_style,border_style=border_style,no_wrap_style=no_wrap_style,top_align_style=top_align_style)
  openxlsx::setColWidths(wb,"Inställningar",cols=seq_len(ncol(settings_table)),widths="auto") 
  
  # -------------------------------------------------------------------------- #
  # 12.2 j) Slutlig lagring                                                    #
  # -------------------------------------------------------------------------- #
  
  openxlsx::saveWorkbook(wb, export_path, overwrite = TRUE)
  message(sprintf("Resultat exporterade till: %s", dirname(export_path)))
  invisible(export_path)
}