# Fil:                segment_lengths.R
# Författare:         Erik Hårrskog
# Datum:              2026-05-08
#
# Innehåll:
#
# 6.1 Publika hjälpfunktioner
#     a) Beräkning av längd per MV-delkant (build_mv_subedge_lengths)
#     b) Summering av segmentlängd (get_segment_length_m)

# ============================================================================ #
# 6.1) Publika hjälpfunktioner                                                 #
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 6.1 a) Beräkning av längd per MV-delkant (build_mv_subedge_lengths)          #
# ---------------------------------------------------------------------------- #


# Beräknar längd per MV-delkant inom ett subnät
build_mv_subedge_lengths <- function(res, inputs, flags) {
  
  # Väljer nodrymd beroende på FULL eller REDUCED
  subnet_nodes <- if (isTRUE(flags$REDUCED)) {
    as.character(names(res$node_rep_map))
  } else {
    as.character(res$keep_nodes_base)
  }
  
  # Filtrerar fram MV-delkanter från full topologi
  mv_subedges <- res$all_edges_full |>
    dplyr::filter(SRC == "MVPART") |>
    dplyr::transmute(
      MVPART_ID = as.character(ID),
      EDGE_KEY  = as.character(EDGE_KEY),
      from      = as.character(from),
      to        = as.character(to)
    ) |>
    
    # Normaliserar noder för jämförelse mot basnycklar
    dplyr::mutate(
      from_base = sub("\\|[AB]$", "", from),
      to_base   = sub("\\|[AB]$", "", to)
    ) |>
    
    # Begränsar till MV-delkanter som ingår i aktuellt subnät
    dplyr::filter(from_base %in% subnet_nodes & to_base %in% subnet_nodes) |>
    
    # Markerar delkanter utan geometrisk längd (stubbar)
    dplyr::mutate(
      is_stub = (from_base == to_base)
    ) |>
    
    # Tar bort temporära hjälpkolumner
    dplyr::select(-from_base, -to_base)
  
  # Läser in total längd per MVPART
  mvpart_lengths <- inputs$MVPART_LENGTHS |>
    dplyr::transmute(
      MVPART_ID   = as.character(MVPART_ID),
      LEN_TOTAL_M = as.numeric(LEN_TOTAL_M)
    )
  
  mv_subedges <- mv_subedges |>
    dplyr::left_join(mvpart_lengths, by = "MVPART_ID")
  
  # Säkerställer att alla MVPART har definierad total längd
  missing_lengths <- mv_subedges |>
    dplyr::filter(is.na(LEN_TOTAL_M)) |>
    dplyr::distinct(MVPART_ID)
  
  if (nrow(missing_lengths) > 0) {
    stop(
      "Saknar LEN_TOTAL_M för minst ett MVPART_ID. Exempel: ",
      paste(utils::head(missing_lengths$MVPART_ID, 10), collapse = ", ")
    )
  }
  
  # Validerar att MVPART endast är osplittrad eller har exakt en stub
  invalid_patterns <- mv_subedges |>
    dplyr::count(MVPART_ID, is_stub) |>
    dplyr::mutate(is_stub = factor(is_stub, levels = c(FALSE, TRUE))) |>
    tidyr::pivot_wider(
      names_from   = is_stub,
      values_from  = n,
      values_fill  = 0,
      names_expand = TRUE
    ) |>
    dplyr::mutate(
      n_total = `FALSE` + `TRUE`,
      ok = (n_total == 1) | (n_total == 2 & `TRUE` == 1 & `FALSE` == 1)
    ) |>
    dplyr::filter(!ok)
  
  if (nrow(invalid_patterns) > 0) {
    stop(
      "Oväntat MVPART-splitmönster i subnätet. Exempel på MVPART_ID: ",
      paste(utils::head(invalid_patterns$MVPART_ID, 10), collapse = ", ")
    )
  }
  
  # Tilldelar längd till delkanter med stubbar som 0 m
  mv_subedges |>
    dplyr::mutate(
      length_m = dplyr::if_else(is_stub, 0, LEN_TOTAL_M)
    ) |>
    dplyr::select(
      EDGE_KEY,
      MVPART_ID,
      from,
      to,
      is_stub,
      LEN_TOTAL_M,
      length_m
    )
}

# ---------------------------------------------------------------------------- #
# 6.1 b) Summering av segmentlängd (get_segment_length_m)                      #
# ---------------------------------------------------------------------------- #


# Summerar total längd för ett givet nätssegment
get_segment_length_m <- function(target_seg_id, res, flags) {
  
  # Om längddata saknas kan segmentlängd inte beräknas
  if (is.null(res$mv_subedge_lengths)) {
    return(0)
  }
  
  # Bygger index som mappar noder till segment
  segment_index <- build_segment_index(res, flags)
  node_to_segment <- segment_index$seg_id
  
  # Summerar längd för delkanter som tillhör angivet segment
  segment_length_m <- res$mv_subedge_lengths |>
    dplyr::mutate(
      segment_id = dplyr::coalesce(
        node_to_segment[from],
        node_to_segment[to]
      )
    ) |>
    dplyr::filter(segment_id == as.integer(target_seg_id)) |>
    dplyr::summarise(
      length_m = sum(length_m, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::pull(length_m)
  
  # Returnerar 0 om segmentet saknar längd
  if (length(segment_length_m) == 0 ||
      is.na(segment_length_m) ||
      !is.finite(segment_length_m)) {
    0
  } else {
    segment_length_m
  }
}
