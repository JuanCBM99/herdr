#' Plot herdr results dynamically with custom grouping and premium aesthetics
#'
#' @param df Dataframe containing the results.
#' @param group_cols Character vector of columns to group the plot by.
#' @param func_name Name of the function that generated the data.
#' @return A ggplot2 object.
#' @export
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr group_by summarise across all_of cur_column arrange mutate
#' @importFrom scales comma
plot_herdr_results <- function(df, group_cols = c("animal_tag", "region", "subregion", "class_flex"), func_name = NULL) {
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

  valid_groups <- intersect(group_cols, names(df))
  if (length(valid_groups) == 0) return(NULL)

  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  exclude_cols <- c("year", "fixed_coef", "weight_kg", "management_months")
  target_cols <- setdiff(num_cols, exclude_cols)

  # 1. Smart aggregation: mean for rates/percentages, sum for totals
  if (length(target_cols) > 0) {
    df_agg <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(valid_groups))) %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(target_cols), function(x) {
          col_name <- dplyr::cur_column()
          if (grepl("pct|ef|factor", col_name, ignore.case = TRUE)) mean(x, na.rm = TRUE) else sum(x, na.rm = TRUE)
        }),
        .groups = "drop"
      )
  } else {
    df_agg <- df
  }

  # 2. Build a single readable label per row from the grouping columns
  df_agg$plot_label <- apply(df_agg[, valid_groups, drop = FALSE], 1, function(x) {
    valid_vals <- x[!is.na(x) & trimws(x) != ""]
    if (length(valid_vals) == 0) return("Unknown")
    paste(valid_vals, collapse = " - ")
  })

  # =================================================================
  # ROUNDED BARS HELPER
  # Uses ggchicklet if installed (real rounded "pill" bars). Falls back
  # to standard geom_col so the function never hard-fails on a missing
  # optional dependency.
  # =================================================================
  has_chicklet <- requireNamespace("ggchicklet", quietly = TRUE)

  rounded_col <- function(..., radius = grid::unit(4, "pt")) {
    if (has_chicklet) {
      ggchicklet::geom_chicklet(..., radius = radius)
    } else {
      ggplot2::geom_col(...)
    }
  }

  # =================================================================
  # VISUAL THEME — soft "card" look, tuned for embedding in a UI
  # =================================================================
  bg_color     <- "#FFFFFF"
  panel_color  <- "#FBFBFC"
  grid_color   <- "#EEF0F3"
  text_dark    <- "#111827"
  text_mid     <- "#4B5563"
  text_light   <- "#9CA3AF"

  theme_herdr_plot <- function() {
    theme_minimal(base_size = 13, base_family = "sans") +
      theme(
        plot.title = element_text(face = "bold", size = 18, color = text_dark, margin = margin(b = 4)),
        plot.subtitle = element_text(color = text_mid, size = 12, margin = margin(b = 20)),
        plot.caption = element_text(color = text_light, size = 9, margin = margin(t = 14), hjust = 0),
        axis.text.y = element_text(face = "bold", color = text_mid, size = 11.5, margin = margin(r = 4)),
        axis.text.x = element_text(color = text_light, size = 10),
        axis.title.x = element_text(face = "bold", color = text_mid, size = 10.5, margin = margin(t = 12)),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(color = grid_color, linewidth = 0.7),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1.2, "lines"),
        legend.position = "top",
        legend.justification = "left",
        legend.title = element_blank(),
        legend.text = element_text(size = 10.5, color = text_mid),
        legend.key.size = unit(1, "lines"),
        legend.margin = margin(b = 8),
        strip.text = element_text(face = "bold", color = text_mid, size = 11),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.margin = margin(t = 20, r = 32, b = 18, l = 18)
      )
  }

  # Format large numbers nicely: 1234.56 -> 1,234.6
  fmt_num <- function(x) formatC(x, format = "f", digits = 1, big.mark = ",")

  # Curated categorical palette (used instead of flat defaults)
  palette_categorical <- c("#2F6B4F", "#D9A441", "#B4483C", "#3B6EA5", "#6C5B9E")

  # =================================================================
  # SPECIAL-CASE PLOTS
  # =================================================================

  # A) EMISSIONS BREAKDOWN (stacked bars)
  if (all(c("ch4_enteric", "ch4_manure", "n2o_manure") %in% names(df_agg))) {
    df_agg$Total_Gg <- df_agg$ch4_enteric + df_agg$ch4_manure + df_agg$n2o_manure
    df_agg <- df_agg[order(df_agg$Total_Gg), ]
    df_agg$plot_label <- factor(df_agg$plot_label, levels = df_agg$plot_label)

    df_long <- tidyr::pivot_longer(
      df_agg,
      cols = c("ch4_enteric", "ch4_manure", "n2o_manure"),
      names_to = "Emission_Source", values_to = "Gg_CO2e"
    )

    return(
      ggplot(df_long, aes(x = Gg_CO2e, y = plot_label, fill = Emission_Source)) +
        geom_col(width = 0.62) +
        theme_herdr_plot() +
        scale_x_continuous(expand = expansion(mult = c(0, 0.06)), labels = scales::comma) +
        scale_fill_manual(
          values = c("ch4_enteric" = "#2F6B4F", "ch4_manure" = "#D9A441", "n2o_manure" = "#B4483C"),
          labels = c("CH4 Enteric", "CH4 Manure", "N2O Manure")
        ) +
        labs(
          title = "Greenhouse Gas Emissions",
          subtitle = "Total emissions breakdown by source (Gg CO2e)",
          x = "Total Emissions (Gg CO2e)"
        )
    )
  }

  # B) DIET / NUTRIENT PROFILES (grouped bars)
  if (all(c("DE_pct", "CP_pct") %in% names(df_agg))) {
    cols_to_pivot <- intersect(c("DE_pct", "CP_pct", "NDF_pct", "ASH_pct"), names(df_agg))
    df_agg <- df_agg[order(df_agg$DE_pct), ]
    df_agg$plot_label <- factor(df_agg$plot_label, levels = df_agg$plot_label)

    df_long <- tidyr::pivot_longer(df_agg, cols = dplyr::all_of(cols_to_pivot), names_to = "Nutrient", values_to = "Percentage")

    return(
      ggplot(df_long, aes(x = Percentage, y = plot_label, fill = Nutrient)) +
        geom_col(position = position_dodge(width = 0.72), width = 0.66) +
        theme_herdr_plot() +
        scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
        scale_fill_manual(values = palette_categorical) +
        labs(
          title = "Diet Composition Profiles",
          subtitle = "Nutritional variables comparison (%)",
          x = "Percentage (%)"
        )
    )
  }

  # =================================================================
  # UNIVERSAL PLOT (all other functions)
  # =================================================================
  target_var_dict <- list(
    calculate_DMI                         = "DMI_kgday",
    calculate_ge                          = "GE_MJday",
    calculate_vs                          = "VS_kgday",
    calculate_monogastric_energy          = "ME_total_kcal_day",
    calculate_population                  = "population",
    calculate_emissions_enteric           = "total_CH4_enteric_Ggyear",
    calculate_CH4_manure                  = "total_CH4_mm_kgyear",
    calculate_N2O_direct_manure           = "direct_N2O_kgyear",
    calculate_N2O_indirect_leaching       = "N2O_leach_kgyear",
    calculate_N2O_indirect_volatilization = "N2O_vol_kgyear",
    calculate_land_use                    = "total_land_use_m2",
    calculate_NE_pregnancy                = "NEpregnancy_MJday",
    calculate_NE_wool                     = "NEwool_MJday",
    calculate_NE_work                     = "NEwork_MJday",
    calculate_NEa                         = "NEa_MJday",
    calculate_NEg                         = "NEg_MJday",
    calculate_NEl                         = "NEl_MJday",
    calculate_NEm                         = "NEm_MJday"
  )

  main_var <- NULL
  if (!is.null(func_name) && func_name %in% names(target_var_dict)) {
    if (target_var_dict[[func_name]] %in% names(df_agg)) main_var <- target_var_dict[[func_name]]
  }
  if (is.null(main_var)) {
    known_vars <- unlist(target_var_dict, use.names = FALSE)
    found_vars <- intersect(known_vars, names(df_agg))
    if (length(found_vars) > 0) main_var <- found_vars[1]
  }
  if (is.null(main_var)) {
    if (length(target_cols) == 0) return(NULL)
    main_var <- tail(target_cols, 1)
  }

  clean_title <- gsub("_", " ", main_var)
  palette <- if (grepl("NE|GE|ME", main_var, ignore.case = TRUE)) "inferno" else "mako"

  # Sort so the largest bar sits on top
  df_agg <- df_agg[order(df_agg[[main_var]]), ]
  df_agg$plot_label <- factor(df_agg$plot_label, levels = df_agg$plot_label)

  # Continuous fill: longer bars get a more intense color. Rounded bars
  # via ggchicklet when available, for a softer "pill" look.
  p <- ggplot(df_agg, aes(x = .data[[main_var]], y = plot_label, fill = .data[[main_var]])) +
    rounded_col(show.legend = FALSE, width = 0.6) +
    geom_text(
      aes(label = fmt_num(.data[[main_var]])),
      hjust = -0.18, size = 4, fontface = "bold", color = text_mid
    ) +
    theme_herdr_plot() +
    scale_fill_viridis_c(option = palette, begin = 0.35, end = 0.85) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
    labs(
      title = paste("Results for", clean_title),
      subtitle = paste("Aggregated by:", paste(valid_groups, collapse = ", ")),
      x = clean_title
    )

  return(p)
}
