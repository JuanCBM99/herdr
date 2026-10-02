#' Plot herdr results dynamically with custom grouping and premium aesthetics
#'
#' @param df Dataframe containing the results.
#' @param group_cols Character vector of columns to group the plot by.
#' @param func_name Name of the function that generated the data.
#' @param gwp_report Character string or named numeric vector. IPCC GWP standard used as fallback when GWP columns are not pre-calculated. Options: `"AR5"`, `"AR6"`, `"AR4"`, `"SAR"`. Default is `"AR5"`.
#' @param ar Optional alias for `gwp_report`.
#' @param functional_unit Character string specifying the functional unit for impact assessment plots: `"total"` (Gg CO2e and ha), `"protein"` (kg CO2e and m2 per kg edible protein), `"product"` (kg CO2e and m2 per kg commercial product), `"fcr"` (Mottet et al. 2017 Protein Feed Conversion Ratio and human food competition), `"milk"`, `"meat"`, `"wool"`, `"egg"`, or `"head"` (kg CO2e and m2 per animal head). Default is `"total"`.
#' @return A ggplot2 object.
#' @export
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr group_by summarise across all_of cur_column arrange mutate filter select bind_rows
#' @importFrom scales comma
plot_herdr_results <- function(df, group_cols = c("animal_tag", "region", "subregion", "class_flex"), func_name = NULL, gwp_report = "AR5", ar = NULL, functional_unit = "total") {
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
          if (grepl("pct|ef|factor|intensity|fcr|rate|dmi", col_name, ignore.case = TRUE)) mean(x, na.rm = TRUE) else sum(x, na.rm = TRUE)
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
  # VISUAL THEME
  # =================================================================
  grid_color   <- "#EEF0F3"
  text_dark    <- "#111827"
  text_mid     <- "#4B5563"
  text_light   <- "#9CA3AF"

  theme_herdr_plot <- function() {
    theme_minimal(base_size = 14, base_family = "sans") +
      theme(
        plot.title = element_text(face = "bold", size = 20, color = text_dark, margin = margin(b = 6)),
        plot.subtitle = element_text(color = text_mid, size = 13.5, margin = margin(b = 20)),
        plot.caption = element_text(color = text_light, size = 10, margin = margin(t = 14), hjust = 0),
        axis.text.y = element_text(face = "bold", color = "#1F2937", size = 13, margin = margin(r = 6)),
        axis.text.x = element_text(color = "#374151", size = 12),
        axis.title.x = element_text(face = "bold", color = text_mid, size = 12.5, margin = margin(t = 12)),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(color = grid_color, linewidth = 0.7),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2.0, "lines"),
        legend.position = "top",
        legend.justification = "left",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, color = text_mid),
        legend.key.size = unit(1.1, "lines"),
        legend.margin = margin(b = 10),
        strip.text = element_text(face = "bold", color = text_mid, size = 13.5),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.margin = margin(t = 22, r = 36, b = 20, l = 20)
      )
  }

  fmt_num <- function(x) formatC(x, format = "f", digits = 1, big.mark = ",")
  palette_categorical <- c("#2F6B4F", "#D9A441", "#B4483C", "#3B6EA5", "#6C5B9E")

  # =================================================================
  # SPECIAL-CASE PLOTS
  # =================================================================

  # A0) ENVIRONMENTAL IMPACT ASSESSMENT (Stacked Emissions + Land Footprint)
  is_impact_assessment <- identical(func_name, "generate_impact_assessment") ||
    all(c("CH4_enteric_Gg", "CH4_manure_Gg") %in% names(df_agg)) ||
    all(c("CO2eq_enteric", "CO2eq_manure") %in% names(df_agg))

  if (is_impact_assessment) {
    effective_gwp <- if (!is.null(ar)) ar else gwp_report
    gwp_presets <- list(
      AR6 = c(CH4 = 27, N2O = 273),
      AR5 = c(CH4 = 28, N2O = 265),
      AR4 = c(CH4 = 25, N2O = 298),
      SAR = c(CH4 = 21, N2O = 310)
    )
    if (is.character(effective_gwp) && toupper(effective_gwp[1]) %in% names(gwp_presets)) {
      default_factors <- gwp_presets[[toupper(effective_gwp[1])]]
    } else if (is.numeric(effective_gwp) && all(c("CH4", "N2O") %in% names(effective_gwp))) {
      default_factors <- effective_gwp
    } else {
      default_factors <- gwp_presets[["AR5"]]
    }

    co2e_ch4_ent <- if ("CO2eq_enteric" %in% names(df_agg)) {
      df_agg$CO2eq_enteric
    } else if ("CH4_enteric_Gg" %in% names(df_agg)) {
      df_agg$CH4_enteric_Gg * default_factors[["CH4"]]
    } else {
      0
    }

    co2e_ch4_man <- if ("CO2eq_manure" %in% names(df_agg)) {
      df_agg$CO2eq_manure
    } else if ("CH4_manure_Gg" %in% names(df_agg)) {
      df_agg$CH4_manure_Gg * default_factors[["CH4"]]
    } else {
      0
    }

    co2e_n2o_dir <- if ("CO2eq_N2O_direct" %in% names(df_agg)) {
      df_agg$CO2eq_N2O_direct
    } else if ("N2O_direct_Gg" %in% names(df_agg)) {
      df_agg$N2O_direct_Gg * default_factors[["N2O"]]
    } else {
      0
    }

    co2e_n2o_ind <- if ("CO2eq_N2O_indirect" %in% names(df_agg)) {
      df_agg$CO2eq_N2O_indirect
    } else if (all(c("N2O_vol_Gg", "N2O_lea_Gg") %in% names(df_agg))) {
      (df_agg$N2O_vol_Gg + df_agg$N2O_lea_Gg) * default_factors[["N2O"]]
    } else if ("CO2eq_N2O" %in% names(df_agg)) {
      pmax(0, df_agg$CO2eq_N2O - co2e_n2o_dir)
    } else {
      0
    }

    # Functional unit scaling
    fu <- tolower(functional_unit[1])
    if (!fu %in% c("protein", "product", "head", "milk", "meat", "wool", "egg", "fcr", "protein_fcr")) fu <- "total"

    emiss_scale <- rep(1, nrow(df_agg))
    land_scale  <- rep(1 / 10000, nrow(df_agg)) # m2 to ha
    emiss_panel <- "GHG Emissions (Gg CO2e)"
    land_panel  <- "Land Footprint (ha)"
    sub_title   <- "Greenhouse gas emissions breakdown (Gg CO2e) and land footprint (ha)"

    # Pre-identify dairy animal_tags from input df if animal_subtype is not in grouped df_agg
    dairy_tags <- if (all(c("animal_tag", "animal_subtype") %in% names(df))) {
      unique(df$animal_tag[df$animal_subtype %in% "dairy"])
    } else {
      character()
    }

    # Dedicated Protein FCR and Human Food Competition View (Mottet et al. 2017)
    if (fu %in% c("fcr", "protein_fcr") && any(c("total_protein_kg", "Protein_FCR_total") %in% names(df_agg))) {
      is_dairy <- if ("animal_subtype" %in% names(df_agg)) {
        df_agg$animal_subtype %in% "dairy"
      } else if ("animal_tag" %in% names(df_agg)) {
        df_agg$animal_tag %in% dairy_tags
      } else {
        FALSE
      }
      is_non_milking_dairy <- is_dairy & (if ("milk_FPCM_kg" %in% names(df_agg)) df_agg$milk_FPCM_kg == 0 else FALSE)

      has_direct_fcr <- "Protein_FCR_total" %in% names(df_agg) && "Protein_FCR_cropland" %in% names(df_agg)
      has_raw_cp     <- all(c("feed_CP_total_kg", "feed_CP_cropland_kg", "total_protein_kg") %in% names(df_agg))

      valid_prot <- (df_agg$total_protein_kg > 0) & !is_non_milking_dairy

      if (has_raw_cp) {
        df_agg$fcr_total    <- ifelse(valid_prot & df_agg$total_protein_kg > 0, df_agg$feed_CP_total_kg / df_agg$total_protein_kg, 0)
        df_agg$fcr_cropland <- ifelse(valid_prot & df_agg$total_protein_kg > 0, df_agg$feed_CP_cropland_kg / df_agg$total_protein_kg, 0)
      } else if (has_direct_fcr) {
        df_agg$fcr_total    <- ifelse(valid_prot, tidyr::replace_na(df_agg$Protein_FCR_total, 0), 0)
        df_agg$fcr_cropland <- ifelse(valid_prot, tidyr::replace_na(df_agg$Protein_FCR_cropland, 0), 0)
      } else {
        df_agg$fcr_total    <- 0
        df_agg$fcr_cropland <- 0
      }

      # Order by total protein FCR
      df_agg <- df_agg[order(df_agg$fcr_total), ]
      df_agg$plot_label <- factor(df_agg$plot_label, levels = unique(df_agg$plot_label))

      plot_long <- df_agg %>%
        dplyr::select(dplyr::all_of(c("plot_label", "fcr_total", "fcr_cropland"))) %>%
        tidyr::pivot_longer(
          cols = c("fcr_total", "fcr_cropland"),
          names_to = "metric_raw",
          values_to = "value"
        ) %>%
        dplyr::mutate(
          metric = factor(
            metric_raw,
            levels = c("fcr_total", "fcr_cropland"),
            labels = c("Total Protein FCR (Biological)", "Cropland Protein FCR (Human Competition)")
          ),
          label_text = ifelse(value > 0, sprintf("%.2f", value), "")
        )

      palette_fcr <- c(
        "Total Protein FCR (Biological)"           = "#0284C7",
        "Cropland Protein FCR (Human Competition)" = "#D97706"
      )

      max_val <- if (nrow(plot_long) > 0 && max(plot_long$value, na.rm = TRUE) > 0) max(plot_long$value, na.rm = TRUE) else 2.0

      p <- ggplot(plot_long, aes(x = value, y = plot_label, fill = metric)) +
        geom_col(position = position_dodge(width = 0.75), width = 0.65) +
        geom_vline(xintercept = 1.0, linetype = "dashed", color = "#DC2626", linewidth = 0.85) +
        geom_text(
          aes(label = label_text),
          position = position_dodge(width = 0.75),
          hjust = -0.2,
          fontface = "bold",
          size = 4.0,
          color = "#1F2937"
        ) +
        scale_fill_manual(values = palette_fcr) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.18)), limits = c(0, max(max_val, 1.2) * 1.15)) +
        theme_herdr_plot() +
        theme(
          legend.position = "top",
          legend.title = element_blank(),
          legend.text = element_text(size = 12, face = "bold"),
          axis.title.x = element_text(margin = margin(t = 10), face = "bold", color = "#1F2937", size = 12)
        ) +
        labs(
          title = "Protein Feed Efficiency & Human Food Competition",
          subtitle = "kg Crude Protein Intake per kg Animal Edible Protein Output (Mottet et al. 2017)\nDashed red line at 1.0 indicates threshold: < 1.0 = Net Food Producer, > 1.0 = Net Consumer",
          x = "Protein Feed Conversion Ratio (kg CP feed / kg edible protein)",
          y = NULL
        )

      return(p)
    }

    if (fu == "protein" && "total_protein_kg" %in% names(df_agg)) {
      prot <- df_agg$total_protein_kg
      is_dairy <- if ("animal_subtype" %in% names(df_agg)) {
        df_agg$animal_subtype %in% "dairy"
      } else if ("animal_tag" %in% names(df_agg)) {
        df_agg$animal_tag %in% dairy_tags
      } else {
        FALSE
      }
      is_dairy_dry <- is_dairy & (if ("milk_FPCM_kg" %in% names(df_agg)) df_agg$milk_FPCM_kg == 0 else FALSE)
      valid <- prot > 0 & !is_dairy_dry
      emiss_scale <- ifelse(valid, 1e6 / prot, 0)
      land_scale  <- ifelse(valid, 1 / prot, 0)
      emiss_panel <- "GHG Intensity (kg CO2e / kg protein)"
      land_panel  <- "Land Footprint (m2 / kg protein)"
      sub_title   <- "Emissions intensity and land use per kg of edible protein"
    } else if (fu == "head" && "population" %in% names(df_agg)) {
      pop <- df_agg$population
      valid <- pop > 0
      emiss_scale <- ifelse(valid, 1e6 / pop, 0)
      land_scale  <- ifelse(valid, 1 / pop, 0)
      emiss_panel <- "GHG per Head (kg CO2e / head)"
      land_panel  <- "Land per Head (m2 / head)"
      sub_title   <- "Emissions and land use per animal head per year"
    } else if (fu %in% c("product", "milk", "meat", "wool", "egg") && any(c("milk_FPCM_kg", "meat_carcass_weight_kg", "egg_fresh_kg", "wool_kg") %in% names(df_agg))) {
      milk_kg      <- if ("milk_FPCM_kg" %in% names(df_agg)) df_agg$milk_FPCM_kg else 0
      meat_kg      <- if ("meat_carcass_weight_kg" %in% names(df_agg)) df_agg$meat_carcass_weight_kg else 0
      meat_live_kg <- if ("meat_live_weight_kg" %in% names(df_agg)) df_agg$meat_live_weight_kg else (if ("meat_carcass_weight_kg" %in% names(df_agg)) df_agg$meat_carcass_weight_kg / 0.54 else 0)
      egg_kg       <- if ("egg_fresh_kg" %in% names(df_agg)) df_agg$egg_fresh_kg else 0
      wool_kg      <- if ("wool_kg" %in% names(df_agg)) df_agg$wool_kg else 0

      is_dairy     <- if ("animal_subtype" %in% names(df_agg)) {
        df_agg$animal_subtype %in% "dairy"
      } else if ("animal_tag" %in% names(df_agg)) {
        df_agg$animal_tag %in% dairy_tags
      } else {
        FALSE
      }
      is_non_milking_dairy <- is_dairy & (milk_kg == 0)

      # Pre-calculated allocation factors if present, or Enfoque A Net Energy fallback
      nel_total_mj <- 3.1 * milk_kg
      neg_total_mj <- ifelse(egg_kg > 0 | is_non_milking_dairy, 0, 15.0 * meat_live_kg)
      e_wool_mj    <- if ("animal_subtype" %in% names(df_agg)) ifelse(df_agg$animal_subtype == "meat", 24.0 * wool_kg, 0) else 0
      e_tot_mj     <- nel_total_mj + neg_total_mj + e_wool_mj

      af_milk_calc <- if ("AF_milk" %in% names(df_agg)) df_agg$AF_milk else ifelse(e_tot_mj > 0 & egg_kg == 0 & !is_non_milking_dairy, nel_total_mj / e_tot_mj, ifelse(milk_kg > 0, 1, 0))
      af_meat_calc <- if ("AF_meat" %in% names(df_agg)) df_agg$AF_meat else ifelse(egg_kg > 0 | is_non_milking_dairy, 0, ifelse(e_tot_mj > 0, neg_total_mj / e_tot_mj, ifelse(meat_kg > 0, 1, 0)))
      af_wool_calc <- if ("AF_wool" %in% names(df_agg)) df_agg$AF_wool else ifelse(e_tot_mj > 0 & egg_kg == 0 & !is_non_milking_dairy, e_wool_mj / e_tot_mj, 0)
      af_egg_calc  <- if ("AF_egg" %in% names(df_agg))  df_agg$AF_egg  else ifelse(egg_kg > 0, 1, 0)

      if (fu == "milk") {
        prod_yield  <- ifelse(is_non_milking_dairy, 0, milk_kg)
        af          <- ifelse(is_non_milking_dairy, 0, af_milk_calc)
        emiss_panel <- "GHG Intensity (kg CO2e / kg FPCM milk)"
        land_panel  <- "Land Footprint (m2 / kg FPCM milk)"
        sub_title   <- "Emissions intensity and land use per kg of FPCM milk (IDF allocation)"
      } else if (fu == "meat") {
        prod_yield  <- ifelse(is_non_milking_dairy, 0, meat_kg)
        af          <- ifelse(is_non_milking_dairy, 0, af_meat_calc)
        emiss_panel <- "GHG Intensity (kg CO2e / kg carcass meat)"
        land_panel  <- "Land Footprint (m2 / kg carcass meat)"
        sub_title   <- "Emissions intensity and land use per kg of carcass meat (IDF allocation)"
      } else if (fu == "wool") {
        prod_yield  <- ifelse(is_non_milking_dairy, 0, wool_kg)
        af          <- ifelse(is_non_milking_dairy, 0, af_wool_calc)
        emiss_panel <- "GHG Intensity (kg CO2e / kg wool)"
        land_panel  <- "Land Footprint (m2 / kg wool)"
        sub_title   <- "Emissions intensity and land use per kg of wool (IPCC allocation)"
      } else if (fu == "egg") {
        prod_yield  <- egg_kg
        af          <- af_egg_calc
        emiss_panel <- "GHG Intensity (kg CO2e / kg egg)"
        land_panel  <- "Land Footprint (m2 / kg egg)"
        sub_title   <- "Emissions intensity and land use per kg of fresh eggs"
      } else {
        # Default fu == "product" (primary product)
        prod_yield <- dplyr::case_when(
          is_non_milking_dairy           ~ 0,
          milk_kg > 0                    ~ milk_kg,
          is_dairy                       ~ 0,
          egg_kg > 0                     ~ egg_kg,
          meat_kg > 0                    ~ meat_kg,
          wool_kg > 0 & af_wool_calc > 0 ~ wool_kg,
          TRUE ~ 0
        )
        af <- dplyr::case_when(
          is_non_milking_dairy           ~ 0,
          milk_kg > 0                    ~ af_milk_calc,
          is_dairy                       ~ 0,
          egg_kg > 0                     ~ af_egg_calc,
          meat_kg > 0                    ~ af_meat_calc,
          wool_kg > 0 & af_wool_calc > 0 ~ af_wool_calc,
          TRUE ~ 0
        )
        emiss_panel <- "GHG Intensity (kg CO2e / kg product)"
        land_panel  <- "Land Footprint (m2 / kg product)"
        sub_title   <- "Emissions intensity and land use per kg of commercial product"
      }

      valid <- prod_yield > 0 & af > 0
      emiss_scale <- ifelse(valid, (1e6 * af) / prod_yield, 0)
      land_scale  <- ifelse(valid, af / prod_yield, 0)
    }

    df_agg$co2e_ch4_ent <- co2e_ch4_ent * emiss_scale
    df_agg$co2e_ch4_man <- co2e_ch4_man * emiss_scale
    df_agg$co2e_n2o_dir <- co2e_n2o_dir * emiss_scale
    df_agg$co2e_n2o_ind <- co2e_n2o_ind * emiss_scale
    df_agg$total_co2e   <- df_agg$co2e_ch4_ent + df_agg$co2e_ch4_man + df_agg$co2e_n2o_dir + df_agg$co2e_n2o_ind

    has_land <- "Land_m2" %in% names(df_agg) && any(df_agg$Land_m2 > 0, na.rm = TRUE)
    has_land_breakdown <- all(c("Land_cropland_m2", "Land_grassland_convertible_m2", "Land_grassland_unconvertible_m2") %in% names(df_agg))

    if (has_land) {
      df_agg$land_val <- df_agg$Land_m2 * land_scale
      if (has_land_breakdown) {
        df_agg$land_crop   <- df_agg$Land_cropland_m2 * land_scale
        df_agg$land_g_conv <- df_agg$Land_grassland_convertible_m2 * land_scale
        df_agg$land_g_unco <- df_agg$Land_grassland_unconvertible_m2 * land_scale
        if ("Land_other_m2" %in% names(df_agg)) {
          df_agg$land_other <- df_agg$Land_other_m2 * land_scale
        }
      }
    }

    # Order cohorts by total emissions
    df_agg <- df_agg[order(df_agg$total_co2e), ]
    df_agg$plot_label <- factor(df_agg$plot_label, levels = unique(df_agg$plot_label))

    # Long dataset for emissions
    emissions_df <- df_agg %>%
      dplyr::select(dplyr::all_of(c("plot_label", "co2e_ch4_ent", "co2e_ch4_man", "co2e_n2o_dir", "co2e_n2o_ind"))) %>%
      tidyr::pivot_longer(
        cols = c("co2e_ch4_ent", "co2e_ch4_man", "co2e_n2o_dir", "co2e_n2o_ind"),
        names_to = "component_raw",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        panel = emiss_panel,
        component = factor(
          component_raw,
          levels = c("co2e_ch4_ent", "co2e_ch4_man", "co2e_n2o_dir", "co2e_n2o_ind"),
          labels = c("CH4 Enteric", "CH4 Manure", "N2O Direct", "N2O Indirect")
        )
      )

    all_impact_levels <- c(
      "CH4 Enteric", "CH4 Manure", "N2O Direct", "N2O Indirect",
      "Cropland", "Grassland (Conv.)", "Grassland (Unconv.)", "Land Other", "Land Use"
    )

    palette_impact <- c(
      "CH4 Enteric"         = "#38BDF8",  # Sky blue
      "CH4 Manure"          = "#1E40AF",  # Dark navy blue
      "N2O Direct"          = "#7E22CE",  # Deep royal purple
      "N2O Indirect"        = "#C084FC",  # Lavender / light purple
      "Cropland"            = "#D97706",  # Warm amber arable
      "Grassland (Conv.)"   = "#16A34A",  # Meadow green
      "Grassland (Unconv.)" = "#15803D",  # Deep forest green
      "Land Other"          = "#9CA3AF",  # Neutral gray
      "Land Use"            = "#15803D"   # Forest / emerald green fallback
    )

    if (has_land) {
      if (has_land_breakdown) {
        has_other     <- "land_other" %in% names(df_agg) && any(df_agg$land_other > 0, na.rm = TRUE)
        cols_to_use   <- if (has_other) c("land_crop", "land_g_conv", "land_g_unco", "land_other") else c("land_crop", "land_g_conv", "land_g_unco")
        labels_to_use <- if (has_other) c("Cropland", "Grassland (Conv.)", "Grassland (Unconv.)", "Land Other") else c("Cropland", "Grassland (Conv.)", "Grassland (Unconv.)")

        land_df <- df_agg %>%
          dplyr::select(dplyr::all_of(c("plot_label", cols_to_use))) %>%
          tidyr::pivot_longer(
            cols = dplyr::all_of(cols_to_use),
            names_to = "component_raw",
            values_to = "value"
          ) %>%
          dplyr::mutate(
            panel = land_panel,
            component = factor(
              component_raw,
              levels = cols_to_use,
              labels = labels_to_use
            )
          ) %>%
          dplyr::select(plot_label, value, panel, component)
      } else {
        land_df <- df_agg %>%
          dplyr::select(dplyr::all_of(c("plot_label", "land_val"))) %>%
          dplyr::mutate(
            value = land_val,
            panel = land_panel,
            component = factor("Land Use", levels = all_impact_levels)
          ) %>%
          dplyr::select(plot_label, value, panel, component)
      }

      emissions_df$component <- factor(emissions_df$component, levels = all_impact_levels)
      land_df$component      <- factor(land_df$component, levels = all_impact_levels)

      plot_data <- dplyr::bind_rows(
        emissions_df %>% dplyr::select(plot_label, value, panel, component),
        land_df
      )
      plot_data$panel <- factor(plot_data$panel, levels = c(emiss_panel, land_panel))

      # Retain only components that have positive values across the dataset to keep legend clean
      active_components <- plot_data %>%
        dplyr::group_by(component) %>%
        dplyr::summarise(s = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(s > 0) %>%
        dplyr::pull(component)

      if (length(active_components) > 0) {
        plot_data <- plot_data %>% dplyr::filter(component %in% active_components)
      }

      totals_df <- plot_data %>%
        dplyr::group_by(plot_label, panel) %>%
        dplyr::summarise(total_val = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(total_val > 0) %>%
        dplyr::mutate(
          label_text = dplyr::case_when(
            total_val >= 1000 ~ scales::comma(total_val, accuracy = 1),
            total_val >= 10   ~ formatC(total_val, digits = 1, format = "f"),
            total_val >= 1    ~ formatC(total_val, digits = 2, format = "f"),
            TRUE              ~ formatC(total_val, digits = 3, format = "f")
          )
        )

      p <- ggplot(plot_data, aes(x = value, y = plot_label, fill = component)) +
        geom_col(position = position_stack(reverse = TRUE), width = 0.62) +
        geom_text(
          data = totals_df,
          aes(x = total_val, y = plot_label, label = label_text),
          inherit.aes = FALSE,
          hjust = -0.15,
          size = 4.0,
          fontface = "bold",
          color = "#1F2937"
        ) +
        facet_wrap(~ panel, scales = "free_x") +
        theme_herdr_plot() +
        scale_x_continuous(expand = expansion(mult = c(0, 0.20)), labels = scales::comma) +
        scale_fill_manual(values = palette_impact, drop = TRUE) +
        labs(
          title = "Environmental Impact Assessment",
          subtitle = sub_title,
          x = NULL
        )
    } else {
      # Retain only active components
      active_emiss <- emissions_df %>%
        dplyr::group_by(component) %>%
        dplyr::summarise(s = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(s > 0) %>%
        dplyr::pull(component)

      if (length(active_emiss) > 0) {
        emissions_df <- emissions_df %>% dplyr::filter(component %in% active_emiss)
      }

      totals_df <- emissions_df %>%
        dplyr::group_by(plot_label) %>%
        dplyr::summarise(total_val = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(total_val > 0) %>%
        dplyr::mutate(
          label_text = dplyr::case_when(
            total_val >= 1000 ~ scales::comma(total_val, accuracy = 1),
            total_val >= 10   ~ formatC(total_val, digits = 1, format = "f"),
            total_val >= 1    ~ formatC(total_val, digits = 2, format = "f"),
            TRUE              ~ formatC(total_val, digits = 3, format = "f")
          )
        )

      p <- ggplot(emissions_df, aes(x = value, y = plot_label, fill = component)) +
        geom_col(position = position_stack(reverse = TRUE), width = 0.62) +
        geom_text(
          data = totals_df,
          aes(x = total_val, y = plot_label, label = label_text),
          inherit.aes = FALSE,
          hjust = -0.15,
          size = 4.0,
          fontface = "bold",
          color = "#1F2937"
        ) +
        theme_herdr_plot() +
        scale_x_continuous(expand = expansion(mult = c(0, 0.20)), labels = scales::comma) +
        scale_fill_manual(values = palette_impact) +
        labs(
          title = "Greenhouse Gas Emissions",
          subtitle = sub_title,
          x = emiss_panel
        )
    }

    return(p)
  }

  # A) EMISSIONS BREAKDOWN (stacked bars)
  if (all(c("ch4_enteric", "ch4_manure", "n2o_manure") %in% names(df_agg))) {
    df_agg$Total_Gg <- df_agg$ch4_enteric + df_agg$ch4_manure + df_agg$n2o_manure
    df_agg <- df_agg[order(df_agg$Total_Gg), ]
    df_agg$plot_label <- factor(df_agg$plot_label, levels = unique(df_agg$plot_label))

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
    df_agg$plot_label <- factor(df_agg$plot_label, levels = unique(df_agg$plot_label))

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

  # C) LAND USE BREAKDOWN (stacked bars by land_type)
  if ("land_type" %in% names(df) && any(c("total_land_use_m2", "land_use_per_animal_m2") %in% names(df))) {
    target_land_var <- if ("total_land_use_m2" %in% names(df)) "total_land_use_m2" else "land_use_per_animal_m2"

    df_land <- df %>%
      dplyr::filter(!land_type %in% c("none", "no_land", NA_character_))

    if (nrow(df_land) > 0) {
      df_land_agg <- df_land %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(c(valid_groups, "land_type")))) %>%
        dplyr::summarise(
          Land_Val = sum(.data[[target_land_var]], na.rm = TRUE),
          .groups = "drop"
        )

      df_land_agg$plot_label <- apply(df_land_agg[, valid_groups, drop = FALSE], 1, function(x) {
        valid_vals <- x[!is.na(x) & trimws(x) != ""]
        if (length(valid_vals) == 0) return("Unknown")
        paste(valid_vals, collapse = " - ")
      })


      totals <- stats::aggregate(Land_Val ~ plot_label, data = df_land_agg, FUN = sum)
      totals <- totals[order(totals$Land_Val), ]
      df_land_agg$plot_label <- factor(df_land_agg$plot_label, levels = unique(totals$plot_label))

      totals$label_text <- dplyr::case_when(
        totals$Land_Val >= 1000 ~ scales::comma(totals$Land_Val, accuracy = 1),
        totals$Land_Val >= 10   ~ formatC(totals$Land_Val, digits = 1, format = "f"),
        totals$Land_Val >= 1    ~ formatC(totals$Land_Val, digits = 2, format = "f"),
        TRUE                    ~ formatC(totals$Land_Val, digits = 3, format = "f")
      )

      palette_land <- c(
        "cropland"                = "#D97706",
        "grassland_convertible"   = "#16A34A",
        "grassland_unconvertible" = "#15803D"
      )

      labels_land <- c(
        "cropland"                = "Cropland",
        "grassland_convertible"   = "Grassland (Convertible)",
        "grassland_unconvertible" = "Grassland (Unconvertible)"
      )

      unit_title <- if (target_land_var == "total_land_use_m2") "Total Land Use (m2)" else "Land Use per Animal (m2)"
      subtitle_txt <- if (target_land_var == "total_land_use_m2") "Total land footprint breakdown by agroecological type" else "Land footprint per animal by agroecological type"

      return(
        ggplot(df_land_agg, aes(x = Land_Val, y = plot_label, fill = land_type)) +
          geom_col(position = position_stack(reverse = TRUE), width = 0.62) +
          geom_text(
            data = totals,
            aes(x = Land_Val, y = plot_label, label = label_text),
            inherit.aes = FALSE,
            hjust = -0.15,
            size = 4.0,
            fontface = "bold",
            color = "#1F2937"
          ) +
          theme_herdr_plot() +
          scale_x_continuous(expand = expansion(mult = c(0, 0.18)), labels = scales::comma) +
          scale_fill_manual(
            values = palette_land,
            labels = labels_land,
            drop = TRUE
          ) +
          labs(
            title = "Feed-Related Land Use",
            subtitle = subtitle_txt,
            x = unit_title
          )
      )
    }
  }

  # D) EDIBLE PROTEIN PRODUCTION (stacked bars by commodity)
  if (all(c("milk_protein_kg", "meat_protein_kg", "egg_protein_kg") %in% names(df_agg))) {
    df_agg$Total_Protein <- df_agg$milk_protein_kg + df_agg$meat_protein_kg + df_agg$egg_protein_kg
    df_agg <- df_agg[order(df_agg$Total_Protein), ]
    df_agg$plot_label <- factor(df_agg$plot_label, levels = unique(df_agg$plot_label))

    df_long <- tidyr::pivot_longer(
      df_agg,
      cols = c("milk_protein_kg", "meat_protein_kg", "egg_protein_kg"),
      names_to = "Protein_Source", values_to = "Protein_kg"
    )

    palette_protein <- c(
      "milk_protein_kg" = "#3B6EA5",
      "meat_protein_kg" = "#B4483C",
      "egg_protein_kg"  = "#D9A441"
    )

    labels_protein <- c(
      "milk_protein_kg" = "Milk Protein",
      "meat_protein_kg" = "Meat Protein",
      "egg_protein_kg"  = "Egg Protein"
    )

    return(
      ggplot(df_long, aes(x = Protein_kg, y = plot_label, fill = Protein_Source)) +
        geom_col(width = 0.62) +
        theme_herdr_plot() +
        scale_x_continuous(expand = expansion(mult = c(0, 0.08)), labels = scales::comma) +
        scale_fill_manual(
          values = palette_protein,
          labels = labels_protein
        ) +
        labs(
          title = "Edible Protein Production",
          subtitle = "Total human-edible protein output by product (kg protein)",
          x = "Edible Protein (kg)"
        )
    )
  }

  # =================================================================
  # UNIVERSAL PLOT (all other functions)
  # =================================================================
  target_var_dict <- list(
    generate_impact_assessment            = "CO2eq_Total_Gg",
    calculate_DMI                         = "DMI_kgday",
    calculate_ge                          = "GE_MJday",
    calculate_vs                          = "VS_kgday",
    calculate_monogastric_energy          = "ME_total_kcal_day",
    calculate_population                  = "population",
    calculate_production                  = "total_protein_kg",
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

  df_agg <- df_agg[order(df_agg[[main_var]]), ]
  df_agg$plot_label <- factor(df_agg$plot_label, levels = unique(df_agg$plot_label))

  p <- ggplot(df_agg, aes(x = .data[[main_var]], y = plot_label, fill = .data[[main_var]])) +
    geom_col(show.legend = FALSE, width = 0.6) +
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
