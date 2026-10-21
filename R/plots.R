#' Plot herdr results dynamically with custom grouping and premium aesthetics
#'
#' @param df Dataframe containing the results.
#' @param group_cols Character vector of columns to group the plot by.
#' @param func_name Name of the function that generated the data.
#' @param gwp_report Character string or named numeric vector. IPCC GWP standard used as fallback when GWP columns are not pre-calculated. Options: `"AR5"`, `"AR6"`, `"AR4"`, `"SAR"`. Default is `"AR5"`.
#' @param ar Optional alias for `gwp_report`.
#' @param functional_unit Character string specifying the functional unit for impact assessment plots: `"total"` (Gg CO2e, ha, m3), `"protein"` (per kg edible protein), `"product"` (per kg commercial product), `"fcr"` (Mottet et al. 2017 Protein Feed Conversion Ratio and human food competition), `"milk"`, `"meat"`, `"wool"`, `"egg"`, or `"head"` (per animal head). Default is `"total"`.
#' @param impact_category Character string specifying the environmental impact category to visualize: `"ghg"` (carbon footprint), `"land"` (land use footprint), `"water"` (feed water deprivation footprint), `"overview"` (multi-criteria comparison of all footprints), or `"fcr"` (protein feed efficiency). Default is `NULL` (auto-selected).
#' @return A ggplot2 object.
#' @export
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr group_by summarise across all_of cur_column arrange mutate filter select bind_rows
#' @importFrom scales comma
plot_herdr_results <- function(df, group_cols = c("animal_tag", "region", "subregion", "class_flex"), func_name = NULL, gwp_report = "AR5", ar = NULL, functional_unit = "total", impact_category = NULL) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(NULL)
  }

  valid_groups <- intersect(group_cols, names(df))
  if (length(valid_groups) == 0) {
    return(NULL)
  }

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
    if (length(valid_vals) == 0) {
      return("Unknown")
    }
    paste(valid_vals, collapse = " - ")
  })

  # =================================================================
  # VISUAL THEME
  # =================================================================
  grid_color <- "#EEF0F3"
  text_dark <- "#111827"
  text_mid <- "#4B5563"
  text_light <- "#9CA3AF"

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
    # 1. Resolve impact_category & functional_unit
    fu_raw <- tolower(functional_unit[1])
    fu <- fu_raw

    if (is.null(impact_category)) {
      if (fu %in% c("fcr", "protein_fcr")) {
        impact_cat <- "fcr"
      } else if (fu %in% c("cradle_breakdown", "cradle", "overview")) {
        impact_cat <- "overview"
        fu <- "total"
      } else if (fu %in% c("water", "water_total", "water_protein", "water_product", "water_head")) {
        impact_cat <- "water"
        fu <- dplyr::case_when(
          fu == "water_protein" ~ "protein",
          fu == "water_head"    ~ "head",
          fu == "water_product" ~ "product",
          TRUE                  ~ "total"
        )
      } else if (fu %in% c("land", "land_total")) {
        impact_cat <- "land"
        fu <- "total"
      } else {
        # Default fallback when not specified: overview maintains backwards-compatibility for legacy multi-panel tests
        impact_cat <- "overview"
      }
    } else {
      impact_cat <- tolower(impact_category[1])
    }

    if (impact_cat %in% c("carbon", "emissions")) impact_cat <- "ghg"
    if (impact_cat == "protein_fcr") impact_cat <- "fcr"
    if (!impact_cat %in% c("ghg", "land", "water", "overview", "fcr")) impact_cat <- "ghg"

    if (!fu %in% c("total", "protein", "product", "milk", "meat", "wool", "egg", "head", "fcr", "overview")) {
      fu <- "total"
    }

    # 2. GWP factors
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

    # 3. Raw GHG sources (Gg CO2e)
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

    co2e_feed_fossil <- if ("CO2eq_feed_fossil_Gg" %in% names(df_agg)) {
      df_agg$CO2eq_feed_fossil_Gg
    } else {
      0
    }

    co2e_feed_luc <- if ("CO2eq_feed_luc_Gg" %in% names(df_agg)) {
      df_agg$CO2eq_feed_luc_Gg
    } else if ("CO2eq_feed_Gg" %in% names(df_agg)) {
      pmax(0, df_agg$CO2eq_feed_Gg - co2e_feed_fossil)
    } else {
      0
    }

    # 4. Raw Land components (m2)
    has_land <- "Land_m2" %in% names(df_agg) && any(df_agg$Land_m2 > 0, na.rm = TRUE)
    has_land_breakdown <- all(c("Land_cropland_m2", "Land_grassland_convertible_m2", "Land_grassland_unconvertible_m2") %in% names(df_agg))
    land_crop <- if ("Land_cropland_m2" %in% names(df_agg)) df_agg$Land_cropland_m2 else 0
    land_g_conv <- if ("Land_grassland_convertible_m2" %in% names(df_agg)) df_agg$Land_grassland_convertible_m2 else 0
    land_g_unco <- if ("Land_grassland_unconvertible_m2" %in% names(df_agg)) df_agg$Land_grassland_unconvertible_m2 else 0
    land_other <- if ("Land_other_m2" %in% names(df_agg)) df_agg$Land_other_m2 else 0
    land_total_m2 <- if ("Land_m2" %in% names(df_agg)) df_agg$Land_m2 else (land_crop + land_g_conv + land_g_unco + land_other)

    # 5. Raw Water components (m3)
    has_water <- ("Water_feed_m3" %in% names(df_agg) && any(df_agg$Water_feed_m3 > 0, na.rm = TRUE)) ||
                 ("feed_water_m3_year" %in% names(df_agg) && any(df_agg$feed_water_m3_year > 0, na.rm = TRUE))
    water_total_m3 <- if ("Water_feed_m3" %in% names(df_agg)) df_agg$Water_feed_m3 else if ("feed_water_m3_year" %in% names(df_agg)) df_agg$feed_water_m3_year else 0

    dairy_tags <- if (all(c("animal_tag", "animal_subtype") %in% names(df))) {
      unique(df$animal_tag[df$animal_subtype %in% "dairy"])
    } else {
      character()
    }

    # Special Case: Protein FCR and Human Food Competition (Mottet et al. 2017)
    if (impact_cat == "fcr" && any(c("total_protein_kg", "Protein_FCR_total") %in% names(df_agg))) {
      is_dairy <- if ("animal_subtype" %in% names(df_agg)) {
        df_agg$animal_subtype %in% "dairy"
      } else if ("animal_tag" %in% names(df_agg)) {
        df_agg$animal_tag %in% dairy_tags
      } else {
        FALSE
      }
      is_non_milking_dairy <- is_dairy &
        (if ("milk_FPCM_kg" %in% names(df_agg)) df_agg$milk_FPCM_kg == 0 else FALSE) &
        (if ("meat_carcass_weight_kg" %in% names(df_agg)) df_agg$meat_carcass_weight_kg == 0 else TRUE)

      has_direct_fcr <- "Protein_FCR_total" %in% names(df_agg) && "Protein_FCR_cropland" %in% names(df_agg)
      has_raw_cp <- all(c("feed_CP_total_kg", "feed_CP_cropland_kg", "total_protein_kg") %in% names(df_agg))

      valid_prot <- (df_agg$total_protein_kg > 0) & !is_non_milking_dairy

      if (has_raw_cp) {
        df_agg$fcr_total <- ifelse(valid_prot & df_agg$total_protein_kg > 0, df_agg$feed_CP_total_kg / df_agg$total_protein_kg, 0)
        df_agg$fcr_cropland <- ifelse(valid_prot & df_agg$total_protein_kg > 0, df_agg$feed_CP_cropland_kg / df_agg$total_protein_kg, 0)
      } else if (has_direct_fcr) {
        df_agg$fcr_total <- ifelse(valid_prot, tidyr::replace_na(df_agg$Protein_FCR_total, 0), 0)
        df_agg$fcr_cropland <- ifelse(valid_prot, tidyr::replace_na(df_agg$Protein_FCR_cropland, 0), 0)
      } else {
        df_agg$fcr_total <- 0
        df_agg$fcr_cropland <- 0
      }

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

    # 6. Scaling factors for functional units
    if (fu == "protein" && "total_protein_kg" %in% names(df_agg)) {
      prot <- df_agg$total_protein_kg
      is_dairy <- if ("animal_subtype" %in% names(df_agg)) df_agg$animal_subtype %in% "dairy" else if ("animal_tag" %in% names(df_agg)) df_agg$animal_tag %in% dairy_tags else FALSE
      is_dairy_dry <- is_dairy &
        (if ("milk_FPCM_kg" %in% names(df_agg)) df_agg$milk_FPCM_kg == 0 else FALSE) &
        (if ("meat_carcass_weight_kg" %in% names(df_agg)) df_agg$meat_carcass_weight_kg == 0 else TRUE)
      valid <- prot > 0 & !is_dairy_dry
      emiss_scale <- ifelse(valid, 1e6 / prot, 0)
      land_scale  <- ifelse(valid, 1 / prot, 0)
      water_scale <- ifelse(valid, 1 / prot, 0)
      emiss_unit  <- "kg CO2e / kg protein"
      land_unit   <- "m2 / kg protein"
      water_unit  <- "m³ depriv. / kg protein"
      scale_desc  <- "per kg of edible animal protein"
    } else if (fu == "head" && "population" %in% names(df_agg)) {
      pop <- df_agg$population
      valid <- pop > 0
      emiss_scale <- ifelse(valid, 1e6 / pop, 0)
      land_scale  <- ifelse(valid, 1 / pop, 0)
      water_scale <- ifelse(valid, 1 / pop, 0)
      emiss_unit  <- "kg CO2e / head"
      land_unit   <- "m2 / head"
      water_unit  <- "m³ depriv. / head"
      scale_desc  <- "per animal head per year"
    } else if (fu %in% c("product", "milk", "meat", "wool", "egg") && any(c("milk_FPCM_kg", "meat_carcass_weight_kg", "egg_fresh_kg", "wool_kg") %in% names(df_agg))) {
      milk_kg <- if ("milk_FPCM_kg" %in% names(df_agg)) df_agg$milk_FPCM_kg else 0
      meat_kg <- if ("meat_carcass_weight_kg" %in% names(df_agg)) df_agg$meat_carcass_weight_kg else 0
      meat_live_kg <- if ("meat_live_weight_kg" %in% names(df_agg)) df_agg$meat_live_weight_kg else (if ("meat_carcass_weight_kg" %in% names(df_agg)) df_agg$meat_carcass_weight_kg / 0.54 else 0)
      egg_kg <- if ("egg_fresh_kg" %in% names(df_agg)) df_agg$egg_fresh_kg else 0
      wool_kg <- if ("wool_kg" %in% names(df_agg)) df_agg$wool_kg else 0

      is_dairy <- if ("animal_subtype" %in% names(df_agg)) df_agg$animal_subtype %in% "dairy" else if ("animal_tag" %in% names(df_agg)) df_agg$animal_tag %in% dairy_tags else FALSE
      is_non_milking_dairy <- is_dairy & (milk_kg == 0) & (meat_kg == 0)

      nel_total_mj <- 3.1 * milk_kg
      neg_total_mj <- ifelse(egg_kg > 0 | is_non_milking_dairy, 0, 15.0 * meat_live_kg)
      e_wool_mj <- if ("animal_subtype" %in% names(df_agg)) ifelse(df_agg$animal_subtype == "meat", 24.0 * wool_kg, 0) else 0
      e_tot_mj <- nel_total_mj + neg_total_mj + e_wool_mj

      af_milk_calc <- if ("AF_milk" %in% names(df_agg)) df_agg$AF_milk else ifelse(e_tot_mj > 0 & egg_kg == 0 & !is_non_milking_dairy, nel_total_mj / e_tot_mj, ifelse(milk_kg > 0, 1, 0))
      af_meat_calc <- if ("AF_meat" %in% names(df_agg)) df_agg$AF_meat else ifelse(egg_kg > 0 | is_non_milking_dairy, 0, ifelse(e_tot_mj > 0, neg_total_mj / e_tot_mj, ifelse(meat_kg > 0, 1, 0)))
      af_wool_calc <- if ("AF_wool" %in% names(df_agg)) df_agg$AF_wool else ifelse(e_tot_mj > 0 & egg_kg == 0 & !is_non_milking_dairy, e_wool_mj / e_tot_mj, 0)
      af_egg_calc  <- if ("AF_egg" %in% names(df_agg)) df_agg$AF_egg else ifelse(egg_kg > 0, 1, 0)

      if (fu == "milk") {
        prod_yield <- ifelse(is_non_milking_dairy, 0, milk_kg)
        af <- ifelse(is_non_milking_dairy, 0, af_milk_calc)
        prod_label <- "FPCM milk"
      } else if (fu == "meat") {
        prod_yield <- ifelse(is_non_milking_dairy, 0, meat_kg)
        af <- ifelse(is_non_milking_dairy, 0, af_meat_calc)
        prod_label <- "carcass meat"
      } else if (fu == "wool") {
        prod_yield <- ifelse(is_non_milking_dairy, 0, wool_kg)
        af <- ifelse(is_non_milking_dairy, 0, af_wool_calc)
        prod_label <- "wool"
      } else if (fu == "egg") {
        prod_yield <- egg_kg
        af <- af_egg_calc
        prod_label <- "fresh eggs"
      } else {
        prod_yield <- dplyr::case_when(
          is_non_milking_dairy ~ 0,
          milk_kg > 0 ~ milk_kg,
          meat_kg > 0 ~ meat_kg,
          egg_kg > 0 ~ egg_kg,
          wool_kg > 0 & af_wool_calc > 0 ~ wool_kg,
          TRUE ~ 0
        )
        af <- dplyr::case_when(
          is_non_milking_dairy ~ 0,
          milk_kg > 0 ~ af_milk_calc,
          meat_kg > 0 ~ af_meat_calc,
          egg_kg > 0 ~ af_egg_calc,
          wool_kg > 0 & af_wool_calc > 0 ~ af_wool_calc,
          TRUE ~ 0
        )
        prod_label <- "product"
      }

      valid <- prod_yield > 0 & af > 0
      emiss_scale <- ifelse(valid, (1e6 * af) / prod_yield, 0)
      land_scale  <- ifelse(valid, af / prod_yield, 0)
      water_scale <- ifelse(valid, af / prod_yield, 0)
      emiss_unit  <- paste0("kg CO2e / kg ", prod_label)
      land_unit   <- paste0("m2 / kg ", prod_label)
      water_unit  <- paste0("m³ depriv. / kg ", prod_label)
      scale_desc  <- paste0("per kg of ", prod_label, " (biophysical allocation)")
    } else {
      # Default: fu == "total"
      emiss_scale <- rep(1, nrow(df_agg))
      land_scale  <- rep(1 / 10000, nrow(df_agg)) # m2 to ha
      water_scale <- rep(1 / 1e6, nrow(df_agg))   # m3 to Mm3 (10^6 m3 depriv.)
      emiss_unit  <- "Gg CO2e"
      land_unit   <- "ha"
      water_unit  <- "Mm³ depriv."
      scale_desc  <- "total herd annual footprint"
    }

    # 7. Standardized Chart Renderers

    # --- Mode A: Carbon Footprint (GHG) ---
    if (impact_cat == "ghg") {
      df_agg$co2e_ch4_ent     <- co2e_ch4_ent * emiss_scale
      df_agg$co2e_ch4_man     <- co2e_ch4_man * emiss_scale
      df_agg$co2e_n2o_dir     <- co2e_n2o_dir * emiss_scale
      df_agg$co2e_n2o_ind     <- co2e_n2o_ind * emiss_scale
      df_agg$co2e_feed_fossil <- co2e_feed_fossil * emiss_scale
      df_agg$co2e_feed_luc    <- co2e_feed_luc * emiss_scale
      df_agg$total_val        <- df_agg$co2e_ch4_ent + df_agg$co2e_ch4_man + df_agg$co2e_n2o_dir +
                                 df_agg$co2e_n2o_ind + df_agg$co2e_feed_fossil + df_agg$co2e_feed_luc

      df_agg <- df_agg[order(df_agg$total_val), ]
      df_agg$plot_label <- factor(df_agg$plot_label, levels = unique(df_agg$plot_label))

      emiss_cols   <- c("co2e_ch4_ent", "co2e_ch4_man", "co2e_n2o_dir", "co2e_n2o_ind", "co2e_feed_fossil", "co2e_feed_luc")
      emiss_labels <- c("CH4 Enteric", "CH4 Manure", "N2O Direct", "N2O Indirect", "Feed Fossil GHG", "Feed LUC Deforestation")

      plot_long <- df_agg %>%
        dplyr::select(dplyr::all_of(c("plot_label", emiss_cols))) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(emiss_cols), names_to = "component_raw", values_to = "value") %>%
        dplyr::mutate(component = factor(component_raw, levels = emiss_cols, labels = emiss_labels))

      active_comps <- plot_long %>%
        dplyr::group_by(component) %>%
        dplyr::summarise(s = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(s > 0) %>%
        dplyr::pull(component)

      if (length(active_comps) > 0) plot_long <- plot_long %>% dplyr::filter(component %in% active_comps)

      totals_df <- df_agg %>%
        dplyr::filter(total_val > 0) %>%
        dplyr::mutate(
          label_text = dplyr::case_when(
            total_val >= 1000 ~ scales::comma(total_val, accuracy = 1),
            total_val >= 10   ~ formatC(total_val, digits = 1, format = "f"),
            total_val >= 1    ~ formatC(total_val, digits = 2, format = "f"),
            TRUE              ~ formatC(total_val, digits = 3, format = "f")
          )
        )

      palette_ghg <- c(
        "CH4 Enteric"            = "#38BDF8",
        "CH4 Manure"             = "#1E40AF",
        "N2O Direct"             = "#7E22CE",
        "N2O Indirect"           = "#C084FC",
        "Feed Fossil GHG"        = "#D97706",
        "Feed LUC Deforestation" = "#DC2626"
      )

      p <- ggplot(plot_long, aes(x = value, y = plot_label, fill = component)) +
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
        scale_fill_manual(values = palette_ghg) +
        labs(
          title = paste0("Carbon Footprint (", emiss_unit, ")"),
          subtitle = paste0("Greenhouse gas emissions breakdown ", scale_desc),
          x = emiss_unit,
          y = NULL
        )
      return(p)
    }

    # --- Mode B: Land Footprint (Land Use) ---
    if (impact_cat == "land") {
      df_agg$land_crop   <- land_crop * land_scale
      df_agg$land_g_conv <- land_g_conv * land_scale
      df_agg$land_g_unco <- land_g_unco * land_scale
      df_agg$land_other  <- land_other * land_scale
      df_agg$total_val   <- df_agg$land_crop + df_agg$land_g_conv + df_agg$land_g_unco + df_agg$land_other

      df_agg <- df_agg[order(df_agg$total_val), ]
      df_agg$plot_label <- factor(df_agg$plot_label, levels = unique(df_agg$plot_label))

      if (has_land_breakdown) {
        has_other <- any(df_agg$land_other > 0, na.rm = TRUE)
        l_cols <- if (has_other) c("land_crop", "land_g_conv", "land_g_unco", "land_other") else c("land_crop", "land_g_conv", "land_g_unco")
        l_labs <- if (has_other) c("Cropland", "Grassland (Conv.)", "Grassland (Unconv.)", "Land Other") else c("Cropland", "Grassland (Conv.)", "Grassland (Unconv.)")

        plot_long <- df_agg %>%
          dplyr::select(dplyr::all_of(c("plot_label", l_cols))) %>%
          tidyr::pivot_longer(cols = dplyr::all_of(l_cols), names_to = "component_raw", values_to = "value") %>%
          dplyr::mutate(component = factor(component_raw, levels = l_cols, labels = l_labs))
      } else {
        plot_long <- df_agg %>%
          dplyr::select(dplyr::all_of(c("plot_label", "total_val"))) %>%
          dplyr::mutate(value = total_val, component = factor("Land Use"))
      }

      active_comps <- plot_long %>%
        dplyr::group_by(component) %>%
        dplyr::summarise(s = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(s > 0) %>%
        dplyr::pull(component)

      if (length(active_comps) > 0) plot_long <- plot_long %>% dplyr::filter(component %in% active_comps)

      totals_df <- df_agg %>%
        dplyr::filter(total_val > 0) %>%
        dplyr::mutate(
          label_text = dplyr::case_when(
            total_val >= 1000 ~ scales::comma(total_val, accuracy = 1),
            total_val >= 10   ~ formatC(total_val, digits = 1, format = "f"),
            total_val >= 1    ~ formatC(total_val, digits = 2, format = "f"),
            TRUE              ~ formatC(total_val, digits = 3, format = "f")
          )
        )

      palette_land <- c(
        "Cropland"            = "#FFEA00",
        "Grassland (Conv.)"   = "#16A34A",
        "Grassland (Unconv.)" = "#15803D",
        "Land Other"          = "#9CA3AF",
        "Land Use"            = "#15803D"
      )

      p <- ggplot(plot_long, aes(x = value, y = plot_label, fill = component)) +
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
        scale_fill_manual(values = palette_land) +
        labs(
          title = paste0("Land Footprint (", land_unit, ")"),
          subtitle = paste0("Land use requirement by agroecological type ", scale_desc),
          x = land_unit,
          y = NULL
        )
      return(p)
    }

    # --- Mode C: Water Footprint (Water Deprivation) ---
    if (impact_cat == "water") {
      df_agg$water_val <- water_total_m3 * water_scale
      df_agg <- df_agg[order(df_agg$water_val), ]
      df_agg$plot_label <- factor(df_agg$plot_label, levels = unique(df_agg$plot_label))

      totals_df <- df_agg %>%
        dplyr::filter(water_val > 0) %>%
        dplyr::mutate(
          label_text = dplyr::case_when(
            water_val >= 1000 ~ scales::comma(water_val, accuracy = 1),
            water_val >= 10   ~ formatC(water_val, digits = 1, format = "f"),
            water_val >= 1    ~ formatC(water_val, digits = 2, format = "f"),
            TRUE              ~ formatC(water_val, digits = 3, format = "f")
          )
        )

      p <- ggplot(df_agg, aes(x = water_val, y = plot_label)) +
        geom_col(width = 0.62, fill = "#0284C7", color = "#0369A1", linewidth = 0.3) +
        geom_text(
          data = totals_df,
          aes(x = water_val, y = plot_label, label = label_text),
          hjust = -0.15,
          size = 4.0,
          fontface = "bold",
          color = "#1F2937"
        ) +
        theme_herdr_plot() +
        scale_x_continuous(expand = expansion(mult = c(0, 0.20)), labels = scales::comma) +
        labs(
          title = paste0("Water Deprivation Footprint (", water_unit, ")"),
          subtitle = paste0("Freshwater deprivation from feed production (AWARE / EF 3.1 scarcity-weighted) ", scale_desc),
          x = if (fu %in% c("protein", "product", "milk", "meat", "wool", "egg")) paste0("Water Intensity (", water_unit, ")") else if (fu == "head") paste0("Water Deprivation per Head (", water_unit, ")") else paste0("Water Deprivation (", water_unit, ")"),
          y = NULL
        )
      return(p)
    }

    # --- Mode D: Multi-Impact Overview ---
    if (impact_cat == "overview") {
      df_agg$co2e_ch4_ent     <- co2e_ch4_ent * emiss_scale
      df_agg$co2e_ch4_man     <- co2e_ch4_man * emiss_scale
      df_agg$co2e_n2o_dir     <- co2e_n2o_dir * emiss_scale
      df_agg$co2e_n2o_ind     <- co2e_n2o_ind * emiss_scale
      df_agg$co2e_feed_fossil <- co2e_feed_fossil * emiss_scale
      df_agg$co2e_feed_luc    <- co2e_feed_luc * emiss_scale
      df_agg$total_co2e       <- df_agg$co2e_ch4_ent + df_agg$co2e_ch4_man + df_agg$co2e_n2o_dir +
                                 df_agg$co2e_n2o_ind + df_agg$co2e_feed_fossil + df_agg$co2e_feed_luc

      df_agg <- df_agg[order(df_agg$total_co2e), ]
      df_agg$plot_label <- factor(df_agg$plot_label, levels = unique(df_agg$plot_label))

      emiss_cols   <- c("co2e_ch4_ent", "co2e_ch4_man", "co2e_n2o_dir", "co2e_n2o_ind", "co2e_feed_fossil", "co2e_feed_luc")
      emiss_labels <- c("CH4 Enteric", "CH4 Manure", "N2O Direct", "N2O Indirect", "Feed Fossil GHG", "Feed LUC Deforestation")

      if (fu_raw %in% c("cradle_breakdown", "cradle")) {
        emiss_panel <- "Cradle-to-Farm GHG (Gg CO2e)"
        land_panel  <- "Land Footprint (ha)"
        water_panel <- "Water Deprivation (Mm³ depriv.)"
        overview_title <- "Cradle-to-Farm-Gate Scope 1 + Scope 3 LCA Breakdown"
      } else if (fu == "head") {
        emiss_panel <- "GHG per Head (kg CO2e / head)"
        land_panel  <- "Land per Head (m2 / head)"
        water_panel <- "Water per Head (m³ depriv. / head)"
        overview_title <- "Environmental Impact Assessment"
      } else if (fu == "protein") {
        emiss_panel <- "GHG Intensity (kg CO2e / kg protein)"
        land_panel  <- "Land Footprint (m2 / kg protein)"
        water_panel <- "Water Intensity (m³ depriv. / kg protein)"
        overview_title <- "Environmental Impact Assessment"
      } else if (fu %in% c("product", "milk", "meat", "wool", "egg")) {
        emiss_panel <- paste0("GHG Intensity (", emiss_unit, ")")
        land_panel  <- paste0("Land Footprint (", land_unit, ")")
        water_panel <- paste0("Water Intensity (", water_unit, ")")
        overview_title <- "Environmental Impact Assessment"
      } else {
        emiss_panel <- "GHG Emissions (Gg CO2e)"
        land_panel  <- "Land Footprint (ha)"
        water_panel <- "Water Deprivation (Mm³ depriv.)"
        overview_title <- "Environmental Impact Assessment"
      }

      emissions_df <- df_agg %>%
        dplyr::select(dplyr::all_of(c("plot_label", emiss_cols))) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(emiss_cols), names_to = "component_raw", values_to = "value") %>%
        dplyr::mutate(
          panel = emiss_panel,
          component = factor(component_raw, levels = emiss_cols, labels = emiss_labels)
        ) %>%
        dplyr::select(plot_label, value, panel, component)

      panel_list <- list(emissions_df)

      if (has_land) {
        if (has_land_breakdown) {
          df_agg$land_crop   <- land_crop * land_scale
          df_agg$land_g_conv <- land_g_conv * land_scale
          df_agg$land_g_unco <- land_g_unco * land_scale
          df_agg$land_other  <- land_other * land_scale
          has_other <- any(df_agg$land_other > 0, na.rm = TRUE)
          l_cols <- if (has_other) c("land_crop", "land_g_conv", "land_g_unco", "land_other") else c("land_crop", "land_g_conv", "land_g_unco")
          l_labs <- if (has_other) c("Cropland", "Grassland (Conv.)", "Grassland (Unconv.)", "Land Other") else c("Cropland", "Grassland (Conv.)", "Grassland (Unconv.)")

          land_df <- df_agg %>%
            dplyr::select(dplyr::all_of(c("plot_label", l_cols))) %>%
            tidyr::pivot_longer(cols = dplyr::all_of(l_cols), names_to = "component_raw", values_to = "value") %>%
            dplyr::mutate(panel = land_panel, component = factor(component_raw, levels = l_cols, labels = l_labs)) %>%
            dplyr::select(plot_label, value, panel, component)
        } else {
          df_agg$land_val <- land_total_m2 * land_scale
          land_df <- df_agg %>%
            dplyr::select(dplyr::all_of(c("plot_label", "land_val"))) %>%
            dplyr::mutate(value = land_val, panel = land_panel, component = factor("Land Use")) %>%
            dplyr::select(plot_label, value, panel, component)
        }
        panel_list <- c(panel_list, list(land_df))
      }

      if (has_water) {
        df_agg$water_val <- water_total_m3 * water_scale
        water_df <- df_agg %>%
          dplyr::select(dplyr::all_of(c("plot_label", "water_val"))) %>%
          dplyr::mutate(value = water_val, panel = water_panel, component = factor("Water Deprivation")) %>%
          dplyr::select(plot_label, value, panel, component)
        panel_list <- c(panel_list, list(water_df))
      }

      plot_data <- dplyr::bind_rows(panel_list)
      unique_panels <- unique(plot_data$panel)
      plot_data$panel <- factor(plot_data$panel, levels = unique_panels)

      palette_overview <- c(
        "CH4 Enteric"            = "#38BDF8",
        "CH4 Manure"             = "#1E40AF",
        "N2O Direct"             = "#7E22CE",
        "N2O Indirect"           = "#C084FC",
        "Feed Fossil GHG"        = "#D97706",
        "Feed LUC Deforestation" = "#DC2626",
        "Cropland"               = "#FFEA00",
        "Grassland (Conv.)"      = "#16A34A",
        "Grassland (Unconv.)"    = "#15803D",
        "Land Other"             = "#9CA3AF",
        "Land Use"               = "#15803D",
        "Water Deprivation"      = "#0284C7"
      )

      active_comps <- plot_data %>%
        dplyr::group_by(component) %>%
        dplyr::summarise(s = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(s > 0) %>%
        dplyr::pull(component)

      if (length(active_comps) > 0) plot_data <- plot_data %>% dplyr::filter(component %in% active_comps)

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
        facet_wrap(~panel, scales = "free_x") +
        theme_herdr_plot() +
        scale_x_continuous(expand = expansion(mult = c(0, 0.20)), labels = scales::comma) +
        scale_fill_manual(values = palette_overview, drop = TRUE) +
        labs(
          title = overview_title,
          subtitle = paste0("Multi-criteria environmental footprint summary ", scale_desc),
          x = NULL
        )
      return(p)
    }

    return(NULL)
  }

  # A_feed) FEED ENVIRONMENTAL FOOTPRINT (Standalone calculate_feed_emissions)
  is_feed_footprint <- identical(func_name, "calculate_feed_emissions") ||
    all(c("feed_CO2e_fossil_kg_year", "feed_CO2e_luc_kg_year", "feed_water_m3_year") %in% names(df_agg))

  if (is_feed_footprint) {
    fu <- tolower(functional_unit[1])
    is_per_head <- fu == "head" && ("population" %in% names(df_agg) || "feed_CO2e_per_head_year" %in% names(df_agg))
    feed_impact <- if (!is.null(impact_category)) tolower(impact_category[1]) else "overview"

    pop <- if ("population" %in% names(df_agg)) df_agg$population else 1
    pop_safe <- ifelse(pop > 0, pop, 1)

    if (is_per_head) {
      df_agg$fossil_val <- df_agg$feed_CO2e_fossil_kg_year / pop_safe
      df_agg$luc_val    <- df_agg$feed_CO2e_luc_kg_year / pop_safe
      df_agg$water_val  <- df_agg$feed_water_m3_year / pop_safe
      emiss_unit <- "kg CO2e / head"
      water_unit <- "m³ depriv. / head"
      sub_title  <- "Annual feed environmental footprint per animal head (ECOALIM V9 / EF 3.1 AWARE)"
    } else {
      df_agg$fossil_val <- df_agg$feed_CO2e_fossil_kg_year / 1000
      df_agg$luc_val    <- df_agg$feed_CO2e_luc_kg_year / 1000
      df_agg$water_val  <- df_agg$feed_water_m3_year
      emiss_unit <- "t CO2e"
      water_unit <- "m³ depriv."
      sub_title  <- "Total annual feed greenhouse gas emissions and water deprivation (ECOALIM V9 / EF 3.1 AWARE)"
    }

    df_agg$total_ghg <- df_agg$fossil_val + df_agg$luc_val

    if (feed_impact == "water") {
      df_agg <- df_agg[order(df_agg$water_val), ]
      df_agg$plot_label <- factor(df_agg$plot_label, levels = unique(df_agg$plot_label))

      totals_df <- df_agg %>%
        dplyr::filter(water_val > 0) %>%
        dplyr::mutate(label_text = ifelse(water_val >= 1000, scales::comma(water_val, accuracy = 1), formatC(water_val, digits = 1, format = "f")))

      p <- ggplot(df_agg, aes(x = water_val, y = plot_label)) +
        geom_col(width = 0.62, fill = "#0284C7", color = "#0369A1", linewidth = 0.3) +
        geom_text(data = totals_df, aes(x = water_val, y = plot_label, label = label_text), hjust = -0.15, size = 4.0, fontface = "bold", color = "#1F2937") +
        theme_herdr_plot() +
        scale_x_continuous(expand = expansion(mult = c(0, 0.20)), labels = scales::comma) +
        labs(
          title = paste0("Feed Water Deprivation (", water_unit, ")"),
          subtitle = sub_title,
          x = paste0("Water Deprivation (", water_unit, ")"),
          y = NULL
        )
      return(p)
    }

    if (feed_impact == "ghg") {
      df_agg <- df_agg[order(df_agg$total_ghg), ]
      df_agg$plot_label <- factor(df_agg$plot_label, levels = unique(df_agg$plot_label))

      ghg_long <- df_agg %>%
        dplyr::select(dplyr::all_of(c("plot_label", "fossil_val", "luc_val"))) %>%
        tidyr::pivot_longer(cols = c("fossil_val", "luc_val"), names_to = "component_raw", values_to = "value") %>%
        dplyr::mutate(component = factor(component_raw, levels = c("fossil_val", "luc_val"), labels = c("Fossil & Inputs", "LUC Deforestation")))

      totals_df <- df_agg %>%
        dplyr::filter(total_ghg > 0) %>%
        dplyr::mutate(label_text = ifelse(total_ghg >= 1000, scales::comma(total_ghg, accuracy = 1), formatC(total_ghg, digits = 1, format = "f")))

      p <- ggplot(ghg_long, aes(x = value, y = plot_label, fill = component)) +
        geom_col(position = position_stack(reverse = TRUE), width = 0.62) +
        geom_text(data = totals_df, aes(x = total_ghg, y = plot_label, label = label_text), inherit.aes = FALSE, hjust = -0.15, size = 4.0, fontface = "bold", color = "#1F2937") +
        theme_herdr_plot() +
        scale_x_continuous(expand = expansion(mult = c(0, 0.20)), labels = scales::comma) +
        scale_fill_manual(values = c("Fossil & Inputs" = "#D97706", "LUC Deforestation" = "#DC2626")) +
        labs(
          title = paste0("Feed Greenhouse Gas Emissions (", emiss_unit, ")"),
          subtitle = sub_title,
          x = paste0("Feed GHG (", emiss_unit, ")"),
          y = NULL
        )
      return(p)
    }

    # Default: overview (2 panels)
    df_agg <- df_agg[order(df_agg$total_ghg), ]
    df_agg$plot_label <- factor(df_agg$plot_label, levels = unique(df_agg$plot_label))

    emiss_panel <- if (is_per_head) "Feed GHG per Head (kg CO2e / head)" else "Feed GHG Emissions (t CO2e)"
    water_panel <- if (is_per_head) "Water per Head (m³ depriv. / head)" else "Water Deprivation (m³ depriv.)"

    ghg_df <- df_agg %>%
      dplyr::select(dplyr::all_of(c("plot_label", "fossil_val", "luc_val"))) %>%
      tidyr::pivot_longer(cols = c("fossil_val", "luc_val"), names_to = "component_raw", values_to = "value") %>%
      dplyr::mutate(
        panel = emiss_panel,
        component = factor(component_raw, levels = c("fossil_val", "luc_val"), labels = c("Fossil & Inputs", "LUC Deforestation"))
      ) %>%
      dplyr::select(plot_label, value, panel, component)

    water_df <- df_agg %>%
      dplyr::select(dplyr::all_of(c("plot_label", "water_val"))) %>%
      dplyr::mutate(
        value = water_val,
        panel = water_panel,
        component = factor("Water Deprivation")
      ) %>%
      dplyr::select(plot_label, value, panel, component)

    plot_data <- dplyr::bind_rows(ghg_df, water_df)
    plot_data$panel <- factor(plot_data$panel, levels = c(emiss_panel, water_panel))

    palette_feed <- c(
      "Fossil & Inputs"   = "#D97706",
      "LUC Deforestation" = "#DC2626",
      "Water Deprivation" = "#0284C7"
    )

    totals_df <- plot_data %>%
      dplyr::group_by(plot_label, panel) %>%
      dplyr::summarise(total_val = sum(value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(total_val > 0) %>%
      dplyr::mutate(label_text = ifelse(total_val >= 1000, scales::comma(total_val, accuracy = 1), formatC(total_val, digits = 1, format = "f")))

    p <- ggplot(plot_data, aes(x = value, y = plot_label, fill = component)) +
      geom_col(position = position_stack(reverse = TRUE), width = 0.62) +
      geom_text(data = totals_df, aes(x = total_val, y = plot_label, label = label_text), inherit.aes = FALSE, hjust = -0.15, size = 4.0, fontface = "bold", color = "#1F2937") +
      facet_wrap(~panel, scales = "free_x") +
      theme_herdr_plot() +
      scale_x_continuous(expand = expansion(mult = c(0, 0.20)), labels = scales::comma) +
      scale_fill_manual(values = palette_feed) +
      labs(
        title = "Feed Environmental Footprint",
        subtitle = sub_title,
        x = NULL
      )
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
        if (length(valid_vals) == 0) {
          return("Unknown")
        }
        paste(valid_vals, collapse = " - ")
      })


      totals <- stats::aggregate(Land_Val ~ plot_label, data = df_land_agg, FUN = sum)
      totals <- totals[order(totals$Land_Val), ]
      df_land_agg$plot_label <- factor(df_land_agg$plot_label, levels = unique(totals$plot_label))

      totals$label_text <- dplyr::case_when(
        totals$Land_Val >= 1000 ~ scales::comma(totals$Land_Val, accuracy = 1),
        totals$Land_Val >= 10 ~ formatC(totals$Land_Val, digits = 1, format = "f"),
        totals$Land_Val >= 1 ~ formatC(totals$Land_Val, digits = 2, format = "f"),
        TRUE ~ formatC(totals$Land_Val, digits = 3, format = "f")
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
    calculate_NEm                         = "NEm_MJday",
    calculate_feed_emissions              = "feed_CO2e_total_kg_year"
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
    if (length(target_cols) == 0) {
      return(NULL)
    }
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
