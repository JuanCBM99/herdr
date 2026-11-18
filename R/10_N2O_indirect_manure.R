#' Shared Internal Engine for Indirect Manure N2O Emissions
#' @noRd
herdr_calculate_N2O_indirect <- function(mode = c("volatilization", "leaching"),
                                         automatic_cycle = FALSE,
                                         saveoutput = TRUE,
                                         data_dir = "user_data",
                                         direct_N2O_df = NULL,
                                         pop_df = NULL,
                                         ipcc_master = NULL) {
  mode <- match.arg(mode)
  is_vol <- mode == "volatilization"
  message(sprintf("Calculating indirect N2O emissions (%s)...", mode))

  if (is.null(ipcc_master)) {
    ipcc_master <- herdr_read_manure_table(file.path(data_dir, "ipcc_mm.csv"))
  }

  join_keys <- c("farm_id", "region", "subregion", "animal_tag", "animal_type", "animal_subtype")
  sys_keys <- c(
    "system_base", "management_months", "system_climate",
    "system_subclimate", "climate_zone", "system_variant", "climate_moisture"
  )
  prejoined_cols <- c(join_keys, "N_excreted_kgheadyear", sys_keys, "allocation", "population")

  has_prejoined <- !is.null(direct_N2O_df) && all(prejoined_cols %in% names(direct_N2O_df))

  if (has_prejoined) {
    base_df <- direct_N2O_df %>%
      dplyr::select(dplyr::all_of(prejoined_cols)) %>%
      dplyr::distinct()
  } else {
    user_manure <- herdr_read_manure_table(file.path(data_dir, "manure_management.csv"))

    if (is.null(direct_N2O_df)) {
      direct_N2O_df <- suppressMessages(
        calculate_N2O_direct_manure(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir)
      )
    }
    if (is.null(pop_df)) {
      pop_df <- suppressMessages(
        calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir)
      )
    }

    herdr_validate_manure_systems(user_manure, ipcc_master, animal_metadata = direct_N2O_df)
    herdr_validate_manure_allocation(user_manure)
    herdr_validate_manure_coverage(pop_df, user_manure)

    base_df <- direct_N2O_df %>%
      dplyr::select(dplyr::all_of(join_keys), N_excreted_kgheadyear) %>%
      dplyr::distinct() %>%
      dplyr::left_join(
        pop_df %>% dplyr::select(dplyr::all_of(join_keys), population),
        by = join_keys
      ) %>%
      dplyr::left_join(
        user_manure %>%
          dplyr::select(farm_id, region, subregion, animal_tag, dplyr::all_of(sys_keys), allocation),
        by = c("farm_id", "region", "subregion", "animal_tag")
      )
  }

  frac_col <- if (is_vol) "frac_gas" else "frac_leach"
  ef_col   <- if (is_vol) "EF4" else "EF5"
  loss_col <- if (is_vol) "N_volatilization_kg_year" else "N_leaching_kg_year"
  n2o_col  <- if (is_vol) "N2O_vol_kgyear" else "N2O_leach_kgyear"
  out_file <- if (is_vol) "output/N2O_indirect_volatilization.csv" else "output/N2O_indirect_leaching.csv"

  results <- base_df %>%
    dplyr::left_join(
      ipcc_master %>%
        dplyr::select(dplyr::all_of(sys_keys), animal_type, animal_subtype, dplyr::all_of(c(frac_col, ef_col))),
      by = c(sys_keys, "animal_type", "animal_subtype")
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c("population", "N_excreted_kgheadyear", "allocation", frac_col, ef_col)),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      )
    )

  results[[loss_col]] <- results$population * results$N_excreted_kgheadyear * results$allocation * results[[frac_col]]
  results[[n2o_col]]  <- results[[ef_col]] * results[[loss_col]] * (44 / 28)

  results <- results %>%
    dplyr::select(
      dplyr::all_of(join_keys),
      system_base, system_variant,
      N_excreted_kgheadyear, dplyr::all_of(c(frac_col, ef_col, loss_col, n2o_col))
    )

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, out_file)
    message(sprintf("[herdr] Saved output to %s", out_file))
  }

  results
}

#' Calculate indirect N2O emissions from volatilization
#'
#' Computes indirect N2O emissions derived from volatilization of excreted nitrogen (IPCC Eq 10.26 and 10.28).
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput Logical. If TRUE (default), results are saved to output folder.
#' @param data_dir Path to the directory containing input CSV/data files. Defaults to `"user_data"`.
#' @param direct_N2O_df Optional precalculated direct N2O manure dataframe.
#' @param pop_df Optional precalculated population dataframe.
#' @return A tibble with volatilized nitrogen (\code{N_volatilization_kg_year}, kg N/year)
#'   and indirect N2O emissions from volatilization (\code{N2O_vol_kgyear}, kg N2O/year).
#' @examples
#' \dontrun{
#' calculate_N2O_indirect_volatilization(saveoutput = FALSE, data_dir = "user_data")
#' }
#' @export
calculate_N2O_indirect_volatilization <- function(automatic_cycle = FALSE, saveoutput = TRUE, data_dir = "user_data",
                                                  direct_N2O_df = NULL, pop_df = NULL) {
  herdr_calculate_N2O_indirect(
    mode = "volatilization",
    automatic_cycle = automatic_cycle,
    saveoutput = saveoutput,
    data_dir = data_dir,
    direct_N2O_df = direct_N2O_df,
    pop_df = pop_df
  )
}

#' Calculate indirect N2O emissions from leaching
#'
#' Computes indirect N2O emissions derived from nitrogen leaching (IPCC Eq 10.27 and 10.29).
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput Logical. If TRUE (default), results are saved to output folder.
#' @param data_dir Path to the directory containing input CSV/data files. Defaults to `"user_data"`.
#' @param direct_N2O_df Optional precalculated direct N2O manure dataframe.
#' @param pop_df Optional precalculated population dataframe.
#' @return A tibble with leached nitrogen (\code{N_leaching_kg_year}, kg N/year)
#'   and indirect N2O emissions from leaching (\code{N2O_leach_kgyear}, kg N2O/year).
#' @examples
#' \dontrun{
#' calculate_N2O_indirect_leaching(saveoutput = FALSE, data_dir = "user_data")
#' }
#' @export
calculate_N2O_indirect_leaching <- function(automatic_cycle = FALSE, saveoutput = TRUE, data_dir = "user_data",
                                            direct_N2O_df = NULL, pop_df = NULL) {
  herdr_calculate_N2O_indirect(
    mode = "leaching",
    automatic_cycle = automatic_cycle,
    saveoutput = saveoutput,
    data_dir = data_dir,
    direct_N2O_df = direct_N2O_df,
    pop_df = pop_df
  )
}
