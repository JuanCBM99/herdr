#' calculate gross energy (ge)
#'
#' computes gross energy requirements by summing net energy components for maintenance,
#' activity, growth, work, pregnancy, lactation, and wool production, then adjusting by
#' digestible energy (de). coefficients for maintenance (rem) and growth (reg) follow nrc methodology.
#'
#' @param animal character. type of animal ("cattle", "sheep", "goat", etc.).
#' @export
#' @examples
#' \donttest{
#'   library(efch4)
#'   calculate_ge(animal = "cattle")
#' }
calculate_ge <- function(animal) {

  NEm <- calculate_NEm(animal) %>% dplyr::select(code, NEm)
  NEa <- calculate_NEa(animal) %>% dplyr::select(code, NEa)
  NEg <- calculate_NEg(animal) %>% dplyr::select(code, NEg)
  NE_work <- calculate_NE_work(animal) %>% dplyr::select(code, NE_work)
  NE_pregnancy <- calculate_NE_pregnancy(animal) %>% dplyr::select(code, NE_pregnancy)
  NEl <- calculate_NEl(animal) %>% dplyr::select(code, NEl)
  NE_wool <- calculate_NE_wool(animal) %>% dplyr::select(code, NE_wool)

  de_df <- calculate_weighted_variable(animal = animal) %>%
    dplyr::select(code, de)

  final <- NEm %>%
    dplyr::full_join(NEa, by = "code") %>%
    dplyr::full_join(NEg, by = "code") %>%
    dplyr::full_join(NE_work, by = "code") %>%
    dplyr::full_join(NE_pregnancy, by = "code") %>%
    dplyr::full_join(NEl, by = "code") %>%
    dplyr::full_join(NE_wool, by = "code")  %>%
    dplyr::full_join(de_df, by = "code") %>%
    dplyr::mutate(dplyr::across(
      c(NEm, NEa, NEg, NE_work, NE_pregnancy, NEl, NE_wool),
      ~ tidyr::replace_na(., 0)
    )) %>%
    dplyr::mutate(
      de_percent = de/100,
      reg = 1.164 - (5.16e-3 * de) + (1.308e-5 * de^2) - (37.4 / de),
      rem = 1.123 - (4.092e-3 * de) + (1.126e-5 * de^2) - (25.4 / de),
      ge = ((NEm + NEa + NEl + NE_work + NE_pregnancy) / rem +
              ((NEg + NE_wool) / reg)) / de_percent
    ) %>%
    dplyr::select(code, NEm, NEa, NEg, NE_work, NE_pregnancy,
                  NEl, NE_wool, de, rem, reg, ge)

  return(final)
}




