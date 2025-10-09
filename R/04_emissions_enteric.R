#' Calculate methane emissions from enteric fermentation
#'
#' Estimates methane emissions using IPCC Tier 2 methodology based on
#' digestible energy (DE), neutral detergent fiber (NDF), gross energy (GE),
#' and animal population. Category-specific methane conversion factors (Ym)
#' are applied depending on animal type and diet quality.
#'
#' @param animal character. Type of animal ("Cattle", "Sheep", "Goat").
#' @param type Optional character. Only for "Cattle" (e.g., "Dairy", "Beef").
#' @param zone Optional character vector. Only for "Cattle" when type is specified.
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default FALSE.
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item code: category code
#'   \item de: digestible energy (% of GE)
#'   \item ndf: neutral detergent fiber fraction
#'   \item ge: gross energy (MJ/day)
#'   \item ym: methane conversion factor (%)
#'   \item ef_kg_animal_year: emission factor (kg CH4/animal/year)
#'   \item n_population: animal population
#'   \item emissions_total: total emissions (kt CH4/year)
#' }
#'
#' @export
#' @examples
#' \donttest{
#' calculate_emissions_enteric(animal = "Cattle", type = "Dairy", zone = "A")
#' calculate_emissions_enteric(animal = "Sheep")
#' calculate_emissions_enteric(animal = "Goat")
#' }
calculate_emissions_enteric <- function(animal=NULL,type=NULL,zone=NULL,saveoutput=TRUE){
  categories <- load_dataset("categories")
  resultado_list <- list()

  animals_use <- if(is.null(animal)) unique(categories$animal_type) else animal

  for(an in animals_use){
    types_use <- if(is.null(type)) unique(categories$animal_subtype[categories$animal_type==an]) else type[type %in% unique(categories$animal_subtype[categories$animal_type==an])]
    for(tp in types_use){
      diet_vars <- calculate_weighted_variable(animal=an,type=tp,zone=zone,saveoutput=FALSE) %>%
        dplyr::select(code,animal_type,animal_subtype,de,ndf,zone)
      if(nrow(diet_vars)==0) next

      ge_df <- calculate_ge(animal=an,type=tp,zone=zone,saveoutput=FALSE) %>%
        dplyr::select(code,ge,animal_type,animal_subtype,zone)
      pop_df <- categories %>% dplyr::filter(animal_type==an,animal_subtype==tp) %>% dplyr::select(code,animal_type,animal_subtype,n_population)

      df <- diet_vars %>%
        dplyr::inner_join(ge_df,by=c("code","animal_type","animal_subtype","zone")) %>%
        dplyr::inner_join(pop_df,by=c("code","animal_type","animal_subtype"))

      df <- df %>%
        dplyr::mutate(
          ym = dplyr::case_when(
            an=="Sheep" ~ 6.7,
            an=="Goat" ~ 5.5,
            an=="Cattle" & code=="k23" & de>=70 & ndf<=35 ~ 5.7,
            an=="Cattle" & code=="k23" & de>=70 & ndf>35  ~ 6.0,
            an=="Cattle" & code=="k23" & de>=63 & de<70 & ndf>37 ~ 6.3,
            an=="Cattle" & code=="k23" & de<=62 & ndf>38 ~ 6.5,
            an=="Cattle" & code!="k23" & de>=75 ~ 3.0,
            an=="Cattle" & code!="k23" & de>=72 ~ 4.0,
            an=="Cattle" & code!="k23" & de>=62 & de<=71 ~ 6.3,
            an=="Cattle" & code!="k23" & de<62 ~ 7.0,
            TRUE ~ NA_real_
          ),
          ef_kg_animal_year = (ge*(ym/100)*365)/55.65,
          emissions_total = ef_kg_animal_year*(n_population/1e6)
        ) %>%
        dplyr::select(code,animal_type,animal_subtype,zone,de,ndf,ge,ym,
                      ef_kg_animal_year,n_population,emissions_total)

      resultado_list[[paste0(an,"_",tp)]] <- df
    }
  }

  final <- dplyr::bind_rows(resultado_list)
  if(saveoutput){
    dir.create("output",showWarnings=FALSE)
    write.csv(final,"output/enteric_emissions.csv",row.names=FALSE)
  }
  final
}
