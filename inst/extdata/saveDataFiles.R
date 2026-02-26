library(usethis)
library(magrittr)

data_folder <- "C:/Users/juancarlos.baez/Desktop/CSV_R/Definitive_csv"

#=========================================================
# Constants
#=========================================================

kg_to_g <- 1000
use_data(kg_to_g, overwrite = TRUE)

#=========================================================
# Datasets
#=========================================================
diet <- read.csv(paste0(data_folder,"/","diet.csv"))
ingredients <- read.csv(paste0(data_folder,"/","ingredients.csv"))
characteristics <- read.csv(paste0(data_folder,"/","characteristics.csv"))
weights <- read.csv(paste0(data_folder,"/","weights.csv"))
coefficients <- read.csv(paste0(data_folder,"/","coefficients.csv"))
categories <- read.csv(paste0(data_folder,"/","categories.csv"))
ch4_mm <- read.csv(paste0(data_folder,"/","ch4_mm.csv"))
mcf <- read.csv(paste0(data_folder,"/","mcf.csv"))
emission_factors_direct <- read.csv(paste0(data_folder,"/","emission_factors_direct.csv"))
n2o_direct <- read.csv(paste0(data_folder,"/","n2o_direct.csv"))
n2o_indirect <- read.csv(paste0(data_folder,"/","n2o_indirect.csv"))
fractions <- read.csv(paste0(data_folder,"/","fractions.csv"))
emission_factors_indirect <- read.csv(paste0(data_folder,"/","emission_factors_indirect.csv"))
crops <- read.csv(paste0(data_folder,"/","crops.csv"))
rate_parameters <- read.csv(paste0(data_folder,"/","rate_parameters.csv"))
census <-  read.csv(paste0(data_folder,"/","census.csv"))
#Load data
use_data(diet, ingredients, characteristics, coefficients, categories, weights,
         ch4_mm, mcf,emission_factors_direct, n2o_direct, n2o_indirect, emission_factors_indirect, fractions, crops, rate_parameters, census, overwrite = TRUE)
