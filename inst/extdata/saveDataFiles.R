# Converting raw data into package data
library(usethis)
library(magrittr)

data_folder <- "C:/Users/juancarlos.baez/Desktop/CSV_R/Cattle_simplified_csv"

#=========================================================
# Constants
#=========================================================

kg_to_g <- 1000
use_data(kg_to_g, overwrite = TRUE)

#=========================================================
# Datasets
#=========================================================
diet <- read.csv(paste0(data_folder,"/","diet_c_s.csv"))
ingredients <- read.csv(paste0(data_folder,"/","ingredients_c_s.csv"))
characteristics <- read.csv(paste0(data_folder,"/","characteristics_c_s.csv"))
weights <- read.csv(paste0(data_folder,"/","weights_c_s.csv"))
coefficients <- read.csv(paste0(data_folder,"/","coefficients_c_s.csv"))
categories <- read.csv(paste0(data_folder,"/","categories_c_s.csv"))
ch4_mm <- read.csv(paste0(data_folder,"/","ch4_mm_c_s.csv"))
mcf <- read.csv(paste0(data_folder,"/","mcf_c_s.csv"))
emission_factors_direct <- read.csv(paste0(data_folder,"/","emission_factors_direct_c_s.csv"))
n2o_direct <- read.csv(paste0(data_folder,"/","n2o_direct_c_s.csv"))
n2o_indirect <- read.csv(paste0(data_folder,"/","n2o_indirect_c_s.csv"))
fractions <- read.csv(paste0(data_folder,"/","fractions_c_s.csv"))
emission_factors_volatilization <- read.csv(paste0(data_folder,"/","emission_factors_volatilization_c_s.csv"))
crops <- read.csv(paste0(data_folder,"/","crops_c_s.csv"))
rate_parameters <- read.csv(paste0(data_folder,"/","rate_parameters_c_s.csv"))
#Load data
use_data(diet, ingredients, characteristics, coefficients, categories, weights,
         ch4_mm, mcf,emission_factors_direct, n2o_direct, n2o_indirect, emission_factors_volatilization, fractions, crops, rate_parameters, overwrite = TRUE)
