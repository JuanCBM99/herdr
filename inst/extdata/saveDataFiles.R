# Converting raw data into package data
library(usethis)
library(magrittr)

data_folder <- "C:/Users/juancarlos.baez/Desktop/CSV_R/Ruminants_csv"

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
ch4_mm <- read.csv(paste0(data_folder,"/","CH4_M_M.csv"))
mcf <- read.csv(paste0(data_folder,"/","mcf.csv"))
use_data(diet, ingredients, characteristics, coefficients, categories, weights, ch4_mm, mcf, overwrite = TRUE)
