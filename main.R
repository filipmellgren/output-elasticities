# Order of operations for obtaining production function at the firm level.
library(dplyr)
library(tidylog)
library(collapse) # Pkg for efficient weighted group means, sums etc.
source("src/load_data/load.R") # For loading data
#source("src/clean_func.R")
library(fixest)
source("src/prod_func.R")
#TODO UHV data for more prices

IVP_PATH <- "output/clean_qdata.csv"
FEK_PATH <- "output/fek.csv"

# Get FEK DATA, do minor cleaning
if (!file.exists(FEK_PATH)){
  source("src/clean_fek.R")
}

fek <- fread(FEK_PATH)

# Get IVP DATA
# Clean the data, obtain a price variable at the firm level.
if (!file.exists(IVP_PATH)){
  source("src/clean_ivp.R")
}

df <- fread(IVP_PATH)

# Estimate TFPQ using the ACF procedure by 2 digit SNI sector.


if (!file.exists("output/tl_prod.csv")){
  source("src/prod_estim.R")
}

# In addition, estimate TFPR for FEK-data.
if (!file.exists("output/fek_tl_prod.csv")){
  source("src/prod_estim_tfpr.R")
}
