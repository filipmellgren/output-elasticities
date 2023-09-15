library(dplyr)
library(tidylog)
library(collapse) # Pkg for efficient weighted group means, sums etc.
library(prodest) # Pkg for estimating production functions
source("src/load_data/load.R") # For loading data

# PPI deflator ####
BASEYEAR <- 1997
ppi <- read.csv("data/ppi.csv", header = FALSE)
ppi$year <- parse_number(ppi$V1)
ppi <- ppi[3:nrow(ppi),]
ppi <- ppi %>% group_by(year) %>% summarize(index = mean(as.numeric(V2))) %>% ungroup()
ppi$index <- ppi$index / pull(subset(ppi, year == BASEYEAR, index))

# FEK ####
# Load data
download_fek() # From SCB's server if unavailable.
fek_filepaths <-  paste0("data/fek/", list.files("data/fek", pattern = "^FE"))
fek <- load_data(fek_filepaths)
fek <- fix_sni(fek)

# Rename columns
fek <- fek %>%
  select(
    firm_id = LopNr_PeOrgNrHE,
    employees = MedelantalAnstallda,
    wage_cost = KostnaderForLonerOchAndraErsattningar,
    commodity_cost = KostnaderForRavaror,
    input_goods = KostnaderForHandelsvaror,
    capital = SummaMateriellaAnlaggningstillgangar,
    intangibles = SummaImmateriellaAnlaggningstillgangar,
    value_added = Foradlingsvarde,
    prod_value = Produktionsvarde,
    year,
    sni_regime, sni, sni2, sni3, sni4
    )

# Keep sane observations
fek <- fek[employees >= 10]
fek <- fek[capital > 0]
fek$wage_cost <- -fek$wage_cost
fek <- fek[wage_cost > 0]
fek <- fek[value_added > 0]
fek <- fek[prod_value > 0]
fek$input_goods <- -fek$input_goods
fek <- fek[input_goods > 0]

# Define new variables
fek <- merge(fek, ppi)
fek$wage_cost <- fek$wage_cost / fek$index
fek$value_added <- fek$value_added / fek$index
fek$prod_value <- fek$prod_value / fek$index
fek$labor_share <- fek$wage_cost / fek$value_added

# IVP ####
# Load data
ivp_files <- list.files("data/ivp", pattern = "^IVP")
ivp <- load_data(paste0("data/ivp/", ivp_files))
ivp[["good_id"]] <-  ivp  |>
    group_by(VaruNr, KvantText)  |>
    group_indices()

# Rename variables
ivp <- ivp |>
    select(firm_id = LopNr_PeOrgNrHE,
    year = Ar,
    good_id,
    value = ProdVarde,
    q = Totkvant)

# Keep sane values
ivp <- ivp |> filter(value > 0 & !is.na(value) & q > 0 & !is.na(q))

# Define new variables
ivp <- merge(ivp, ppi)
ivp$value <- ivp$value / ivp$index
ivp$p <- ivp$value / ivp$q

# Standardize price
good_mean_price <- ivp |>
  fgroup_by(good_id, year) |>
  fselect(p) |>
  fmean(w = ivp$value)
names(good_mean_price) <- c("good_id", "year", "good_mean_price")

ivp <- merge(ivp, good_mean_price, by = c("good_id", "year"))
ivp$p <- ivp$p / ivp$good_mean_price

ivp <- ivp |>
  fgroup_by(firm_id, year) |>
  fselect(p) |>
  fmean(w = ivp$value)

# Trim observations
QTILE_TRIM <- 0.02
low_p <- quantile(ivp$p, QTILE_TRIM)
high_p <- quantile(ivp$p, 1 - QTILE_TRIM)

ivp <- subset(ivp, ivp$p > low_p & ivp$p < high_p)

# Combine ####
df <- merge(ivp, fek, by = c("firm_id", "year"))
# Note, quantity defined using FEK production value, not IVP prod value from where price came.
df$q <- df$prod_value / df$p

log_dev_from_mean <- function(var) {
  return(log(var) - mean(log(var)))
}

df$q <- log_dev_from_mean(df$q)
df$val <- log_dev_from_mean(df$prod_value)
df$p <- log_dev_from_mean(df$p)
df$v <- log_dev_from_mean(df$employees)
df$k <- log_dev_from_mean(df$capital) 
df$m <- log_dev_from_mean(df$input_goods)

# Estimate output elasticity for manufacturing ####

prodest_sector <- function(sector, yvar, df) {
  tmp <- data.table(df)[group_id == sector]
  fit <- prodestWRDG_GMM(Y = tmp[[yvar]],
                         fX = tmp$v,
                         sX = tmp$k,
                         pX = tmp$m,
                         idvar = tmp$firm_id,
                         timevar = tmp$year
  )
  
  return(cbind("group_id" = sector, t(data.frame(fit@Estimates$pars))))
}

df[["group_id"]] <-  df  |>
  group_by(sni2, sni_regime)  |>
  group_indices()

sectors <- df %>% group_by(group_id) %>%
  summarise(count = n()) %>%
  ungroup() %>% filter(count > 50) %>%
  pull(group_id)

tmp <- lapply(sectors, prodest_sector, "q", df)
prod_fun.q <- data.frame(do.call(rbind, tmp))

tmp <- lapply(sectors, prodest_sector, "val", df)
prod_fun.val <- data.frame(do.call(rbind, tmp))

cost_share_o_elas <- df |> group_by(group_id) |>
  summarise(cost_share_o_elas = quantile(labor_share , 0.5)) |>
  ungroup()

cor(prod_fun.val$fX1, prod_fun.q$fX1)

tmp <- merge(cost_share_o_elas, prod_fun.q)

cor(tmp$fX1, tmp$cost_share_o_elas)

# For FEK data only, accepting the slight p bias. ####
df <- fek
df[["group_id"]] <-  df  |>
  group_by(sni4, sni_regime)  |>
  group_indices()
df$val <- log_dev_from_mean(df$prod_value)
df$v <- log_dev_from_mean(df$employees)
df$k <- log_dev_from_mean(df$capital) 
df$m <- log_dev_from_mean(df$input_goods)

sectors <- df %>% group_by(group_id) %>%
  summarise(count = n()) %>%
  ungroup() %>% filter(count > 1000) %>%
  pull(group_id)

tmp1 <- lapply(sectors, prodest_sector, "val", df)
prod_fun <- do.call(rbind, tmp1)

# Next, estimate markups using a cost share approach.
# This assumes the median firm has a markup of 1.
cost_share_o_elas <- df |> group_by(group_id) |>
    summarise(cost_share_o_elas = quantile(labor_share , 0.5)) |>
    ungroup()

o_elas <- merge(cost_share_o_elas, prod_fun)

cor(o_elas$fX1, o_elas$cost_share_o_elas)



