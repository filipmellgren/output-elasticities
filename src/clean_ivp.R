# Clean the manufacrturing data

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

ivp$p <- ivp$value / ivp$q

# Standardize price
good_mean_price <- ivp |>
  fgroup_by(good_id, year) |>
  fselect(p) |>
  fmean(w = ivp$revenue)
names(good_mean_price) <- c("good_id", "year", "good_mean_price")

ivp <- merge(ivp, good_mean_price, by = c("good_id", "year"))
ivp$p <- ivp$p / ivp$good_mean_price

ivp <- ivp |>
  fgroup_by(firm_id, year) |>
  fselect(p) |>
  fmean(w = ivp$value)

write.csv(ivp, "output/firm_level_prices.csv")

# Combine ####
df <- merge(data.frame(ivp), data.frame(fek))

# Note, quantity defined using FEK production value, not IVP prod value from where price came.
df$q <- df$prod_value / df$p

df[["group_id"]] <-  df  |>
  group_by(sni2, sni_regime)  |>
  group_indices()

groupid.key <- df %>% as_tibble() %>% distinct(group_id, sni2, sni_regime)

df <- df %>% group_by(group_id, year) %>%
  mutate(val = log(prod_value) - mean(log(prod_value)),
         v = log(employees) - mean(log(employees)),
         k = log(caplag) - mean(log(caplag)),
         m = log(input_goods) - mean(log(input_goods))) %>%
  ungroup()

df <- df %>% group_by(group_id) %>%
  mutate(val_min = quantile(val, 0.01),
         val_max = quantile(val, 0.99),
         k_min = quantile(k, 0.01),
         k_max = quantile(k, 0.99),
         m_min = quantile(m, 0.01),
         m_max = quantile(m, 0.99)) %>%
  ungroup() %>%
  filter(val > val_min, val < val_max, k > k_min, k < k_max, m > m_min, m < m_max)

write.csv(df, "output/clean_qdata.csv")