# Translog case, pq data ####
MIN_OBS <- 100

fek[["group_id"]] <-  fek  |>
  group_by(sni2, sni_regime)  |>
  group_indices()

fek <- fek %>% group_by(group_id, year) %>%
  mutate(val = log(value_added) - mean(log(value_added)),
         v = log(employees) - mean(log(employees)),
         k = log(caplag) - mean(log(caplag)),
         m = log(input_goods) - mean(log(input_goods))) %>%
  ungroup()

sectors <- fek %>% group_by(group_id) %>%
  summarise(count = n()) %>%
  ungroup() %>% filter(count >= MIN_OBS) %>%
  pull(group_id)

## Estimation, this takes some time
fek_sub <- data.frame(subset(fek, select = c(firm_id, year, sni2, sni_regime)))
library(furrr)
library(tictoc)
plan(multisession, workers = 10)
tic()
translog.val <- future_map(sectors, prodest_sector, .options = furrr_options(seed = TRUE), "val", fek, degree = TL, num_bootstrap = 50)
toc()
#translog.val <- lapply(sectors, prodest_sector, "val", fek, degree = TL, num_bootstrap = 1)
tl_export.val <- extract_results(translog.val, groupid.key, fek_sub)

write.csv(tl_export.val, "output/fek_tl_prod.csv")