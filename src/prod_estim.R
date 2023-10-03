
# Estimate output elasticity for manufacturing ####
df_sub <- subset(df, select = c(firm_id, year, sni2, sni_regime))

MIN_OBS <- 100

extract_results <- function(out, groupid.key, df_sub){
  estims <- lapply(out, `[[`, 1)
  estims <- data.frame(do.call(rbind, estims))
  estims <- merge(estims, groupid.key)
  estims <- subset(estims, select = -c(group_id, R))
  
  estims <- estims %>%
    pivot_wider(id_cols = c(sni2, sni_regime), names_from = param,
                values_from = c(original, bootBias, bootSE, bootMed))
  
  export <- subset(merge(df_sub, estims), select = -c(sni2, sni_regime))
  
  productivity <- lapply(out, `[[`, 2)
  productivity <- data.frame(do.call(rbind, productivity))
  
  export <- merge(export, productivity, by.x = c("firm_id", "year"), by.y = c("idvar", "timevar"))
  export <- export %>% rename(prod = V1)
  return(export)
}

# Prepare sectors ####

groupid.key <- df %>%
  distinct(group_id, sni2, sni_regime)

sectors <- df %>% group_by(group_id) %>%
  summarise(count = n()) %>%
  ungroup() %>% filter(count >= MIN_OBS) %>%
  pull(group_id)

# Cobb Douglas case, q data ####
CD <- 1
cobb_douglas <- lapply(sectors, prodest_sector, "val", df, degree = CD, num_bootstrap = 100)

cd_export <- extract_results(cobb_douglas, groupid.key, df_sub)

write.csv(cd_export, "output/cd_prod.csv")

# Translog case, q data ####
TL <- 2
translog <- lapply(sectors, prodest_sector, "val", df, degree = TL, num_bootstrap = 100)
tl_export <- extract_results(translog, groupid.key, df_sub)

write.csv(tl_export, "output/tl_prod.csv")
