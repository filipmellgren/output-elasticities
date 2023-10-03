# Cleans the FEK data
# Load data
#download_fek() # From SCB's server if unavailable.
fek_filepaths <-  paste0("data/fek/", list.files("data/fek", pattern = "^FE"))
fek <- load_data(fek_filepaths)

fek <- fix_sni(fek)

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
    gross_inv = SummaBruttoinvesteringar,
    sni_regime, sni, sni2, sni3, sni4
  )

fek <- fixest::panel(fek, panel.id = c("firm_id", "year"))

fek <- fek_filter_obs(fek)

# Capital is measured at end of year, want amount going into year.
fek[, caplag := l(capital)]

fek$labor_share <- fek$wage_cost / fek$value_added

fek <- na.omit(fek)

fwrite(fek, "output/fek.csv")