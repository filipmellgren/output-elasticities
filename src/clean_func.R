
fek_select_variables <- function(fek){
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
  return(fek)
}

fek_filter_obs <- function(fek){
  # Keep sane observations
  fek <- fek[employees >= 10]
  fek <- fek[capital > 1]
  fek$wage_cost <- -fek$wage_cost
  fek <- fek[wage_cost > 1]
  fek <- fek[value_added > 1]
  fek <- fek[prod_value > 1]
  fek$input_goods <- -fek$input_goods
  fek <- fek[input_goods > 1]
  return(fek)
}



log_dev_from_mean <- function(var) {
  return(log(var) - mean(log(var)))
}


fek_wrangle <- function(fek){
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
  fek[, caplag := l(capital)] # TODO this won't work inside a function for some reason.
  
  fek$labor_share <- fek$wage_cost / fek$value_added
  
  fek <- na.omit(fek)
  return(fek)
}
