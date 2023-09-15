library(RODBC)
library(tidyverse)
library(data.table)
library(furrr)

gen_datanames <- function(prefix, year){
  out <- paste0(prefix, year)
}

download_fek <- function(filter_con = "TRUE"){
  fek_files <-  list.files("data/fek", pattern = "^FE")
  
  if (!identical(character(0), fek_files)) {
    return()
  }
    
  conn <- odbcConnect("P0253")
    
  datanames <- gen_datanames("FE", 1997:2016)
  datanames <- c(datanames[datanames != "FE2011"], "FE2011_rev")
    
  folder <- "fek"
    
  lapply(datanames, download_data, 
         folder = folder,
         conn = conn, 
        filter_con = filter_con)
  close(conn)
}

download_data <- function(read_from, folder, conn, filter_con){
  #' Download data from SCBs MONA database.
  #' read_from is a filename, like "FE2016"
  #' folder is a folder where the data should be stored. for FE files, use "fek"
  #' conn is a connectino to the SCB database. 
  #'  Use conn <- odbcConnect("P0253")
  #' filter_con is a character with a data filtration. 
  #' If you don't wich to filter the data, just pass "TRUE" and eevrything is kept. 
  #read_from <- datanames[1] Eg read_from <- "FE2016"
  file_path <- paste0("data/", folder, "/", read_from, ".csv")
  
  if (!file.exists(file_path)) {
    
    d <- sqlFetch(conn, read_from)
    
    d <- d %>% filter(eval(rlang::parse_expr(filter_con)))
    
    d <- d %>% mutate(year = parse_number(read_from))
    
    fwrite(d, file_path)
  }
  
  return
}

# Data preparation

load_data <- function(paths){
  #' Load data stored in paths (a list of relative paths)
  workers <- min(length(paths), parallel::detectCores())
  
  plan(multisession, workers = workers)
  d_list <- future_map(paths, fread)
  #d_list <- lapply(paths, fread)
  data = rbindlist(d_list, fill = TRUE)
  
  return(data)
}

fix_sni <- function(df){
  #' Some SNI codes start at 0 and should contain 5 digits.
  #' To avoid the software from converting to 4 digit integer, divide all sni
  #' by 10 and work with the as numerical values. Faster and more reliable than 
  #' treating them as strings.
  
  df <- df %>%
    mutate(Ng2007 = case_when(
     !is.na(Ng2007) ~ Ng2007,
      is.na(Ng2007) & !is.na(NG) & year == 2015 ~ NG,
      TRUE ~ Ng2007)
     )
  sni92 <- "Ng"
  sni2002 <- "Ng"
  sni2007 <- "Ng2007"
  break_02 <- 2002
  break_07 <- 2008
  
  df <- df %>% create_snigroups(break_02, break_07, sni92, sni2002, sni2007)
  
  SNIMAX_LEVEL <- 5
  
  df <- df %>% mutate(sni2 = floor(sni/10^(SNIMAX_LEVEL-2)),
                    sni3 = floor(sni/10^(SNIMAX_LEVEL-3)),
                    sni4 = floor(sni/10^(SNIMAX_LEVEL-4)))
  return(df)
}


create_snigroups <- function(d, break_02, break_07, sni92, sni2002, sni2007){
  #' Creates the sni_regime and sni variables. Together, these two form the
  #' aggregation level that we'll look at later. Sniregime is like a time dimension. 
  #' @param break_02: first year with sni2002
  #' @param break_07: first year with sni2007
  d <- d %>% mutate(
    sni_regime = case_when(
      year < break_02 ~ 0,
      year < break_07 ~ 1, 
      year >= break_07 ~ 2),
    sni = case_when(
      sni_regime == 0 ~ .data[[sni92]],
      sni_regime == 1 ~ .data[[sni2002]],
      TRUE ~ .data[[sni2007]]
    ))
  return(d)
}