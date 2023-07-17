coveragerates_check_range <- function(inputFile){
  ### Read coverage sheet
  scenarios <- read_xlsx(inputFile, sheet="Scenarios")
  coverage_sheets <- unique(scenarios$sheet_Coverage)
 
  description <- "All coverage values must be between 0 and 1. Please check the following tasks for out-of-range values"
  severity <- "error"
  
  if (exists("violations")) remove(violations)
  for (coverage_sheet in  coverage_sheets){
    df_coverage <- read_xlsx(inputFile, sheet=coverage_sheet)
    violation <- df_coverage %>% 
      mutate_all(~replace(., is.na(.), 1)) %>%
      filter_at(vars(-c(Indicator, CommonName)), any_vars(. <0 | . >1)) %>%
      dplyr::mutate(Coverage_Sheet_Name = coverage_sheet) %>%
      select(c(Indicator, CommonName, Coverage_Sheet_Name))
    if (exists("violations")){
      violations <- rbind(violations, violation)
    }
    else{
      violations <- violation
    } 
  }
  return (list(description, severity, violations))
}

coveragerates_check_trend <- function(inputFile){
  ### Read coverage sheet
  scenarios <- read_xlsx(inputFile, sheet="Scenarios")
  coverage_sheets <- unique(scenarios$sheet_Coverage)
  description <- "Year over year Coverage trend is usually monotonically increasing or decreasing, please check the following tasks for trends"
  severity <- "warning"
  
  if (exists("violations")) remove(violations)
  for (coverage_sheet in  coverage_sheets){
    df_coverage <- read_xlsx(inputFile, sheet=coverage_sheet)
    df_coverage <- df_coverage %>% 
      mutate_all(~replace(., is.na(.), 1)) %>%
      pivot_longer(starts_with("Year"), names_to="Year", values_to = "Coverage", names_prefix = "Year ") %>%
      dplyr::mutate(Year = as.numeric(Year)) %>%
      dplyr::arrange(Indicator, Year) %>%
      as.data.frame()
    df_inc <- df_coverage %>% monotonic(id.col = "Indicator", y.col = "Coverage", direction="inc") %>% as.data.frame()
    df_dec <- df_coverage %>% monotonic(id.col = "Indicator", y.col = "Coverage", direction="dec") %>% as.data.frame()
    violation <- df_inc %>% 
      inner_join(df_dec, by =c("id")) %>% 
      filter(Montonic.x ==FALSE & Montonic.y == FALSE) %>% 
      dplyr::rename(Indicator = id) %>%
      dplyr::mutate(Coverage_Sheet_Name = coverage_sheet) %>%
      select (Indicator, Coverage_Sheet_Name)
    if (exists("violations")){
      violations <- rbind(violations, violation)
    }
    else{
      violations <- violation
    } 
  }
  return (list(description, severity, violations))
}