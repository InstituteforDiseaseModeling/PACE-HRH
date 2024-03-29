.cadre_check_scenarios <- function(df, inputFile, custom_dir){
  
  ### Read Cadre headers in long format
  cadre_headers <- read_xlsx(inputFile, sheet=unique(df$sheet_Cadre), n_max=2, col_names = FALSE)
  cadre_headers <- cadre_headers[,grepl("StartYear", cadre_headers[1, ])] %>% 
    mutate_all(~str_replace(.,"StartYear", ""))
  cadre_headers <- as.data.frame(t(cadre_headers))
  colnames(cadre_headers) <- c("StartYear", "RoleID")
  cadre_headers <- cadre_headers %>% dplyr::mutate(StartYear = as.numeric(StartYear))
  cadre_headers <- cadre_headers[cadre_headers$RoleID!="Total",]
  
  ### Plot cadre headers
  cadreoplot <- ggplot(cadre_headers, aes(x = StartYear, y = RoleID, color= RoleID)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = glue('Headers in sheet: {unique(df$sheet_Cadre)}'),
         x ="StartYear",
         y = "RoleID",
         color ="RoleID")
  
  ### Check2: (EndYear+1) must appear in StartYear 
  StartYears <- df %>% select(StartYear) %>% unique()
  violation2 <- df %>% dplyr::mutate(EndYear_plus = EndYear+1) %>%
    select(-StartYear) %>%
    filter(!is.na(EndYear)) %>%
    anti_join(StartYears, by=c("EndYear_plus" = "StartYear")) %>%
    select(-EndYear_plus)
  filename2 <- file.path(custom_dir, glue("violation_endyear_not_in_startyear_{unique(df$sheet_Cadre)}.csv"))
  description2 <- glue("For{unique(df$sheet_Cadre)}, (EndYear+1) must appear in StartYear Column.")
  if (nrow(violation2) >0 ){
    # Stop the check here as this is fatal
    .custom_check_write_result(description2, filename2, "error", violation2)
    log_print(glue::glue("Fatal error found in CadreRoles sheet: {description2}, unable to check further. Please fix it and rerun the validation."))
    return (cadreoplot)
  }
  
  ### Check3: "ScenarioID" + "RoleID" + "StartYear" must appear in the corresponding cadre_ sheets' headers
  bucket_rank <- as.data.frame(sort(unique(cadre_headers$StartYear), index.return=TRUE), col.names = c("StartYear", "Rank"))
  cadre_rank <- cadre_headers %>% 
    inner_join(bucket_rank) 
  violation3 <- df %>% anti_join(cadre_rank)
  filename3 <- file.path(custom_dir, glue("violation_startyear_not_in_header_{unique(df$sheet_Cadre)}.csv"))
  description3 <- glue("Sheet: {unique(df$sheet_Cadre)} is missing columns corresponding to the following pair: RoleID + StartYear")
  .custom_check_write_result(description3, filename3, "error", violation3)
  
  ### Check4: For each "ScenarioID" with no "EndYear" it must appear in the max(StartYear) section in the corresponding cadre_ sheets
  max_startYear <- max(df$StartYear)
  max_year_roles <- df %>% filter(is.na(EndYear)) %>%
    select(-StartYear) %>% 
    inner_join(cadre_rank, multiple = "all") %>%
    filter(StartYear == max_startYear) %>%
    select(RoleID) 
  violation4 <- df  %>% filter(is.na(EndYear)) %>% anti_join(max_year_roles)
  filename4 <- file.path(custom_dir, glue("violation_startyear_max_missing_{unique(df$sheet_Cadre)}.csv"))
  description4 <- glue("Sheet: {unique(df$sheet_Cadre)} is missing columns corresponding to the following RoleID + {max_startYear}")
  .custom_check_write_result(description4, filename4, "error", violation4)
  
  
  ### Check5: For each "RoleID", it should appear in continuous sections on the cadre_ sheets' headers in between StartYear and EndYear.
  expected <- df %>% 
    dplyr::mutate(no_EndYear = dplyr::if_else(is.na(EndYear), T , F)) %>%
    dplyr::mutate(EndYear = dplyr::if_else(is.na(EndYear), max(StartYear)-1 , EndYear)) %>%
    dplyr::mutate(lastBucketYear = EndYear + 1) %>%
    select(RoleID, StartYear, lastBucketYear, no_EndYear)
  expected <- bind_cols(expected, 
                        expected %>%  #look up for rank for Start/End
                          mutate_all(~bucket_rank$Rank[match(., bucket_rank$StartYear)]) %>% 
                          select(-RoleID, -no_EndYear) %>% 
                          dplyr::rename(c('StartRank' = 'StartYear', "EndRank" ='lastBucketYear'))) %>%
    drop_na(StartRank, EndRank) %>%
    group_by(RoleID) %>% 
    dplyr::mutate(EndRank = ifelse(no_EndYear==T, EndRank, EndRank-1)) %>%
    complete(StartRank = StartRank:EndRank) %>%
    dplyr::rename(c("Rank"='StartRank')) %>%
    select(RoleID, Rank)
  
  ### expected to see the role appears in 1 to LastRank
  violation5 <- expected %>% anti_join(
    df %>%
      select(-StartYear) %>%
      inner_join(cadre_rank, multiple="all")
  ) %>% inner_join(df) %>%
    select(-StartYear)  %>% 
    inner_join(bucket_rank, by=c("Rank"="Rank")) %>%
    dplyr::rename(missing_bucket= StartYear) %>%
    select(-Rank)
  filename5 <- file.path(custom_dir, glue("roles_missing_years_{unique(df$sheet_Cadre)}.csv"))
  description5 <- glue("sheet {unique(df$sheet_Cadre)} is missing columns corresponding the following RoleID + missing_bucket")
  .custom_check_write_result(description5, filename5, "error", violation5)
  
  ### Check6: On each scenario-specific cadre_sheet, the pair "StartYear" + "RoleID" must appear in CadreRoles for the corresponding ScenarioID
  violation6 <- cadre_headers %>% anti_join(df, by=c("RoleID" = "RoleID")) %>% arrange(StartYear)
  description6 <- glue('On Sheet {unique(df$sheet_Cadre)}, the pair "StartYear" + "RoleID" must appear in CadreRoles for the corresponding ScenarioID')
  filename6 <- file.path(custom_dir, glue("no_definition_in_cadreroles_{unique(df$sheet_Cadre)}.csv"))
  .custom_check_write_result(description6, filename6, "error", violation6)
  
  if (all(sapply(2:6,FUN = \(x) {nrow(get(paste0("violation", x))) == 0}) == TRUE)){
    # if no violation objects are present, it means sheet is good
    log_print(glue::glue("Sheet: {unique(df$sheet_Cadre)} passed the CadreRoles checks!"))
  }
  return (cadreoplot)
}


cadre_check_complete <- function(inputFile, custom_dir){
  ##### More complex Built-in checks 
  ##### Check if cadre sheets are valid
  defaultEndYear = pacehrh:::GPE$endYear
  defaultStartYear = pacehrh:::GPE$startYear
  scenarios <- read_xlsx(inputFile, sheet="Scenarios")
  cadreRoles <- read_xlsx(inputFile, sheet="CadreRoles")
  cadreRoles <- cadreRoles %>% 
    inner_join(scenarios, by=c('ScenarioID'='UniqueID')) %>%
    select(ScenarioID, RoleID, StartYear, EndYear, sheet_Cadre) %>%
    dplyr::mutate(StartYear = as.numeric(StartYear), EndYear = as.numeric(EndYear))
  
  ### Check 1: columns StartYear and EndYear can not be outside of the model default range
  violation1 <- cadreRoles %>% filter(StartYear < defaultStartYear | EndYear > defaultEndYear)
  filename1 <- file.path(custom_dir, glue("violation_cadreroles_def.csv"))
  description1 <- glue("StartYear and EndYear can not be outside of the model default range: {defaultStartYear} to {defaultEndYear}.")
  if (nrow(violation1) >0 ){
    # Stop the check here as this is fatal
    .custom_check_write_result(description1, filename1, "error", violation1)
    log_print(glue::glue("Fatal error found in CadreRoles sheet: {description1}, unable to check further. Please fix it and rerun the validation."))
  }else{
    # Split by scenario as the cader_sheet is different
    # Check rule 2 to rule 6
    cadre_list <- split(cadreRoles, cadreRoles$sheet_Cadre)
    plot_list <-lapply(cadre_list, .cadre_check_scenarios, inputFile=inputFile, custom_dir=custom_dir)
    combined_cadre_plots <- do.call(grid.arrange, c(plot_list, nrow = length(plot_list)))
    ggsave(file.path(custom_dir, "custom_cadres.png"), combined_cadre_plots, width = 160, height = 60*length(plot_list), units="mm")
  }
}
