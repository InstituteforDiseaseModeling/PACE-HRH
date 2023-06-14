check_offsets_exceed_range <- function(inputFile){
  ##### Check seasonality offset validity #####
  SO <- read_xlsx(inputFile ,sheet="SeasonalityOffsets")
  description <- "Seasonality offsets should represent an adjustment period within 1 year (12 months) of the originating event,
  so values less than -11 or greater than 11 are not accepted."
  severity <- "error"
  violation <- SO %>% filter_at(vars(starts_with('Offset')), any_vars(.< -11 | .>11))
  return (list(description, severity, violation))
}


check_offsets_not_integer <- function(inputFile){
  ##### Check seasonality offset is integer #####
  SO <- read_xlsx(inputFile ,sheet="SeasonalityOffsets")
  description <- "The simulation operates on a monthly basis, so all offsets must be integers."
  severity <- "error"
  violation <- SO %>% filter_at(vars(starts_with('Offset')), any_vars(!round(.)==.))
  return (list(description, severity, violation))
}


check_offsets_not_in_taskvalue_sheet <- function(inputFile){
  ##### Check seasonality offset is in taskValue sheet" #####
  SO <- read_xlsx(inputFile ,sheet="SeasonalityOffsets")
  scenarios <- read_xlsx(inputFile, sheet="Scenarios")
  taskvalue_sheets <- unique(scenarios$sheet_TaskValues)
  description <- "Task has a seasonality offset, but it is not listed in the taskvalue sheet. 
    Thus, this task will not have time allocated and thus the seasonality offset will not have any impact
    on the model’s results. Please verify that this is expected behavior."
  severity <- "warning"
  if (exists("violations")) remove(violations)
  for (taskvalue_sheet in taskvalue_sheets){
    TV <- read_xlsx(inputFile ,sheet=taskvalue_sheet)
    violation <- SO %>%
      filter(!(Task %in%  unique(TV$Indicator))) %>%
      select(Task, Description, Curve) %>%
      mutate(Not_Exist_In_Sheet = taskvalue_sheet)
    if (exists("violations")){
      violations <- rbind(violations, violation)
    }
    else{
      violations <- violation
    }
  }
  return (list(description, severity, violations))
}

check_offsets_not_in_seasonality_sheet <- function(){
  ##### Check seasonality offset is in seasonalityCurve" #####
  SO <- read_xlsx(inputFile ,sheet="SeasonalityOffsets")
  scenarios <- read_xlsx(inputFile, sheet="Scenarios")
  seasonality_sheets <- unique(scenarios$sheet_SeasonalityCurves)
  if (exists("violations")) remove(violations)
  for (seasonality_sheet in seasonality_sheets){
    filename <- file.path(custom_dir, glue("violation_offsets_not_in_seasonality_sheet.csv"))
    description <- "Seasonality offsets link to the seasonality_sheet columns.
    This offset does not match any seasonality curve, and thus will not be used
    or impact the model’s results. Please verify that this is expected behavior."
    severity <- "warning"
    SC <- read_xlsx(inputFile ,sheet=seasonality_sheet)
    violation <- SO %>%
      filter(!(Curve %in% colnames(SC))) %>%
      select(Task, Description, Curve) %>%
      mutate(Seasonality_Sheet_Name = seasonality_sheet)
    if (exists("violations")){
      violations <- rbind(violations, violation)
    }
    else{
      violations <- violation
    }
  }
  return (list(description, severity, violations))
}

