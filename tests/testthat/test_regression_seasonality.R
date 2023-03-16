testthat::local_edition(3)
packages = c("dplyr", "ggplot2", "stringr", "vdiffr", "testthat", "patrick")
for(i in packages){
  if(!require(i, character.only = T)){
    install.packages(i)
    library(i, character.only = T)
  }
}


library(pacehrh)
setwd("../..")

compare_plot <- function(expected_seasonal, decomposed_seasonal, scenario, task, type, use_offset){
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  ymax <- max(max(decomposed_seasonal), max(expected_seasonal))
  ymin <- min(min(decomposed_seasonal), min(expected_seasonal))
  plot(x = seq_along(1:length(expected_seasonal)),
       y=expected_seasonal,
       type = "b", col='red',
       xlab = "Month",
       ylab = "Weight %", ylim =c(ymin,ymax),
       yaxt = "n")
  lines(x = seq_along(1:length(expected_seasonal))
        , y=decomposed_seasonal,
        type = 'b', col='blue',  lty = 2)
  legend("top",
         inset = c(-0.4,0),
         legend = c("expected", "actual"),
         lty = 1,
         col = c("red","blue"))
  axis(side = 2,
       ## Rotate the labels.
       las = 2,
       ## Adjust the label position.
       mgp = c(3.5, 1, 0))
  title(main = glue::glue("{scenario}_{task}_{type}_{use_offset}"))
}

calculate_decomposition <- function(DR_test, scenario, task, type){
  avg_service <-DR_test %>% 
    dplyr::group_by(Task_ID, Scenario_ID, Year, Month) %>%
    dplyr::summarize(mean_total_service = mean(Service_time)) %>%
    ungroup()
  
  target_service <- avg_service %>% 
    filter(Scenario_ID == scenario) %>%
    filter(Task_ID == task) %>%
    arrange(Year, Month) %>% 
    select(mean_total_service) %>%
    .$mean_total_service
  
  # Create time series
  service_timeseries <- ts(target_service, frequency=12, start=c(min(DR_test$Year),1))
  # decompose 
  components <- decompose(service_timeseries, type='additive')
  decomposed_seasonal <- colMeans(matrix(components$seasonal, ncol = 12, byrow = TRUE))
  
  # calculate expected seasonal
  base_expected_seasonal <- as.vector(pacehrh:::loadSeasonalityCurves()[type][[1]])
  
  # Determine if offset lags
  simple_so <- pacehrh:::loadSeasonalityOffsets() %>% filter(Task==task)
  lags <- vector()
  total <- 0
  for (i2 in seq(1:6)){
    offset <- glue::glue("Offset{as.character(i2)}")
    if(!is.na(as.integer(simple_so[1, offset]))){
      total <- total + 1
      if (length(lags) ==0){
        lags <- DescTools::VecRot(base_expected_seasonal, as.integer(simple_so[1, offset]))
      }
      else{
        lags <- lags + DescTools::VecRot(base_expected_seasonal, as.integer(simple_so[1, offset]))
      }
    }
  }
  expected_seasonal <- lags / total
  
  if (all(abs(base_expected_seasonal - expected_seasonal) < 1e-6)){
    use_offset <- "non_offset"
  }
  else{
    use_offset <- "offeset"
  }
  
  #normalizing decompose data to align with expected
  decomposed_seasonal <- decomposed_seasonal - min(decomposed_seasonal)
  decomposed_seasonal <- decomposed_seasonal*(max(expected_seasonal)-min(expected_seasonal))/max((decomposed_seasonal)-min(decomposed_seasonal))
  decomposed_seasonal <-  decomposed_seasonal + min(expected_seasonal)
  
  return (compare_plot(expected_seasonal, decomposed_seasonal, scenario, task, type, use_offset))
  
}

# Set up necessary configuration for demo
test_template <- function(input_file, rounding="", setting="annual", popSheet="CountryPop", start = 2020, end = 040){
  dir.create("tests/results", showWarnings = FALSE)
  dir.create("tests/results/regression_demo", showWarnings = FALSE)
  pacehrh::SetInputExcelFile(inputExcelFilePath = input_file)
  Trace(TRUE)
  InitializePopulation(popSheet = popSheet)
  InitializeScenarios()
  InitializeStochasticParameters()
  InitializeSeasonality()
  SetGlobalStartEndYears(start = start, end = end)
  if (rounding!=""){pacehrh::SetRoundingLaw(rounding)}
  pacehrh::SetPerAgeStats(setting)
  withr::defer_parent(unlink("tests/results", recursive = TRUE,  force = TRUE))
}


with_parameters_test_that("demo model seasonality",{
  input_file <- "config/model_inputs_demo.xlsx"
  local({
    test_template(input_file)
    numtrials <- 2
    start = 2020
    end = 2040
    # scenario <- "BasicServices"
    # task <- "Malaria_test"
    # type <- "Malaria"
    results <- RunExperiments(scenarioName = scenario, trials = numtrials, debug = TRUE)
    expect_true(all(!is.na(results)))
    pacehrh::SaveSuiteResults(results, paste("tests/results/regression_demo/test_seasonality_", scenario,".csv",sep=""), scenario, 1)
    pattern = glue::glue("test_seasonality_{scenario}.*.csv")
    DR_test <- do.call(rbind,
                       lapply(paste("tests/results/regression_demo", list.files(path = "tests/results/regression_demo", pattern = pattern), sep="/"), read.csv))
   
    DR_test <- DR_test %>% 
      filter(Task_ID == task) %>%
      select(Task_ID, Scenario_ID, Year, Month, Service_time, Num_services, Trial_num, Run_num)
    
    DR_test <- DR_test %>% 
      filter(Year > start) %>%
      filter(Year < end)
    
    expect_doppelganger(glue::glue("{task}_seasonality"), calculate_decomposition(DR_test, scenario, task, type))
    
  })
},  
cases(
  malaria = list(scenario = "BasicServices", task = "Malaria_test", type="Malaria"),
  ANC = list(scenario = "BasicServices", task = "ANC", type="Births"),
  Malnutrition_U5 = list(scenario = "BasicServices", task = "Malnutrition_U5", type ="Malnutrition")
))



