packages = c("tidyverse")
for(i in packages){
  if(!require(i, character.only = T)){
    install.packages(i)
    library(i, character.only = T)
  }
}

draw_comparison <- function(results, taskname, trials, setting, shoulderYears=1){
  
  # calculate average annual times for all trails
  xa <-lapply(c(1:trials), 
              function(x) {y <-data.frame(results[[x]][["AnnualTimes"]][taskname,])
              colnames(y) <- c("reported_anuual") 
              y
              }
  )
  reported_annual <- rowMeans(do.call(cbind.data.frame, xa))
  reported_annual <- reported_annual[1:length(reported_annual)-shoulderYears] # do not compare shoulder years
  names(reported_annual) <- seq(2020, 2020+length(reported_annual)-1)
  
  # calculate Per age Results
  if (setting == "annual"){
    variable_name <- "AnnualPerAge"
    interval <- 1
  } else if (setting == "monthly"){
    variable_name <- "MonthlyPerAge"
    interval <- 12
  }else{
    stop("Only support per age setting: annual or monthly")
  }
  
  x0 <- lapply(c(1:trials), 
               function(x){
                 zm <- colSums(results[[x]][[variable_name]][["Times"]][["Male"]][taskname,,], 2)
                 zf <- colSums(results[[x]][[variable_name]][["Times"]][["Female"]][taskname,,], 2)
                 total  <-  colSums(as.data.frame(split(zm, ceiling(seq_along(zm)/interval)))) + 
                   colSums(as.data.frame(split(zf, ceiling(seq_along(zf)/interval))))
                 total
               })
  
  reported_per_age <- rowMeans(do.call(cbind.data.frame, x0))
  if (variable_name=="AnnualPerAge"){
    reported_per_age <- reported_per_age[1:length(reported_per_age)-shoulderYears]
  }
  names(reported_per_age) <- seq(2020, 2020+length(reported_per_age)-1)
  
  
  # calculate seasonalityResults if applicable
  x1 <- lapply(c(1:trials), 
               function(x){
                 t <- results[[x]][["SeasonalityResults"]][[taskname]]$Time
                 # combine to yearly
                 t <- colSums(as.data.frame(split(t, ceiling(seq_along(t)/12))))
                 t
               }
  )
  reported_seasonal <-rowMeans(do.call(cbind.data.frame, x1))
  names(reported_seasonal) <- seq(2020, 2020+length(reported_seasonal)-1)
  
  data <- data.frame(cbind(reported_annual, reported_per_age, reported_seasonal))
  year <- rownames(data)
  data <- cbind(year, data)
  
  df <- data %>%
    select(year, reported_annual, reported_per_age, reported_seasonal) %>%
    gather(key = "variable", value = "value", -year)
  head(df)
  
  # Visualization
  g<- ggplot(df, aes(x = year, y = value, group=year)) +
    geom_point(aes(color = variable, shape=variable, size=variable)) + 
    # geom_line(aes(color = variable)) + 
    labs(x = "Year", y = "Times",  title = taskname) +
    theme(axis.text.x=element_text(angle = 90, hjust = 1)) +
    scale_colour_manual(values=(c("blue", "green", "orange"))) +
    scale_shape_manual(values=c(10,17,19)) +
    scale_size_manual(values=c(5,3,1))
  print(g)
  #ggplot2::ggsave(paste("temp/", taskname, ".png"))
  #data.frame(data)
  
}


# pick 3 different tasks to check values 
# FH.MN.ANC.1 (OFFSET)
# FH.MN.D.3 (No-OFFSET)
# Record keeping (NO Seasonality)
# 
# tasknames <- c("FH.MN.ANC.1", "FH.MN.D.3", "Record keeping")
# #tasknames <- row.names(results[["1"]][["AnnualTimes"]])
# dfs <- list()
# for (taskname in tasknames){
#   df <- draw_comparison(taskname, trials, setting)
#   dfs <- append(dfs,list(df))
# }
