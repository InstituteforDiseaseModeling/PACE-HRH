library(dplyr)
library(stringr)

list.files(path = ".", recursive = T, full.names = T) %>%
  str_subset("[.][R]$") %>%
  sapply(function(x) x %>% readLines() %>% length()) %>%
  sum()
