library(dplyr)
library(data.table)
library(tidyverse)

setwd("D:/Python_Code/Corona_tracker/")

data_path <- "./data/raw/"

#function to transpose tibble while keeping names
#https://stackoverflow.com/questions/42790219/how-do-i-transpose-a-tibble-in-r
transpose_df <- function(df) {
  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    tibble::rownames_to_column(.data = .) %>%
    tibble::as_tibble(.)
  rownames(t_df) <- t_df$rowname
  return(t_df)
}

#cleaning
unallocated_remove <- function(df){
  df <- filter(df, countyFIPS != 0 & countyFIPS != 1)
  return(df)
}



confirmed <- read_csv(paste0(data_path,"covid_confirmed_usafacts.csv"))
deaths <- read_csv(paste0(data_path,"covid_deaths_usafacts.csv"))
population <- read_csv(paste0(data_path,"covid_county_population_usafacts.csv"))

confirmed <- unallocated_remove(confirmed)
deaths <- unallocated_remove(deaths)
population <- unallocated_remove(population)

dates <- colnames(confirmed)
dates <- dates[!dates %in% c("countyFIPS", "County Name", "State", "stateFIPS")]
county_name_uni <- population[c("countyFIPS", "County Name")]

confirmed <- confirmed %>%
  pivot_longer(dates,
               names_to = c("date"),
               values_to = c("value"))

deaths <- deaths %>%
  pivot_longer(dates,
               names_to = c("date"),
               values_to = c("value"))

graph_df <- filter(confirmed, `County Name` == "Harris County" & `State` == "TX" |
                     `County Name` == "Houston County" & `State` == "TX")

diff_df <- confirmed