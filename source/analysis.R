library(tidyverse)
library(dplyr)
library(ggplot2)

# The functions might be useful for A4
#source("../source/a4-helpers.R")
incarceration_trends<- read.csv(
  file = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", 
  stringsAsFactors = FALSE
)
View(incarceration_trends)

get_data <- function(num_records=-1) {
  fname <- "~/Documents/info201/data/incarceration_trends.csv"
  df <- read.csv(fname, nrows=num_records)
  return(df)
}

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#

california <- incarceration_trends %>%
  select(state, county_name, year, female_jail_pop, male_jail_pop, total_jail_pop) %>%
  filter(state == "CA", 
         year == 2018)
View(california)

whole <- incarceration_trends %>%
  select(state, year, female_jail_pop, male_jail_pop, total_jail_pop) %>%
  filter(year == 2018)
  pull(state)

max_female_cal <- california %>%
  filter(female_jail_pop == max(female_jail_pop, na.rm = TRUE))
print(max_female_cal)
View(max_female_cal)

min_male_cal <- california %>%
  filter(male_jail_pop == min(male_jail_pop, na.rm = TRUE))
print(min_male_cal)
View(min_male_cal)

all_fem_vs_male <- incarceration_trends %>%
  select(state, county_name, year, female_jail_pop, male_jail_pop, total_jail_pop) %>%
  filter(year == 2018)
View(all_fem_vs_male)

max_male_all <- all_fem_vs_male %>%
  filter(male_jail_pop == max(male_jail_pop, na.rm = TRUE))
print(max_male_all)


california_1970 <- incarceration_trends %>%
  select(state, county_name, year, female_jail_pop, male_jail_pop, total_jail_pop) %>%
  filter(state == "CA", 
         year == 1970)

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# Data Wrangling Function
get_year_jail_pop <- function() {
  df <- incarceration_trends %>%
    select(year, total_jail_pop) %>%
    group_by(year) %>%
    summarize(total_population = sum(total_jail_pop, na.rm = TRUE))
}   
  
View(get_year_jail_pop())


# Plotting Function
plot_jail_pop_for_us <- function()  {
  increase_of_jail_population <- ggplot(data = get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = total_population)) +
    labs(caption = "Increase of Jail Population in the United States (1970 - 2018)")
  return(plot)   
} 
plot_jail_pop_for_us()



## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
 get_jail_pop_by_states <- function(state1, state2, state3) {
   states_list <- c(state1, state2, state3)
   
   df <- incarceration_trends %>%
     select(year, state, total_jail_pop) %>%
     group_by(year, state) %>%
     summarise(total_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
     filter(state %in% states_list)
 } ## Data Wrangling Function

View(get_jail_pop_by_states("WA", "CA", "WY"))

plot_jail_pop_by_states <- function(state1, state2, state3) {
  growth_of_prison_pop_state <- ggplot(data = get_jail_pop_by_states(state1, state2, state3)) +
    geom_line(mapping = aes(x = year, y = total_pop, color = state)) +
    labs(caption = "Growth of Prison Population by State")
  return(line) 
}
plot_jail_pop_by_states("WA", "CA", "WY")  ##Plotting Function

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#

##Data Wrangling Function
  
get_race_data <- function() {
  new_df <- incarceration_trends %>%
  select(year, black_pop_15to64, white_pop_15to64, division, black_jail_pop, white_jail_pop) %>%
    group_by(year) %>%
    summarize(black_population = sum(black_pop_15to64, na.rm = TRUE), 
              white_population = sum(white_pop_15to64, na.rm = TRUE), 
              white_jail = sum(white_jail_pop, na.rm = TRUE), 
              black_jail = sum(black_jail_pop, na.rm = TRUE)) %>%
    filter(year == 2018)
    mutate(black_prop = black_jail / black_population, 
           white_prop = white_jail / white_population) %>%
    gather(key = "race", value = "proportion", 3:4)
    return(new_df)
}
View(get_race_data())

## Plotting Function
bar_graph_prop_data <- function() {
  bar_of_race_proportions <- ggplot(data = get_race_data() %>% gather(Key,year, black_prop, white_prop )) +
   (mapping = aes(x = year, y = "race", fill = Key)) +
    geom_bar(stat = "identity", position = "dodge" ) +
    (xlab= "proportions")
  return(bar)
}
bar_graph_prop_data()



#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
## Data Wrangling Function
women_vs_men <- function() {
  df <- incarceration_trends %>%
  select(year, state, female_jail_pop, male_jail_pop) %>%
  filter(year == 2018, 
         state == "CA") %>%
  summarize(fem_jail <- sum(female_jail_pop, na.rm = TRUE), 
            male_jail <- sum(male_jail_pop, na.rm = TRUE)) %>%

return(df) 
}
View(women_vs_men())


## Load data frame ---- 
get_data <- function(num_records=-1) {
  fname <- "~/Documents/info201/data/incarceration_trends.csv"
  df <- read.csv(fname, nrows=num_records)
  return(df)
}


states_with_no_jail_pop <- function(df) {
  t <- incarceration_df %>%
    group_by(state) %>%
    summarise(p = sum(total_jail_pop, na.rm = TRUE)) %>%
    filter(p == 0) %>%
    select(state, p) %>%
    pull(state)
  return(t)
}

