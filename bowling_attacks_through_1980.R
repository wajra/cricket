# Bowling attacks

library(tidyverse)
library(stringr)

# Read the data
bowling <- read_csv("data/bowling_performances_from_1985.csv")

# We have to replace the 'v ' in the Opposition column
bowling <- bowling %>% mutate(Opposition = str_replace(Opposition, c("v "), ""))
# We'll replace 'd' at the end of the Runs column for the declared innings
bowling <- bowling %>% mutate(Score = str_replace(Score, c("d"), ""))

# Now we need to extract the wickets and runs from the Score column
# First the wickets
# Test out the regex
str_match("225/7", "/([0-9])")[1,2]
# Then the runs
# Test out the regex for that
str_detect("223", "^[0-9]{2,3}$")

# First we extract the wickets using the str_match we tested above
bowling <- bowling %>% mutate(Wickets = str_match(Score, "/([0-9])")[,2])
# Now we replace the NA in the Wickets column with 10
bowling <- bowling %>% mutate(Wickets = replace_na(Wickets, 10))

# Now we need to extract the Runs from the score
bowling <- bowling %>% mutate(Runs = str_match(Score, "^([0-9]{2,3})")[,2])

# Writing a function to get the aggregate mean from vectors of runs and wickets
get_wickets_mean <- function (runs, wickets){
  runs_sum = sum(runs)
  wickets_sum = sum(wickets)
  return (runs_sum/wickets_sum)
}

# Define vectors for runs and wickets
wickets_c <- c(10, 0, 7, 6, 10, 10)
runs_c <- c(360, 25, 590, 250, 400, 300)

# Test out the function
get_wickets_mean(runs_c, wickets_c)