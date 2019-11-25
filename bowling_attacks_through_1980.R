# Bowling attacks

library(tidyverse)
library(stringr)
library(zoo)

# Read the data
bowling <- read_csv("data/bowling_performances_from_1985.csv")

# Drop rows that have Runs as 'DNB' and forfeite'
bowling <- bowling %>% filter(!Score %in% c("DNB", "forfeite"))

# We have to replace the 'v ' in the Opposition column
bowling <- bowling %>% mutate(Opposition = str_replace(Opposition, c("v "), ""))
# We'll replace 'd' at the end of the Runs column for the declared innings
bowling <- bowling %>% mutate(Score = str_replace(Score, c("d"), ""))
# Format the data column
bowling <- bowling %>% mutate(Date = as.Date(Date, format="%d %b %Y"))

# Now we need to extract the wickets and runs from the Score column
# First the wickets
# Test out the regex
str_match("225/7", "/([0-9])")[1,2]
# Then the runs
# Test out the regex for that
str_detect("2/7", "^[0-9]{1,3}$")

# First we extract the wickets using the str_match we tested above
bowling <- bowling %>% mutate(Wickets = str_match(Score, "/([0-9])")[,2])
# Now we replace the NA in the Wickets column with 10
bowling <- bowling %>% mutate(Wickets = replace_na(Wickets, 10))

# Now we need to extract the Runs from the score
bowling <- bowling %>% mutate(Runs = str_match(Score, "^([0-9]{1,3})")[,2])

# Set wickets and Runs columns as numeric
bowling <- bowling %>%mutate_at(vars(12,13), as.numeric)

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

# Apply the rolling functions
# Now let's try to apply 'get_wickets_mean' function to West Indies
# Let's get the West Indies team
bowling_west_indies <- bowling %>% filter(Team == 'West Indies')
# Let's sort them by the Date and the Innings
bowling_west_indies <- bowling_west_indies %>% arrange(Date, Inns)

# rollapply(bowling_west_indies, width=10, get_wickets_mean_df, fill=NA)

bowling_west_indies[,"ten_inns_mean"] <- NA

wi_len <- dim(bowling_west_indies)[1]

for (i in 11:wi_len){
  start_index <- i - 10
  end_index <- i - 1
  computing_df <- bowling_west_indies[start_index:end_index, ]
  print(dim(computing_df)[1])
  wickets_mean <- get_wickets_mean(computing_df$Runs, computing_df$Wickets)
  bowling_west_indies[i, "ten_inns_mean"] <- wickets_mean
}

ggplot(bowling_west_indies, aes(Date, ten_inns_mean)) + geom_line() + 
  xlab("Month - Year") + ylab("Bowling form coming into the innings")

ggplot(bowling_west_indies, aes(Date, ten_inns_mean)) + geom_point() + 
  xlab("Month - Year") + ylab("Bowling form coming into the innings")

ggplot(bowling_west_indies, aes(x=Index, y=ten_inns_mean)) + geom_bar(stat="identity", color='black',
                                                                     width=0.25, position = position_dodge(width = 0))

bowling[, "ten_inns_mean"] <- NA

for (i in unique(bowling$Team)){
  print(i)
  print(dim(bowling %>% filter(Team==i)))
  team_df <- bowling %>% filter(Team==i)
  team_df <- team_df %>% arrange(Date, Inns)
  df_length <- dim(team_df)[1]
  if (df_length < 11){
    next
  }
  else{
    for (i in 11: df_length){
      start_index <- i - 10
      end_index <- i - 1
      inns_index <- team_df[i,]$Index
      computing_df <- team_df[start_index:end_index, ]
      wickets_mean <- get_wickets_mean(computing_df$Runs, computing_df$Wickets)
      bowling[(bowling$Index==inns_index), "ten_inns_mean"] <- wickets_mean
    }
  }
}

# Write the CSV with the formatted data
write.csv(bowling, file="data/bowling_performances_1985_formatted.csv")
# west_indies_df <- bowling %>% filter(Team=='West Indies')