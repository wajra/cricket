# Cricket Grounds

library(tidyverse)
library(stringr)
library(zoo)
library(RColorBrewer)
# Read the bowling data. It also has the ground name so we can 
# start from there
grounds <- read_csv("data/bowling_performances_1985_formatted.csv")

# Now let's read the legends batting data
legends <- read_csv("data/legends_batting_formatted.csv")

# Writing a function to get the aggregate mean from vectors of runs and wickets
get_wickets_mean <- function (runs, wickets){
  runs_sum = sum(runs)
  wickets_sum = sum(wickets)
  return (runs_sum/wickets_sum)
}

# Setting up an empty column for ground averages
grounds[, "ground_inns_mean"] <- NA


# Applying the function to that column
for (i in unique(grounds$Ground)){
  print(i)
  print(dim(grounds %>% filter(Ground==i)))
  ground_df <- grounds %>% filter(Ground==i)
  ground_df <- ground_df %>% arrange(Date, Inns)
  df_length <- dim(ground_df)[1]
  if (df_length < 11){
    next
  }
  else{
    for (i in 11: df_length){
      start_index <- i - 10
      end_index <- i - 1
      inns_index <- ground_df[i,]$Index
      computing_df <- ground_df[start_index:end_index, ]
      wickets_mean <- get_wickets_mean(computing_df$Runs, computing_df$Wickets)
      grounds[(grounds$Index==inns_index), "ground_inns_mean"] <- wickets_mean
    }
  }
}

# Joining legends with the grounds tibble
joined_df <- left_join(legends, grounds, by=c("Team","Date","Inns"))

# Now let's filter Ricky Ponting and see how he did against great bowling attacks
ponting <- joined_df %>% filter(Name=='Ricky Ponting')

# Making plots
ggplot(data = ponting, aes(x=ten_inns_mean, y=ground_inns_mean)) + 
  geom_point(aes(color=cut(Runs.x, c(0,25,50,100,Inf)))) +
  scale_color_manual(name="Runs.x",
                     values = c("(0,25]" = "black",
                                "(25,50]" = "cyan",
                                "(50,100]" = "purple",
                                "(100,Inf]" = "blue"),
                     labels = c("0-25", "25-50", "50-100", "100+")) +
  xlab("Bowling attack average/form") + ylab("Pitch average/form")

ggplot(ponting, aes(x=ten_inns_mean)) + geom_histogram()

ponting_d <- density(ponting$ten_inns_mean, na.rm=TRUE)
plot(ponting_d)

#------------------------------------------------------------------------
#------------------------------------------------------------------------

# Now let's do the same for Sachin
sachin <- joined_df %>% filter(Name=='Sachin Tendulkar')

# Making plots
ggplot(data = sachin, aes(x=ten_inns_mean, y=ground_inns_mean)) + 
  geom_point(aes(color=cut(Runs.x, c(0,25,50,100,Inf)))) +
  scale_color_manual(name="Runs.x",
                     values = c("(0,25]" = "black",
                                "(25,50]" = "cyan",
                                "(50,100]" = "purple",
                                "(100,Inf]" = "blue"),
                     labels = c("0-25", "25-50", "50-100", "100+")) +
  xlab("Bowling attack average/form") + ylab("Pitch average/form")

ggplot(sachin, aes(x=ten_inns_mean)) + geom_histogram()

sachin_d <- density(sachin$ten_inns_mean, na.rm=TRUE)
plot(sachin_d)

#------------------------------------------------------------------------
#------------------------------------------------------------------------

# Now let's do the same for Lara
lara <- joined_df %>% filter(Name=='Brian Lara')

# Making plots
ggplot(data = lara, aes(x=ten_inns_mean, y=ground_inns_mean)) + 
  geom_point(aes(color=cut(Runs.x, c(0,25,50,100,Inf)))) +
  scale_color_manual(name="Runs.x",
                     values = c("(0,25]" = "black",
                                "(25,50]" = "cyan",
                                "(50,100]" = "purple",
                                "(100,Inf]" = "blue"),
                     labels = c("0-25", "25-50", "50-100", "100+")) +
  xlab("Bowling attack average/form") + ylab("Pitch average/form")

ggplot(lara, aes(x=ten_inns_mean)) + geom_histogram()

lara_d <- density(lara$ten_inns_mean, na.rm=TRUE)
plot(lara_d)