library(tidyverse)

# Read the data
legends <- read_csv("data/legends_batting_ver_2.csv")
# Drop 'DNB' rows
legends <- legends %>% filter(!Runs %in% c('DNB', 'TDNB'))
# The problem is that 'Runs' column has a '*' at the end if
# he was not dismissed. We need to get rid of that
# So we'll use the 'stringr' library to substitute '' for '*'
legends <- legends %>% mutate(Runs = str_replace(Runs, c("[*]"), ""))

# We need to recast the following columns as numerical
# 4,5,6,7,8,9,10, 12
legends <- legends %>% mutate_at(vars(4, 12), as.numeric)

legends_grouped <- legends %>% 
                  group_by(Name) %>% 
                  summarise(rpi=mean(Runs))

legends <- legends %>% 
          group_by(Name) %>%
          mutate(cum_rpi = dplyr::cummean(Runs))

# Say I want to plot time series data for batsmen
# Then I want to convert the 'Date' column to a friendlier format
legends <- legends %>% mutate(Date = as.Date(Date, format="%d %b %Y"))
legends <- legends %>% mutate(Opposition = str_replace(Opposition, c("v "), ""))
# Rename the 13th column 'Opposition' as 'Team'. So that we can join them
colnames(legends) [13] <- "Team"

for (batsman_name in unique(legends$Name)){
  batsman <- legends %>% filter(Name == batsman_name)
  batsman_plot <- ggplot(batsman, aes(Date, cum_rpi)) + geom_line() + 
    xlab("Month - Year") + ylab("Cumulate Runs per Innings") + 
    xlim(as.Date(c("1989-01-01", "2014-12-30"))) +
    ggtitle(batsman_name)
  print(batsman_plot)
}

# Let's just say that for visual purposes I want to drop outlandish
# cumulative RPI values
# legends <- legends %>% filter(cum_rpi<=60 & cum_rpi>=25)
# for (batsman_name in unique(legends$Name)){
#  batsman <- legends %>% filter(Name == batsman_name)
#  batsman_plot <- ggplot(batsman, aes(Date, cum_rpi)) + geom_line(size=0.25) +
#   xlab("Time") + ylab("Cumulative Runs per Innings") + 
#    xlim(as.Date(c("1989-01-01", "2014-12-30"))) +
#    ylim(25, 60) +
#    ggtitle(paste(batsman_name, "extremes removed"))
#  print(batsman_plot)
#}

write.csv(legends, file="data/legends_batting_formatted.csv")