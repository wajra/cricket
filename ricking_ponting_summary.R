library(tidyverse)
# Read the data
ricky_ponting <- read_csv("data/ricky_ponting_test_innings.csv")
# Let's say we want to know the number of innings he's actually batted in
ricky_ponting %>% filter(Runs != 'DNB')
# Now let's say we want his
# 1. Batting average
# 2. Runs per innings (A far more valuable statistic that BA imo)

# The problem is that 'Runs' column has a '*' at the end if
# he was not dismissed. We need to get rid of that
# So we'll use the 'stringr' library to substitute '' for '*'
ricky_ponting <- ricky_ponting %>% mutate(Runs = str_replace(Runs, c("[*]"), ""))

ricky_ponting %>% filter(Dismissal == 'not out')

# Ricking Ponting valid innings
ricky_ponting_vi <- ricky_ponting %>% filter(Runs != 'DNB')
# Batting Average
not_out_innings <- dim(ricky_ponting_vi %>% filter(Dismissal %in% c('not out','retired notout')))[1]
total_innings <- dim(ricky_ponting_vi)[1]
# Now we cast the Runs as a numeric table
ricky_ponting_vi <- ricky_ponting_vi %>%mutate_at(vars(2,7), as.numeric)
# ricky_ponting_vi <- ricky_ponting_vi %>%mutate_at(vars(SR), as.numeric)
ricky_ponting_vi %>% summarise(runs_sum = sum(Runs), rpi = mean(Runs), 
                                 avg = sum(Runs)/(total_innings - not_out_innings))
# Cumulative RPI
ricky_ponting_rpi_cum <- dplyr::cummean(ricky_ponting_vi$Runs)
ricky_ponting_vi <- add_column(ricky_ponting_vi, rpi_cum = ricky_ponting_rpi_cum)

ggplot(ricky_ponting_vi, aes(x=Runs, y=SR)) + geom_point(shape=1) + ylim(0,200)