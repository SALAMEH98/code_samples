## Tidyverse Code Example One
## Cleaning the Governor Data Set
## Sief Salameh

# In this data set, I am identifying the political party of each state
# governor for the years 2000 - 2020. This data set was provided to me by 
# JACOB KAPLAN - a researcher at the University of Princeton. His 
# information can be found here: 
# https://jacobdkaplan.weebly.com/

library(tidyverse)
library(magrittr)
library(dplyr)
library(haven)
library(lubridate)
library(fredr)

setwd("~/Downloads/work_samples/tidyverse/raw_data")

# The data was originally provided to me in a .dta file, so I read it in R
# to transcribe the data.

governor_df <- read_dta("governors.dta")

governor_df <- governor_df %>%
  filter(year >= 2000) %>%
  select(state, party, year)

governor_df <- governor_df[!governor_df$party == "", ]

governor_df$state_abb <- state.abb[match(governor_df$state, state.name)]

governor_df <- governor_df %>%
  filter(!is.na(state_abb))

# I came to notice that if there was an election held in one of the years, the 
# following year will have two values - one value for the political party of 
# the incumbent governor and another value for the political party of the newly 
# elected governor. So I implemented several steps to address and fix this
# issue. 

# First, I grouped the data by the state and year and created a column that 
# tells me whether or not there was a duplicate. 

election_year <- governor_df %>%
  group_by(state, year) %>%
  mutate(duplicate = n()) %>%
  filter(duplicate == "2" | duplicate == "4")

# Then I created a function that deleted the first value within the duplicated 
# values. I chose the first value because that political party represented the 
# incumbent governor for the year prior. For example, if Illinois held an
# election in 2014, 2014 will have one political party but 2015 will have two 
# political parties listed for that year. So I deleted the value that
# represented the older governor since the majority of states switch positions 
# during the 2nd week of January (the new year). So the older governor's
# political party should not be included. 

to_delete <- seq(0, nrow(election_year), 2)

election_year <- election_year[-to_delete, ]

election_year <- election_year[-c(109), ]

governor_df_final <- governor_df %>%
  group_by(state, year) %>%
  mutate(duplicate = n()) %>%
  filter(!duplicate == "2") %>%
  filter(!duplicate == "4")

governor_df_final <- rbind(election_year, governor_df_final) %>%
  arrange(year) %>%
  select(state, state_abb, party, year)

governor_df_final %>% head(5)

# write.csv(governor_df_final,
#  "~/Downloads/work_samples/tidyverse/raw_data/clean_gov_data.csv",
#  row.names = FALSE
# )

## END