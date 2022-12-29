## Tidyverse Code Example Two
## Cleaning the State Legislature Data Set
## Sief Salameh

# In this data set, I am identifying the majority party for each state
# legislature. This data set was created by web scraping pdf tables that were 
# created by the National Conference of State Legislatures (NCSL) and exists 
# from the years 1999 - 2014. 

library(tidyverse)
library(dplyr)
library(haven)
library(tidytext)
library(udpipe)
library(ggplot2)
library(nametagger)
library(SnowballC)
library(maps)
library(pdftools)
library(pdftables)

setwd("~/Downloads/work_samples/tidyverse/raw_data")

# First, I use an external api to convert the pdf tables into .csv files. I 
# save the .csv files in the same working directory, which is the raw_data file. 

convert_pdf("legiscontrol_2002_2014.pdf",
  output_file = NULL,
  format = "csv",
  message = TRUE,
  api_key = "############"
)

convert_pdf("legiscontrol_1990_2000.pdf",
  output_file = NULL,
  format = "csv",
  message = TRUE,
  api_key = "############"
)

# Next, I read in the files and clean the data by removing unnecessary rows.

state_party_one <- read.csv(
  file = "legiscontrol_2002_2014.csv",
  sep = ",",
  header = FALSE
) %>%
  slice(-c(1, 2, 54:58))

# In this step, I create a function that converts the first row of each column 
# into a column header. 

header_adjust <- function(df) {
  names(df) <- as.character(unlist(df[1, ]))
  df[-1, ]
}

state_party_one <- header_adjust(state_party_one)

# I filter the column header names so that only the year shows.

names(state_party_one) <- substring(names(state_party_one), 1, 5)

trim <- function(x) gsub("^\\s+|\\s+$", "", x)

names(state_party_one) <- trim(names(state_party_one))

# I follow the previous steps to also include the data from the year 2000, 
# which is on a different pdf table. 

state_party_two <- read.csv(
  file = "legiscontrol_1990_2000.csv",
  sep = ",",
  header = FALSE
) %>%
  slice(-c(1, 2, 54:58)) %>%
  select(V1, V8)

state_party_two <- header_adjust(state_party_two)

names(state_party_two) <- substring(names(state_party_two), 1, 5)

names(state_party_two) <- trim(names(state_party_two))

# Now I merge the data from the year 2000 to the data set that includes the years 
# 2001-2014.

state_party_df <- left_join(state_party_two, state_party_one, by = "State")

# I pivot the data longer so that it is organized by the state, the year, and 
# the party the state legislature was for that year. 

state_party_df <- state_party_df %>% pivot_longer(
  cols = c(
    "2000",
    "2002",
    "2004",
    "2006",
    "2008",
    "2010",
    "2012",
    "2014"
  ),
  names_to = "year",
  values_to = "party"
)

# I attach the corresponding state abbreviations using a built-in R package. 

state_party_df$state_abb <- state.abb[match(state_party_df$State, state.name)]

state_party_df %>% head(5)

# Since the data frame only includes even numbered years. I duplicated the
# same political party of each even year and passed it onto the next odd year. 
# I did this because I know that the odd year will not be an election 
# year, hence there would have not been a change in political parties. 

fix_one <- transform(state_party_df, year = party)

fix_two <- rbind(state_party_df, fix_one)

n <- nrow(state_party_df)

state_party_fix <- fix_two[kronecker(1:n, c(0, n), "+"), ]

# After I duplicated the political party of each even year and passed it onto
# the next row (which would be the next odd year), I attached a list of years 
# that started from 2000 and ended at 2015. I repeated this process 50 times
# so that that each year from that sequence corresponds with one of the fifty
# states. 

years <- rep(2000:2015, times = 50)

year_df <- data.frame(years)

year_df$years <- as.character(year_df$years)

state_party_final <- cbind(state_party_fix, year_df)

# After I created the list of political parties for each state legislature from
# the years 2000-2015, I removed the old year column that had missing odd years. 

state_party_final <- subset(state_party_final, select = -c(year))

state_party_final %>% head(5)

# write.csv(state_party_final,
#  "~/Downloads/work_samples/tidyverse/raw_data"/clean_state_party_data.csv",
#  row.names = FALSE
# )

## END
