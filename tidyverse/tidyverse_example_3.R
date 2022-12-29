## Tidyverse Code Example Three
## Cleaning BEA Data Set
## Sief Salameh

# In section one, I am cleaning a raw data file from the BEA using the following
# formatting requirements:

# 1) Load and merge the data into a panel data frame, with the columns: "state", 
# "year", and one for each of the 10 industries. Every state-year combination 
# should uniquely identify a row.  No more and no less than 12 columns should 
# remain.

# 2) The values should be given as the share of the total employment in that place 
# and time, e.g. if total employment in a place and time was 100, and the 
# employment in one industry was 10, then the value shown for that state-year 
# industry should be 0.1.

## Section One

setwd("~/Downloads/work_samples/tidyverse/raw_data")
library(tidyverse)
library(dplyr)
library(stringr)
library(matrixStats)

total_data <- read.csv(
  file = "SAEMP25N total.csv", sep = ",",
  header = FALSE
) %>%
  slice(-c(1, 2, 3, 4, 57, 58, 59)) %>%
  subset(select = -c(V1))

# The original csv file was separated by space for the headers and by commas
# for the rest of the data. Therefore, I separated the csv file by comma but 
# told my code to ignore the header spacing. Then I cleaned the data by 
# removing the foot notes and all unnecessary data rows and columns.

total_data <- rename(total_data,
  "state" = "V2", "2000" = "V3",
  "2017" = "V4"
) %>%
  slice(-c(1))

total_data <- pivot_longer(total_data,
  cols = 2:3, names_to = "year",
  values_to = "total_employment"
)

head(total_data, 5)

industry_data <- read.csv(
  file = "SAEMP25N by industry.csv", sep = ",",
  header = FALSE
) %>%
  slice(-c(1, 2, 3, 4, 567, 568, 569, 570, 571)) %>%
  subset(select = -c(V1, V3))

# I followed the same steps as the total data, but I did not utilize a function
# or loop because the number of unnecessary rows and columns differentiates
# between the two data sets. Hence I did the codes individually.  

industry_data <- rename(industry_data,
  "state" = "V2", "industry" = "V4",
  "2000" = "V5", "2017" = "V6"
) %>%
  slice(-c(1))

industry_data <- filter(industry_data, industry != "By industry")

industry_data <- pivot_longer(industry_data,
  cols = 3:4, names_to = "year",
  values_to = "employment_level"
)

industry_data <- pivot_wider(industry_data,
  names_from = industry,
  values_from = employment_level
)

full_data <- left_join(total_data, industry_data,
  by = c("state" = "state", "year" = "year")
)

full_data <- full_data %>% rename_all(function(x) trimws(x))

sapply(full_data, class)

full_data[, c(4:13)] <- sapply(full_data[, c(4:13)], as.numeric)

# I removed the white space before and after the column names and then I set
# the values to numeric in order to perform the calculation. I generated 7 NA's
# due to some values being previously categorized as character values. These
# came from the (T) values that were in the original data set. 

colSums(is.na(full_data))

full_data <- full_data %>%
  mutate(
    across(c(4:13),
      .fns = ~ . / total_employment
    )
  )

# I divided all the column values by the corresponding values in the total 
# employment column.

full_data <- subset(full_data, select = -c(total_employment))

# I removed the total employment column after performing my calculations.

head(full_data, 5)

# write.csv(full_data, "C:\\Downloads\\work_samples\\tidyverse\\raw_data\\data.csv", row.names=FALSE)

## Section Two

# In section two, I answer two summary statistics questions.

## PART A: Find the states with the top five share of manufacturing employment 
## in the year 2000, then show how their share of employment in manufacturing 
## changed between 2000 and 2017.  Use a basic plot to display the information.

part_a <- full_data %>%
  filter(year == 2000) %>%
  select(state, year, Manufacturing) %>%
  arrange(desc(Manufacturing)) %>%
  head(5)

# I find the states with the top five shares of manufacturing 
# employment in the year 2000. 

part_b <- full_data %>%
  filter(state %in% c(
    "Indiana", "Wisconsin", "Michigan",
    "Arkansas", "North Carolina"
  )) %>%
  select(state, year, Manufacturing)

# I isolate the five states so that I can graph them easily.

part_b %>% ggplot(mapping = aes(x = state, y = Manufacturing, fill = year)) +
  geom_bar(stat = "identity", width = .5, position = "dodge") +
  labs(
    title = "Share of Manufacturing Employment between 2000 and 2017",
    x = "State", y = "Employment Level"
  )

## PART B: Show which five states have the highest concentration of 
## employment in a any single industry in each of 2000 and 2017, and what those 
## industries are.

first_year <- full_data %>% filter(year == 2000)

highest_employment_a <- do.call("cbind", lapply(first_year[3:12], function(x) {
  rank <- head(order(x, decreasing = TRUE), 1)
  data.frame(state = first_year$state[rank], employment_level = x[rank])
}))

# This function ranks the columns by highest value and generates an output of
# only one row with the highest employment level in each column category,
# along with their correlating states.

print(highest_employment_a)

table_form_a <- data.frame(matrix(unlist(highest_employment_a),
  nrow = 10, byrow = TRUE
), stringsAsFactors = FALSE)

table_form_a <- rename(table_form_a, state = X1, employment_level = X2) %>%
  arrange(desc(employment_level)) %>%
  head(5)

# I take the function output and turn it into a data frame with two columns
# that show me the top fives states with the highest employment levels. Then I
# match each state and employment level with the corresponding 
# industry in that year.

print(table_form_a)

# The five states that have the highest concentration of employment 
# in 2000 from highest to least include 1) D.C. = Government; 
# 2) Indiana = Manufacturing; 3) New Hampshire = Retail Trade; 
# 4) Rhode Island = Health Care; 5) Delaware = Finance

second_year <- full_data %>% filter(year == 2017)

highest_employment_b <- do.call("cbind", lapply(
  second_year[3:12],
  function(x) {
    rank <- head(order(x, decreasing = TRUE), 1)
    data.frame(state = second_year$state[rank], employment_level = x[rank])
  }
))

print(highest_employment_b)

table_form_b <- data.frame(matrix(unlist(highest_employment_b),
  nrow = 10, byrow = TRUE
), stringsAsFactors = FALSE)

table_form_b <- rename(table_form_b, state = X1, employment_level = X2) %>%
  arrange(desc(employment_level)) %>%
  head(5)

# I repeat the same steps and functions but with the data that is filtered for 
# the year 2017

print(table_form_b)

# The five states that have the highest concentration of employment 
# in 2017 from highest to least include 1) D.C. = Government; 
# 2) Massachusetts = Health Care; 3) Indiana = Manufacturing; 
# 4) New Hampshire = Retail Trade; 5) Delaware = Finance

## END