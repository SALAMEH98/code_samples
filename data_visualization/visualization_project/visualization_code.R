## Data Visualization Code Example
## Sief Salameh

library("tidyverse")

setwd("~/Downloads/work_samples/data_visualization/visualization_project/raw_data")

cook_county_df <- read.csv("Data_Visualization_Final.csv")

chicago_geocodes <- read.csv("Final_Geocodes.csv")

## First Data Set

final_df <- left_join(cook_county_df, chicago_geocodes,
  by = c(
    "Township.name" = "township_name",
    "Township.code" = "township_code",
    "CCAO.Neighborhood" = "neighborhood"
  )
)

final_df <- final_df[1:(length(final_df) - 3)]

final_df <- subset(final_df, select = -c(PIN, Tax.code))

df_2010 <- final_df %>%
  filter(Year == "2010") %>%
  group_by(CCAO.Neighborhood, Township.name) %>%
  summarise(assessment_total_2010 = (sum(BOR.certified.AV..final.)))

df_2010 <- df_2010 %>%
  mutate(property_values_2010 = (assessment_total_2010 / .10))

df_2020 <- final_df %>%
  filter(Year == "2020") %>%
  group_by(CCAO.Neighborhood, Township.name) %>%
  summarise(assessment_total_2020 = (sum(BOR.certified.AV..final.)))

df_2020 <- df_2020 %>%
  mutate(property_values_2020 = assessment_total_2020 / .10)

merged_df <- left_join(df_2010,
  df_2020,
  by = c("CCAO.Neighborhood", "Township.name")
)

merged_df <- merged_df %>%
  mutate(
    percent_change =
      ((property_values_2020 - property_values_2010) / property_values_2010) * 100
  )

merged_df <- merged_df %>%
  mutate(
    national_adjuster =
      (percent_change - 36.5)
  )

merged_df <- left_join(merged_df, chicago_geocodes,
  by = c(
    "CCAO.Neighborhood" = "neighborhood",
    "Township.name" = "township_name"
  )
)

merged_df <- merged_df[1:(length(merged_df) - 4)]

merged_df <- rename(merged_df, "wkt" = "multipolygon")

merged_df %>%
  head(5)

# write.csv(merged_df,
#  "~/Downloads/work_samples/data_visualization/visualization_project/raw_data/property_data.csv",
#  row.names = FALSE
# )

## Second Data Set

appeals_filed <- final_df %>%
  filter(Year == "2020") %>%
  filter(Appeal.filed.with.CCAO == "true") %>%
  group_by(CCAO.Neighborhood, Township.name) %>%
  count(Appeal.filed.with.CCAO) %>%
  rename("total_appeals_filed" = "n")

all_assessments <- final_df %>%
  filter(Year == "2020") %>%
  group_by(CCAO.Neighborhood, Township.name) %>%
  count(n()) %>%
  rename("total_assessments" = "n")

appeal_rate <- left_join(appeals_filed, all_assessments,
  by = c("CCAO.Neighborhood", "Township.name")
)

appeal_rate <- appeal_rate %>%
  mutate(
    rate_of_appeals =
      ((total_appeals_filed / total_assessments) * 100)
  )

appeal_rate <- left_join(appeal_rate, chicago_geocodes,
  by = c(
    "CCAO.Neighborhood" = "neighborhood",
    "Township.name" = "township_name"
  )
)

appeal_rate <- appeal_rate[-c(5)]

appeal_rate <- rename(appeal_rate, "wkt" = "multipolygon")

appeal_rate  <- appeal_rate [1:(length(appeal_rate ) - 4)]

appeal_rate %>%
  head(5)

# write.csv(appeal_rate,
#  "~/Downloads/work_samples/data_visualization/visualization_project/raw_data/appeal_rate.csv",
#  row.names = FALSE
# )

## Third Data Set

total_2020 <- final_df %>%
  filter(Appeal.filed.with.CCAO == "true") %>%
  filter(Year == "2020") %>%
  group_by(CCAO.Neighborhood, Township.name) %>%
  count(Appeal.filed.with.CCAO) %>%
  rename("total_appeals" = "n")

approved_2020 <- final_df %>%
  filter(CCAO.adjustment.indicator == "true") %>%
  filter(Year == "2020") %>%
  group_by(CCAO.Neighborhood, Township.name) %>%
  count(CCAO.adjustment.indicator) %>%
  rename("approved_appeals" = "n")

merged_appeals <- left_join(approved_2020,
  total_2020,
  by = c("CCAO.Neighborhood", "Township.name")
)

merged_appeals <- merged_appeals %>%
  mutate(
    approval_rate =
      ((approved_appeals / total_appeals) * 100)
  )

merged_appeals <- left_join(merged_appeals, chicago_geocodes,
  by = c(
    "CCAO.Neighborhood" = "neighborhood",
    "Township.name" = "township_name"
  )
)

merged_appeals <- merged_appeals[1:(length(merged_appeals) - 4)]

merged_appeals <- rename(merged_appeals, "wkt" = "multipolygon")

merged_appeals %>%
  head(5)

# write.csv(merged_appeals,
#  "~/Downloads/work_samples/data_visualization/visualization_project/raw_data/approved_appeals.csv",
#  row.names = FALSE
# )

single_family <- read.csv("single_family.csv")

single_family  <- single_family[-c(1:2, 4:5, 7:16, 19)]

# write.csv(single_family,
#          "~/Downloads/work_samples/data_visualization/visualization_project/raw_data/single_family.csv",
#          row.names = FALSE
# )

## END