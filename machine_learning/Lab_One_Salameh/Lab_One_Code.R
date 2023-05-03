## Lab One Machine Learning
## Sief Salameh
## 04/11/2023

library(tidyverse)

setwd("~/Downloads/Machine_Learning/Lab_One_Salameh")

## Preparing the Data

## Question 2a:

edu_df <- read.csv("usa_00001.csv")

crosswalk_df <- read.csv("Crosswalk.csv")

edu_new <- left_join(edu_df, crosswalk_df, by = c("EDUCD" = "educd"))

## Question 2b i:

edu_new$hsdip <- ifelse(edu_new$EDUCD >= 62 & edu_new$EDUCD <= 100, 1, 0)

## Question 2b ii: 

edu_new$coldip <- ifelse(edu_new$EDUCD >= 101 & edu_new$EDUCD <= 116, 1, 0)

edu_new$no_hs <- ifelse(edu_new$EDUCD <= 61, 1, 0)

## Question 2b iii: 

edu_new$white <- ifelse(edu_new$RACE == 1, 1, 0)

## Question 2b iv:

edu_new$black <- ifelse(edu_new$RACE == 2, 1, 0)

## Question 2b v: 

edu_new$hispanic <- ifelse(edu_new$HISPAN >= 1 & edu_new$HISPAN <= 4, 1, 0)

## Question 2b vi:

edu_new$married <- ifelse(edu_new$MARST >= 1 & edu_new$MARST <= 2, 1, 0)

## Question 2b vii:

edu_new$female <- ifelse(edu_new$SEX == 2, 1, 0)

## Question 2b viii: 

edu_new$vet <- ifelse(edu_new$VETSTAT == 2, 1, 0)

## Question 2c:

edu_new$interaction_hs <- edu_new$hsdip * edu_new$educdc

edu_new$interaction_clg <- edu_new$coldip * edu_new$educdc

edu_new$interaction_no_hs <- edu_new$no_hs * edu_new$educdc

## Question 2d i:

edu_new$age_sq <- edu_new$AGE ^ 2

## Question 2d ii:

edu_new$log_incwage <- log(edu_new$INCWAGE)

## Data Analysis

## Question 1:

selected_cols <- c(
  "YEAR", "INCWAGE", "log_incwage", "educdc", "female", "AGE",
  "age_sq", "white", "black", "hispanic", "married", "NCHILD",
  "vet", "hsdip", "coldip", "no_hs", "interaction_hs", "interaction_clg",
  "interaction_no_hs"
)

edu_selected <- edu_new[, selected_cols]

summary(edu_selected)

sd_df <- apply(edu_selected, 2, sd)

print(sd_df)

## Question 2:

edu_filtered <- subset(edu_selected, is.finite(log_incwage))

ggplot(edu_filtered, aes(educdc, log_incwage)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Years of Education", y = "Natural Log of Income Wage",
    title = "Relationship Between Years of Education and Log Wages"
  )

## Question 3:

model_one <- lm(log_incwage ~ educdc + female + AGE + age_sq +
  white + black + hispanic + married + NCHILD + vet,
data = edu_filtered
)

summary(model_one)

## Question 3 Part g:

model_reduced <- lm(log_incwage ~ educdc + female + AGE + hispanic +
  age_sq + married + NCHILD + vet,
data = edu_filtered
)

anova(model_reduced, model_one)

## Question 4:

no_hs <- edu_filtered[edu_filtered$hsdip == 0 & edu_filtered$coldip == 0, ]

hs <- edu_filtered[edu_filtered$hsdip == 1 & edu_filtered$coldip == 0, ]

college <- edu_filtered[edu_filtered$coldip == 1 & edu_filtered$hsdip == 0, ]


ggplot(data = edu_filtered, aes(x = educdc, y = log_incwage, color = educdc)) +
  geom_point() +
  geom_smooth(
    method = "lm", se = FALSE, aes(group = 1),
    formula = y ~ x, data = no_hs, color = "red"
  ) +
  geom_smooth(
    method = "lm", se = FALSE, aes(group = 1),
    formula = y ~ x, data = hs, color = "blue"
  ) +
  geom_smooth(
    method = "lm", se = FALSE, aes(group = 1),
    formula = y ~ x, data = college, color = "purple"
  ) +
  xlab("Years of Education") +
  ylab("Natural Log of Income Wage") +
  ggtitle("Relationship between Education and Log Wages with Distinct Regression Lines for Different Education Levels")


# Question 5:

model_two <- lm(log_incwage ~ hsdip + coldip + female + AGE + age_sq +
  white + black + hispanic + married + NCHILD + vet,
data = edu_filtered
)

summary(model_two)

## END
