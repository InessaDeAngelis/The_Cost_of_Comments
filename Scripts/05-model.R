#### Preamble ####
# Purpose: Prepares and models data
# Author: Inessa De Angelis
# Date: 19 May 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(marginaleffects)

all_analyzed <- read_csv(here::here("2024_1/Outputs/Data/Perspective/all_analyzed.csv"))

#### Prepare data for modeling ####
analysis_data <- all_analyzed |>
  mutate(gender = case_when(
    gender == "Woman" ~ 0,
    gender == "Man" ~ 1),
    profile_status = case_when(
      profile_status == "Low" ~ 0,
      profile_status == "High" ~ 1),
    background = case_when(
      background == "White" ~ 0,
      background == "Racialized" ~ 1,
      background == "Indigenous" ~ 2),
    political_affiliation = case_when(
      political_affiliation == "Liberal" ~ 0,
      political_affiliation == "NDP" ~ 1,
      political_affiliation == "Conservative" ~ 2,
      political_affiliation == "Green Party" ~ 3,
      political_affiliation == "Independent" ~ 4),
  toxicity_score = case_when(
    toxicity_score >= 0 & toxicity_score < 0.5 ~ 0,
    toxicity_score >= 0.5 & toxicity_score <= 1 ~ 1),
  severe_toxicity_score = case_when(
    severe_toxicity_score >= 0 & severe_toxicity_score < 0.5 ~ 0,
    severe_toxicity_score >= 0.5 & severe_toxicity_score <= 1 ~ 1),
  insult_score = case_when(
    insult_score >= 0 & insult_score < 0.5 ~ 0,
    insult_score >= 0.5 & insult_score <= 1 ~ 1),
  sexually_explicit_score = case_when(
    sexually_explicit_score >= 0 & sexually_explicit_score < 0.5 ~ 0,
    sexually_explicit_score >= 0.5 & sexually_explicit_score <= 1 ~ 1),
  profanity_score = case_when(
    profanity_score >= 0 & profanity_score < 0.5 ~ 0,
    profanity_score >= 0.5 & profanity_score <= 1 ~ 1),
  threat_score = case_when(
    threat_score >= 0 & threat_score < 0.5 ~ 0,
    threat_score >= 0.5 & threat_score <= 1 ~ 1),
  flirtation_score = case_when(
    flirtation_score >= 0 & flirtation_score < 0.5 ~ 0,
    flirtation_score >= 0.5 & flirtation_score <= 1 ~ 1)) |>
  rename(status = profile_status) |>
  select(-c(AuthorDisplayName, AuthorChannelID, CommentID, ParentID, VideoID, VideoDate))

## Factor independent variables ##
analysis_data$gender <- factor(analysis_data$gender)
analysis_data$status <- factor(analysis_data$status)
analysis_data$background <- factor(analysis_data$background)
analysis_data$political_affiliation <- factor(analysis_data$political_affiliation)

#### Test data ####
## Check class of all variables ##
class(analysis_data$gender) == "factor"
class(analysis_data$background) == "factor"
class(analysis_data$status) == "factor"
class(analysis_data$political_affiliation) == "factor"
class(analysis_data$toxicity_score) == "numeric"
class(analysis_data$severe_toxicity_score) == "numeric"
class(analysis_data$insult_score) == "numeric"
class(analysis_data$sexually_explicit_score) == "numeric"
class(analysis_data$profanity_score) == "numeric"
class(analysis_data$threat_score) == "numeric"
class(analysis_data$flirtation_score) == "numeric"

#### Make models ####
## Toxicity logistic regression model ##
set.seed(16)
toxicity_model <- glm(toxicity_score ~ gender + status + background + political_affiliation, 
                      data = analysis_data, family = "binomial")

# Save model #
saveRDS(toxicity_model, "2024_1/Outputs/toxicity_model.rds")

## Severe toxicity logistic regression model ##
set.seed(16)
severe_toxicity_model <- glm(severe_toxicity_score ~ gender + status + background + political_affiliation, 
                      data = analysis_data, family = "binomial")

# Save model #
saveRDS(severe_toxicity_model, "2024_1/Outputs/severe_toxicity_model.rds")

## Insult logistic regression model ##
set.seed(16)
insult_model <- glm(insult_score ~ gender + status + background + political_affiliation, 
                             data = analysis_data, family = "binomial")

# Save model #
saveRDS(insult_model, "2024_1/Outputs/insult_model.rds")

## Sexually explicit logistic regression model ##
set.seed(16)
sexually_explicit_model <- glm(sexually_explicit_score ~ gender + status + background + political_affiliation, 
                    data = analysis_data, family = "binomial")

# Save model #
saveRDS(sexually_explicit_model, "2024_1/Outputs/sexually_explicit_model.rds")

## Profanity logistic regression model ##
set.seed(16)
profanity_model <- glm(profanity_score ~ gender + status + background + political_affiliation, 
                    data = analysis_data, family = "binomial")

# Save model #
saveRDS(profanity_model, "2024_1/Outputs/profanity_model.rds")

## Threat logistic regression model ##
set.seed(16)
threat_model <- glm(threat_score ~ gender + status + background + political_affiliation, 
                       data = analysis_data, family = "binomial")

# Save model #
saveRDS(threat_model, "2024_1/Outputs/threat_model.rds")

## Flirtation logistic regression model ##
set.seed(16)
flirtation_model <- glm(flirtation_score ~ gender + status + background + political_affiliation, 
                      data = analysis_data, family = "binomial")

# Save model #
saveRDS(flirtation_model, "2024_1/Outputs/flirtation_model.rds")
