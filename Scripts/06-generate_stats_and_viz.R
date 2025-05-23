#### Preamble ####
# Purpose: Calculate summary statistics & create figures
# Author: Inessa De Angelis
# Date: 12 October 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(kableExtra)
library(marginaleffects)

## Read in datset ##
all_analyzed <- read_csv("2024_1/Outputs/Data/Perspective/all_analyzed.csv")
all_analyzed <- all_analyzed |> rename(status = profile_status)

## Read in models ##
toxicity_model <- read_rds(here::here("2024_1/Outputs/toxicity_model.rds"))
threat_model <- read_rds(here::here("2024_1/Outputs/threat_model.rds"))
sexually_explicit_model <- read_rds(here::here("2024_1/Outputs/sexually_explicit_model.rds"))
severe_toxicity_model <- read_rds(here::here("2024_1/Outputs/severe_toxicity_model.rds"))
profanity_model <- read_rds(here::here("2024_1/Outputs/profanity_model.rds"))
insult_model <- read_rds(here::here("2024_1/Outputs/insult_model.rds"))
flirtation_model <- read_rds(here::here("2024_1/Outputs/flirtation_model.rds"))

#### Results & discussion: Gender ####
## Descriptive stats ##
scores_by_gender <- all_analyzed |> 
  group_by(gender) |> 
  summarize(
    Toxicity = (sum(toxicity_score >= 0.7) / n()) * 100,
    Severe_toxicity = (sum(severe_toxicity_score >= 0.7) / n()) * 100,
    Insult = (sum(insult_score >= 0.7) / n()) * 100,
    Sexually_explicit = (sum(sexually_explicit_score >= 0.7) / n()) * 100,
    Profanity = (sum(profanity_score >= 0.7) / n()) * 100,
    Threat = (sum(threat_score >= 0.7) / n()) * 100,
    Flirtation = (sum(flirtation_score >= 0.7) / n()) * 100) |> 
  select(gender, Toxicity, Insult, Profanity, Flirtation, Sexually_explicit, Severe_toxicity, Threat) |>
  arrange(desc(Toxicity)) |> 
  mutate(across(where(is.numeric), ~ round(., 2))) |>
  kable(
    col.names = c("Gender", "Toxicity", "Insult", "Profanity", "Sexually Explicit", "Flirtation", "Severe Toxicity", "Threat"),
    booktabs = TRUE)
scores_by_gender 

## Logistic regression average predictions ##
t <- avg_predictions(toxicity_model, by = "gender")
t <- t |> 
  mutate(gender = case_when(
    gender == "0" ~ "Woman",
    gender == "1" ~ "Man"),
    attribute = "Toxicity",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

i <- avg_predictions(insult_model, by = "gender")
i <- i |> 
  mutate(gender = case_when(
    gender == "0" ~ "Woman",
    gender == "1" ~ "Man"),
    attribute = "Insult",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

p <- avg_predictions(profanity_model, by = "gender")
p <- p |> 
  mutate(gender = case_when(
    gender == "0" ~ "Woman",
    gender == "1" ~ "Man"),
    attribute = "Profanity",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

se <- avg_predictions(sexually_explicit_model, by = "gender")
se <- se |> 
  mutate(gender = case_when(
    gender == "0" ~ "Woman",
    gender == "1" ~ "Man"),
    attribute = "Sexually explicit",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

f <- avg_predictions(flirtation_model, by = "gender")
f <- f |> 
  mutate(gender = case_when(
    gender == "0" ~ "Woman",
    gender == "1" ~ "Man"),
    attribute = "Flirtation",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

st <- avg_predictions(severe_toxicity_model, by = "gender")
st <- st |> 
  mutate(gender = case_when(
    gender == "0" ~ "Woman",
    gender == "1" ~ "Man"),
    attribute = "Severe Toxicity",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

tr <- avg_predictions(threat_model, by = "gender")
tr <- tr |> 
  mutate(gender = case_when(
    gender == "0" ~ "Woman",
    gender == "1" ~ "Man"),
    attribute = "Threat",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

combined <- rbind(t, i, p, se, f, st, tr) |>
  select(attribute, gender, estimate, p.value)

combined |> kable(col.names = c("Attribute", "Gender", "Estimate", "P value"))

#### Results & discussion: Status ####
## Descriptive stats ##
scores_by_status <- all_analyzed |>
  group_by(status) |>
  summarize(
    Toxicity = (sum(toxicity_score >= 0.7) / n()) * 100,
    Severe_toxicity = (sum(severe_toxicity_score >= 0.7) / n()) * 100,
    Insult = (sum(insult_score >= 0.7) / n()) * 100,
    Sexually_explicit = (sum(sexually_explicit_score >= 0.7) / n()) * 100,
    Profanity = (sum(profanity_score >= 0.7) / n()) * 100,
    Threat = (sum(threat_score >= 0.7) / n()) * 100,
    Flirtation = (sum(flirtation_score >= 0.7) / n()) * 100) |> 
  select(status, Toxicity, Insult, Profanity, Flirtation, Sexually_explicit, Threat, Severe_toxicity) |>
  arrange(desc(Toxicity)) |>
  mutate(across(where(is.numeric), ~ round(., 2))) |>
  kable(
    col.names = c("Status", "Toxicity", "Insult", "Profanity", "Sexually Explicit", "Flirtation", "Severe Toxicity", "Threat"),
    booktabs = TRUE)
scores_by_status

## Logistic regression average predictions ##
t <- avg_predictions(toxicity_model, by = "status")
t <- t |> 
  mutate(status = case_when(
    status == "0" ~ "Low",
    status == "1" ~ "High"),
    attribute = "Toxicity",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

i <- avg_predictions(insult_model, by = "status")
i <- i |> 
  mutate(status = case_when(
    status == "0" ~ "Low",
    status == "1" ~ "High"),
    attribute = "Insult",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

p <- avg_predictions(profanity_model, by = "status")
p <- p |> 
  mutate(status = case_when(
    status == "0" ~ "Low",
    status == "1" ~ "High"),
    attribute = "Profanity",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

se <- avg_predictions(sexually_explicit_model, by = "status")
se <- se |> 
  mutate(status = case_when(
    status == "0" ~ "Low",
    status == "1" ~ "High"),
    attribute = "Sexually explicit",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

f <- avg_predictions(flirtation_model, by = "status")
f <- f |> 
  mutate(status = case_when(
    status == "0" ~ "Low",
    status == "1" ~ "High"),
    attribute = "Flirtation",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

st <- avg_predictions(severe_toxicity_model, by = "status")
st <- st |> 
  mutate(status = case_when(
    status == "0" ~ "Low",
    status == "1" ~ "High"),
    attribute = "Severe Toxicity",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

tr <- avg_predictions(threat_model, by = "status")
tr <- tr |> 
  mutate(status = case_when(
    status == "0" ~ "Low",
    status == "1" ~ "High"),
    attribute = "Threat",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

combined <- rbind(t, i, p, se, f, st, tr) |>
  select(attribute, status, estimate, p.value)

combined |> kable(col.names = c("Attribute", "Status", "Estimate", "P value"))

#### Results & discussion: Background ####
## Descriptive stats ##
scores_by_background <- all_analyzed |>
  group_by(background) |>
  summarize(
    Toxicity = (sum(toxicity_score >= 0.7) / n()) * 100,
    Severe_toxicity = (sum(severe_toxicity_score >= 0.7) / n()) * 100,
    Insult = (sum(insult_score >= 0.7) / n()) * 100,
    Sexually_explicit = (sum(sexually_explicit_score >= 0.7) / n()) * 100,
    Profanity = (sum(profanity_score >= 0.7) / n()) * 100,
    Threat = (sum(threat_score >= 0.7) / n()) * 100,
    Flirtation = (sum(flirtation_score >= 0.7) / n()) * 100) |> 
  select(background, Toxicity, Insult, Profanity, Flirtation, Sexually_explicit, Threat, Severe_toxicity) |>
  arrange(desc(Toxicity)) |>
  mutate(across(where(is.numeric), ~ round(., 2))) |>
  kable(
    col.names = c("Background", "Toxicity", "Insult", "Profanity", "Sexually Explicit", "Flirtation", "Severe Toxicity", "Threat"),
    booktabs = TRUE)
scores_by_background

## Logistic regression average predictions ##
t <- avg_predictions(toxicity_model, by = "background")
t <- t |> 
  mutate(background = case_when(
    background == "0" ~ "White",
    background == "1" ~ "Racialized",
    background == "2" ~ "Indigenous"),
    attribute = "Toxicity",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

i <- avg_predictions(insult_model, by = "background")
i <- i |> 
  mutate(background = case_when(
    background == "0" ~ "White",
    background == "1" ~ "Racialized",
    background == "2" ~ "Indigenous"),
    attribute = "Insult",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

p <- avg_predictions(profanity_model, by = "background")
p <- p |> 
  mutate(background = case_when(
    background == "0" ~ "White",
    background == "1" ~ "Racialized",
    background == "2" ~ "Indigenous"),
    attribute = "Profanity",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

se <- avg_predictions(sexually_explicit_model, by = "background")
se <- se |> 
  mutate(background = case_when(
    background == "0" ~ "White",
    background == "1" ~ "Racialized",
    background == "2" ~ "Indigenous"),
    attribute = "Sexually explicit",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

f <- avg_predictions(flirtation_model, by = "background")
f <- f |> 
  mutate(background = case_when(
    background == "0" ~ "White",
    background == "1" ~ "Racialized",
    background == "2" ~ "Indigenous"),
    attribute = "Flirtation",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

st <- avg_predictions(severe_toxicity_model, by = "background")
st <- st |> 
  mutate(background = case_when(
    background == "0" ~ "White",
    background == "1" ~ "Racialized",
    background == "2" ~ "Indigenous"),
    attribute = "Severe toxicity",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

tr <- avg_predictions(threat_model, by = "background")
tr <- tr |> 
  mutate(background = case_when(
    background == "0" ~ "White",
    background == "1" ~ "Racialized",
    background == "2" ~ "Indigenous"),
    attribute = case_when(
      background == "White" ~ "Threat",
      background == "Racialized" ~ "Threat",
      background == "Indigenous" ~ "Threat"),
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |>
  select(attribute, everything())

combined <- rbind(t, i, p, se, f, st, tr) |>
  select(attribute, background, estimate, p.value)

combined |> kable(col.names = c("Attribute", "Background", "Estimate", "P value"))

#### Results & discussion: Political affiliation ####
## Descriptive stats ##
scores_by_pol <- all_analyzed |>
  group_by(political_affiliation) |>
  summarize(
    Toxicity = (sum(toxicity_score >= 0.7) / n()) * 100,
    Severe_toxicity = (sum(severe_toxicity_score >= 0.7) / n()) * 100,
    Insult = (sum(insult_score >= 0.7) / n()) * 100,
    Sexually_explicit = (sum(sexually_explicit_score >= 0.7) / n()) * 100,
    Profanity = (sum(profanity_score >= 0.7) / n()) * 100,
    Threat = (sum(threat_score >= 0.7) / n()) * 100,
    Flirtation = (sum(flirtation_score >= 0.7) / n()) * 100) |> 
  select(political_affiliation, Toxicity, Insult, Profanity, Flirtation, Sexually_explicit, Threat, Severe_toxicity) |>
  arrange(desc(Toxicity)) |>
  mutate(across(where(is.numeric), ~ round(., 2))) |>
  kable(
    col.names = c("Political affiliation", "Toxicity", "Insult", "Profanity", "Sexually Explicit", "Flirtation", "Severe Toxicity", "Threat"),
    booktabs = TRUE)
scores_by_pol

## Logistic regression average predictions ##
t <- avg_predictions(toxicity_model, by = "political_affiliation") 
t <- t |> 
  mutate(
    political_affiliation = case_when(
      political_affiliation == "0" ~ "Liberal",
      political_affiliation == "1" ~ "NDP",
      political_affiliation == "2" ~ "Conservative",
      political_affiliation == "3" ~ "Green Party",
      political_affiliation == "4" ~ "Independent"),
    attribute = "Toxicity",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |> 
  select(attribute, everything())

i <- avg_predictions(insult_model, by = "political_affiliation")
i <- i |> 
  mutate(
    political_affiliation = case_when(
      political_affiliation == "0" ~ "Liberal",
      political_affiliation == "1" ~ "NDP",
      political_affiliation == "2" ~ "Conservative",
      political_affiliation == "3" ~ "Green Party",
      political_affiliation == "4" ~ "Independent"),
    attribute = "Insult",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |> 
  select(attribute, everything()) 

p <- avg_predictions(profanity_model, by = "political_affiliation")
p <- p |> 
  mutate(
    political_affiliation = case_when(
      political_affiliation == "0" ~ "Liberal",
      political_affiliation == "1" ~ "NDP",
      political_affiliation == "2" ~ "Conservative",
      political_affiliation == "3" ~ "Green Party",
      political_affiliation == "4" ~ "Independent"),
    attribute = "Profanity",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |> 
  select(attribute, everything()) 

se <- avg_predictions(sexually_explicit_model, by = "political_affiliation")
se <- se |> 
  mutate(
    political_affiliation = case_when(
      political_affiliation == "0" ~ "Liberal",
      political_affiliation == "1" ~ "NDP",
      political_affiliation == "2" ~ "Conservative",
      political_affiliation == "3" ~ "Green Party",
      political_affiliation == "4" ~ "Independent"),
    attribute = "Sexually explicit",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |> 
  select(attribute, everything()) 

f <- avg_predictions(flirtation_model, by = "political_affiliation")
f <- f |> 
  mutate(
    political_affiliation = case_when(
      political_affiliation == "0" ~ "Liberal",
      political_affiliation == "1" ~ "NDP",
      political_affiliation == "2" ~ "Conservative",
      political_affiliation == "3" ~ "Green Party",
      political_affiliation == "4" ~ "Independent"),
    attribute = "Flirtation",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |> 
  select(attribute, everything()) 

st <- avg_predictions(severe_toxicity_model, by = "political_affiliation")
st <- st |> 
  mutate(
    political_affiliation = case_when(
      political_affiliation == "0" ~ "Liberal",
      political_affiliation == "1" ~ "NDP",
      political_affiliation == "2" ~ "Conservative",
      political_affiliation == "3" ~ "Green Party",
      political_affiliation == "4" ~ "Independent"),
    attribute = "Severe Toxicity",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |> 
  select(attribute, everything()) 

tr <- avg_predictions(threat_model, by = "political_affiliation")
tr <- tr |> 
  mutate(
    political_affiliation = case_when(
      political_affiliation == "0" ~ "Liberal",
      political_affiliation == "1" ~ "NDP",
      political_affiliation == "2" ~ "Conservative",
      political_affiliation == "3" ~ "Green Party",
      political_affiliation == "4" ~ "Independent"),
    attribute = "Threat",
    across(c(estimate, p.value, s.value, conf.low, conf.high), round, 3)) |> 
  select(attribute, everything()) 

combined <- rbind(t, i, p, se, f, st, tr) |>
  select(attribute, political_affiliation, estimate, p.value)

combined |> kable(col.names = c("Attribute", "Political affiliation", "Estimate", "P value"))