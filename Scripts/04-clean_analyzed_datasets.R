#### Preamble ####
# Purpose: Clean up and rejoin analyzed datasets
# Author: Inessa De Angelis
# Date: 10 October 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT

#### Workspace setup ####
library(tidyverse)

## Read in 2024_1 YouTube comment data ##
LPC_analyzed <- read_csv("2024_1/Inputs/Data/Perspective/LPC_analyzed.csv")
NDP_analyzed <- read_csv("2024_1/Inputs/Data/Perspective/NDP_analyzed.csv")
Other_analyzed <- read_csv("2024_1/Inputs/Data/Perspective/Other_analyzed.csv")

list_of_files <- list.files(path = "2024_1/Inputs/Data/Perspective/CPC/", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
CPC_analyzed <- readr::read_csv(list_of_files, id = "file_name") |>
  drop_na(Comment) |>
  select(-c(file_name, PublishedAt, ReplyCount, LikeCount, Gender))

list_of_files <- list.files(path = "2024_1/Inputs/Data/Perspective/Pierre/", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
PP_analyzed <- readr::read_csv(list_of_files, id = "file_name") |>
  drop_na(Comment) |>
  select(-c(file_name, PublishedAt, ReplyCount, LikeCount, Gender))

## Read in and tidy demographic data ##
usernames <- read_csv("2024_2/Inputs/Data/Demographic/Cdn_Politicians_YouTube_2025-1.csv")
usernames <- unite(usernames, Name, c(first_name, last_name), sep = " ")
usernames <- usernames |> select(-c(honorific_title, profile_URL))

#### Clean datasets ####
## LPC ##
LPC_analyzed <- LPC_analyzed |>
  drop_na(Comment) |>
  select(-c(PublishedAt, ReplyCount, LikeCount, Gender))

# Fix name #
LPC_analyzed <- LPC_analyzed |>
  mutate(Name = case_when(
    Name == "Mark Garretsen" ~ "Mark Gerretsen",
    TRUE ~ Name)) 

## NDP ##
NDP_analyzed <- NDP_analyzed |>
  drop_na(Comment) |>
  select(-c(PublishedAt, ReplyCount, LikeCount, Gender))

## Other ##
Other_analyzed <- Other_analyzed |>
  drop_na(Comment) |>
  select(-c(PublishedAt, ReplyCount, LikeCount, Gender))

## Combine datasets ##
all_analyzed <- rbind(LPC_analyzed, NDP_analyzed, Other_analyzed, CPC_analyzed, PP_analyzed)

#### Join in demographic data ####
all_analyzed_final <- all_analyzed |> 
  left_join(usernames, by = join_by(Name==Name)) |>
  select(-c(username, age, lgbtq_out, status, constituency, province_territory))

## Save dataset ##
write_csv(all_analyzed_final, "2024_1/Outputs/Data/Perspective/all_analyzed.csv")
