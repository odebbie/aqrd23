#Library and loading----
library(tidyverse)
library(ggplot2)

df <- read_csv("data/broward_agg.csv",
         na = "NA")

df <- df |>
  mutate(race = as.factor(race),
         sex = as.factor(sex),
         charge_degree = as.factor(charge_degree))

df |>
  group_by(aggregated) |>
  count() |>
  filter(n >= 20) |>
  ggplot(aes(x = fct_reorder(aggregated, n), y = n)) +
  geom_col() +
  labs(main = "Distribution of Charges Using New Label Aggregates",
       x = "Charge Type",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) #rotate label

#visualizing the dataset----
df |>
  filter(race == 1 | race == 2) |>
  group_by(race) |>
  count() |>
  ggplot(aes(race, n)) +
  geom_col()

df |>
  filter(aggregated == "Drug-Related") |>
  group_by(drug_type) |>
  count() |>
  ggplot(aes(drug_type, n)) +
  geom_col()

df |>
  filter(aggregated == "Drug-Related") |>
  group_by(drug_type) |>
  count(drug_type, compas_decile_score, .drop = FALSE) |>
  ggplot(aes(x = compas_decile_score, y = n, fill = drug_type)) + 
  geom_bar(position="dodge", stat="identity") #can check if theres a uniform distribution here... GOF test
#NAs from Trafficking unspecified drug


df |>
  filter(aggregated == "Drug-Related",
         !is.na(drug_type)) |>
  group_by(drug_type) |>
  count(drug_type, race, .drop = FALSE) |>
  filter(race %in% 1:2) |>
  ggplot(aes(x = drug_type, y = n, fill = race)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(main = "Bar Plot Comparing Race and Drug Type",
       x = "Charge Type",
       y = "Count") +
  theme(title = element_text(hjust = 0.5), #not showing title??
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

df |>
  filter(race %in% 1:2) |>
  group_by(compas_decile_score) |>
  count(race, compas_decile_score) |>
  ggplot(aes(x = compas_decile_score, y = n, fill = race)) + 
  geom_bar(position="dodge", stat="identity")