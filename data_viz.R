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
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + #rotate label 
  labs(title = "Distribution of Charges Using New Label Aggregates",
       x = "Charge Type",
       y = "Count")
  

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
  geom_bar(position="dodge", stat="identity") + #can check if theres a uniform distribution here... GOF test
#NAs from Trafficking unspecified drug
  labs(title = "Distribution of Drug Usage",
       x = "COMPAS Decile Score",
       y = "Count",
       fill = "Drug Type")


df |>
  filter(aggregated == "Drug-Related",
         !is.na(drug_type)) |>
  group_by(drug_type) |>
  count(drug_type, race, .drop = FALSE) |>
  filter(race %in% 1:2) |>
  ggplot(aes(x = drug_type, y = n, fill = race)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Bar Plot Comparing Race and Drug Type",
       x = "Charge Type",
       y = "Count",
       fill = "Race") +
  scale_fill_discrete("Race", 
                      labels=c("White", "Black")) +
  theme(title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

df |>
  filter(race %in% 1:2) |>
  group_by(compas_decile_score) |>
  count(race, compas_decile_score) |>
  ggplot(aes(x = compas_decile_score, y = n, fill = race)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Bar Plot Comparing Race and Decile Score",
       x = "COMPAS Decile Score",
       y = "Count",
       fill = "Race") +
  scale_fill_discrete("Race", 
                      labels=c("White", "Black")) +
  scale_x_discrete(limits = factor(1:10))

#compas guess and correct
df |>
  filter(race %in% 1:2) |>
  group_by(race) |>
  ggplot(aes(x = priors_count, y = compas_guess, color = race)) +
  geom_point(position = position_jitter(width = 0.05, height = 0.05), alpha = 0.5)

df |>
  filter(race %in% 1:2) |>
  group_by(race) |>
  ggplot(aes(x = priors_count, y = compas_correct, color = race)) +
  geom_point(position = position_jitter(width = 0.05, height = 0.05), alpha = 0.5)

# you see that, btwn the graphs, theres an increase in blue at the bottom for the correct graph, showing how Black people are more likely to be misguessed

#FOCUS: i want to figure out a relationship btwn priors/age and another variable...
df |>
  filter(!is.na(drug_interaction)) |>
  group_by(drug_interaction) |>
  count(drug_interaction, race, .drop = FALSE) |>
  filter(race %in% 1:2) #not much difference here~


