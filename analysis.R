#Library and loading----
library(ggplot2)
library(glmnet)
library(glmnetUtils)
library(tidyverse)
library(pROC)
library(rsample)

set.seed(438)

df <- read_csv("data/broward_agg.csv",
               na = "NA")

df <- df |>
  mutate(race = as.factor(race),
         sex = as.factor(sex),
         charge_degree = as.factor(charge_degree),
         charge_id = as.factor(charge_id))

# mturk <- read_csv("data/MTURK_CHARGE_DESC.csv",
#                   na = "NA") |>
#   select(-c(3:25)) |>
#   filter(!is.na(charge_id)) #MTURK variables chosen
# 
# df_subset <- df |>
#   filter(charge_id %in% mturk$charge_id) #using charges from paper

#train test split----
splitobj <- initial_split(df, prop = 4/5) #initial object, 80/20 split
train <- training(splitobj)
test <- testing(splitobj)

#TOP 1: The Paper's Linear Models----
#1: 7-predictor logistic algorithm assessment (uses full dataset)
train_sub <- train |>
  select(two_year_recid, age, sex, priors_count, juv_fel_count, juv_misd_count, charge_degree, charge_id) #fixes contrasts error

#sev_pred <- glm(two_year_recid ~ age + sex + priors_count
#    + juv_fel_count + juv_misd_count + charge_degree + charge_id,
#    data = train_sub, family = "binomial")

sev_pred <- glm(two_year_recid ~.-charge_id, data=train_sub, family="binomial") #fixing error w rank deficiency
sev_pred$xlevels[["charge_id"]] <- union(sev_pred$xlevels[["charge_id"]], levels(test$charge_id))

y_hat <- predict(sev_pred, newdata=test, type="response")
y_hat <- as.numeric(y_hat > 0.5)
mean(y_hat == df$two_year_recid[df$id %in% test$id]) #accuracy

roc(df$two_year_recid[df$id %in% train$id] ~ sev_pred$fitted.values, plot = TRUE, print.auc = TRUE)

#2: 2-predictor logistic algorithm assessment (uses full dataset)
two_pred <- glm(two_year_recid ~ age + priors_count, data=train, family="binomial") #fixing error w rank deficiency

y_hat <- predict(two_pred, newdata=test, type="response")
y_hat <- as.numeric(y_hat > 0.5)
mean(y_hat == df$two_year_recid[df$id %in% test$id]) #accuracy

roc(df$two_year_recid[df$id %in% train$id] ~ two_pred$fitted.values, plot = TRUE, print.auc = TRUE)
#bootstrap standard errors

#3: My 7-predictor model (different aggregation)
train_sub <- train |>
  select(two_year_recid, age, sex, priors_count, juv_fel_count, juv_misd_count, charge_degree, aggregated) #fixes contrasts error

my_sev <- glm(two_year_recid ~.-aggregated, data=train_sub, family="binomial") #fixing error w rank deficiency
my_sev$xlevels[["aggregated"]] <- union(my_sev$xlevels[["aggregated"]], levels(test$aggregated))

y_hat <- predict(my_sev, newdata=test, type="response")
y_hat <- as.numeric(y_hat > 0.5)
mean(y_hat == df$two_year_recid[df$id %in% test$id]) #accuracy

roc(df$two_year_recid[df$id %in% train$id] ~ my_sev$fitted.values, plot = TRUE, print.auc = TRUE)

#TOP 2: Given race, white people have a negative correlation with decile score, while Black people have a uniform one----
dec_white <- df |>
  filter(race == 1) |>
  group_by(compas_decile_score) |>
  count(race, compas_decile_score)
chisq.test(dec_white$n) #probability uniform
chisq.test(dec_white$n[1:9]) #probability uniform without 10th decile

dec_black <- df |>
  filter(race == 2) |>
  group_by(compas_decile_score) |>
  count(compas_decile_score)
chisq.test(dec_black$n) #probability uniform dist
chisq.test(dec_black$n[1:9]) #probability uniform without 10th decile

#TOP 3: Finding vars that have a similarly poor prediction w race----
lm(compas_decile_score ~ drug_type*race, df |> filter(race %in% 1:2))
lm(compas_correct ~ drug_type, df)
lm(compas_correct ~ drug_type, df |> filter(race %in% 1:2)) #also run as OLS

correct_drug <- glm(compas_correct ~ decile*drug_type |> filter(drug_type == "Cocaine"), data=train, family="binomial")
correct_drug_sub <- glm(compas_correct ~ drug_type, data=train |> filter(race %in% 1:2), family="binomial")
#exponentiate coeffs bc log odds
correct_drug_race <- glm(compas_correct ~ drug_type*race, data=train |> filter(race %in% 1:2), family="binomial")

y_hat_drug <- predict(correct_drug, newdata=train, type="response")
predict(correct_drug_sub, newdata=test |> filter(race %in% 1:2), type="response")
predict(correct_drug_race, newdata=test |> filter(race %in% 1:2), type="response")

plot(train, y_hat_drug, pch = 16, xlab = "dat", ylab = "VS")
lines(xdisp, ydisp)

#since i know that priors can be used to estimate correct guess, can instrument for...?