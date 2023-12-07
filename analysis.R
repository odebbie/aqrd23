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

#1: The Paper's Linear Models----
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

#3: My 7-predictor model (different aggregation)
train_sub <- train |>
  select(two_year_recid, age, sex, priors_count, juv_fel_count, juv_misd_count, charge_degree, aggregated) #fixes contrasts error

my_sev <- glm(two_year_recid ~.-aggregated, data=train_sub, family="binomial") #fixing error w rank deficiency
my_sev$xlevels[["aggregated"]] <- union(my_sev$xlevels[["aggregated"]], levels(test$aggregated))

y_hat <- predict(my_sev, newdata=test, type="response")
y_hat <- as.numeric(y_hat > 0.5)
mean(y_hat == df$two_year_recid[df$id %in% test$id]) #accuracy

roc(df$two_year_recid[df$id %in% train$id] ~ my_sev$fitted.values, plot = TRUE, print.auc = TRUE)
