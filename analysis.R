#Library and loading----
library(glmnet)
library(glmnetUtils)
library(pROC)
library(rsample)
library(ModelMetrics)
library(ggplot2)
library(tidyverse)


set.seed(438)

df <- read_csv("data/broward_agg.csv",
               na = "NA")

df <- df |>
  mutate(race = as.factor(race),
         sex = as.factor(sex),
         charge_degree = as.factor(charge_degree),
         charge_id = as.factor(charge_id))

#not getting rid of other races because we need to consider overall accuracy

#TOP 1: The Paper's Linear Models----
#1: 7-predictor logistic algorithm assessment (uses full dataset)
#initialize vars
accuracy <- NULL #overall accuracy
acc_white <- NULL #accuracy white
acc_black <- NULL #accuracy black
fp_black <- NULL #false positive black
fp_white <- NULL #false positive white
fn_black <- NULL #false negative black
fn_white <- NULL #false negative white

for (i in 1:1000) {
  #random split
  splitobj <- initial_split(df, prop = 4/5) #initial object, 80/20 split
  train <- training(splitobj)
  test <- testing(splitobj)
  
  #train model
  sev_pred <- glm(two_year_recid ~ age + sex + priors_count + juv_fel_count +
                    juv_misd_count + charge_degree + charge_id, data=train, family="binomial")
  
  #make prediction
  test$charge_id[which(!(test$charge_id %in% unique(train$charge_id)))] <- NA #editing test data
  #fixes error with new levels...cant predict for these, so just ignore?
  
  y_hat <- predict(sev_pred, newdata=test, type="response")
  y_hat <- as.numeric(y_hat > 0.5) #binary predictions
  
  #get ids in this test batch
  y <- df[which(df$id %in% test$id),] |>
    select(race, two_year_recid) |> #add race variables so i can filter by this condition
    rename(y_act = two_year_recid)
  
  #combine obs
  y$y_hat <- y_hat
  y <- na.omit(y, cols="y_hat")#get rid of NA rows...
  
  #store relevant values
  accuracy[i] <- mean(y$y_act == y$y_hat) #overall accuracy for this round
  acc_black[i] <- mean(y$y_act[y$race == 2] == y$y_hat[y$race == 2])
  acc_white[i] <- mean(y$y_act[y$race == 1] == y$y_hat[y$race == 1])
  
  #Confusion Matrix Black and white
  cm_black <- confusionMatrix(y$y_act[(y$race == 2)], y$y_hat[(y$race == 2)])
  cm_white <- confusionMatrix(y$y_act[(y$race == 1)], y$y_hat[(y$race == 1)])
  
  #False Pos and False Neg
  fn_black[i] <- cm_black[2,1]/sum(cm_black[2,]) #2nd row is "actually true" row
  fn_white[i] <- cm_white[2,1]/sum(cm_white[2,])
  fp_black[i] <- cm_black[1,2]/sum(cm_black[1,]) #1st row is "actually false" row
  fp_white[i] <- cm_white[1,2]/sum(cm_white[1,])
  
}

#2: 2-predictor logistic algorithm assessment (uses full dataset)
#bootstrap standard errors

for (i in 1:1000) { #bootstrapped = need to resample?
  #random split
  splitobj <- initial_split(df, prop = 4/5) #initial object, 80/20 split
  train <- training(splitobj)
  test <- testing(splitobj)
  
  #train <- sample_n(df, size = floor(0.8*dim(df)[1]), replace = TRUE) #bootstrap
  #test <- sample_n(df, size = floor(0.2*dim(df)[1]), replace = TRUE) #bootstrap
  
  #train model
  two_pred <- glm(two_year_recid ~ age + priors_count, data=train, family="binomial")
  
  #make prediction
  y_hat <- predict(two_pred, newdata=test, type="response") #needs to include race info
  y_hat <- as.numeric(y_hat > 0.5) #binary predictions
  
  #get ids in this test batch
  y <- df[which(df$id %in% test$id),] |>
    #to use bootstrap version, need to change from which to get duplicates
    select(race, two_year_recid) |> #add race variables so i can filter by this condition
    rename(y_act = two_year_recid)
  
  #combine obs
  y$y_hat <- y_hat
  
  #store relevant values
  accuracy[i] <- mean(y$y_act == y$y_hat) #overall accuracy for this round
  acc_black[i] <- mean(y$y_act[y$race == 2] == y$y_hat[y$race == 2])
  acc_white[i] <- mean(y$y_act[y$race == 1] == y$y_hat[y$race == 1])
  #Confusion Matrix Black and white
  cm_black <- confusionMatrix(y$y_act[(y$race == 2)], y$y_hat[(y$race == 2)])
  cm_white <- confusionMatrix(y$y_act[(y$race == 1)], y$y_hat[(y$race == 1)])
  
  #False Pos and False Neg
  fn_black[i] <- cm_black[2,1]/sum(cm_black[2,]) #2nd row is "actually true" row
  fn_white[i] <- cm_white[2,1]/sum(cm_white[2,])
  fp_black[i] <- cm_black[1,2]/sum(cm_black[1,]) #1st row is "actually false" row
  fp_white[i] <- cm_white[1,2]/sum(cm_white[1,])
  
}

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