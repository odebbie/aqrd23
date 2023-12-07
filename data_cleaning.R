library(tidyverse)
library(ggplot2)

dat <- read_csv("data/BROWARD_CLEAN.csv") |>
  select(-c(14:16)) #data by person
charge_type <- read_csv("data/CHARGE_ID.csv") #information on charge type
charge_type <- charge_type |>
  filter(!is.na(charge_id)) |>
  select(c(1:3))
joined_dat <- left_join(dat, charge_type)
joined_dat <- joined_dat |> rename(charge_name = mturk_charge_name)

joined_dat |>
  group_by(charge_id) |>
  count() |>
  arrange(desc(n)) |>
  ggplot(aes(x = charge_id, y = n)) +
  geom_col()
#most common groups from study
joined_dat |>
  group_by(charge_name) |>
  count() |>
  arrange(desc(n)) |>
  filter(n >= 20,
         n < 1000) |>
  print(n = 200) |>
  ggplot(aes(x = charge_name, y = n)) +
  geom_col()

#i need a way to validate the way i group together charges
#large grouping----

#labeling by drug type
joined_dat <- joined_dat |>
  mutate(drug_type = case_when(
    grepl("Cannabis", charge_name, ignore.case = TRUE) ~ "Cannabis/Marijuana",
    grepl("Marijuana", charge_name, ignore.case = TRUE) ~ "Cannabis/Marijuana",
    grepl("Oxycodone", charge_name, ignore.case = TRUE) ~ "Oxycodone",
    grepl("Morphine", charge_name, ignore.case = TRUE) ~ "Morphine",
    grepl("Meth", charge_name, ignore.case = TRUE) ~ "Meth",
    grepl("LSD", charge_name, ignore.case = TRUE) ~ "LSD",
    grepl("Heroin", charge_name, ignore.case = TRUE) ~ "Heroin",
    grepl("Cocaine", charge_name, ignore.case = TRUE) ~ "Cocaine",
    grepl("Ecstasy", charge_name, ignore.case = TRUE) ~ "Ecstasy",
    grepl("Controlled Substance", charge_name, ignore.case = TRUE) ~ "Controlled Substance",
    .default = NA
  ))

joined_dat <- joined_dat |>
  mutate(main_groups = case_when(
    grepl("Sex|Sexual|Molestation|Voyeurism|porn", charge_name, ignore.case = TRUE) ~ "Sex Crimes",
    grepl("Assault", charge_name, ignore.case = TRUE) ~ "Assault",
    grepl("Battery", charge_name, ignore.case = TRUE) ~ "Battery",
    grepl("Marijuana|Cannabis|Oxycodone|Morphine|Meth|LSD|Heroin|Cocaine|Controlled Substance|Drug|Ecstasy",
          charge_name, ignore.case = TRUE) ~ "Drug-Related",
    grepl("Fraud|Forgery|Counterfeit", charge_name, ignore.case = TRUE) ~ "Fraud/Forgery",
    grepl("Neglect", charge_name, ignore.case = TRUE) ~ "Neglect",
    grepl("Abuse|Domestic Violence", charge_name, ignore.case = TRUE) ~ "Abuse", #including domestic violence
    grepl("Driving with a|Failure to Register|without a valid|vehicle with",
          charge_name, ignore.case = TRUE) ~ "Car Verification Violations",
    grepl("arrest case no charge",
          charge_name, ignore.case = TRUE) ~ "No Charge",
    grepl("robbery|theft|shoplifting",
          charge_name, ignore.case = TRUE) ~ "Robbery/Theft",
    grepl("Carrying an Open Beverage in Public|Consuming Alcoholic Beverage in Public|Alcoholic Beverage Violation|Intoxicat",
          charge_name, ignore.case=TRUE) ~ "Non-DUI, Alcohol-Related",
    grepl("Resisting|obey police officer",
          charge_name, ignore.case=TRUE) ~ "Resisting",
    grepl("Burglary",
          charge_name, ignore.case=TRUE) ~ "Burglary",
    grepl("Tampering|Refusing to|False information",
          charge_name, ignore.case=TRUE) ~ "Obstruction of Justice",
    grepl("Murder|Manslaughter",
          charge_name, ignore.case=TRUE) ~ "Murder/Manslaughter",
    grepl("Driving Under the Influence",
          charge_name, ignore.case=TRUE) ~ "DUI",
    grepl("Loitering",
          charge_name, ignore.case=TRUE) ~ "Loitering",
    grepl("Trespassing",
          charge_name, ignore.case=TRUE) ~ "Trespassing",
    grepl("Criminal Damage",
          charge_name, ignore.case=TRUE) ~ "Criminal Damage",
    grepl("Prostitution",
          charge_name, ignore.case=TRUE) ~ "Prostitution",
    grepl("Stalking",
          charge_name, ignore.case=TRUE) ~ "Stalking",
    grepl("Fleeing|Escape|Extradition",
          charge_name, ignore.case=TRUE) ~ "Fleeing and Aiding Escape",
    grepl("Threat",
          charge_name, ignore.case=TRUE) ~ "Threatening a Public Servant",
    grepl("False Imprisonment",
          charge_name, ignore.case=TRUE) ~ "False Imprisonment",
    grepl("Carrying a Concealed Weapon",
          charge_name, ignore.case=TRUE) ~ "Carrying a Concealed Weapon",
    grepl("Harassment",
          charge_name, ignore.case=TRUE) ~ "Harassment",
    grepl("Alcohol Under 21|Tobacco Product Under 18",
          charge_name, ignore.case=TRUE) ~ "Underage Consumption",
    grepl("Animal Cruelty",
          charge_name, ignore.case=TRUE) ~ "Animal Cruelty",
    grepl("Arson",
          charge_name, ignore.case=TRUE) ~ "Arson",
    grepl("Reckless Driving",
          charge_name, ignore.case=TRUE) ~ "Reckless Driving",
    .default = "Other"
  ))
#grouping accuracy for presence of weapon
joined_dat <- joined_dat |>
  mutate(weapon = case_when(
    #case where its robbery/theft, assault, or battery AND they used a weapon
    grepl("robbery/theft|assault|battery", main_groups, ignore.case = TRUE) 
    & grepl("with a weapon|with a deadly weapon", charge_name, ignore.case = TRUE) ~ TRUE,
    #case where its robbery/theft, assault, or battery and they DID NOT USE a weapon
    grepl("robbery/theft|assault|battery", main_groups, ignore.case = TRUE) ~ FALSE,
    .default = NA))

#type of interaction with drug
joined_dat <- joined_dat |>
  mutate(drug_interaction = case_when(
    grepl("Drug Trafficking", charge_name, ignore.case = TRUE) ~ "Drug Trafficking",
    grepl("Paraphernalia", charge_name, ignore.case = TRUE) ~ "Paraphernalia",
    grepl("Possession", charge_name, ignore.case = TRUE) &
      grepl("Drug-Related", main_groups, ignore.case = TRUE) ~ "Possession",
    grepl("Dealing", charge_name, ignore.case = TRUE) &
      grepl("Drug-Related", main_groups, ignore.case = TRUE) ~ "Dealing",
    grepl("Purchasing", charge_name, ignore.case = TRUE) &
      grepl("Drug-Related", main_groups, ignore.case = TRUE) ~ "Purchasing",
    grepl("Manufacturing", charge_name, ignore.case = TRUE) &
      grepl("Drug-Related", main_groups, ignore.case = TRUE) ~ "Manufacturing",
    .default = NA))

#save

df <- joined_dat |>
  select(-c(c_charge_desc)) |>
  rename(mturk_charge = charge_name,
         aggregated = main_groups,
         charge_degree = "charge_degree (misd/fel)")

write.csv(df, file = "data\\broward_agg.csv", na = "NA",
          row.names=FALSE)
