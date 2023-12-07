#Loading----
library(gt)
library(tidyverse)
library(ggplot2)
library(car)


dat <- read_csv("data/BROWARD_CLEAN.csv") |>
  select(-c(14:16)) #data by person
charge_type <- read_csv("data/CHARGE_ID.csv") #information on charge type
charge_type <- charge_type |>
  filter(!is.na(charge_id)) |>
  select(c(1:3))
joined_dat <- left_join(dat, charge_type)
joined_dat <- joined_dat |> rename(charge_name = mturk_charge_name)

#Aggregating Charges----
#first view most common IDs
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
    .default = "Other" #lowkey wanna do "Other" so i need to look into this moar
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

#drop the original column (gasp!) and view----
df <- joined_dat |>
  select(-c(charge_name, c_charge_desc)) |>
  rename(aggregated = main_groups,
         charge_degree = "charge_degree (misd/fel)") |>
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
  theme(title = element_text(hjust = 0.5), #not showing title
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

df |>
  filter(race %in% 1:2) |>
  group_by(compas_decile_score) |>
  count(race, compas_decile_score) |>
  ggplot(aes(x = compas_decile_score, y = n, fill = race)) + 
  geom_bar(position="dodge", stat="identity")
#need to find a variable that explains this difference

#see correlation between race/crime and ___----
pairs()
cor(df[, c(2, 3, 4, 5, 10, 13, 14, 15, 16)])

#Data Manip for Table 1----
avgs <- data.frame( #brute force my friend
  Statistic = c("mean", "sd", "mean", "sd", "mean", "sd", "mean", "sd"),
  Variable = c("age", "age", "juv_fel_count", "juv_fel_count", "juv_misd_count",
               "juv_misd_count", "priors_count", "priors_count"),
  Value = c(mean(dat$age), sd(dat$age), mean(dat$juv_fel_count), sd(dat$juv_fel_count), 
            mean(dat$juv_misd_count), sd(dat$juv_misd_count), mean(dat$priors_count), 
            sd(dat$priors_count)))

avgs <- avgs |>
  pivot_wider(values_from = "Value", names_from = "Statistic")

dat |>
  summarize(na_age = sum(is.na(age)),
            na_jf = sum(is.na(juv_fel_count)),
            na_jm = sum(is.na(juv_misd_count)),
            na_pr = sum(is.na(priors_count)),
            na_rcd = sum(is.na(two_year_recid))) #checking NAs but idk if it worked

dage <- dat |>
  mutate(name = cut(age, c(18, 24, 34, 44, 54, 64, Inf))) |>
  group_by(name) |>  
  summarize(n = n(),
            pct = n/7214) |>
  filter(!row_number() %in% c(7))


drace <- dat |>
  count(race) |>
  pivot_longer(race) |>
  mutate(name = case_when(value == 1 ~ "White",
                          value == 2 ~ "Black",
                          value == 3 ~ "Hispanic",
                          value == 4 ~ "Asian",
                          value == 5 ~ "Native American",
                          value == 6 ~ "Other"),
         pct = n/sum(n))

dsex <- dat |>
  count(sex) |>
  pivot_longer(sex) |>
  mutate(name = case_when(value == 0 ~ "Male",
                          value == 1 ~ "Female"),
         pct = n/sum(n))

drec <- dat |>
  summarize(name = "recidivism",
    pct = mean(two_year_recid),
    n = pct*n())

try_again <- full_join(drace, dsex, by = c("name", "n", "pct", "value")) %>%
  select(-c("value")) %>%
  full_join(., drec, by = c("name", "n", "pct")) %>%
  full_join(., dage, by = c("name", "n",  "pct")) %>%
  full_join(., avgs, by = c("name" = "Variable"))
  

  
#Table 1----


try_again %>%
  gt() %>%
    tab_row_group(
      group = "Recividism",
      rows = 9
    ) |>
    tab_row_group(
      group = "Race",
      rows = 1:6) %>%
    tab_row_group(
      group = "Sex",
      rows = 7:8
    ) %>%
    tab_row_group(
      group = "Age Groups",
      rows = 10:15
    ) %>%
  tab_row_group(
    group = "Statistics",
    rows = 16:19
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |>
  fmt_number(mean) |>
  fmt_number(sd) |>
  text_replace(
    locations = cells_body(columns = "name"),
    pattern = "age",
    replacement = "Age"
  ) |>
  text_replace(
    locations = cells_body(columns = "name"),
    pattern = "recidivism",
    replacement = "Recividated People"
  ) |>
  text_replace(
    locations = cells_body(columns = "name"),
    pattern = "juv_fel_count",
    replacement = "Number of Juvenile Felonies"
  ) |>
  text_replace(
    locations = cells_body(columns = "name"),
    pattern = "juv_misd_count",
    replacement = "Number of Juvenile Misdemeanors"
  ) |>
  text_replace(
    locations = cells_body(columns = "name"),
    pattern = "priors_count",
    replacement = "Number of Non-Juvenile Criminal Charges"
  ) |>
  text_replace(
    locations = cells_body(columns = "name"),
    pattern = "\\((.*?)[/g,](.*?)\\]",
    replacement = "\\1 to \\2"
  ) |>
  text_replace(
    locations = cells_body(columns = "name"),
    pattern = "64 to Inf",
    replacement = "65 and Older") |>
  text_replace(
    locations = cells_body(columns = "name"),
    pattern = "4 to",
    replacement = "5 to") |>
  cols_label(name = "") |>
  cols_move(columns = pct,
            after = mean) |>
  cols_move_to_end(n) |>
  fmt_percent(pct) |>
  sub_missing(
    missing_text = ""
  ) |>
  text_replace(
    locations = cells_body(columns = "mean"),
    pattern = "\\(\\)",
    replacement = "") |>
  cols_merge(
    columns = c(mean, sd),
    pattern = "{1} ({2})") |>
  tab_header(
    title = "Table 1. Demographics of People Incarcerted in Broward County",
    subtitle = "Descriptive Statistics") |>
  tab_footnote("Total Num. Obs. = 7214") |>
  cols_label(mean = "Mean (SD)",
             pct = "Percentage (%)") |>
  tab_style(locations = cells_footnotes(),
            cell_text(align = "right",
                      style = "oblique")) |>
  gtsave("tab_1_proj.pdf", expand = 100)

      