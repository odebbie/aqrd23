library(gt)
library(tidyverse)
library(ggplot2)

df <- read_csv("data/broward_agg.csv",
               na = "NA")

avgs <- data.frame( #brute force my friend
  Statistic = c("mean", "sd", "mean", "sd", "mean", "sd", "mean", "sd"),
  Variable = c("age", "age", "juv_fel_count", "juv_fel_count", "juv_misd_count",
               "juv_misd_count", "priors_count", "priors_count"),
  Value = c(mean(df$age), sd(df$age), mean(df$juv_fel_count), sd(df$juv_fel_count), 
            mean(df$juv_misd_count), sd(df$juv_misd_count), mean(df$priors_count), 
            sd(df$priors_count)))

avgs <- avgs |>
  pivot_wider(values_from = "Value", names_from = "Statistic")

df |>
  summarize(na_age = sum(is.na(age)),
            na_jf = sum(is.na(juv_fel_count)),
            na_jm = sum(is.na(juv_misd_count)),
            na_pr = sum(is.na(priors_count)),
            na_rcd = sum(is.na(two_year_recid))) #checking NAs but idk if it worked

dage <- df |>
  mutate(name = cut(age, c(18, 24, 34, 44, 54, 64, Inf))) |>
  group_by(name) |>  
  summarize(n = n(),
            pct = n/7214) |>
  filter(!row_number() %in% c(7))


drace <- df |>
  count(race) |>
  pivot_longer(race) |>
  mutate(name = case_when(value == 1 ~ "White",
                          value == 2 ~ "Black",
                          value == 3 ~ "Hispanic",
                          value == 4 ~ "Asian",
                          value == 5 ~ "Native American",
                          value == 6 ~ "Other"),
         pct = n/sum(n))

dsex <- df |>
  count(sex) |>
  pivot_longer(sex) |>
  mutate(name = case_when(value == 0 ~ "Male",
                          value == 1 ~ "Female"),
         pct = n/sum(n))

drec <- df |>
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
    replacement = "Recividfed People"
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
  gtsave("tables/tab_1_proj.pdf", expand = 100)
