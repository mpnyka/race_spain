setwd("~/Desktop/Sociology/Research/Immigration Spain/ICPSR_36286")
library(haven)
library(tidyverse)

## data from ICPSR 36286
load("/Users/anna/Documents/Github/race_spain/ICPSR_36286/DS0001/36286-0001-Data.rda")
data_raw <- da36286.0001

# country_label <- read_csv("/Users/anna/Downloads/ICPSR_36286/country_labels.csv")

## list of countries with majority afro-descent/Black populations:
afro_countries <- c("Dominican Republic", 
                    "Cuba", "Other, Subsaharan Africa", 
                    "Equatorial Guinea", "Other, Caribbean")

## clean the data
d <- data_raw %>% as_tibble() %>%
  mutate(V8A =  str_trim(str_remove(V8A, "\\([:digit:]{1,}\\)")), # clean the country of origin variables to include only the name of the country
         V32 =  str_trim(str_remove(V32, "\\([:digit:]{1,}\\)")),
         V35 =  str_trim(str_remove(V35, "\\([:digit:]{1,}\\)"))) %>%
  mutate_if(is.factor, ~ as.numeric(str_extract(.x, "[:digit:]{1,}"))) %>% ## weird stata code: recode to the numeric value for factors (by default all the raw variables)
  mutate(reason_nationality = V61B2,
         reason_race = V61B3,
         reason_religion = V61B4,
         reason_sex = V61B5,
         birth_country = as.factor(V8A),
         female = ifelse(V6 == 2, 1, 0),
         catalan = V11A - 1, # 0-3 scale
         father_country = as.factor(V32),
         mother_country = as.factor(V35),
         afro_origin = case_when(father_country %in% afro_countries | 
                                   mother_country %in% afro_countries | 
                                   birth_country %in% afro_countries ~ 1,
                                 TRUE ~ 0),
         years_in_spain = V10,
         spanish_skills = V11 - 1,
         barcelona = CITY - 1,
         age = V7,
         second_gen = ifelse(birth_country == "Spain", 1, 0))


fit_1 <- glm(reason_nationality ~ afro_origin + second_gen + years_in_spain + spanish_skills,
             family = binomial(link = "logit"), data = d)
summary(fit_1) ##nationality

fit_2 <- glm(reason_race ~ afro_origin + second_gen + years_in_spain + spanish_skills +
               barcelona + age + female, family = binomial(link = "logit"), data = d)
summary(fit_2) ##race

library(lme4)
library(lmerTest)

fit_3 <- glmer(reason_race ~ afro_origin + second_gen + years_in_spain + spanish_skills +
                 barcelona + age + female +
                 (1|birth_country), 
               family = binomial(link = "logit"), data = d)
summary(fit_3)
