library(broom)
library(dplyr)
library(dietaryindex)
library(emmeans)
library(ggplot2)
library(gtsummary)
library(here)
library(haven)
library(janitor)
library(labelled)
library(nhanesA)
library(openxlsx)
library(purrr)
library(splines)
library(srvyr)
library(survey)
library(tableone)
library(tidyverse)

#--------------------------------------------------
# Analysis settings
#--------------------------------------------------

options(survey.lonely.psu = "adjust")

set.seed(2026)

dir.create(here("results"), showWarnings = FALSE)


read_nhanes <- function(path) {
  read_xpt(path) %>% clean_names()
}

#---------------------- data import, all cycle -------------------
cycles <- tibble(
  cycle = c("2001-02","2003-04","2005-06","2007-08","2009-10",
            "2011-12","2013-14","2015-16","2017-18"),
  suffix = c("B","C","D","E","F","G","H","I","J")
)

nhanes_all <- pmap_dfr(cycles, function(cycle, suffix) {
  
  demo <- read_nhanes(
  here("data_raw", paste0("DEMO_", suffix, ".xpt"))
  )%>%
    select(seqn, ridageyr, riagendr, ridreth1, indfmpir, sdmvpsu, sdmvstra)
  
  diet <- if (suffix == "B") {
    read_nhanes(
      here("data_raw", "DRXTOT_B.xpt")
      )%>%
      select(seqn, vitk = drxtvk, kcal = drxtkcal)
  } else {
    read_nhanes(
      here("data_raw", paste0("DR1TOT_", suffix, ".xpt"))
      )%>%
      select(seqn, vitk = dr1tvk, kcal = dr1tkcal)
  }
  
  diq <- read_nhanes(
    here("data_raw", paste0("DIQ_", suffix, ".xpt"))
    )%>%
    select(seqn, doctor_diabetes = diq010)
  
  
  labs <- if (suffix %in% c("B", "C")) {
    
    # 2001–02, 2003–04
    read_nhanes(
      here("data_raw", paste0("L10AM_", suffix, ".xpt"))
      )%>%
      select(
        seqn,
        glucose = lbxglu,
        insulin = lbxin,
        wtsaf2yr
      )
    
  } else if (suffix %in% c("D", "E", "F", "G")) {
    
    # 2005–06 through 2011–12
    read_nhanes(
      here("data_raw", paste0("GLU_", suffix, ".xpt"))
      )%>%
      select(
        seqn,
        glucose = lbxglu,
        insulin = lbxin,
        wtsaf2yr
      )
    
  } else if (suffix %in% c("H", "I", "J")) {
    
    # 2013–14 onward
    read_nhanes(
      here("data_raw", paste0("INS_", suffix, ".xpt"))
      )%>%
      select(
        seqn,
        insulin = lbxin
      ) %>%
      left_join(
        read_nhanes(
          here("data_raw", paste0("GLU_", suffix, ".xpt")) 
          )%>%
          select(seqn, glucose = lbxglu, wtsaf2yr),
        by = "seqn"
      )
    
  }
    
  hba1c <- if (suffix %in% c("B","C")) {
    read_nhanes(
      here("data_raw", paste0("L10_", suffix, ".xpt")) 
      )%>%
      select(seqn, hba1c = lbxgh)
  } else {
    read_nhanes(
      here("data_raw", paste0("GHB_", suffix, ".xpt"))
      )%>%
      select(seqn, hba1c = lbxgh)
  }
  
  bmx <- read_nhanes(
    here("data_raw", paste0("BMX_", suffix, ".xpt")) 
    )%>%
    select(seqn, bmi = bmxbmi, waist = bmxwaist)
  
  trig <- if (suffix %in% c("B","C")) {
    read_nhanes(
      here("data_raw", paste0("L40_", suffix, ".xpt")) 
      )%>%
      select(seqn, triglycerides = lbxstr)
  } else {
    read_nhanes(
      here("data_raw", paste0("BIOPRO_", suffix, ".xpt"))
      )%>%
      select(seqn, triglycerides = lbxstr)
  }
  
  smq_raw <- read_nhanes(
    here("data_raw", paste0("SMQ_", suffix, ".xpt"))
  )
  
  smq <- smq_raw %>%
    transmute(
      seqn,
      
      smoking = case_when(
        smq020 == 1 & smq040 %in% c(1, 2) ~ "Current smoker",
        smq020 %in% c(1, 2) ~ "Non-smoker",
        TRUE ~ NA_character_
      )
    )
  
  
  alq_raw <- read_nhanes(
    here("data_raw", paste0("ALQ_", suffix, ".xpt"))
  )
  
  alq <- if (suffix == "B") {
    
    alq_raw %>%
      transmute(
        seqn,
        alcohol = case_when(
          ald100 == 1 ~ "Drinker",
          ald100 == 2 ~ "Non-drinker",
          TRUE ~ NA_character_
        )
      )
    
  } else if (suffix %in% c("C","D","E","F","G","H","I")) {
    
    alq_raw %>%
      transmute(
        seqn,
        alcohol = case_when(
          alq101 == 1 ~ "Drinker",
          alq101 == 2 ~ "Non-drinker",
          TRUE ~ NA_character_
        )
      )
    
  } else if (suffix == "J") {
    
    alq_raw %>%
      transmute(
        seqn,
        alcohol = case_when(
          alq111 == 1 ~ "Drinker",
          alq111 == 2 ~ "Non-drinker",
          TRUE ~ NA_character_
        )
      )
    
  }


  paq_raw <- read_nhanes(
    here("data_raw", paste0("PAQ_", suffix, ".xpt"))
  )
  
  if (suffix %in% c("B","C","D")) {
    
    paq <- paq_raw %>%
      transmute(
        seqn,
        physical_activity = case_when(
          pad200 == 1 ~ "Yes",
          pad200 == 2 ~ "No",
          TRUE ~ NA_character_
        )
      )
    
  } else if (suffix %in% c("E","F","G","H","I","J")) {
    
    paq <- paq_raw %>%
      transmute(
        seqn,
        physical_activity = case_when(
          paq650 == 1 ~ "Yes",
          paq650 == 2 ~ "No",
          TRUE ~ NA_character_
        )
      )
  }
    
  
  demo %>%
    left_join(diet, by = "seqn") %>%
    left_join(labs, by = "seqn") %>%
    left_join(hba1c, by = "seqn") %>%
    left_join(bmx, by = "seqn") %>%
    left_join(trig, by = "seqn") %>%
    left_join(smq, by = "seqn") %>%
    left_join(alq, by = "seqn") %>%
    left_join(diq, by = "seqn") %>%
    left_join(paq, by = "seqn") %>%
    mutate(cycle = cycle)
})

#-------------------- data import, hei (2005-2018) ---------------------

library(dietaryindex)

cycles_hei <- tibble(
  cycle = c("2005-06","2007-08","2009-10",
            "2011-12","2013-14","2015-16","2017-18"),
  suffix = c("D","E","F","G","H","I","J")
)

hei_scores <- pmap_dfr(cycles_hei, function(cycle, suffix) {
  
  fped_file <- case_when(
    suffix == "D" ~ "fped_dr1tot_0506.sas7bdat",
    suffix == "E" ~ "fped_dr1tot_0708.sas7bdat",
    suffix == "F" ~ "fped_dr1tot_0910.sas7bdat",
    suffix == "G" ~ "fped_dr1tot_1112.sas7bdat",
    suffix == "H" ~ "fped_dr1tot_1314.sas7bdat",
    suffix == "I" ~ "fped_dr1tot_1516.sas7bdat",
    suffix == "J" ~ "fped_dr1tot_1718.sas7bdat"
  )
  
  hei_cycle <- HEI2020_NHANES_FPED(
    FPED_PATH = here("data_raw", fped_file),
    NUTRIENT_PATH = here("data_raw", paste0("DR1TOT_", suffix, ".xpt")),
    DEMO_PATH = here("data_raw", paste0("DEMO_", suffix, ".xpt"))
  )
  
  hei_cycle$cycle <- cycle
  
  return(hei_cycle)
})

hei_final <- hei_scores %>%
  select(seqn = SEQN, hei = HEI2020_ALL)


#------------------------ cleanup/compute -----------------------------
nhanes_all <- nhanes_all %>%
  left_join(hei_final, by = "seqn")

nhanes_all <- nhanes_all %>%
  filter(ridageyr >= 18)

nhanes_fasting <- nhanes_all %>%
  filter(
    !is.na(wtsaf2yr),
    !is.na(vitk)
  )

nhanes_fasting <- nhanes_fasting %>%
  mutate(
    diabetes = case_when(
      glucose >= 126 ~ 1,
      hba1c >= 6.5 ~ 1,
      doctor_diabetes == 1 ~ 1,
      TRUE ~ 0
    )
  )

nhanes_fasting <- nhanes_fasting %>%
  mutate(
    diabetes = factor(diabetes, levels = c(0,1), labels = c("No","Yes"))
  )

nhanes_fasting <- nhanes_fasting %>%
  mutate(
    homa_ir = (glucose * insulin) / 405,
  )

nhanes_fasting <- nhanes_fasting %>%
  mutate(
    # Energy-adjusted vitamin K (µg / 1000 kcal)
    vitk_1000kcal = vitk / (kcal / 1000)
  )

# SD of energy-adjusted VK (survey-unweighted, as typically done)
vitk_sd <- sd(nhanes_fasting$vitk_1000kcal, na.rm = TRUE)

nhanes_fasting <- nhanes_fasting %>%
  mutate(
    # SD-scaled exposure
    vitk_1000kcal_sd = vitk_1000kcal / vitk_sd
  )

nhanes_fasting <- nhanes_fasting %>%
  mutate(
    log_vitk = log(vitk_1000kcal + 1)
  )

nhanes_fasting <- nhanes_fasting %>%
  mutate(
    physical_activity = factor(
      physical_activity,
      levels = c("No", "Yes")
    )
  )

nhanes_fasting <- nhanes_fasting %>%
  mutate(
    riagendr = factor(
      riagendr,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    ridreth1 = factor(
      ridreth1,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "Mexican American",
        "Other Hispanic",
        "Non-Hispanic White",
        "Non-Hispanic Black",
        "Other race"
      )
    )
  ) %>%
  set_variable_labels(
    ridageyr = "Age, years",
    riagendr = "Sex",
    ridreth1 = "Race/ethnicity",
    indfmpir = "Poverty–income ratio",
    kcal = "Energy intake, kcal/day",
    bmi = "Body mass index, kg/m²",
    waist = "Waist circumference, cm",
    glucose = "Fasting glucose, mg/dL",
    hba1c = "HbA1c, %",
    homa_ir = "HOMA-IR",
    triglycerides = "Triglycerides, mg/dL",
    smoking = "Smoking status",
    alcohol = "Alcohol use",
    diabetes = "Diabetes"
  )

#--------------------------- weights -------------------------------------
n_cycles <- n_distinct(nhanes_fasting$cycle)

nhanes_fasting <- nhanes_fasting %>%
  mutate(wt_fasting = wtsaf2yr / n_cycles)

nhanes_design <- svydesign(
  ids = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wt_fasting,
  data = nhanes_fasting,
  nest = TRUE
)

design_insulin <- subset(nhanes_design, !is.na(insulin))
design_glucose <- subset(nhanes_design, !is.na(glucose))
design_hba1c   <- subset(nhanes_design, !is.na(hba1c))
design_homa    <- subset(nhanes_design, !is.na(insulin) & !is.na(glucose))


nhanes_hei <- nhanes_fasting %>%
  filter(!is.na(hei))

n_cycles_hei <- 7

nhanes_hei <- nhanes_hei %>%
  mutate(wt_fasting_hei = wtsaf2yr / n_cycles_hei)

nhanes_design_hei <- svydesign(
  ids = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wt_fasting_hei,
  data = nhanes_hei,
  nest = TRUE
)

#----------------------- quartiles ---------------------------

# Survey-weighted quartiles
vitk_q <- svyquantile(
  ~vitk_1000kcal,
  nhanes_design,
  quantiles = c(0.25, 0.5, 0.75),
  na.rm = TRUE
)

cuts <- coef(vitk_q)

nhanes_fasting <- nhanes_fasting %>%
  mutate(
    vitk_q = cut(
      vitk_1000kcal,
      breaks = c(-Inf, cuts, Inf),
      labels = c("Q1", "Q2", "Q3", "Q4")
    )
  )

nhanes_design <- update(
  nhanes_design,
  vitk_q = nhanes_fasting$vitk_q
)

design_glucose <- subset(nhanes_design, !is.na(glucose))
design_hba1c   <- subset(nhanes_design, !is.na(hba1c))
design_insulin <- subset(nhanes_design, !is.na(insulin))
design_homa    <- subset(nhanes_design, !is.na(glucose) & !is.na(insulin))

#-------------------------- table 1 --------------------------
n_overall  <- nrow(nhanes_fasting)
n_by_q     <- nhanes_fasting |>
  count(vitk_q)

table_1 <- tbl_svysummary(
  nhanes_design,
  by = vitk_q,
  include = c(
    vitk_1000kcal,
    ridageyr,
    riagendr,
    ridreth1,
    indfmpir,
    kcal,
    bmi,
    waist,
    insulin,
    homa_ir,
    glucose,
    hba1c,
    triglycerides,
    smoking,
    alcohol,
    diabetes
  ),
  statistic = list(
    vitk_1000kcal ~ "{median} ({p25}, {p75})",
    ridageyr ~ "{mean} ({sd})",
    indfmpir ~ "{mean} ({sd})",
    kcal ~ "{mean} ({sd})",
    bmi ~ "{mean} ({sd})",
    waist ~ "{mean} ({sd})",
    insulin ~ "{median} ({p25}, {p75})",
    homa_ir ~ "{median} ({p25}, {p75})",
    glucose ~ "{mean} ({sd})",
    hba1c ~ "{mean} ({sd})",
    triglycerides ~ "{mean} ({sd})",
    all_categorical() ~ "{p}%"
  ),
  digits = list(
    vitk_1000kcal ~ 1,
    all_continuous() ~ 1
  ),
  missing = "no"
) %>%
  add_overall() %>%
  bold_labels()

header_map <- c(
  stat_0 = paste0("Overall\nN = ", n_overall),
  setNames(
    paste0(n_by_q$vitk_q, "\nN = ", n_by_q$n),
    paste0("stat_", seq_len(nrow(n_by_q)))
  )
)

table_1 <- table_1 |>
  modify_header(!!!header_map)

table_1_df <- as_tibble(table_1, col_labels = TRUE)

write.xlsx(
  table_1_df,
  file = here("results", "Table1_population_characteristics.xlsx"),
  overwrite = TRUE
)

#---------------------- glucose analyses -----------------------
extract_table <- function(model, model_name) {
  tidy(model, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Exposure = recode(
        term,
        "vitk_qQ2" = "Vitamin K Q2 vs Q1",
        "vitk_qQ3" = "Vitamin K Q3 vs Q1",
        "vitk_qQ4" = "Vitamin K Q4 vs Q1"
      ),
      `β (95% CI)` = sprintf(
        "%.2f (%.2f, %.2f)",
        estimate, conf.low, conf.high
      ),
      `P value` = signif(p.value, 3),
      Model = model_name,
      N = nobs(model)
    ) %>%
    select(
      Model,
      Exposure,
      `β (95% CI)`,
      `P value`,
      N
    )
}

m1_glucose <- svyglm(
  glucose ~ vitk_q,
  design = design_glucose
)

m2_glucose <- svyglm(
  glucose ~ vitk_q +
    ridageyr +
    riagendr +
    ridreth1 +
    indfmpir +
    smoking +
    alcohol +
    kcal,
  design = design_glucose
)

m3_glucose <- svyglm(
  glucose ~ vitk_q +
    ridageyr +
    riagendr +
    ridreth1 +
    indfmpir +
    smoking +
    alcohol +
    kcal +
    bmi +
    waist,
  design = design_glucose
)

table_m1_glu <- extract_table(m1_glucose, "Model 1: Crude")
table_m2_glu <- extract_table(m2_glucose, "Model 2: + Demographics & lifestyle")
table_m3_glu <- extract_table(m3_glucose, "Model 3: + Adiposity")

write.xlsx(
  list(
    "Model 1 - Crude" = table_m1_glu,
    "Model 2 - Lifestyle" = table_m2_glu,
    "Model 3 - Adiposity" = table_m3_glu
  ),
  file = here("results", "Table2_Glucose_M1to3.xlsx"),
  overwrite = TRUE
)


#------------------- HbA1c analyses -------------------

m1_hba1c <- svyglm(
  hba1c ~ vitk_q,
  design = design_hba1c
)

m2_hba1c <- svyglm(
  hba1c ~ vitk_q +
    ridageyr +
    riagendr +
    ridreth1 +
    indfmpir +
    smoking +
    alcohol +
    kcal,
  design = design_hba1c
)

m3_hba1c <- svyglm(
  hba1c ~ vitk_q +
    ridageyr +
    riagendr +
    ridreth1 +
    indfmpir +
    smoking +
    alcohol +
    kcal +
    bmi +
    waist,
  design = design_hba1c
)

table_m1_hba1c <- extract_table(m1_hba1c, "Model 1: Crude")
table_m2_hba1c <- extract_table(m2_hba1c, "Model 2: + Demographics & lifestyle")
table_m3_hba1c <- extract_table(m3_hba1c, "Model 3: + Adiposity")

write.xlsx(
  list(
    "Model 1 - Crude" = table_m1_hba1c,
    "Model 2 - Lifestyle" = table_m2_hba1c,
    "Model 3 - Adiposity" = table_m3_hba1c
  ),
  file = here("results", "Table2_HbA1c_M1to3.xlsx"),
  overwrite = TRUE
)

#------------------- insulin analyses -------------------
extract_table_log <- function(model, model_name) {
  tidy(model, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Exposure = recode(
        term,
        "vitk_qQ2" = "Vitamin K Q2 vs Q1",
        "vitk_qQ3" = "Vitamin K Q3 vs Q1",
        "vitk_qQ4" = "Vitamin K Q4 vs Q1"
      ),
      Percent_diff = (exp(estimate) - 1) * 100,
      CI_low = (exp(conf.low) - 1) * 100,
      CI_high = (exp(conf.high) - 1) * 100,
      `Percent difference (95% CI)` = sprintf(
        "%.1f%% (%.1f, %.1f)",
        Percent_diff, CI_low, CI_high
      ),
      `P value` = signif(p.value, 3),
      Model = model_name,
      N = nobs(model)
    ) %>%
    select(
      Model,
      Exposure,
      `Percent difference (95% CI)`,
      `P value`,
      N
    )
}

m1_insulin <- svyglm(
  log(insulin) ~ vitk_q,
  design = design_insulin
)

m2_insulin <- svyglm(
  log(insulin) ~ vitk_q +
    ridageyr +
    riagendr +
    ridreth1 +
    indfmpir +
    smoking +
    alcohol +
    kcal,
  design = design_insulin
)

m3_insulin <- svyglm(
  log(insulin) ~ vitk_q +
    ridageyr +
    riagendr +
    ridreth1 +
    indfmpir +
    smoking +
    alcohol +
    kcal +
    bmi +
    waist,
  design = design_insulin
)

table_m1_ins <- extract_table_log(m1_insulin, "Model 1: Crude")
table_m2_ins <- extract_table_log(m2_insulin, "Model 2: + Demographics & lifestyle")
table_m3_ins <- extract_table_log(m3_insulin, "Model 3: + Adiposity")

write.xlsx(
  list(
    "Model 1 - Crude" = table_m1_ins,
    "Model 2 - Lifestyle" = table_m2_ins,
    "Model 3 - Adiposity" = table_m3_ins
  ),
  file = here("results", "Table2_Insulin_M1to3.xlsx"),
  overwrite = TRUE
)


#------------------- HOMA-IR analyses -------------------

m1_homa <- svyglm(
  log(homa_ir) ~ vitk_q,
  design = design_homa
)

m2_homa <- svyglm(
  log(homa_ir) ~ vitk_q +
    ridageyr +
    riagendr +
    ridreth1 +
    indfmpir +
    smoking +
    alcohol +
    kcal,
  design = design_homa
)

m3_homa <- svyglm(
  log(homa_ir) ~ vitk_q +
    ridageyr +
    riagendr +
    ridreth1 +
    indfmpir +
    smoking +
    alcohol +
    kcal +
    bmi +
    waist,
  design = design_homa
)

table_m1_homa <- extract_table_log(m1_homa, "Model 1: Crude")
table_m2_homa <- extract_table_log(m2_homa, "Model 2: + Demographics & lifestyle")
table_m3_homa <- extract_table_log(m3_homa, "Model 3: + Adiposity")

write.xlsx(
  list(
    "Model 1 - Crude" = table_m1_homa,
    "Model 2 - Lifestyle" = table_m2_homa,
    "Model 3 - Adiposity" = table_m3_homa
  ),
  file = here("results", "Table2_HOMAIR_M1to3.xlsx"),
  overwrite = TRUE
)


#---------------------- trend analyses -----------------------
# Quartile medians of energy-adjusted vitamin K
vitk_medians <- nhanes_fasting %>%
  group_by(vitk_q) %>%
  summarise(
    vitk_q_median = median(vitk_1000kcal, na.rm = TRUE),
    .groups = "drop"
  )

nhanes_fasting <- nhanes_fasting %>%
  left_join(vitk_medians, by = "vitk_q")

nhanes_design <- update(
  nhanes_design,
  vitk_q_median = nhanes_fasting$vitk_q_median
)

design_glucose <- subset(nhanes_design, !is.na(glucose))
design_hba1c   <- subset(nhanes_design, !is.na(hba1c))
design_insulin <- subset(nhanes_design, !is.na(insulin))
design_homa    <- subset(nhanes_design, !is.na(glucose) & !is.na(insulin))

extract_trend_p <- function(model) {
  broom::tidy(model) %>%
    filter(term == "vitk_q_median") %>%
    transmute(
      `P for trend` = signif(p.value, 3)
    ) %>%
    pull(`P for trend`)
}

m3_glucose_trend <- svyglm(
  glucose ~ vitk_q_median +
    ridageyr +
    riagendr +
    ridreth1 +
    indfmpir +
    smoking +
    alcohol +
    kcal +
    bmi +
    waist,
  design = design_glucose
)

p_trend_glucose <- extract_trend_p(m3_glucose_trend)


m3_hba1c_trend <- svyglm(
  hba1c ~ vitk_q_median +
    ridageyr +
    riagendr +
    ridreth1 +
    indfmpir +
    smoking +
    alcohol +
    kcal +
    bmi +
    waist,
  design = design_hba1c
)

p_trend_hba1c <- extract_trend_p(m3_hba1c_trend)


m3_insulin_trend <- svyglm(
  log(insulin) ~ vitk_q_median +
    ridageyr +
    riagendr +
    ridreth1 +
    indfmpir +
    smoking +
    alcohol +
    kcal +
    bmi +
    waist,
  design = design_insulin
)

p_trend_insulin <- extract_trend_p(m3_insulin_trend)


m3_homa_trend <- svyglm(
  log(homa_ir) ~ vitk_q_median +
    ridageyr +
    riagendr +
    ridreth1 +
    indfmpir +
    smoking +
    alcohol +
    kcal +
    bmi +
    waist,
  design = design_homa
)

p_trend_homa <- extract_trend_p(m3_homa_trend)

table2_m3_trends <- tibble::tibble(
  Outcome = c(
    "Fasting glucose",
    "HbA1c",
    "Fasting insulin",
    "HOMA-IR"
  ),
  `P for trend (Model 3)` = c(
    p_trend_glucose,
    p_trend_hba1c,
    p_trend_insulin,
    p_trend_homa
  )
)

write.xlsx(
  table2_m3_trends,
  file = here("results", "Table2_M3_PforTrend.xlsx"),
  overwrite = TRUE
)

#---------------------- continuous analyses ------------------------
m3_glucose_sd <- svyglm(
  glucose ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_glucose
)

m3_hba1c_sd <- svyglm(
  hba1c ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_hba1c
)

m3_insulin_sd <- svyglm(
  log(insulin) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_insulin
)

m3_homa_sd <- svyglm(
  log(homa_ir) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_homa
)

m3_glucose_logvk <- svyglm(
  glucose ~ log_vitk +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_glucose
)

m3_hba1c_logvk <- svyglm(
  hba1c ~ log_vitk +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_hba1c
)

m3_insulin_logvk <- svyglm(
  log(insulin) ~ log_vitk +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_insulin
)

m3_homa_logvk <- svyglm(
  log(homa_ir) ~ log_vitk +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_homa
)

extract_linear <- function(model, exposure, outcome_label) {
  tidy(model, conf.int = TRUE) %>%
    filter(term == exposure) %>%
    mutate(
      Outcome = outcome_label,
      `Effect (95% CI)` = sprintf(
        "%.2f (%.2f, %.2f)",
        estimate, conf.low, conf.high
      ),
      `P value` = signif(p.value, 3),
      N = nobs(model)
    ) %>%
    select(Outcome, `Effect (95% CI)`, `P value`, N)
}

extract_log <- function(model, exposure, outcome_label) {
  tidy(model, conf.int = TRUE) %>%
    filter(term == exposure) %>%
    mutate(
      Percent = (exp(estimate) - 1) * 100,
      CI_low = (exp(conf.low) - 1) * 100,
      CI_high = (exp(conf.high) - 1) * 100,
      Outcome = outcome_label,
      `Effect (95% CI)` = sprintf(
        "%.1f%% (%.1f, %.1f)",
        Percent, CI_low, CI_high
      ),
      `P value` = signif(p.value, 3),
      N = nobs(model)
    ) %>%
    select(Outcome, `Effect (95% CI)`, `P value`, N)
}

table_continuous <- bind_rows(
  
  # ----- Per SD -----
  extract_linear(m3_glucose_sd, "vitk_1000kcal_sd",
                 "Fasting glucose (per SD)"),
  
  extract_linear(m3_hba1c_sd, "vitk_1000kcal_sd",
                 "HbA1c (per SD)"),
  
  extract_log(m3_insulin_sd, "vitk_1000kcal_sd",
              "Fasting insulin (per SD)"),
  
  extract_log(m3_homa_sd, "vitk_1000kcal_sd",
              "HOMA-IR (per SD)"),
  
  # ----- Log(VK + 1) -----
  extract_linear(m3_glucose_logvk, "log_vitk",
                 "Fasting glucose (log VK+1)"),
  
  extract_linear(m3_hba1c_logvk, "log_vitk",
                 "HbA1c (log VK+1)"),
  
  extract_log(m3_insulin_logvk, "log_vitk",
              "Fasting insulin (log VK+1)"),
  
  extract_log(m3_homa_logvk, "log_vitk",
              "HOMA-IR (log VK+1)")
)

write.xlsx(
  table_continuous,
  file = here("results", "Table_Continuous_VK_Model3.xlsx"),
  overwrite = TRUE
)

#----------------------- qunatify attenuation -------------------------

# Model 1
m1_glucose_sd <- svyglm(
  glucose ~ vitk_1000kcal_sd,
  design = design_glucose
)

m1_hba1c_sd <- svyglm(
  hba1c ~ vitk_1000kcal_sd,
  design = design_hba1c
)

m1_insulin_sd <- svyglm(
  log(insulin) ~ vitk_1000kcal_sd,
  design = design_insulin
)

m1_homa_sd <- svyglm(
  log(homa_ir) ~ vitk_1000kcal_sd,
  design = design_homa
)

# Model 2
m2_glucose_sd <- svyglm(
  glucose ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol + kcal,
  design = design_glucose
)

m2_hba1c_sd <- svyglm(
  hba1c ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol + kcal,
  design = design_hba1c
)

m2_insulin_sd <- svyglm(
  log(insulin) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol + kcal,
  design = design_insulin
)

m2_homa_sd <- svyglm(
  log(homa_ir) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol + kcal,
  design = design_homa
)

get_beta <- function(model) {
  tidy(model) %>%
    filter(term == "vitk_1000kcal_sd") %>%
    pull(estimate)
}

calc_attenuation <- function(m1, m2, m3, outcome_name) {
  
  b1 <- get_beta(m1)
  b2 <- get_beta(m2)
  b3 <- get_beta(m3)
  
  tibble(
    Outcome = outcome_name,
    Beta_M1 = b1,
    Beta_M2 = b2,
    Beta_M3 = b3,
    Attenuation_M1_to_M2 = (b1 - b2) / b1 * 100,
    Attenuation_M2_to_M3 = (b2 - b3) / b2 * 100,
    Total_Attenuation_M1_to_M3 = (b1 - b3) / b1 * 100
  )
}

atten_glucose <- calc_attenuation(
  m1_glucose_sd,
  m2_glucose_sd,
  m3_glucose_sd,
  "Fasting glucose"
)

atten_hba1c <- calc_attenuation(
  m1_hba1c_sd,
  m2_hba1c_sd,
  m3_hba1c_sd, 
  "HbA1c"
  )

atten_insulin <- calc_attenuation(
  m1_insulin_sd,
  m2_insulin_sd,
  m3_insulin_sd,
  "Fasting insulin (log)"
  )

atten_homa <- calc_attenuation(
  m1_homa_sd,
  m2_homa_sd,
  m3_homa_sd,
  "HOMA-IR (log)"
  )

attenuation_table <- bind_rows(
  atten_glucose,
  atten_hba1c,
  atten_insulin,
  atten_homa
)

attenuation_table_clean <- attenuation_table %>%
  mutate(
    Attenuation_M1_to_M2 = round(Attenuation_M1_to_M2, 1),
    Attenuation_M2_to_M3 = round(Attenuation_M2_to_M3, 1),
    Total_Attenuation_M1_to_M3 = round(Total_Attenuation_M1_to_M3, 1)
  )

write.xlsx(
  attenuation_table_clean,
  file = here("results", "Table_Attenuation_PerSD_Model1to3.xlsx"),
  overwrite = TRUE
)

#--------------------- exclude implausible vk (1–99%) ------------------------

#Define percentile cutoffs (full sample)
vk_p1  <- quantile(nhanes_fasting$vitk_1000kcal, 0.01, na.rm = TRUE)
vk_p99 <- quantile(nhanes_fasting$vitk_1000kcal, 0.99, na.rm = TRUE)

#Trim dataset
nhanes_trim_vk <- nhanes_fasting %>%
  filter(
    vitk_1000kcal >= vk_p1,
    vitk_1000kcal <= vk_p99
  )

#Recalculate SD in trimmed dataset
vitk_sd_trim <- sd(nhanes_trim_vk$vitk_1000kcal, na.rm = TRUE)

#Create trimmed SD-scaled exposure
nhanes_trim_vk <- nhanes_trim_vk %>%
  mutate(
    vitk_1000kcal_sd_trim = vitk_1000kcal / vitk_sd_trim
  )

#Recreate survey design
nhanes_design_trim_vk <- svydesign(
  ids = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wt_fasting,
  data = nhanes_trim_vk,
  nest = TRUE
)

design_insulin_trim_vk <- subset(nhanes_design_trim_vk, !is.na(insulin))
design_homa_trim_vk    <- subset(nhanes_design_trim_vk, !is.na(insulin) & !is.na(glucose))

#Run Model 3 using trimmed SD variable

m3_insulin_trim_vk <- svyglm(
  log(insulin) ~ vitk_1000kcal_sd_trim +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_insulin_trim_vk
)

m3_homa_trim_vk <- svyglm(
  log(homa_ir) ~ vitk_1000kcal_sd_trim +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_homa_trim_vk
)

#Extract function (note new variable name)

extract_log_compare <- function(
    model,
    exposure,
    outcome_label,
    model_label
) {
  
  tidy(model, conf.int = TRUE) %>%
    filter(term == exposure) %>%
    mutate(
      Percent = (exp(estimate) - 1) * 100,
      CI_low = (exp(conf.low) - 1) * 100,
      CI_high = (exp(conf.high) - 1) * 100,
      Outcome = outcome_label,
      Model = model_label,
      N = nobs(model)
    ) %>%
    select(
      Outcome, 
      Model, 
      Percent, 
      CI_low, 
      CI_high, 
      p.value, 
      N
      )
}


#Compare original vs trimmed (properly scaled)

compare_trim_vk <- bind_rows(
  
  extract_log_compare(
    m3_insulin_sd,
    "vitk_1000kcal_sd",
    "Fasting insulin",
    "Original"
  ),
  
  extract_log_compare(
    m3_insulin_trim_vk,
    "vitk_1000kcal_sd_trim",
    "Fasting insulin",
    "Trimmed VK (1–99%)"
  ),
  
  extract_log_compare(
    m3_homa_sd,
    "vitk_1000kcal_sd",
    "HOMA-IR",
    "Original"
  ),
  
  extract_log_compare(
    m3_homa_trim_vk,
    "vitk_1000kcal_sd_trim",
    "HOMA-IR",
    "Trimmed VK (1–99%)"
  )
)

write.xlsx(
  compare_trim_vk,
  file = here("results", "Sensitivity_Trimmed_VK_1to99.xlsx"),
  overwrite = TRUE
)

#----------------------- exclude implausible kcal --------------------------

nhanes_plausible_kcal <- nhanes_fasting %>%
  filter(
    (riagendr == "Female" & kcal >= 500 & kcal <= 4000) |
      (riagendr == "Male"   & kcal >= 800 & kcal <= 5000)
  )

nhanes_design_plausible <- svydesign(
  ids = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wt_fasting,
  data = nhanes_plausible_kcal,
  nest = TRUE
)

design_insulin_plausible <- subset(nhanes_design_plausible, !is.na(insulin))
design_homa_plausible    <- subset(nhanes_design_plausible, !is.na(insulin) & !is.na(glucose))

m3_insulin_sd_plausible <- svyglm(
  log(insulin) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_insulin_plausible
)

m3_homa_sd_plausible <- svyglm(
  log(homa_ir) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_homa_plausible
)

compare_implausible <- bind_rows(
  
  extract_log_compare(
    m3_insulin_sd,
    "vitk_1000kcal_sd",
    "Fasting insulin",
    "Original"
  ),
  
  extract_log_compare(
    m3_insulin_sd_plausible,
    "vitk_1000kcal_sd",
    "Fasting insulin",
    "Plausible kcal"
  ),
  
  extract_log_compare(
    m3_homa_sd,
    "vitk_1000kcal_sd",
    "HOMA-IR",
    "Original"
  ),
  
  extract_log_compare(
    m3_homa_sd_plausible,
    "vitk_1000kcal_sd",
    "HOMA-IR",
    "Plausible kcal"
  )
)

write.xlsx(
  compare_implausible,
  file = here("results", "Sensitivity_Plausible_Kcal_Model3_PerSD.xlsx"),
  overwrite = TRUE
)

#----------------------- exclude diabetes --------------------------

nhanes_nodiab <- nhanes_fasting %>%
  filter(diabetes == "No")

100 * (nrow(nhanes_fasting) - nrow(nhanes_nodiab)) / nrow(nhanes_fasting)

nhanes_design_nodiab <- svydesign(
  ids = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wt_fasting,
  data = nhanes_nodiab,
  nest = TRUE
)

design_insulin_nodiab <- subset(nhanes_design_nodiab, !is.na(insulin))
design_homa_nodiab    <- subset(nhanes_design_nodiab, !is.na(insulin) & !is.na(glucose))

m3_insulin_sd_nodiab <- svyglm(
  log(insulin) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_insulin_nodiab
)

m3_homa_sd_nodiab <- svyglm(
  log(homa_ir) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_homa_nodiab
)

compare_nodiab <- bind_rows(
  
  extract_log_compare(
    m3_insulin_sd,
    "vitk_1000kcal_sd",
    "Fasting insulin",
    "Original"
  ),
  
  extract_log_compare(
    m3_insulin_sd_nodiab,
    "vitk_1000kcal_sd",
    "Fasting insulin",
    "No diabetes"
  ),
  
  extract_log_compare(
    m3_homa_sd,
    "vitk_1000kcal_sd",
    "HOMA-IR",
    "Original"
  ),
  
  extract_log_compare(
    m3_homa_sd_nodiab,
    "vitk_1000kcal_sd",
    "HOMA-IR",
    "No diabetes"
  )
)

write.xlsx(
  compare_nodiab,
  file = here("results", "Sensitivity_Exclude_Diabetes_Model3_PerSD.xlsx"),
  overwrite = TRUE
)

#------------------------- hei-adjust ---------------------------
design_insulin_hei <- subset(nhanes_design_hei, !is.na(insulin))
design_homa_hei    <- subset(nhanes_design_hei, !is.na(insulin) & !is.na(glucose))


m3_insulin_sd_hei <- svyglm(
  log(insulin) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist +
    hei,
  design = design_insulin_hei
)

m3_homa_sd_hei <- svyglm(
  log(homa_ir) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist +
    hei,
  design = design_homa_hei
)

compare_hei <- bind_rows(
  
  extract_log_compare(
    m3_insulin_sd,
    "vitk_1000kcal_sd",
    "Fasting insulin",
    "Original"
  ),
  
  extract_log_compare(
    m3_insulin_sd_hei,
    "vitk_1000kcal_sd",
    "Fasting insulin",
    "+ HEI"
  ),
  
  extract_log_compare(
    m3_homa_sd,
    "vitk_1000kcal_sd",
    "HOMA-IR",
    "Original"
  ),
  
  extract_log_compare(
    m3_homa_sd_hei,
    "vitk_1000kcal_sd",
    "HOMA-IR",
    "+ HEI"
  )
)

write.xlsx(
  compare_hei,
  file = here("results", "Sensitivity_HEI_Adjustment_Model3_PerSD.xlsx"),
  overwrite = TRUE
)

#---------------------- physical activity-adjust -------------------------
m3_insulin_sd_pa <- svyglm(
  log(insulin) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist +
    physical_activity,
  design = design_insulin
)

m3_homa_sd_pa <- svyglm(
  log(homa_ir) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist +
    physical_activity,
  design = design_homa
)

compare_pa <- bind_rows(
  
  extract_log_compare(
    m3_insulin_sd,
    "vitk_1000kcal_sd",
    "Fasting insulin",
    "Model 3"
  ),
  
  extract_log_compare(
    m3_insulin_sd_pa,
    "vitk_1000kcal_sd",
    "Fasting insulin",
    "Model 3 + PA"
  ),
  
  extract_log_compare(
    m3_homa_sd,
    "vitk_1000kcal_sd",
    "HOMA-IR",
    "Model 3"
  ),
  
  extract_log_compare(
    m3_homa_sd_pa,
    "vitk_1000kcal_sd",
    "HOMA-IR",
    "Model 3 + PA"
  )
)

write.xlsx(
  compare_pa,
  file = here("results", "Sensitivity_PhysicalActivity_Model3_PerSD.xlsx"),
  overwrite = TRUE
)


#------------------ alternative adiposity specification ---------------------
m3_insulin_bmi_only <- svyglm(
  log(insulin) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi,
  design = design_insulin
)

m3_homa_bmi_only <- svyglm(
  log(homa_ir) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi,
  design = design_homa
)

m3_insulin_wc_only <- svyglm(
  log(insulin) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + waist,
  design = design_insulin
)

m3_homa_wc_only <- svyglm(
  log(homa_ir) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + waist,
  design = design_homa
)

compare_obesity <- bind_rows(
  
  extract_log_compare(
    m3_insulin_sd,
    "vitk_1000kcal_sd",
    "Fasting insulin",
    "BMI + WC"
  ),
  
  extract_log_compare(
    m3_insulin_bmi_only,
    "vitk_1000kcal_sd",
    "Fasting insulin",
    "BMI only"
  ),
  
  extract_log_compare(
    m3_insulin_wc_only,
    "vitk_1000kcal_sd",
    "Fasting insulin",
    "WC only"
  ),
  
  extract_log_compare(
    m3_homa_sd,
    "vitk_1000kcal_sd",
    "HOMA-IR",
    "BMI + WC"
  ),
  
  extract_log_compare(
    m3_homa_bmi_only,
    "vitk_1000kcal_sd",
    "HOMA-IR",
    "BMI only"
  ),
  
  extract_log_compare(
    m3_homa_wc_only,
    "vitk_1000kcal_sd",
    "HOMA-IR",
    "WC only"
  )
)

write.xlsx(
  compare_obesity,
  file = here("results", "Sensitivity_Obesity_Specifications_Model3_PerSD.xlsx"),
  overwrite = TRUE
)

#-------------------- test nonlinearity ------------------------

m3_glucose_linear <- svyglm(
  glucose ~ vitk_1000kcal +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_glucose
)

m3_glucose_spline <- svyglm(
  glucose ~ ns(vitk_1000kcal, df = 3) +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_glucose
)

m3_hba1c_linear <- svyglm(
  hba1c ~ vitk_1000kcal +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_hba1c
)

m3_hba1c_spline <- svyglm(
  hba1c ~ ns(vitk_1000kcal, df = 3) +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_hba1c
)

m3_insulin_linear <- svyglm(
  log(insulin) ~ vitk_1000kcal +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_insulin
)

m3_insulin_spline <- svyglm(
  log(insulin) ~ ns(vitk_1000kcal, df = 3) +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_insulin
)

m3_homa_linear <- svyglm(
  log(homa_ir) ~ vitk_1000kcal +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_homa
)

m3_homa_spline <- svyglm(
  log(homa_ir) ~ ns(vitk_1000kcal, df = 3) +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_homa
)

extract_spline_results <- function(linear_model, spline_model, outcome_label) {
  
  # Overall spline association
  overall_test <- regTermTest(
    spline_model,
    ~ ns(vitk_1000kcal, df = 3)
  )
  
  # Compare linear vs spline model
  nonlin_test <- anova(
    linear_model,
    spline_model
  )
  
  tibble(
    Outcome = outcome_label,
    N = nobs(spline_model),
    `P overall association` = signif(overall_test$p, 3),
    `P for non-linearity` = signif(nonlin_test$p, 3)
  )
}

spline_results_table <- bind_rows(
  
  extract_spline_results(
    m3_glucose_linear,
    m3_glucose_spline,
    "Fasting glucose"
  ),
  
  extract_spline_results(
    m3_hba1c_linear,
    m3_hba1c_spline,
    "HbA1c"
  ),
  
  extract_spline_results(
    m3_insulin_linear,
    m3_insulin_spline,
    "Fasting insulin (log)"
  ),
  
  extract_spline_results(
    m3_homa_linear,
    m3_homa_spline,
    "HOMA-IR (log)"
  )
)

write.xlsx(
  spline_results_table,
  file = here("results", "Sensitivity_RestrictedCubicSplines_Model3.xlsx"),
  overwrite = TRUE
)

#---------------- effect modification by baseline metabolic status -------------

nhanes_nodiab_em <- nhanes_fasting %>%
  filter(diabetes == "No")

nhanes_design_nodiab_em <- svydesign(
  ids = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wt_fasting,
  data = nhanes_nodiab_em,
  nest = TRUE
)

nhanes_nodiab_em <- nhanes_nodiab_em %>%
  mutate(
    metabolic_status = case_when(
      glucose >= 100 & glucose < 126 ~ "Impaired",
      hba1c >= 5.7 & hba1c < 6.5 ~ "Impaired",
      TRUE ~ "Normoglycemic"
    )
  ) %>%
  mutate(
    metabolic_status = factor(
      metabolic_status,
      levels = c("Normoglycemic", "Impaired")
    )
  )

nhanes_design_nodiab_em <- update(
  nhanes_design_nodiab_em,
  metabolic_status = nhanes_nodiab_em$metabolic_status
)

design_insulin_nodiab_em <- subset(nhanes_design_nodiab_em, !is.na(insulin))
design_homa_nodiab_em   <- subset(nhanes_design_nodiab_em, !is.na(insulin) & !is.na(glucose))


#insulin
m3_insulin_interaction_em <- svyglm(
  log(insulin) ~ vitk_1000kcal_sd * metabolic_status +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_insulin_nodiab_em
)

interaction_p_insulin <- tidy(m3_insulin_interaction_em) %>%
  filter(grepl("vitk_1000kcal_sd:metabolic_status", term)) %>%
  pull(p.value)

m_norm_ins <- svyglm(
  log(insulin) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = subset(design_insulin_nodiab_em,
                  metabolic_status == "Normoglycemic")
)

m_imp_ins <- svyglm(
  log(insulin) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = subset(design_insulin_nodiab_em,
                  metabolic_status == "Impaired")
)

extract_log_strata <- function(model, group_label) {
  tidy(model, conf.int = TRUE) %>%
    filter(term == "vitk_1000kcal_sd") %>%
    mutate(
      Percent = (exp(estimate) - 1) * 100,
      CI_low = (exp(conf.low) - 1) * 100,
      CI_high = (exp(conf.high) - 1) * 100,
      Group = group_label,
      N = nobs(model)
    ) %>%
    select(Group, Percent, CI_low, CI_high, p.value, N)
}

effect_mod_insulin <- bind_rows(
  extract_log_strata(m_norm_ins, "Normoglycemic"),
  extract_log_strata(m_imp_ins, "Impaired (prediabetes)")
)


#homa
m3_homa_interaction_em <- svyglm(
  log(homa_ir) ~ vitk_1000kcal_sd * metabolic_status +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_homa_nodiab_em
)

interaction_p_homa <- tidy(m3_homa_interaction_em) %>%
  filter(grepl("vitk_1000kcal_sd:metabolic_status", term)) %>%
  pull(p.value)

m_norm_homa <- svyglm(
  log(homa_ir) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = subset(design_homa_nodiab_em,
                  metabolic_status == "Normoglycemic")
)

m_imp_homa <- svyglm(
  log(homa_ir) ~ vitk_1000kcal_sd +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = subset(design_homa_nodiab_em,
                  metabolic_status == "Impaired")
)

effect_mod_homa <- bind_rows(
  extract_log_strata(m_norm_homa, "Normoglycemic"),
  extract_log_strata(m_imp_homa, "Impaired (prediabetes)")
)

effect_mod_insulin$`P for interaction` <- interaction_p_insulin
effect_mod_homa$`P for interaction` <- interaction_p_homa

effect_mod_insulin <- effect_mod_insulin %>%
  mutate(Outcome = "Fasting insulin (log)")

effect_mod_homa <- effect_mod_homa %>%
  mutate(Outcome = "HOMA-IR (log)")

effect_mod_combined <- bind_rows(
  effect_mod_insulin,
  effect_mod_homa
) %>%
  select(
    Outcome,
    Group,
    Percent,
    CI_low,
    CI_high,
    p.value,
    N,
    `P for interaction`
  )

effect_mod_combined <- effect_mod_combined %>%
  mutate(
    `Percent difference (95% CI)` = sprintf(
      "%.1f%% (%.1f, %.1f)",
      Percent, CI_low, CI_high
    ),
    `P value` = signif(p.value, 3),
    `P for interaction` = signif(`P for interaction`, 3)
  ) %>%
  select(
    Outcome,
    Group,
    `Percent difference (95% CI)`,
    `P value`,
    N,
    `P for interaction`
  )

write.xlsx(
  effect_mod_combined,
  file = here("results", "EffectModification_MetabolicStatus_Model3_PerSD.xlsx"),
  overwrite = TRUE
)

#---------------- effect modification by sex --------------------

m3_insulin_interaction_sex <- svyglm(
  log(insulin) ~ vitk_1000kcal_sd * riagendr +
    ridageyr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_insulin
)

interaction_p_insulin_sex <- tidy(m3_insulin_interaction_sex) %>%
  filter(grepl("vitk_1000kcal_sd:riagendr", term)) %>%
  pull(p.value)

insulin_sex_slopes <- emtrends(
  m3_insulin_interaction_sex,
  var = "vitk_1000kcal_sd",
  specs = "riagendr"
)

insulin_sex_slopes <- as.data.frame(insulin_sex_slopes)

insulin_sex_results <- insulin_sex_slopes %>%
  mutate(
    Percent = (exp(vitk_1000kcal_sd.trend) - 1) * 100,
    CI_low = (exp(lower.CL) - 1) * 100,
    CI_high = (exp(upper.CL) - 1) * 100,
    Outcome = "Fasting insulin (log)",
    `P for interaction` = interaction_p_insulin_sex
  ) %>%
  select(
    Outcome,
    riagendr,
    Percent,
    CI_low,
    CI_high,
    `P for interaction`
  )

insulin_sex_results <- insulin_sex_results %>%
  rename(Sex = riagendr)

m3_homa_interaction_sex <- svyglm(
  log(homa_ir) ~ vitk_1000kcal_sd * riagendr +
    ridageyr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal + bmi + waist,
  design = design_homa
)

interaction_p_homa_sex <- tidy(m3_homa_interaction_sex) %>%
  filter(grepl("vitk_1000kcal_sd:riagendr", term)) %>%
  pull(p.value)

homa_sex_slopes <- emtrends(
  m3_homa_interaction_sex,
  var = "vitk_1000kcal_sd",
  specs = "riagendr"
)

homa_sex_results <- as.data.frame(homa_sex_slopes) %>%
  mutate(
    Percent = (exp(vitk_1000kcal_sd.trend) - 1) * 100,
    CI_low = (exp(lower.CL) - 1) * 100,
    CI_high = (exp(upper.CL) - 1) * 100,
    Outcome = "HOMA-IR (log)",
    `P for interaction` = interaction_p_homa_sex
  ) %>%
  select(
    Outcome,
    riagendr,
    Percent,
    CI_low,
    CI_high,
    `P for interaction`
  ) %>%
  rename(Sex = riagendr)

sex_effect_mod <- bind_rows(
  insulin_sex_results,
  homa_sex_results
) %>%
  mutate(
    `Percent difference (95% CI)` = sprintf(
      "%.1f%% (%.1f, %.1f)",
      Percent, CI_low, CI_high
    ),
    `P for interaction` = signif(`P for interaction`, 3)
  ) %>%
  select(
    Outcome,
    Sex,
    `Percent difference (95% CI)`,
    `P for interaction`
  )

write.xlsx(
  sex_effect_mod,
  file = here("results", "EffectModification_Sex_Model3_PerSD.xlsx"),
  overwrite = TRUE
)

#---------------- effect modification by obesity ------------------
nhanes_fasting <- nhanes_fasting %>%
  mutate(
    obesity = if_else(bmi >= 30, "Obese", "Non-obese")
  ) %>%
  mutate(
    obesity = factor(
      obesity,
      levels = c("Non-obese", "Obese")
    )
  )

nhanes_design <- update(
  nhanes_design,
  obesity = nhanes_fasting$obesity
)

design_insulin <- subset(nhanes_design, !is.na(insulin))
design_homa    <- subset(nhanes_design, !is.na(glucose) & !is.na(insulin))

m2_insulin_interaction_obesity <- svyglm(
  log(insulin) ~ vitk_1000kcal_sd * obesity +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal,
  design = design_insulin
)

interaction_p_insulin_obesity <- tidy(m2_insulin_interaction_obesity) %>%
  filter(grepl("vitk_1000kcal_sd:obesity", term)) %>%
  pull(p.value)

insulin_obesity_slopes <- emtrends(
  m2_insulin_interaction_obesity,
  var = "vitk_1000kcal_sd",
  specs = "obesity"
)

insulin_obesity_slopes <- as.data.frame(insulin_obesity_slopes)

insulin_obesity_results <- insulin_obesity_slopes %>%
  mutate(
    Percent = (exp(vitk_1000kcal_sd.trend) - 1) * 100,
    CI_low = (exp(lower.CL) - 1) * 100,
    CI_high = (exp(upper.CL) - 1) * 100,
    Outcome = "Fasting insulin (log)",
    `P for interaction` = interaction_p_insulin_obesity
  ) %>%
  select(
    Outcome,
    obesity,
    Percent,
    CI_low,
    CI_high,
    `P for interaction`
  ) %>%
  rename(Group = obesity)

m2_homa_interaction_obesity <- svyglm(
  log(homa_ir) ~ vitk_1000kcal_sd * obesity +
    ridageyr + riagendr + ridreth1 +
    indfmpir + smoking + alcohol +
    kcal,
  design = design_homa
)

interaction_p_homa_obesity <- tidy(m2_homa_interaction_obesity) %>%
  filter(grepl("vitk_1000kcal_sd:obesity", term)) %>%
  pull(p.value)

homa_obesity_slopes <- emtrends(
  m2_homa_interaction_obesity,
  var = "vitk_1000kcal_sd",
  specs = "obesity"
)

homa_obesity_results <- as.data.frame(homa_obesity_slopes) %>%
  mutate(
    Percent = (exp(vitk_1000kcal_sd.trend) - 1) * 100,
    CI_low = (exp(lower.CL) - 1) * 100,
    CI_high = (exp(upper.CL) - 1) * 100,
    Outcome = "HOMA-IR (log)",
    `P for interaction` = interaction_p_homa_obesity
  ) %>%
  select(
    Outcome,
    obesity,
    Percent,
    CI_low,
    CI_high,
    `P for interaction`
  ) %>%
  rename(Group = obesity)

obesity_effect_mod <- bind_rows(
  insulin_obesity_results,
  homa_obesity_results
) %>%
  mutate(
    `Percent difference (95% CI)` = sprintf(
      "%.1f%% (%.1f, %.1f)",
      Percent, CI_low, CI_high
    ),
    `P for interaction` = signif(`P for interaction`, 3)
  ) %>%
  select(
    Outcome,
    Group,
    `Percent difference (95% CI)`,
    `P for interaction`
  )

write.xlsx(
  obesity_effect_mod,
  file = here("results", "EffectModification_Obesity_Model2_PerSD.xlsx"),
  overwrite = TRUE
)

writeLines(
  capture.output(sessionInfo()),
  here("results", "sessionInfo.txt")
)
