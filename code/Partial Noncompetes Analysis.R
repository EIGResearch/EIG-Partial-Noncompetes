
# Title: Partial Noncompete Bans Analysis
# Author: Thomas Cronin
# File Purpose: Create Maps Showing Coverage of Noncompete Bans by State
# Last Updated: 3/6/2026

#load packages
packages <- c("tidyverse", "ipumsr", "openxlsx", "janitor", "tigris")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}
invisible(lapply(packages, library, character.only = TRUE))

## Load and Clean Data ----

#load data
acs_data <- define_extract_micro(collection = "usa",
                                       description = "ACS extract for non-competes calculations",
                                       samples = c("us2024c"),
                                       variables = c("YEAR", "HHWT", "STATEFIP", "HHINCOME", "PERWT",
                                                     "SCHOOL", "EMPSTAT", "EMPSTATD", "CLASSWKR", "AGE",
                                                     "CLASSWKRD", "OCC2010", "INCEARN", "PWSTATE2")) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = "data") %>%
  read_ipums_micro()

state_xwalk <- fips_codes %>%
  distinct(state_code, state_name, state) %>%
  mutate(state_fips = as.numeric(state_code)) %>%
  select(-state_code)

occupations <- read.xlsx("data/Non-Compete Laws by State.xlsx", sheet = "Occupation Limits Data") %>% #created manually by encoding the most recent occupation-based ban statutes in each state into Census 2010 occupation codes. See Appendix for more details.
  clean_names() %>%
  select(-simplifying_assumption_flag, -notes, -questions) %>%
  distinct(state, occupation_code, .keep_all = TRUE) %>%
  full_join(state_xwalk, by = "state")

income_caps <- read.xlsx("data/Non-Compete Laws by State.xlsx", sheet = "Earnings Limits Data") %>% #created manually using the most recent income-based ban statutes in each state
  clean_names() %>%
  full_join(state_xwalk, by = c("state" = "state_name")) %>%
  mutate(across(starts_with("earnings"), as.numeric)) %>%
  select(state_fips, starts_with("earnings"))

cps_data <- define_extract_micro(collection = "cps",
                                 description = "CPS extract for non-competes calculations",
                                 samples = c("cps2025_03s"), #use 2025 ASEC for 2024 data
                                 variables = c("YEAR", "STATEFIP", "ASECWT", "OCC2010", "PAIDHOUR")) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = "data") %>%
  read_ipums_micro()

#restrict sample to employed adults w/ income
acs_workers_full <- acs_data %>%
  clean_names() %>%
  filter(age >= 18 &
           empstat == 1 & #employed
           !empstatd %in% c(13, 14) & #not in armed forces
           incearn > 0) %>% #has earnings
  rename(statefip_work = pwstate2)

## Encode Rules ----

#summarize hourly employees in Nevada
pct_hourly_occ <- cps_data %>%
  clean_names() %>%
  filter(statefip == 32) %>%
  mutate(hourly = case_when(paidhour == 2 ~ 1,
                            paidhour != 2 ~ 0,
                            TRUE ~ paidhour)) %>%
  zap_labels(hourly) %>%
  group_by(year, statefip, occ2010) %>%
  summarize(noncompetes_banned_pct = weighted.mean(hourly, asecwt, na.rm = TRUE), .groups = "drop") %>%
  group_by(statefip, occ2010) %>%
  summarize(noncompetes_banned_pct = mean(noncompetes_banned_pct, na.rm = TRUE), .groups = "drop")

#encode bans
ban_states <- c(6, 16, 27, 30, 38, 40)
no_ban_states <- c(1, 2, 5, 12, 20, 22, 26, 28, 31, 37, 39, 42, 45, 46, 48, 49, 54, 55)

acs_workers <- acs_workers_full %>%
  mutate(occ2010 = as.character(occ2010)) %>%
  left_join(occupations, by = c("statefip_work" = "state_fips", "occ2010" = "occupation_code")) %>% #general occupation bans
  left_join(income_caps, by = c("statefip_work" = "state_fips")) %>%
  mutate(occ2010 = parse_number(occ2010)) %>%
  left_join(pct_hourly_occ, by = c("statefip", "occ2010")) %>%
  mutate(noncompetes_banned = case_when(incearn <= earnings_limit_2024 ~ 1, #general income bans
                                        #additional rules
                                        #occ2010 == 2100, #national ban for lawyers?
                                        statefip_work == 4 & is.na(noncompetes_banned) ~ 0, #Arizona
                                        statefip_work == 8 & is.na(noncompetes_banned) ~ 0, #Colorado
                                        statefip_work == 9 & is.na(noncompetes_banned) ~ 0, #Connecticut
                                        statefip_work == 10 & is.na(noncompetes_banned) ~ 0, #Delaware
                                        statefip_work == 11 & (classwkrd == 25 | (occ2010 == 3060 & incearn > earnings_limit_2_2024)) ~ 0, #federal employees and highly-compensated physicians in DC
                                        statefip_work == 13 & is.na(noncompetes_banned) ~ 1, #Georgia
                                        statefip_work == 15 & is.na(noncompetes_banned) ~ 0, #Hawaii
                                        statefip_work == 17 & classwkr %in% c(27, 28) ~ 0.484, #Illinois unionized public employees
                                        statefip_work == 17 & is.na(noncompetes_banned) ~ 0, #Illinois other employees
                                        statefip_work == 19 & occ2010 %in% c(3255, 3600) ~ 0.039, #Iowa healthcare employment agency employees
                                        statefip_work == 19 & is.na(noncompetes_banned) ~ 0, #Iowa other employees
                                        statefip_work == 21 & occ2010 %in% c(3255, 3600) ~ 0.039, #Kentucky healthcare employment agency employees
                                        statefip_work == 21 & is.na(noncompetes_banned) ~ 0, #Kentucky other employees
                                        statefip_work == 23 & occ2010 == 3110 ~ 1, #Maine veterinarians
                                        statefip_work == 23 & is.na(noncompetes_banned) ~ 0, #Maine other employees
                                        statefip_work == 24 & (occ2010 %in% c(3000, 3010, 3030, 3040, 3050, 3060, 3120, 3255, 3500, 2000, 
                                                                              2010, 2020, 2050, 3150, 3160, 3200, 3210, 3230, 3110, 3645) & incearn <= earnings_limit_2_2024) ~ 1, #Maryland
                                        statefip_work == 24 & occ2010 == 3250 ~ 1, #Maryland veterinarians
                                        statefip_work == 25 & (!(classwkrd %in% c(10, 13, 14)) | age < 18 | school == 2) ~ 1, #Massachusetts self-employed, minors, and students
                                        statefip_work == 25 & is.na(noncompetes_banned) ~ 0, #Massachusetts other employees
                                        statefip_work == 29 & is.na(noncompetes_banned) ~ 0, #Missouri
                                        statefip_work == 32 & is.na(noncompetes_banned) ~ noncompetes_banned_pct, #Nevada part-time workers
                                        statefip_work == 32 & is.na(noncompetes_banned_pct) ~ 0, #Nevada full-time workers
                                        statefip_work == 34 & occ2010 %in% c(4600, 4610, 4640, 4650, 4460, 4500) & (classwkrd %in% c(10, 13, 14)) ~ 1, #New Jersey self-employed domestic workers
                                        statefip_work == 34 & is.na(noncompetes_banned) ~ 0, #New Jersey
                                        statefip_work == 35 & is.na(noncompetes_banned) ~ 0, #New Mexico
                                        statefip_work == 36 & is.na(noncompetes_banned) ~ 0, #New York
                                        statefip_work == 41 & is.na(noncompetes_banned) ~ 0, #Oregon
                                        statefip_work == 44 & (!(classwkrd %in% c(10, 13, 14)) | age < 18 | school == 2) ~ 1, #Rhode Island
                                        statefip_work == 44 & is.na(noncompetes_banned) ~ 0, #Rhode Island other employees
                                        statefip_work == 47 & occ2010 == 3060 ~ 0.046, #Tennessee emergency medicine physicians
                                        statefip_work == 47 & is.na(noncompetes_banned) ~ 0, #Tennessee other employees
                                        statefip_work == 50 & occ2010 %in% c(4510, 4520) & school == 2 ~ 1, #Vermont barber & cosmetology students
                                        statefip_work == 50 & is.na(noncompetes_banned) ~ 0, #Vermont other employees
                                        statefip_work == 51 & (!(classwkrd %in% c(10, 13, 14)) | school == 2) & is.na(noncompetes_banned) ~ 1, #Virginia self-employed & students
                                        statefip_work == 51 & is.na(noncompetes_banned) ~ 0, #Virginia other employees
                                        statefip_work == 53 & (classwkrd %in% c(10, 13, 14)) & incearn < earnings_limit_2_2024 ~ 1, #Washington self-employed
                                        statefip_work == 56 & is.na(noncompetes_banned) ~ 1, #Wyoming
                                        statefip_work %in% ban_states ~ 1,
                                        statefip_work %in% no_ban_states ~ 0,
                                        is.na(noncompetes_banned) == TRUE ~ 0,
                                        TRUE ~ noncompetes_banned))

## Calculate % Covered

#collapse to state totals
acs_workers <- acs_workers %>%
  group_by(statefip_work) %>%
  summarize(total_workforce = round(sum(perwt, na.rm = TRUE), digits = 0), 
            total_banned_workforce = round(sum(perwt * noncompetes_banned, na.rm = TRUE), digits = 0),
            total_income = round(sum(incearn * perwt, na.rm = TRUE), digits = 0), 
            total_banned_income = round(sum(incearn * noncompetes_banned * perwt, na.rm = TRUE), digits = 0),
            .groups = "drop") %>%
  mutate(pct_workforce_banned = round((total_banned_workforce/total_workforce), digits = 4), 
         pct_income_banned = round((total_banned_income/total_income), digits = 4), 
         income_unbanned = round((total_income - total_banned_income), digits = 0)) %>%
  left_join(state_xwalk, by = c("statefip_work" = "state_fips")) %>%
  filter(!is.na(state) & statefip_work <= 56) %>%
  select(statefip_work, state, everything())
  
#adjust Idaho totals
idaho_income_diff <- acs_workers_full %>%
  filter(statefip_work == 16) %>%
  select(perwt, occ2010, incearn) %>%
  group_by(occ2010) %>%
  mutate(p95 = Hmisc::wtd.quantile(incearn, perwt, probs = 0.95),
         top5 = incearn >= p95) %>%
  ungroup() %>%
  filter(top5 == TRUE) %>%
  summarize(total_unbanned_income = round(sum(incearn * perwt, na.rm = TRUE), digits = 0)) %>%
  pull(total_unbanned_income)

acs_workers <- acs_workers %>%
  mutate(pct_workforce_banned = case_when(statefip_work == 16 ~ pct_workforce_banned - 0.05, TRUE ~ pct_workforce_banned),
         total_banned_workforce = case_when(statefip_work == 16 ~ round(total_banned_workforce - (total_workforce * 0.05), digits = 0), TRUE ~ total_banned_workforce),
         total_banned_income = case_when(statefip_work == 16 ~ total_banned_income - idaho_income_diff, TRUE ~ total_banned_income),
         pct_income_banned = case_when(statefip_work == 16 ~ round((total_banned_income/total_income), digits = 4), TRUE ~ pct_income_banned),
         income_unbanned = case_when(statefip_work == 16 ~ idaho_income_diff, TRUE ~ income_unbanned))

#export final totals to excel
acs_workers <- acs_workers %>%
  select(state_name, total_workforce, total_banned_workforce, pct_workforce_banned,
         total_income, income_unbanned, total_banned_income, pct_income_banned) %>%
  rename(State = state_name,
         `Total Workforce` = total_workforce,
         `Total Workforce With Ban` = total_banned_workforce,
         `Total Income` = total_income,
         `Total Income With Ban` = total_banned_income,
         `% Workforce With Ban` = pct_workforce_banned,
         `% Income With Ban` = pct_income_banned,
         `Total Income Without Ban` = income_unbanned)

write.xlsx(acs_workers, "output/State Ban Totals.xlsx", overwrite = TRUE)
