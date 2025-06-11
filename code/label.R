rm(list = ls())
setwd("/Users/beauniverse/Desktop/project 1/data")
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# === 1. Load and keep relevant columns ===
df <- read_csv("dc_md_va_flash_floods_1996_present.csv")

df <- df %>%
  mutate(
    county = CZ_NAME,
    state = STATE  # <<< 保留 STATE 列
  )

# === 2. Combine date ===
df <- df %>%
  mutate(
    year = floor(BEGIN_YEARMONTH / 100),
    month = BEGIN_YEARMONTH %% 100,
    day = BEGIN_DAY,
    date = make_date(year, month, day)
  )

# === 3. Aggregate Event-Level Data ===
df_selected <- df %>%
  select(state, county, date, EPISODE_ID, EVENT_ID)  # <<< 加上 state

df_aggregated <- df_selected %>%
  group_by(state, county, date) %>%
  summarise(
    n_episodes = n_distinct(EPISODE_ID),
    n_events = n_distinct(EVENT_ID),
    .groups = "drop"
  )

# === 4. Convert DAMAGE_PROPERTY ===
df <- df %>%
  mutate(
    DAMAGE_PROPERTY_NUMERIC = case_when(
      grepl("K", DAMAGE_PROPERTY, ignore.case = TRUE) ~ as.numeric(gsub("K", "", DAMAGE_PROPERTY, ignore.case = TRUE)) * 1e3,
      grepl("M", DAMAGE_PROPERTY, ignore.case = TRUE) ~ as.numeric(gsub("M", "", DAMAGE_PROPERTY, ignore.case = TRUE)) * 1e6,
      TRUE ~ as.numeric(DAMAGE_PROPERTY)
    )
  )

# === 5. Aggregate damages, injuries, deaths ===
df_damages <- df %>%
  group_by(state, county, date) %>%  # <<< 加上 state
  summarise(
    total_property_damage = sum(DAMAGE_PROPERTY_NUMERIC, na.rm = FALSE),
    total_injuries = sum(INJURIES_DIRECT, INJURIES_INDIRECT, na.rm = FALSE),
    total_deaths = sum(DEATHS_DIRECT, DEATHS_INDIRECT, na.rm = FALSE),
    .groups = "drop"
  )

# === 6. Join and define label ===
df_labeled <- df_aggregated %>%
  left_join(df_damages, by = c("state", "county", "date")) %>%
  mutate(
    label = case_when(
      is.na(total_property_damage) & is.na(total_injuries) & is.na(total_deaths) ~ NA_integer_,
      (is.na(total_property_damage) | is.na(total_injuries) | is.na(total_deaths)) &
        coalesce(total_property_damage, 0) == 0 &
        coalesce(total_injuries, 0) == 0 &
        coalesce(total_deaths, 0) == 0 ~ NA_integer_,
      total_property_damage > 0 | total_injuries > 0 | total_deaths > 0 ~ 1L,
      total_property_damage == 0 & total_injuries == 0 & total_deaths == 0 ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!is.na(label))

# === 7. Add month and season ===
df_labeled <- df_labeled %>%
  mutate(
    month = month(date),
    season = case_when(
      month %in% 3:5 ~ "Spring",
      month %in% 6:8 ~ "Summer",
      month %in% 9:11 ~ "Fall",
      month %in% c(12, 1, 2) ~ "Winter",
      TRUE ~ NA_character_
    )
  )

# === 8. Days since last damaging flood AND last flood (any) ===
df_labeled <- df_labeled %>%
  arrange(state, county, date) %>%
  group_by(state, county) %>%
  mutate(
    # 上一次 damaging flood
    last_damaging_date = lag(if_else(label == 1, date, NA_Date_)),
    last_damaging_date = zoo::na.locf(last_damaging_date, na.rm = FALSE),
    days_since_last_damaging_flood = if_else(
      is.na(last_damaging_date), NA_real_,
      as.numeric(difftime(date, last_damaging_date, units = "days"))
    ),
    
    # 上一次任意 flood（即当前行的前一条记录，无论是否造成损失）
    last_flood_date = lag(date),
    days_since_prev_flood = if_else(
      is.na(last_flood_date), NA_real_,
      as.numeric(difftime(date, last_flood_date, units = "days"))
    )
  ) %>%
  ungroup() %>%
  select(-last_damaging_date, -last_flood_date)


# === 9. Save ===
write.csv(df_labeled, "data_labeled.csv", row.names = FALSE)
