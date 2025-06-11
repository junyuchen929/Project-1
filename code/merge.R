rm(list = ls())
setwd("/Users/beauniverse/Desktop/project 1/data")

library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# === 1. 缩写转全名函数 ===
convert_abbr_to_full <- function(state_abbr) {
  case_when(
    str_to_upper(state_abbr) == "DC" ~ "District of Columbia",
    str_to_upper(state_abbr) == "VA" ~ "Virginia",
    str_to_upper(state_abbr) == "MD" ~ "Maryland",
    TRUE ~ state_abbr
  )
}

# === 2. CZ_NAME 转为标准 county 名称 ===
cz_to_name_lsad <- function(cz_name_vec) {
  cz_name_vec <- toupper(cz_name_vec)
  case_when(
    cz_name_vec == "DISTRICT OF COLUMBIA" ~ "District of Columbia",
    cz_name_vec == "JAMES CITY" ~ "James City County",
    cz_name_vec == "EMPORIA" ~ "Emporia city",
    cz_name_vec == "CHARLES CITY (C)" ~ "Charles City County", 
    cz_name_vec == "POQUOSON" ~ "Poquoson City", 
    str_detect(cz_name_vec, "CITY$") ~ str_to_title(cz_name_vec),
    str_detect(cz_name_vec, "\\(C\\)") & str_detect(cz_name_vec, "CITY \\(C\\)") ~ str_to_title(str_replace(cz_name_vec, " \\(C\\)", "")),
    str_detect(cz_name_vec, "\\(C\\)") ~ paste0(str_to_title(str_replace(cz_name_vec, " \\(C\\)", "")), " city"),
    TRUE ~ paste0(str_to_title(cz_name_vec), " County")
  )
}


# === 3. 统一 lowercase 用于 join 的函数 ===
prepare_for_join <- function(df) {
  df %>%
    mutate(
      state_join = str_to_lower(state),
      county_join = str_to_lower(county)
    )
}

# === 4. landcover / impervious（直接使用 state 和 county）===
prepare_landcover <- function(df) {
  df %>%
    select(state, county, builtup, forest, agriculture, other) %>%
    prepare_for_join()
}

prepare_impervious <- function(df) {
  df %>%
    select(state, county, impervious) %>%
    prepare_for_join()
}

# === 5. demo / expo / socio 数据中：state 是缩写，需要转全名 ===
prepare_other_data <- function(df) {
  df %>%
    mutate(state = convert_abbr_to_full(state)) %>%
    prepare_for_join()
}

# === 6. 读取主数据 ===
flood_df <- read_csv("data_labeled.csv") %>%
  mutate(
    state = convert_abbr_to_full(state),
    county = cz_to_name_lsad(county),
    date_dt = as.Date(date),
    time_period = case_when(
      date_dt < as.Date("2005-01-01") ~ "2000",
      date_dt < as.Date("2015-01-01") ~ "2010",
      TRUE ~ "2020"
    ),
    time_period_demo = if_else(date_dt < as.Date("2015-01-01"), "2010", "2020")
  ) %>%
  prepare_for_join()

# === 7. 加入 daymetOutput ===
daymet_df <- read_csv("daymetOutput.csv") %>%
  mutate(
    state = convert_abbr_to_full(state),
    county = case_when(
      str_to_lower(state) == "district of columbia" ~ "District of Columbia",
      str_to_lower(county) == "charles city" ~ "charles city county", 
      !str_detect(str_to_lower(county), "(county|city)$") ~ paste0(str_to_title(county), " city"),
      TRUE ~ str_to_title(county)
    ),
    date_dt = as.Date(date)
  ) %>%
  select(-date) %>%
  prepare_for_join()


flood_df <- flood_df %>%
  left_join(daymet_df, by = c("state_join", "county_join", "date_dt"))

# === 8. 加载各数据表 ===
landcover_2000 <- prepare_landcover(read_csv("landcover_2000_wide.csv"))
landcover_2010 <- prepare_landcover(read_csv("landcover_2010_wide.csv"))
landcover_2020 <- prepare_landcover(read_csv("landcover_2020_wide.csv"))

impervious_2000 <- prepare_impervious(read_csv("impervious_2000.csv"))
impervious_2010 <- prepare_impervious(read_csv("impervious_2010.csv"))
impervious_2020 <- prepare_impervious(read_csv("impervious_2020.csv"))

demo_2010 <- prepare_other_data(read_csv("demographics_all_2010.csv"))
expo_2010 <- prepare_other_data(read_csv("exposed_population_2010.csv"))
demo_2020 <- prepare_other_data(read_csv("demographics_all_2020.csv"))
expo_2020 <- prepare_other_data(read_csv("exposed_population_2020.csv"))
socio_2023 <- prepare_other_data(read_csv("socio_2023.csv"))

# === 9. 处理 road density 并转为全名 ===
# === 9. 处理 road density，模糊匹配到 flood county ===

# Step 1: Read and clean road df
road_raw <- read_csv("road_density_by_county_year.csv") %>%
  filter(year == 2023) %>%
  mutate(
    state = convert_abbr_to_full(state),
    county_raw = str_trim(county),
    state_clean = str_to_lower(state),
    county_clean = str_to_lower(county_raw)
  )

# Step 2: Extract unique flood state-county pairs
flood_keys <- flood_df %>%
  distinct(state.x, county.x) %>%
  mutate(
    state_clean = str_to_lower(state.x),
    county_clean = str_to_lower(county.x)
  )


# Step 3: Do fuzzy matching based on county substring (ignore case)
road_matched <- merge(flood_keys, road_raw, by.x = "state_clean", by.y = "state_clean") %>%
  filter(str_detect(county_clean.x, fixed(county_clean.y))) %>%
  transmute(
    state = state.x,
    county = county.x,
    road_density_km_per_km2
  ) %>%
  prepare_for_join()



# === 10. 多表 Join 函数 ===
join_all <- function(df, period, lc_df, imp_df, demo_df, expo_df, socio_df) {
  df %>%
    filter(time_period == period) %>%
    left_join(lc_df, by = c("state_join", "county_join")) %>%
    left_join(imp_df, by = c("state_join", "county_join")) %>%
    left_join(demo_df, by = c("state_join", "county_join")) %>%
    left_join(expo_df, by = c("state_join", "county_join")) %>%
    left_join(socio_df, by = c("state_join", "county_join")) %>%
    left_join(road_matched, by = c("state_join", "county_join"))
}

# === 11. 合并所有时间段 ===
flood_all <- bind_rows(
  join_all(flood_df %>% filter(time_period_demo == "2010"), "2000", landcover_2000, impervious_2000, demo_2010, expo_2010, socio_2023),
  join_all(flood_df %>% filter(time_period_demo == "2010"), "2010", landcover_2010, impervious_2010, demo_2010, expo_2010, socio_2023),
  join_all(flood_df %>% filter(time_period_demo == "2020"), "2020", landcover_2020, impervious_2020, demo_2020, expo_2020, socio_2023)
)

# === 12. 导出结果（保留原始 state, county）===
flood_all_clean <- flood_all %>%
  select(-state_join, -county_join, -time_period, -time_period_demo)

write_csv(flood_all_clean, "flood_df_full_merged.csv")
