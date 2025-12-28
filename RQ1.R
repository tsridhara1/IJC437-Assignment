# 0) Load packages
packages <- c("readxl", "dplyr", "tidyr", "sf", "stringr", "ggplot2", "scales", "colorspace", "ggrepel")
missing <- packages[!packages %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(colorspace)
library(ggrepel)
library(sf)


# 1) Load Excel file
file_path <- "businessdemographyexceltables2024.xlsx"
stopifnot(file.exists(file_path))


# 2) Region codes from dataset
region_regex <- "^(E12[0-9]{6}|W92000004|S92000003|N92000002)$"


# 3) READ + CLEAN: Births (2019–2024)
t11a <- read_excel(file_path, sheet = "Table 1.1a") # 2019
t11b <- read_excel(file_path, sheet = "Table 1.1b") # 2020
t11c <- read_excel(file_path, sheet = "Table 1.1c") # 2021-2023
t11d <- read_excel(file_path, sheet = "Table 1.1d") # 2024
births_19 <- t11a %>%
  filter(str_detect(.[[1]], region_regex)) %>%
  select(1:3) %>%
  rename(RegionCode = 1, RegionName = 2, `2019` = 3) %>%
  mutate(`2019` = ifelse(`2019` == ":", NA, `2019`),
         `2019` = suppressWarnings(as.numeric(`2019`)))
births_20 <- t11b %>%
  filter(str_detect(.[[1]], region_regex)) %>%
  select(1:3) %>%
  rename(RegionCode = 1, RegionName = 2, `2020` = 3) %>%
  mutate(`2020` = ifelse(`2020` == ":", NA, `2020`),
         `2020` = suppressWarnings(as.numeric(`2020`)))
births_21_22_23 <- t11c %>%
  filter(str_detect(.[[1]], region_regex)) %>%
  select(1:5) %>%
  rename(RegionCode = 1, RegionName = 2, `2021` = 3, `2022` = 4, `2023` = 5) %>%
  mutate(
    `2021` = ifelse(`2021` == ":", NA, `2021`),
    `2022` = ifelse(`2022` == ":", NA, `2022`),
    `2023` = ifelse(`2023` == ":", NA, `2023`),
    `2021` = suppressWarnings(as.numeric(`2021`)),
    `2022` = suppressWarnings(as.numeric(`2022`)),
    `2023` = suppressWarnings(as.numeric(`2023`))
  )
births_24 <- t11d %>%
  filter(str_detect(.[[1]], region_regex)) %>%
  select(1:3) %>%
  rename(RegionCode = 1, RegionName = 2, `2024` = 3) %>%
  mutate(`2024` = ifelse(`2024` == ":", NA, `2024`),
         `2024` = suppressWarnings(as.numeric(`2024`)))
births_all <- births_19 %>%
  left_join(births_20 %>% select(RegionCode, `2020`), by = "RegionCode") %>%
  left_join(births_21_22_23 %>% select(RegionCode, `2021`, `2022`, `2023`), by = "RegionCode") %>%
  left_join(births_24 %>% select(RegionCode, `2024`), by = "RegionCode")


# 4) READ + CLEAN: Deaths (2019–2024)
t21a <- read_excel(file_path, sheet = "Table 2.1a")
t21b <- read_excel(file_path, sheet = "Table 2.1b")
t21c <- read_excel(file_path, sheet = "Table 2.1c")
t21d <- read_excel(file_path, sheet = "Table 2.1d")
deaths_19 <- t21a %>%
  filter(str_detect(.[[1]], region_regex)) %>%
  select(1:3) %>%
  rename(RegionCode = 1, RegionName = 2, `2019` = 3) %>%
  mutate(`2019` = ifelse(`2019` == ":", NA, `2019`),
         `2019` = suppressWarnings(as.numeric(`2019`)))
deaths_20 <- t21b %>%
  filter(str_detect(.[[1]], region_regex)) %>%
  select(1:3) %>%
  rename(RegionCode = 1, RegionName = 2, `2020` = 3) %>%
  mutate(`2020` = ifelse(`2020` == ":", NA, `2020`),
         `2020` = suppressWarnings(as.numeric(`2020`)))
deaths_21_22_23 <- t21c %>%
  filter(str_detect(.[[1]], region_regex)) %>%
  select(1:5) %>%
  rename(RegionCode = 1, RegionName = 2, `2021` = 3, `2022` = 4, `2023` = 5) %>%
  mutate(
    `2021` = ifelse(`2021` == ":", NA, `2021`),
    `2022` = ifelse(`2022` == ":", NA, `2022`),
    `2023` = ifelse(`2023` == ":", NA, `2023`),
    `2021` = suppressWarnings(as.numeric(`2021`)),
    `2022` = suppressWarnings(as.numeric(`2022`)),
    `2023` = suppressWarnings(as.numeric(`2023`))
  )
deaths_24 <- t21d %>%
  filter(str_detect(.[[1]], region_regex)) %>%
  select(1:3) %>%
  rename(RegionCode = 1, RegionName = 2, `2024` = 3) %>%
  mutate(`2024` = ifelse(`2024` == ":", NA, `2024`),
         `2024` = suppressWarnings(as.numeric(`2024`)))
deaths_all <- deaths_19 %>%
  left_join(deaths_20 %>% select(RegionCode, `2020`), by = "RegionCode") %>%
  left_join(deaths_21_22_23 %>% select(RegionCode, `2021`, `2022`, `2023`), by = "RegionCode") %>%
  left_join(deaths_24 %>% select(RegionCode, `2024`), by = "RegionCode")


# 5) READ + CLEAN: Active enterprises (2019–2024)
t31a <- read_excel(file_path, sheet = "Table 3.1a")
t31b <- read_excel(file_path, sheet = "Table 3.1b")
t31c <- read_excel(file_path, sheet = "Table 3.1c")
t31d <- read_excel(file_path, sheet = "Table 3.1d")
active_19 <- t31a %>%
  filter(str_detect(.[[1]], region_regex)) %>%
  select(1:3) %>%
  rename(RegionCode = 1, RegionName = 2, `2019` = 3) %>%
  mutate(`2019` = ifelse(`2019` == ":", NA, `2019`),
         `2019` = suppressWarnings(as.numeric(`2019`)))
active_20 <- t31b %>%
  filter(str_detect(.[[1]], region_regex)) %>%
  select(1:3) %>%
  rename(RegionCode = 1, RegionName = 2, `2020` = 3) %>%
  mutate(`2020` = ifelse(`2020` == ":", NA, `2020`),
         `2020` = suppressWarnings(as.numeric(`2020`)))
active_21_22_23 <- t31c %>%
  filter(str_detect(.[[1]], region_regex)) %>%
  select(1:5) %>%
  rename(RegionCode = 1, RegionName = 2, `2021` = 3, `2022` = 4, `2023` = 5) %>%
  mutate(
    `2021` = ifelse(`2021` == ":", NA, `2021`),
    `2022` = ifelse(`2022` == ":", NA, `2022`),
    `2023` = ifelse(`2023` == ":", NA, `2023`),
    `2021` = suppressWarnings(as.numeric(`2021`)),
    `2022` = suppressWarnings(as.numeric(`2022`)),
    `2023` = suppressWarnings(as.numeric(`2023`))
  )
active_24 <- t31d %>%
  filter(str_detect(.[[1]], region_regex)) %>%
  select(1:3) %>%
  rename(RegionCode = 1, RegionName = 2, `2024` = 3) %>%
  mutate(`2024` = ifelse(`2024` == ":", NA, `2024`),
         `2024` = suppressWarnings(as.numeric(`2024`)))
active_all <- active_19 %>%
  left_join(active_20 %>% select(RegionCode, `2020`), by = "RegionCode") %>%
  left_join(active_21_22_23 %>% select(RegionCode, `2021`, `2022`, `2023`), by = "RegionCode") %>%
  left_join(active_24 %>% select(RegionCode, `2024`), by = "RegionCode")


# 5.1) Build region_panel (long) + derived variables
births_long <- births_all %>%
  pivot_longer(cols = matches("^20(19|20|21|22|23|24)$"),
               names_to = "year", values_to = "births") %>%
  mutate(year = as.integer(year))
deaths_long <- deaths_all %>%
  pivot_longer(cols = matches("^20(19|20|21|22|23|24)$"),
               names_to = "year", values_to = "deaths") %>%
  mutate(year = as.integer(year))
active_long <- active_all %>%
  pivot_longer(cols = matches("^20(19|20|21|22|23|24)$"),
               names_to = "year", values_to = "active") %>%
  mutate(year = as.integer(year))
region_panel <- births_long %>%
  left_join(deaths_long %>% select(RegionCode, year, deaths), by = c("RegionCode","year")) %>%
  left_join(active_long %>% select(RegionCode, year, active), by = c("RegionCode","year")) %>%
  mutate(
    net_change = births - deaths,
    churn_rate = (births + deaths) / active
  ) %>%
  arrange(RegionName, year)
View(region_panel)


# 6) Plots
# Convert births + deaths into long format
bd_long <- region_panel %>%
  select(RegionName, year, births, deaths) %>%
  pivot_longer(cols = c(births, deaths),
               names_to = "metric",
               values_to = "value") %>%
  mutate(metric = recode(metric,
                         births = "Births",
                         deaths = "Deaths"))


# 6.1) Plot: Enterprise births and deaths by region (2019–2024)
ggplot(bd_long, aes(x = year, y = value, colour = metric, group = metric)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  facet_wrap(~RegionName, scales = "free_y", ncol = 4) +
  scale_x_continuous(breaks = 2019:2024) +
  scale_colour_manual(values = c("Births" = "blue", "Deaths" = "red")) +
  labs(
    title = "Enterprise births and deaths by region (2019–2024)",
    x = "Year",
    y = "Count",
    colour = "Metric"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# 6.2) Plot: Net change (births − deaths) by region (2019–2024)
ggplot(region_panel, aes(x = year, y = net_change, group = RegionName)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~RegionName, scales = "free_y", ncol = 4) +
  scale_x_continuous(breaks = 2019:2024) +
  labs(title = "Net change (births − deaths) by region (2019–2024)",
       x = "Year", y = "Net change") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# 6.3) Plot: Churn rate heatmap (region × year)
ggplot(region_panel %>% mutate(RegionName = forcats::fct_reorder(RegionName, churn_rate, .fun = mean, .desc = TRUE)),
       aes(x = factor(year), y = RegionName, fill = churn_rate)) +
  geom_tile(color = "white", linewidth = 0.2) +
  labs(title = "Churn rate heatmap (region × year)", x = "Year", y = "Region", fill = "Churn rate") +
  theme_minimal(base_size = 12)


# 7) Train / Test split
train_df <- region_panel %>% filter(year >= 2019, year <= 2023)
test_df <- region_panel %>% filter(year == 2024)


# 7.1) PLOT: Train data (2019–2023)
ggplot(train_df, aes(x = year, y = births)) +
  geom_point() +
  facet_wrap(~RegionName, scales = "free_y") +
  labs(title = "Train data (2019–2023): births vs year",
       x = "Year", y = "Births") +
  theme_minimal(base_size = 11)


# 7.2) Regression per region (lm): predict 2024 + % error, forecast 2025
regions <- train_df %>% distinct(RegionCode, RegionName)
pred_table <- tibble(
  RegionCode = character(),
  RegionName = character(),
  births_pred_2024 = double(),
  deaths_pred_2024 = double(),
  active_pred_2024 = double(),
  births_pct_error_2024 = double(),
  deaths_pct_error_2024 = double(),
  active_pct_error_2024 = double(),
  births_2025 = double(),
  deaths_2025 = double(),
  active_2025 = double()
)
for (i in 1:nrow(regions))
{
  rc <- regions$RegionCode[i]
  rn <- regions$RegionName[i]
  train_r <- train_df %>% filter(RegionCode == rc) %>% arrange(year)
  test_r <- test_df %>% filter(RegionCode == rc)
  # births model
  mod_births <- lm(births ~ year, data = train_r)
  births_pred_2024 <- as.numeric(predict(mod_births, newdata = test_r))
  births_2025 <- as.numeric(predict(mod_births, newdata = data.frame(year = 2025)))
  # deaths model
  mod_deaths <- lm(deaths ~ year, data = train_r)
  deaths_pred_2024 <- as.numeric(predict(mod_deaths, newdata = test_r))
  deaths_2025 <- as.numeric(predict(mod_deaths, newdata = data.frame(year = 2025)))
  # active model
  mod_active <- lm(active ~ year, data = train_r)
  active_pred_2024 <- as.numeric(predict(mod_active, newdata = test_r))
  active_2025 <- as.numeric(predict(mod_active, newdata = data.frame(year = 2025)))
  # % error for 2024 (absolute percent error)
  # guard against divide-by-zero (not expected here, but safe)
  births_pct_err <- ifelse(is.na(test_r$births) | test_r$births == 0, NA,
                           100 * abs(test_r$births - births_pred_2024) / test_r$births)
  deaths_pct_err <- ifelse(is.na(test_r$deaths) | test_r$deaths == 0, NA,
                           100 * abs(test_r$deaths - deaths_pred_2024) / test_r$deaths)
  active_pct_err <- ifelse(is.na(test_r$active) | test_r$active == 0, NA,
                           100 * abs(test_r$active - active_pred_2024) / test_r$active)
  pred_table <- bind_rows(
    pred_table,
    tibble(
      RegionCode = rc,
      RegionName = rn,
      births_pred_2024 = births_pred_2024,
      deaths_pred_2024 = deaths_pred_2024,
      active_pred_2024 = active_pred_2024,
      births_pct_error_2024 = births_pct_err,
      deaths_pct_error_2024 = deaths_pct_err,
      active_pct_error_2024 = active_pct_err,
      births_2025 = births_2025,
      deaths_2025 = deaths_2025,
      active_2025 = active_2025
    )
  )
}
View(pred_table)


# 7.3) PLOT: 2024 Test — Actual vs Predicted births
compare_2024_births <- test_df %>%
  select(RegionCode, RegionName, births) %>%
  left_join(pred_table %>% select(RegionCode, RegionName, births_pred_2024, births_pct_error_2024),
            by = c("RegionCode","RegionName"))
View(compare_2024_births)
ggplot(compare_2024_births, aes(x = births, y = births_pred_2024, label = RegionName)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggrepel::geom_text_repel(size = 3) +
  labs(title = "2024 Test: Actual vs Predicted births (regression)",
       x = "Actual births (2024)", y = "Predicted births (2024)") +
  theme_minimal(base_size = 12)


# 8) Forecast 2025 metrics + PLOT: net enterprise change 2025
forecast_2025 <- pred_table %>%
  transmute(
    RegionCode, RegionName,
    births_2025, deaths_2025, active_2025,
    net_change_2025 = births_2025 - deaths_2025,
    churn_rate_2025 = (births_2025 + deaths_2025) / active_2025
  )
View(forecast_2025)
ggplot(forecast_2025 %>% arrange(net_change_2025),
       aes(x = reorder(RegionName, net_change_2025), y = net_change_2025)) +
  geom_col() +
  coord_flip() +
  labs(title = "Forecast net enterprise change by region (2025, regression)",
       x = "Region", y = "Forecast net change (2025)") +
  theme_minimal(base_size = 12)


# 9) MAP: Forecast net enterprise change 2025 (regression)
nuts_key <- tibble::tribble(
  ~RegionName, ~NUTS_ID,
  "NORTH EAST", "UKC",
  "NORTH WEST", "UKD",
  "YORKSHIRE AND THE HUMBER", "UKE",
  "EAST MIDLANDS", "UKF",
  "WEST MIDLANDS", "UKG",
  "EAST", "UKH",
  "LONDON", "UKI",
  "SOUTH EAST", "UKJ",
  "SOUTH WEST", "UKK",
  "WALES", "UKL",
  "SCOTLAND", "UKM",
  "NORTHERN IRELAND", "UKN"
)
nuts1 <- gisco_get_nuts(country = "UK", nuts_level = 1, year = 2021, resolution = "20") %>%
  select(NUTS_ID, geometry)
map_2025 <- forecast_2025 %>%
  select(RegionName, net_change_2025) %>%
  left_join(nuts_key, by = "RegionName") %>%
  left_join(nuts1, by = "NUTS_ID") %>%
  st_as_sf()
ggplot(map_2025) +
  geom_sf(aes(fill = net_change_2025), colour = "white", linewidth = 0.2) +
  labs(title = "Forecast net enterprise change, 2025 (regression)",
       fill = "Net change (2025)") +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )