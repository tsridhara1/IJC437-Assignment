# 0) Load packages
packages <- c("readxl", "dplyr", "tidyr", "sf", "stringr", "ggplot2", "scales", "colorspace", "ggrepel", "giscoR")
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
library(giscoR)


# 1) Load Excel file
file_path <- "businessdemographyexceltables2024.xlsx"
stopifnot(file.exists(file_path))


# 2) Function to read Table 4.1 and 4.2 (survival tables)
# - Finds the header row where column 2 == "Births"
# - Builds CohortYear from the year rows
# - Converts ":" to NA
read_table4_survival <- function(path, sheet_name, entity_name = c("Region", "Industry"))
{
  entity_name <- match.arg(entity_name)
  raw <- read_excel(path, sheet = sheet_name, col_names = FALSE)
  # Header row is where column 2 has the word "Births"
  header_row <- which(raw[[2]] == "Births")[1]
  if (is.na(header_row)) stop("Could not find the 'Births' header row in column 2.")
  # The table has 12 key columns (entity + births + 5 survival count/% pairs)
  df <- raw %>%
    slice((header_row + 1):n()) %>%
    select(1:12) %>%
    setNames(c(
      "Entity", "Births",
      "Surv_1", "Pct_1",
      "Surv_2", "Pct_2",
      "Surv_3", "Pct_3",
      "Surv_4", "Pct_4",
      "Surv_5", "Pct_5"
    )) %>%
    filter(!(is.na(Entity) & is.na(Births))) %>%
    mutate(across(everything(), ~ na_if(str_trim(as.character(.x)), ":"))) %>%
    mutate(
      CohortYear = if_else(str_detect(Entity, "^\\d{4}$"), as.integer(Entity), NA_integer_)
    ) %>%
    fill(CohortYear, .direction = "down") %>%
    filter(!str_detect(Entity, "^\\d{4}$")) %>%
    mutate(across(c(Births, starts_with("Surv_"), starts_with("Pct_")),
                  ~ suppressWarnings(as.numeric(.x)))) %>%
    rename(!!entity_name := Entity)
  # Long format survival % for plotting
  pct_long <- df %>%
    pivot_longer(starts_with("Pct_"), names_to = "H", values_to = "SurvivalPct") %>%
    mutate(
      HorizonYears = as.integer(str_extract(H, "\\d+")),
      HorizonLabel = factor(HorizonYears, levels = 1:5, labels = paste0(1:5, " Year"))
    ) %>%
    select(CohortYear, all_of(entity_name), Births, HorizonYears, HorizonLabel, SurvivalPct)
  list(wide = df, pct_long = pct_long)
}


# 3) Plot— 1-year survival by region (2019–2023 cohorts)
# End labels remove legend clutter.
t41 <- read_table4_survival(file_path, "Table 4.1", entity_name = "Region")
region_1yr <- t41$wide %>%
  filter(CohortYear %in% 2019:2023, Region != "Total") %>%
  select(CohortYear, Region, Births, Pct_1) %>%
  filter(!is.na(Pct_1)) %>%
  mutate(Pct_1 = round(Pct_1, 2)) %>%
  arrange(Region, CohortYear)
View(region_1yr)
# Colors (high contrast)
region_levels <- sort(unique(region_1yr$Region))
region_cols <- setNames(rainbow_hcl(length(region_levels), c = 130, l = 45), region_levels)
end_labels_reg <- region_1yr %>% filter(CohortYear == 2023)
p_region_1yr <- ggplot(region_1yr, aes(x = CohortYear, y = Pct_1, group = Region, color = Region)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  geom_text_repel(
    data = end_labels_reg,
    aes(label = Region),
    nudge_x = 0.25, hjust = 0, direction = "y",
    size = 3.2, segment.size = 0.2, min.segment.length = 0,
    show.legend = FALSE
  ) +
  scale_color_manual(values = region_cols, guide = "none") +
  scale_x_continuous(breaks = 2019:2023, limits = c(2019, 2023.7)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "1-Year Business Survival Rate by Region (2019–2023 Cohorts)",
    x = "Year of birth (cohort)",
    y = "1-year survival rate (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())
print(p_region_1yr)


# 4) Plot — Industry survival curves (2019 cohort)
# Better readability: grey all industries, colored are top/bottom by 5-yr survival
t42 <- read_table4_survival(file_path, "Table 4.2", entity_name = "Industry")
industry_2019_wide <- t42$wide %>%
  filter(CohortYear == 2019) %>%
  select(Industry, Births, Pct_1, Pct_2, Pct_3, Pct_4, Pct_5) %>%
  mutate(across(starts_with("Pct_"), ~ round(.x, 2))) %>%
  arrange(desc(Pct_5), desc(Births))
View(industry_2019_wide)
industry_2019_long <- t42$pct_long %>%
  filter(CohortYear == 2019, !is.na(SurvivalPct), Industry != "Total")
# Choose how many industries to highlight
TOP_N <- 5
BOTTOM_N <- 5
ranking <- industry_2019_wide %>%
  filter(!is.na(Pct_5), Industry != "Total")
top_inds <- ranking %>% slice_head(n = TOP_N) %>% pull(Industry)
bottom_inds <- ranking %>% slice_tail(n = BOTTOM_N) %>% pull(Industry)
highlight_inds <- unique(c(top_inds, bottom_inds))
plot_ind <- industry_2019_long %>%
  mutate(group_type = if_else(Industry %in% highlight_inds, "Highlight", "Other"))
# Colors only for highlighted industries (high contrast)
highlight_cols <- setNames(
  rainbow_hcl(length(highlight_inds), c = 130, l = 45),
  highlight_inds
)
end_labels_ind <- plot_ind %>%
  filter(HorizonYears == 5, Industry %in% highlight_inds)
p_industry_2019 <- ggplot(plot_ind, aes(x = HorizonYears, y = SurvivalPct, group = Industry)) +
  # Grey context
  geom_line(
    data = plot_ind %>% filter(group_type == "Other"),
    color = "grey75", linewidth = 0.7, alpha = 0.6
  ) +
  geom_point(
    data = plot_ind %>% filter(group_type == "Other"),
    color = "grey75", size = 1.6, alpha = 0.6
  ) +
  # Highlighted lines
  geom_line(
    data = plot_ind %>% filter(group_type == "Highlight"),
    aes(color = Industry),
    linewidth = 1.2
  ) +
  geom_point(
    data = plot_ind %>% filter(group_type == "Highlight"),
    aes(color = Industry),
    size = 2.2
  ) +
  # End labels (removes need for legend)
  geom_text_repel(
    data = end_labels_ind,
    aes(label = Industry, color = Industry),
    nudge_x = 0.2, hjust = 0, direction = "y",
    size = 3.2, segment.size = 0.2, min.segment.length = 0,
    show.legend = FALSE
  ) +
  scale_color_manual(values = highlight_cols, guide = "none") +
  scale_x_continuous(breaks = 1:5, labels = paste0(1:5, " Year"), limits = c(1, 5.7)) +
  scale_y_continuous(limits = c(0, 100), labels = label_percent(scale = 1)) +
  labs(
    title = "Industry Survival Rates (2019 Birth Cohort)",
    subtitle = paste0("Grey = all industries; coloured = Top ", TOP_N,
                      " and Bottom ", BOTTOM_N, " by 5-year survival."),
    x = "Survival period",
    y = "Survival rate (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

print(p_industry_2019)
