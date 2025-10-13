# An analysis of short-term effects of migration inflows on unemployment rates across 38 OECD countries (1991–2020)

## Abstract

This project investigates whether migration inflows are associated with short-term changes in unemployment rates across 38 OECD countries between 1991 and 2020. Using publicly available data from the World Bank and OECD, I constructed a dataset and examined how net migration (as a share of population) relates to unemployment one and five years later.

The results suggest a weak but statistically significant negative relationship between migration inflows and unemployment in the following year. On average, countries experiencing higher migration inflows tend to record slightly lower unemployment rates the next year (β = -1.79, p < 0.001, R² = 0.059). This negative effect diminishes over time but is nonetheless present five years later (β = -0.63, p = 0.027, R² = 0.008).

These findings indicate that migration inflows lead to short-term declines in unemployment. However, the strength and direction of this relationship vary significantly between countries, emphasising the importance of national labour market structures and policies. While the study omits several control variables and serves primarily as a learning exercise in R-based data analysis, it provides an illustration of how open data can be explored.

---

## Introduction

Despite international migrants constituting for less than 4 per cent of the global population (UN Migration, 2024), the impact of migration on national labour markets is a widely debated and polarising topic. This study attempts to address some common preconceptions about how migration affects unemployment, through a data-driven lens.

This research analyses migration data from 38 OECD countries (1991–2020) compared with lagged unemployment data from one and five years later.

---

## Downloading and transforming global data

I downloaded data from publicly available and reliable sources: **World Bank DataBank** and **OECD Data Explorer**. The datasets cover 1991–2020 and include:

- SP.POP.TOTL: Total population  
- SM.POP.NETM: Net migration (immigrants minus emigrants)  
- SL.UEM.TOTL.ZS: Unemployment rate (% of labour force)

After downloading the CSV files, I cleaned and reshaped them in **R** using `tidyverse`.

```r
library(tidyverse)
library(broom)
library(stringr)

# Population data
pop <- read.csv("SP.POP.TOTL.csv")
pop_long <- pop %>%
  select(`Country.Name`, `Country.Code`, matches("^X\\d{4}")) %>%
  rename_with(~ str_extract(., "(\\d{4})"), matches("^X\\d{4}")) %>%
  pivot_longer(cols = -c(`Country.Name`, `Country.Code`),
               names_to = "year", values_to = "population") %>%
  mutate(year = as.integer(year)) %>%
  rename("Country Name" = "Country.Name", "Country Code" = "Country.Code")

# Migration data
migr <- read_csv("SM.POP.NETM.csv")
migr_long <- migr %>%
  select(`Country Name`, `Country Code`, matches("^\\d{4}")) %>%
  rename_with(~ str_extract(., "^\\d{4}"), matches("^\\d{4}")) %>%
  pivot_longer(cols = -c(`Country Name`, `Country Code`),
               names_to = "year", values_to = "net_migration") %>%
  mutate(year = as.integer(year))

# Unemployment data
unemp <- read_csv("SL.UEM.TOTL.ZS.csv")
unemp_long <- unemp %>%
  select(`Country Name`, `Country Code`, matches("^\\d{4}")) %>%
  rename_with(~ str_extract(., "^\\d{4}"), matches("^\\d{4}")) %>%
  pivot_longer(cols = -c(`Country Name`, `Country Code`),
               names_to = "year", values_to = "unemployment_rate") %>%
  mutate(year = as.integer(year))
```

---

## Creating a data frame and independent variable

I merged the converted datasets into one data frame and filtered for 38 OECD countries.

```r
df_wdi <- unemp_long %>%
  left_join(migr_long, by = c("Country Name", "Country Code", "year")) %>%
  left_join(pop_long, by = c("Country Name", "Country Code", "year"))

oecd_iso <- c("AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST","FIN",
              "FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN","KOR","LVA",
              "LTU","LUX","MEX","NLD","NZL","NOR","POL","PRT","SVK","SVN","ESP",
              "SWE","CHE","TUR","GBR","USA")

df_oecd <- df_wdi %>% filter(`Country Code` %in% oecd_iso)
```

I then corrected data formats, added a variable for net migration share as a percentage of population, and checked for missing values.

```r
df_oecd <- df_oecd %>%
  mutate(unemployment_rate = as.numeric(unemployment_rate),
         net_migration = as.numeric(net_migration),
         migration_share = (net_migration / population) * 100)

sum(is.na(df_oecd))  # 0 missing
write_csv(df_oecd, "df_oecd.csv")
```

---

## Exploratory analysis: migration vs same-year unemployment

Before modelling, I explored the existing dataset.

Key insights:
- Average migration inflow (1991–2020): 0.24% of population per year
- Average unemployment rate: 7.7%
- Considerable variation across countries and years

```r
df_oecd %>% summarise(
  avg_migration = mean(migration_share, na.rm = TRUE),
  sd_migration  = sd(migration_share, na.rm = TRUE),
  avg_unemp     = mean(unemployment_rate, na.rm = TRUE),
  sd_unemp      = sd(unemployment_rate, na.rm = TRUE)
)
```

Visual comparison of migration and unemployment trends:

![Avarege Migration Inflows vs Unemployment Rates Across OECD Countries](https://github.com/stan-maliszewski/Data-Analysis-Practice/blob/main/outputs/1-avg_oecd.png)

```r
df_oecd %>%
  group_by(year) %>%
  summarise(mean_migration = mean(migration_share, na.rm = TRUE),
            mean_unemp = mean(unemployment_rate, na.rm = TRUE)) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = mean_migration * 10, color = "Migration (%) x10")) +
  geom_line(aes(y = mean_unemp, color = "Unemployment (%)")) +
  labs(title = "Average Migration and Unemployment in OECD (1991–2020)",
       y = "Rate (%)", x = "Year") +
  theme_minimal()
```

It would arguably be too soon for migration to have a measurable impact on the same year unemployment rates; nonetheless, a visual negative correlation across OECD countries can be observed. 

---

## Core analysis

### Creating lagged variables

To test delayed effects, I created two lagged variables: unemployment one and five years after migration inflows.

```r
library(ggcorrplot)

df_oecd <- df_oecd %>%
  group_by(`Country Code`) %>%
  arrange(year) %>%
  mutate(unemp_next_year = lead(unemployment_rate, 1),
         unemp_5y_later = lead(unemployment_rate, 5)) %>%
  ungroup()
```

### Correlation analysis

![Correlation Matrix: Migration and Unemployment, OECD](https://github.com/stan-maliszewski/Data-Analysis-Practice/blob/main/outputs/4-Heatmap.png)

A simple correlation matrix reveals a weak negative relationship between migration inflows and unemployment in the same and following year (-0.25), with the relationship nearly disappearing after five years (-0.09).

```r
corr_matrix <- df_oecd %>%
  select(migration_share, unemployment_rate, unemp_next_year, unemp_5y_later) %>%
  cor(use = "complete.obs")

ggcorrplot(corr_matrix, lab = TRUE, title = "Correlation Matrix: Migration and Unemployment (OECD, 1991–2020)")
```

### Linear regressions

Regression models test whether migration inflows predict future unemployment.

```r
model_1y <- lm(unemp_next_year ~ migration_share, data = df_oecd)
summary(model_1y)

ggplot(df_oecd, aes(x = migration_share, y = unemp_next_year)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Does higher migration predict next-year unemployment?",
       x = "Net migration (% of population, year t)",
       y = "Unemployment rate (year t+1)") +
  theme_minimal()

model_5y <- lm(unemp_5y_later ~ migration_share, data = df_oecd)
summary(model_5y)

ggplot(df_oecd, aes(x = migration_share, y = unemp_5y_later)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Does higher migration predict unemployment 5 years later?",
       x = "Net migration (% of population, year t)",
       y = "Unemployment rate (year t+5)") +
  theme_minimal()
```
![Migration vs Future Unemployment, 1Y](https://github.com/stan-maliszewski/Data-Analysis-Practice/blob/main/outputs/5-Linear-reg_1y.png)
!![Migration vs Future Unemployment, 5Y](https://github.com/stan-maliszewski/Data-Analysis-Practice/blob/main/outputs/6-Linear-reg_5y.png)


Results indicate a significant short-term negative relationship (R² = 0.059, p < 0.001) that weakens after five years (R² = 0.008, p ≈ 0.027).

---

## Country-level variation and fixed effects

### Country-specific relationships

The direction and strength of relationships differ substantially between countries.

```r
ggplot(df_oecd, aes(x = migration_share, y = unemp_next_year)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  facet_wrap(~ `Country Code`) +
  labs(title = "Migration vs. Unemployment by Country, t+1",
       x = "Net Migration (% of population)",
       y = "Unemployment rate (%)") +
  theme_minimal()

ggplot(df_oecd, aes(x = migration_share, y = unemp_5y_later)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  facet_wrap(~ `Country Code`) +
  labs(title = "Migration vs. Unemployment by Country, t+5",
       x = "Net Migration (% of population)",
       y = "Unemployment rate (%)") +
  theme_minimal()
```
![Migration Unemployment by Country](https://github.com/stan-maliszewski/Data-Analysis-Practice/blob/main/outputs/7-country1.png)
![Migration Unemployment by Country](https://github.com/stan-maliszewski/Data-Analysis-Practice/blob/main/outputs/8-country5.png)



### Fixed-effects models

To account for country-level heterogeneity, I used models with country-specific intercepts.

```r
model_fe_1y <- lm(unemp_next_year ~ migration_share + factor(`Country Code`), data = df_oecd)
model_fe_5y <- lm(unemp_5y_later ~ migration_share + factor(`Country Code`), data = df_oecd)

coefs_1y <- tidy(model_fe_1y) %>% filter(str_detect(term, "Country Code")) %>%
  mutate(country_code = str_extract(term, "[A-Z]+$"))

ggplot(coefs_1y, aes(x = reorder(country_code, estimate), y = estimate)) +
  geom_col(fill = "steelblue") + coord_flip() +
  labs(title = "Country Fixed Effects on Unemployment (1-year model)",
       x = "Country Code", y = "Estimated baseline effect") + theme_minimal()

coefs_5y <- tidy(model_fe_5y) %>% filter(str_detect(term, "Country Code")) %>%
  mutate(country_code = str_extract(term, "[A-Z]+$"))

ggplot(coefs_5y, aes(x = reorder(country_code, estimate), y = estimate)) +
  geom_col(fill = "steelblue") + coord_flip() +
  labs(title = "Country Fixed Effects on Unemployment (5-year model)",
       x = "Country Code", y = "Estimated baseline effect") + theme_minimal()
```
![Country Fixed Effects on Unemployment](https://github.com/stan-maliszewski/Data-Analysis-Practice/blob/main/outputs/9-Horizontal1.png)
![Country Fixed Effects on Unemployment](https://github.com/stan-maliszewski/Data-Analysis-Practice/blob/main/outputs/10-Horizontal-5.png)


Results show consistently higher baseline unemployment in Spain, Greece, and Slovakia, and lower in Korea, Mexico, and Japan, even after controlling for migration.

---

## Findings

Across 38 OECD countries (1991–2020), the results show a weak but statistically significant negative relationship between migration inflows and unemployment rates a year later, and an even weaker one five years later. A one-percentage-point increase in migration share is associated with a roughly 1.8 percentage-point decrease in unemployment the following year (β = -1.79, p < 0.001). The effect diminishes to -0.63 (p = 0.027) after five years.

Overall, migration inflows are associated with a short-term reduction in unemployment, although the magnitude is small and weakens over time. National labour market structures shape unemployment outcomes strongly, but even after accounting for these, migration retains a modest negative association with unemployment.

---

## Limitations

The analysis does not include key control variables such as GDP growth, inflation, or demographic change, which may influence unemployment. Adding these in future models would likely improve explanatory power. Furthermore, this study relies on aggregate data and simple linear methods; causal interpretations should therefore be made with caution.

This project was undertaken as a self-directed learning exercise to practice data analysis in R using open global datasets. It demonstrates a basic but effective workflow for quantitative social research using reproducible code.


