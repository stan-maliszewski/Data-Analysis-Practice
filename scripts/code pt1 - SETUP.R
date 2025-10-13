library(tidyverse)
library(broom)
library(stringr)


# Download and import data 

# Population data: 1991-2020
pop <- read.csv("SP.POP.TOTL.csv")

pop_long <- pop %>%
  select(`Country.Name`, `Country.Code`, matches("^X\\d{4}")) %>%
  rename_with(~ str_extract(., "(\\d{4})"), matches("^X\\d{4}")) %>%
  pivot_longer(
    cols = -c(`Country.Name`, `Country.Code`),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(year = as.integer(year)) %>%
  rename("Country Name" = "Country.Name", "Country Code" = "Country.Code")

# Migration data: 1991-2020
migr <- read_csv("SM.POP.NETM.csv")
# Clean
migr_long <- migr %>%
  select(`Country Name`, `Country Code`, matches("^\\d{4}")) %>%
  rename_with(~ str_extract(., "^\\d{4}"), matches("^\\d{4}")) %>%
  pivot_longer(
    cols = -c(`Country Name`, `Country Code`),
    names_to = "year",
    values_to = "net_migration"
  ) %>%
  mutate(year = as.integer(year))

# Unemployment data: 1991-2020
unemp <- read_csv("SL.UEM.TOTL.ZS.csv")
# Clean
unemp_long <- unemp %>%
  select(`Country Name`, `Country Code`, matches("^\\d{4}")) %>%
  rename_with(~ str_extract(., "^\\d{4}"), matches("^\\d{4}")) %>%
  pivot_longer(
    cols = -c(`Country Name`, `Country Code`),
    names_to = "year",
    values_to = "unemployment_rate"
  ) %>%
  mutate(year = as.integer(year))

---------------------------------------------------
  
# Merge into one data frame
  df_wdi <- unemp_long %>%
  left_join(migr_long, by = c("Country Name", "Country Code", "year")) %>%
  left_join(pop_long, by = c("Country Name", "Country Code", "year"))

# Filter for OECD countries
oecd_iso <- c("AUS","AUT","BEL","CAN","CHL","COL", "CRI", "CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA",
              "JPN","KOR","LVA","LTU","LUX","MEX","NLD","NZL","NOR","POL","PRT","SVK","SVN","ESP","SWE","CHE","TUR","GBR","USA")

df_oecd <- df_wdi %>%
  filter(`Country Code` %in% oecd_iso)

unique(df_oecd$`Country Name`)

----------------------------------------
# Fix data formats
glimpse(df_oecd)

df_oecd <- df_oecd %>%
  mutate(
    unemployment_rate = as.numeric(unemployment_rate),
    net_migration = as.numeric(net_migration))


# Add net migration share as % of population
df_oecd <- df_oecd %>% mutate(migration_share = (net_migration / population) * 100)
head(df_oecd)
names(df_oecd)


# Check for missing data and deal accordingly
sum(is.na(df_oecd)) #0

# Save the data frame
write_csv(df_oecd, "df_oecd.csv")

