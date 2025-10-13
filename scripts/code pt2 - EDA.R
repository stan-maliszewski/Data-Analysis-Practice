# Exploratory analysis

df_oecd %>%
  summarise(
    avg_migration = mean(migration_share, na.rm = TRUE),
    sd_migration  = sd(migration_share, na.rm = TRUE),
    avg_unemp     = mean(unemployment_rate, na.rm = TRUE),
    sd_unemp      = sd(unemployment_rate, na.rm = TRUE)
  )

-------------------------------------------------------------
# Compare average net migration vs unemployment rates
df_oecd %>%
  group_by(year) %>%
  summarise(
    mean_migration = mean(migration_share, na.rm = TRUE),
    mean_unemp = mean(unemployment_rate, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = mean_migration * 10, color = "Migration (%) x10")) +  # scaled to show both
  geom_line(aes(y = mean_unemp, color = "Unemployment (%)")) +
  labs(title = "Average Migration and Unemployment in OECD (1991–2020)",
       y = "Rate (%)", x = "Year") +
  theme_minimal()

# Dummy data frame for narrowing down to a specific country
df_country <- df_oecd %>% 
  filter(`Country Name` == "Poland") # plug-in country of interest

# Average net migration vs unemployment rates in [Italy]
df_country %>%
group_by(year) %>%
  summarise(
    mean_migration = mean(migration_share, na.rm = TRUE),
    mean_unemp = mean(unemployment_rate, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = mean_migration * 10, color = "Migration (%) x10")) +  # scaled to show both
  geom_line(aes(y = mean_unemp, color = "Unemployment (%)")) +
  labs(title = "Average Migration and Unemployment in Poland (1991–2020)",
       y = "Rate (%)", x = "Year") +
  theme_minimal()

