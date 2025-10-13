library(ggcorrplot)

# Adding lagged variables to the data frame

df_oecd <- df_oecd %>%
  group_by(`Country Code`) %>%
  arrange(year) %>%
  mutate(
    unemp_next_year = lead(unemployment_rate, 1),
    unemp_5y_later = lead(unemployment_rate, 5)
  ) %>%
  ungroup()

-----------------------------------------------------------

# Simple correlation
corr_matrix <- df_oecd %>%
  select(migration_share, unemployment_rate, unemp_next_year, unemp_5y_later) %>%
  cor(use = "complete.obs")

# Visualize
ggcorrplot(corr_matrix, lab = TRUE, title = "Correlation Matrix: Migration and Unemployment (OECD, 1991–2020)")


---------------------------------------------------------

# Linear models
# Next year unemployment rates
model_1y <- lm(unemp_next_year ~ migration_share, data = df_oecd)
summary(model_1y)

# Visualize
ggplot(df_clean, aes(x = migration_share, y = unemp_next_year)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Does higher migration predict next-year unemployment?",
       x = "Net migration (% of population, year t)",
       y = "Unemployment rate (year t+1)") +
  theme_minimal()

# Linear models
# 5 years later unemployment rates
model_5y <- lm(unemp_5y_later ~ migration_share, data = df_oecd)
summary(model_5y)

# Visualize
ggplot(df_clean, aes(x = migration_share, y = unemp_5y_later)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Does higher migration predict unemployment 5 years later?",
       x = "Net migration (% of population, year t)",
       y = "Unemployment rate (year t+5)") +
  theme_minimal()

--------------------------------------------------------

# Country-fixed effects
# After 1 year
model_fe_1y <- lm(unemp_next_year ~ migration_share + factor(`Country Code`), data = df_oecd)
summary(model_fe)

# After 5 years
model_fe_5y <- lm(unemp_5y_later ~ migration_share + factor(`Country Code`), data = df_clean)
summary(model_fe)

---------------------------------------------------------

# Visualize 1 year
ggplot(df_oecd, aes(x = migration_share, y = unemp_next_year)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  facet_wrap(~ `Country Code`) +
  labs(
    title = "Migration vs. Unemployment by Country, t+1",
    x = "Net Migration (% of population)",
    y = "Unemployment rate (%)"
  ) +
  theme_minimal()


# Visualize 5 year
ggplot(df_oecd, aes(x = migration_share, y = unemp_5y_later)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  facet_wrap(~ `Country Code`) +
  labs(
    title = "Migration vs. Unemployment by Country, t+5",
    x = "Net Migration (% of population)",
    y = "Unemployment rate (%)"
  ) +
  theme_minimal()

-------------------------------------------------------------
# Country fixed effects coefficients
  
  summary(model_fe_1y)
  
# 1 year model
coefs <- tidy(model_fe_1y) %>%
  filter(str_detect(term, "Country Code")) %>%
  mutate(
    country_code = str_extract(term, "[A-Z]+$")
  )


ggplot(coefs, aes(x = reorder(country_code, estimate), y = estimate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Country Fixed Effects on Unemployment (1-year model)",
    x = "Country Code",
    y = "Estimated baseline effect (vs. omitted country)"
  ) +
  theme_minimal()



# 5 year model
coefs <- tidy(model_fe_5y) %>%
filter(str_detect(term, "Country Code")) %>%
  mutate(
    country_code = str_extract(term, "[A-Z]+$")
  )


ggplot(coefs, aes(x = reorder(country_code, estimate), y = estimate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Country Fixed Effects on Unemployment (5-year model)",
    x = "Country Code",
    y = "Estimated baseline effect (vs. omitted country)"
  ) +
  theme_minimal()
