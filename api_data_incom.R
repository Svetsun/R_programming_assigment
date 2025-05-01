library(pxweb)
library(dplyr)
library(ggplot2)
library(scales)

# SCB API URL for income
url_income <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/HE/HE0110/HE0110A/SamForvInk1"

# Define the query: Riket, age 20+, years 2002-2023
query_income <- list(
  "Region" = c("00"),             # 00 = Riket (whole country)
  "Alder" = c("tot20+"),             # Age 20+
  "ContentsCode" = c("HE0110J8"), # Income (assuming this is average income)
  "Tid" = as.character(2002:2023) # Years 2002 to 2023
)

# Fetch data
px_income <- pxweb_get(url = url_income, query = query_income)

# Convert to data frame
df_income <- as.data.frame(px_income, column.name.type = "text", variable.value.type = "text")
# View first 10 rows
head(df_income, 10)
colnames(df_income)
# Clean columns
df_income_clean <- df_income %>%
  rename(
    Region = region,
    AgeGroup = ålder,
    Year = år,
    Income = `Medianinkomst, tkr`
  ) %>%
  mutate(
    Year = as.integer(Year),
    Income = as.numeric(Income)
  )

# Create Y-axis breaks every 10,000
y_breaks_income <- seq(0, max(df_income_clean$Income, na.rm = TRUE), by = 10000)

# Plot
ggplot(df_income_clean, aes(x = Year, y = Income)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "forestgreen", size = 3) +
  geom_text(aes(label = format(Income, big.mark = " ")), vjust = -0.7, size = 3.5) +
  labs(
    title = "Genomsnittlig inkomst i Sverige (20+ år, Riket, 2002–2023)",
    x = "År",
    y = "Inkomst ( Tusen kr)"
  ) +
  scale_x_continuous(breaks = df_income_clean$Year) +
  scale_y_continuous(
    breaks = y_breaks_income,
    labels = label_number(big.mark = " ", decimal.mark = ",")
  ) +
  theme_minimal()
