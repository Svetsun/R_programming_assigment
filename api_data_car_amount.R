library(pxweb)
library(dplyr)
library(ggplot2)
library(scales)

# SCB API URL
url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/FordonTrafik"

# Correct query with valid variable names and values
query <- list(
  "Region" = c("00"),             # Riket
  "Fordonsslag" = c("10"),        # Personbilar
  "ContentsCode" = c("TK1001AC"), # Antal fordon i trafik
  "Tid" = c("2023", "2022", "2021", "2020", "2019",
            "2018", "2017", "2016", "2015", "2014", "2013",
            "2012", "2011", "2010", "2009","2008",
            "2007", "2006", "2005", "2004", "2003", "2002")
)

# Fetch data from SCB API
px_data <- pxweb_get(url = url, query = query)

# Convert to data frame
df_riket <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Clean columns
df_riket <- df_riket %>%
  rename(
    Region = region,
    VehicleType = fordonsslag,
    Year = år,
    Vehicles = Antal
  ) %>%
  mutate(
    Year = as.integer(Year),
    Vehicles = as.numeric(Vehicles)
  )

# Create Y-axis breaks every 100,000
y_breaks <- seq(0, max(df_riket$Vehicles, na.rm = TRUE), by = 100000)

# Plot
ggplot(df_riket, aes(x = Year, y = Vehicles)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkblue", size = 3) +
  geom_text(aes(label = format(Vehicles, big.mark = " ")), vjust = -0.7, size = 3.5) +
  labs(
    title = "Totalt antal personbilar i trafik i Sverige (Riket, 2002–2023)",
    x = "År",
    y = "Antal personbilar"
  ) +
  scale_x_continuous(breaks = df_riket$Year) +
  scale_y_continuous(
    breaks = y_breaks,
    labels = label_number(big.mark = " ", decimal.mark = ",")
  ) +
  theme_minimal()