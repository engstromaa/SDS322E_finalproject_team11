
mixbev <- read.csv("mixbev_txsub.csv")

library(ggplot2)
library(dplyr)
library(tidyr)  # Make sure tidyr is loaded
library(lubridate)

### total receipts by alc type
mixbev %>%
  summarize(
    total_liquor = sum(Liquor.Receipts),
    total_wine = sum(Wine.Receipts),
    total_beer = sum(Beer.Receipts)
  ) %>%
  pivot_longer(
    cols = starts_with("total_"),
    names_to = "Alcohol_Type",
    values_to = "Total_Receipts"
  ) %>%
  ggplot(aes(x = Alcohol_Type, y = Total_Receipts, fill = Alcohol_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Receipts by Alcohol Type", x = "Alcohol Type", y = "Total Receipts")


### total reciepts over time
mixbev %>%
  mutate(Date = mdy(Obligation.End.Date)) %>%  # Use mdy to parse the date
  group_by(Date) %>%
  summarize(Total_Receipts = sum(Total.Receipts)) %>%
  ggplot(aes(x = Date, y = Total_Receipts)) +
  geom_line() +
  labs(title = "Total Receipts Over Time", x = "Date", y = "Total Receipts") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year")

### total receipts by month and significance
install.packages("stats")
library(stats)

result <- mixbev %>%
  mutate(Month = factor(month(mdy(Obligation.End.Date)), levels = 1:12)) %>%  # Convert Month to factor
  group_by(Month) %>%
  summarize(Total_Receipts = sum(Total.Receipts))

# one-way ANOVA
anova_result <- aov(Total_Receipts ~ Month, data = result)
print(anova_result)

# post-hoc test (Tukey HSD)
posthoc <- TukeyHSD(anova_result)
print(posthoc)

# boxplot
ggplot(mixbev, aes(x = factor(month(mdy(Obligation.End.Date)), labels = month.name), y = Total.Receipts)) +
  geom_boxplot() +
  labs(title = "Distribution of Total Receipts by Month", x = "Month", y = "Total Receipts")

### total receipts by city

# receipts by city (all)
total_receipts_by_city <- mixbev %>%
  group_by(Taxpayer.City) %>%
  summarize(Total_Receipts = sum(Total.Receipts))

# bar plot w/ all cities 
ggplot(total_receipts_by_city, aes(x = reorder(Taxpayer.City, Total_Receipts), y = Total_Receipts)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Receipts by City", x = "City", y = "Total Receipts") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# top cities w/ highest total receipt sum
top_cities <- mixbev %>%
  group_by(Taxpayer.City) %>%
  summarize(Total_Receipts = sum(Total.Receipts)) %>%
  top_n(10, wt = Total_Receipts)  # Choose the top 10 cities

# bar plot w/ top 10 cities
ggplot(top_cities, aes(x = reorder(Taxpayer.City, Total_Receipts), y = Total_Receipts)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Total_Receipts), vjust = -0.5, size = 3.5, color = "black") +  # Add numerical labels on top
  labs(title = "Total Receipts by Top Cities", x = "City", y = "Total Receipts") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  # Adjust size as needed

### inside/outside city limits
mixbev %>%
  ggplot(aes(x = Inside.Outside.City.Limits, y = Total.Receipts, fill = Inside.Outside.City.Limits)) +
  geom_boxplot() +
  labs(title = "Total Receipts by City Limits", x = "City Limits", y = "Total Receipts") +
  scale_fill_manual(values = c("skyblue", "lightcoral"), name = "Location") +
  theme_minimal()

### Texas triangle








