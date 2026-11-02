
# Community Waste Collection Analysis
# Uses synthetic sample data for learning and demonstration.

library(dplyr)

# 1. Load the dataset
waste <- read.csv("community_waste_collection.csv")

# 2. Prepare the data
waste$Date <- as.Date(waste$Date)

# 3. Inspect the dataset
head(waste)
str(waste)
summary(waste)

# 4. Create a summary of the full dataset
overall_summary <- waste %>%
  summarise(
    Total_Records = n(),
    Total_Waste_Collected_kg = sum(WasteCollected_kg, na.rm = TRUE),
    Total_Waste_Recycled_kg = sum(WasteRecycled_kg, na.rm = TRUE),
    Total_Residual_Waste_kg = sum(WasteResidual_kg, na.rm = TRUE),
    Average_Waste_Per_Record_kg = mean(WasteCollected_kg, na.rm = TRUE),
    Total_Collection_Trips = sum(CollectionTrips, na.rm = TRUE)
  )

print(overall_summary)

# 5. Summarize waste collection by zone
zone_summary <- waste %>%
  group_by(Zone) %>%
  summarise(
    Total_Collected_kg = sum(WasteCollected_kg, na.rm = TRUE),
    Total_Recycled_kg = sum(WasteRecycled_kg, na.rm = TRUE),
    Total_Residual_kg = sum(WasteResidual_kg, na.rm = TRUE),
    Collection_Trips = sum(CollectionTrips, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Collected_kg))

print(zone_summary)

# 6. Summarize waste by area type
area_type_summary <- waste %>%
  group_by(AreaType) %>%
  summarise(
    Total_Collected_kg = sum(WasteCollected_kg, na.rm = TRUE),
    Total_Recycled_kg = sum(WasteRecycled_kg, na.rm = TRUE),
    Total_Residual_kg = sum(WasteResidual_kg, na.rm = TRUE),
    .groups = "drop"
  )

print(area_type_summary)

# 7. Calculate recycling rate for each record
waste <- waste %>%
  mutate(
    Recycling_Rate_Percent =
      ifelse(
        WasteCollected_kg > 0,
        (WasteRecycled_kg / WasteCollected_kg) * 100,
        NA
      )
  )

# 8. Summarize daily collection
daily_summary <- waste %>%
  group_by(Date) %>%
  summarise(
    Daily_Collected_kg = sum(WasteCollected_kg, na.rm = TRUE),
    Daily_Recycled_kg = sum(WasteRecycled_kg, na.rm = TRUE),
    Daily_Residual_kg = sum(WasteResidual_kg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Date)

print(daily_summary)

# 9. Save summary tables to the project folder
write.csv(overall_summary, "overall_summary.csv", row.names = FALSE)
write.csv(zone_summary, "zone_summary.csv", row.names = FALSE)
write.csv(area_type_summary, "area_type_summary.csv", row.names = FALSE)
write.csv(daily_summary, "daily_summary.csv", row.names = FALSE)

