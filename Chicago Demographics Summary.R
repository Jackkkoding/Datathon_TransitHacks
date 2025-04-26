# Install and load necessary packages
if (!require("tidycensus")) install.packages("tidycensus")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")

library(tidycensus)
library(dplyr)
library(ggplot2)

# Set your API key (once)
census_api_key("c4a87f4a27b65fba78c70037caf6ec9aa31a5f55", install = TRUE, overwrite=TRUE)
readRenviron("~/.Renviron")

# Step 1: Define Variables to Pull
variables <- c(
  total_population = "B01003_001",
  median_income = "B19013_001",
  avg_household_size = "B25010_001",
  white_alone = "B02001_002",
  black_alone = "B02001_003",
  asian_alone = "B02001_005",
  hispanic = "B03003_003",
  male_total = "B01001_002",
  female_total = "B01001_026",
  
  # Age groups Males
  male_under5 = "B01001_003",
  male_5_9 = "B01001_004",
  male_10_14 = "B01001_005",
  male_15_17 = "B01001_006",
  male_18_19 = "B01001_007",
  male_20_24 = "B01001_008",
  male_25_29 = "B01001_009",
  male_30_34 = "B01001_010",
  male_35_39 = "B01001_011",
  male_40_44 = "B01001_012",
  male_45_49 = "B01001_013",
  male_50_54 = "B01001_014",
  male_55_59 = "B01001_015",
  male_60_61 = "B01001_016",
  male_62_64 = "B01001_017",
  male_65_66 = "B01001_018",
  male_67_69 = "B01001_019",
  male_70_74 = "B01001_020",
  male_75_79 = "B01001_021",
  male_80_84 = "B01001_022",
  male_85_up = "B01001_023",
  
  # Age groups Females
  female_under5 = "B01001_027",
  female_5_9 = "B01001_028",
  female_10_14 = "B01001_029",
  female_15_17 = "B01001_030",
  female_18_19 = "B01001_031",
  female_20_24 = "B01001_032",
  female_25_29 = "B01001_033",
  female_30_34 = "B01001_034",
  female_35_39 = "B01001_035",
  female_40_44 = "B01001_036",
  female_45_49 = "B01001_037",
  female_50_54 = "B01001_038",
  female_55_59 = "B01001_039",
  female_60_61 = "B01001_040",
  female_62_64 = "B01001_041",
  female_65_66 = "B01001_042",
  female_67_69 = "B01001_043",
  female_70_74 = "B01001_044",
  female_75_79 = "B01001_045",
  female_80_84 = "B01001_046",
  female_85_up = "B01001_047"
)

# Step 2: Pull Data
zcta_data <- get_acs(
  geography = "zcta",
  variables = variables,
  year = 2022,
  survey = "acs5",
  output = "wide"
)

# Step 3: Filter for Chicago ZIP Codes
chicago_zips <- zcta_data %>%
  filter(substr(GEOID, 1, 3) == "606" | GEOID %in% c("60707", "60827"))

# Step 4: Create Final Clean Dataset
chicago_clean <- chicago_zips %>%
  mutate(
    # Calculate race percentages
    pct_white = 100 * white_aloneE / total_populationE,
    pct_black = 100 * black_aloneE / total_populationE,
    pct_asian = 100 * asian_aloneE / total_populationE,
    pct_hispanic = 100 * hispanicE / total_populationE,
    
    # Find dominant race
    dominant_race = case_when(
      white_aloneE >= black_aloneE & white_aloneE >= asian_aloneE & white_aloneE >= hispanicE ~ "White",
      black_aloneE >= white_aloneE & black_aloneE >= asian_aloneE & black_aloneE >= hispanicE ~ "Black",
      asian_aloneE >= white_aloneE & asian_aloneE >= black_aloneE & asian_aloneE >= hispanicE ~ "Asian",
      hispanicE >= white_aloneE & hispanicE >= black_aloneE & hispanicE >= asian_aloneE ~ "Hispanic"
    ),
    
    # Gender ratio
    gender_ratio = male_totalE / female_totalE,
    
    # Aggregate ages (male + female)
    children = male_under5E + male_5_9E + male_10_14E + male_15_17E + male_18_19E +
      female_under5E + female_5_9E + female_10_14E + female_15_17E + female_18_19E,
    
    young_adults = male_20_24E + male_25_29E + male_30_34E + male_35_39E +
      female_20_24E + female_25_29E + female_30_34E + female_35_39E,
    
    middle_aged_adults = male_40_44E + male_45_49E + male_50_54E + male_55_59E +
      female_40_44E + female_45_49E + female_50_54E + female_55_59E,
    
    seniors = male_60_61E + male_62_64E + male_65_66E + male_67_69E + male_70_74E +
      male_75_79E + male_80_84E + male_85_upE +
      female_60_61E + female_62_64E + female_65_66E + female_67_69E + female_70_74E +
      female_75_79E + female_80_84E + female_85_upE,
    
    # Find dominant age group
    dominant_age_category = case_when(
      children >= young_adults & children >= middle_aged_adults & children >= seniors ~ "Children (0-19)",
      young_adults >= children & young_adults >= middle_aged_adults & young_adults >= seniors ~ "Young Adults (20-39)",
      middle_aged_adults >= children & middle_aged_adults >= young_adults & middle_aged_adults >= seniors ~ "Middle-aged Adults (40-59)",
      seniors >= children & seniors >= young_adults & seniors >= middle_aged_adults ~ "Seniors (60+)"
    )
  ) %>%
  # Select only requested columns
  select(
    zip_code = GEOID,
    total_population = total_populationE,
    median_income = median_incomeE,
    avg_household_size = avg_household_sizeE,
    dominant_race,
    gender_ratio,
    dominant_age_category
  )

# Step 5: View Final Table
print(chicago_clean)

write.csv(chicago_clean, "~/Desktop/chicago_demographics_summary.csv", row.names = FALSE)


