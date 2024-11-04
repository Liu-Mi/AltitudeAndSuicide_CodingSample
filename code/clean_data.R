# This file cleans the downloaded data and consolidates it into a single dataset.
# The dataset is then saved as a CSV file.

### Setup
# Specify number of years considered for our analysis
NO_YEARS <- 21

### Construct dependent variables
# Load in depnedent variables files
suicide_firearms <- na.omit(read.delim("data/cdc/suicide_firearms.txt")[, c(2, 3, 5, 4)])
suicide_overall <- na.omit(read.delim("data/cdc/suicide_overall.txt")[, c(3, 4)])
suicide_other <- na.omit(read.delim("data/cdc/suicide_other.txt")[, c(3, 4)])

homicide_firearms <- na.omit(read.delim("data/cdc/homicide_firearms.txt")[, c(3, 4)])
homicide_overall <- na.omit(read.delim("data/cdc/homicide_overall.txt")[, c(3, 4)])
homicide_other <- na.omit(read.delim("data/cdc/homicide_other.txt")[, c(3, 4)])

overdoses <- na.omit(read.delim("data/cdc/overdoses_overall.txt")[, c(3, 4)])
liver_disease <- na.omit(read.delim("data/cdc/liver_disease_overall.txt")[, c(3, 4)])

# Join dependent variables into one dataframe
cdc_variables <- suicide_firearms %>%
    full_join(suicide_overall, by = "County.Code") %>%
    full_join(suicide_other, by = "County.Code") %>%
    full_join(homicide_overall, by = "County.Code") %>%
    full_join(homicide_firearms, by = "County.Code") %>%
    full_join(homicide_other, by = "County.Code") %>%
    full_join(overdoses, by = "County.Code") %>%
    full_join(liver_disease, by = "County.Code")

names(cdc_variables) <- c(
    "county_name", "county_code", "population", "suicide_firearms", "suicide_overall", "suicide_other",
    "homicide_overall", "homicide_firearms", "homicide_other", "overdoses", "liver_disease"
)
cdc_variables <- cdc_variables %>% mutate(population = population / NO_YEARS)


### Calculate population density of counties
# extract and join state codes
state_code <- str_pad(as.character(floor(cdc_variables$county_code / 1000)), 2, pad = "0")
cdc_variables <- cbind(state_code, cdc_variables)

# import county geo data and convert to square kilometers
geo_data <- st_read("data/census/county_shapefile")
geo_data <- geo_data %>%
    transmute(county_code = GEO_ID, area = CENSUSAREA * 2.59, y_coordinate = st_coordinates(st_centroid(geo_data))[, "Y"]) %>%
    st_set_geometry(NULL)
geo_data$county_code <- as.numeric(str_sub(geo_data$county_code, -5))

# join to dependent variables and calculate density
cdc_variables <- full_join(cdc_variables, geo_data, by = "county_code")
cdc_variables$log_population_density <- log(cdc_variables$population / cdc_variables$area)

cdc_variables <- cdc_variables %>% select(-"area")


### Clean census variables
# Loading in Census variables
S0101 <- read.csv("data/census/age_and_sex/S0101.csv")
DP03 <- read.csv("data/census/economic_characteristics/DP03.csv")
S1501 <- read.csv("data/census/educational_attainment/S1501.csv")
P1 <- read.csv("data/census/race/P1.csv")
S2101 <- read.csv("data/census/veteran_status/S2101.csv")

# Fix column names
colnames(S0101) <- S0101[1, ]
colnames(DP03) <- DP03[1, ]
colnames(S1501) <- S1501[1, ]
colnames(P1) <- P1[1, ]
colnames(S2101) <- S2101[1, ]

S0101 <- S0101[-1, ]
DP03 <- DP03[-1, ]
S1501 <- S1501[-1, ]
P1 <- P1[-1, ]
S2101 <- S2101[-1, ]

# Choose relevant columns from census files
# P1 - race
percent_white <- as.numeric(P1[, 5]) / as.numeric(P1[, 4]) * 100
percent_native <- as.numeric(P1[, 7]) / as.numeric(P1[, 4]) * 100

P1_selected <- data.frame(county_code = P1[, 1], percent_white, percent_native)

# S1501 - educational attainment
S1501_selected <- S1501[, c(1, 81, 87)]
S1501_selected[, 2] <- as.numeric(S1501_selected[, 2])
S1501_selected[, 3] <- as.numeric(S1501_selected[, 3])
colnames(S1501_selected) <- c("county_code", "percent_highschool_or_higher", "percent_bachelor_or_higher")

S1501_selected <- S1501_selected[1:3221, ]

# S2101 - veteran status
percent_veteran <- as.numeric(S2101[, 5]) / as.numeric(S2101[, 3]) * 100

S2101_selected <- data.frame(county_code = S2101[, 1], percent_veteran)

# S0101 - age and sex
S0101_selected <- S0101[, c(1, 177, 183, 195)]

S0101_selected[, 2] <- as.numeric(S0101_selected[, 2])
S0101_selected[, 3] <- as.numeric(S0101_selected[, 3])
S0101_selected[, 4] <- as.numeric(S0101_selected[, 4]) # Single missing value, NA warning is fine

colnames(S0101_selected) <- c("county_code", "median_age", "sex_ratio", "old_age_dependency")

# construct 50 and above age bracket
S0101_50 <- S0101 %>%
    select(starts_with("total")) %>%
    select(contains("estimate"))
S0101_50 <- S0101_50[, 12:19]

S0101_50 <- S0101_50 %>% transmute(across(everything(), as.numeric))

fifty_and_above <- rowSums(S0101_50)

S0101_selected <- cbind(S0101_selected, fifty_and_above)

# DP03 - economic characteristics
percent_unemployment <- as.numeric(DP03[, 19]) / as.numeric(DP03[, 11]) * 100
mean(as.numeric(DP03[, 21]) / as.numeric(DP03[, 13]) * 100)

DP03_selected <- DP03[, c(1, 9, 247, 514, 541)]
DP03_selected[, 2] <- as.numeric(DP03_selected[, 2])
DP03_selected[, 3] <- log(as.numeric(DP03_selected[, 3]))
DP03_selected[, 4] <- as.numeric(DP03_selected[, 4])
DP03_selected[, 5] <- as.numeric(DP03_selected[, 5]) # Single missing value, NA warning is fine (same value as for S0101)

colnames(DP03_selected) <- c("county_code", "labor_force_participation", "log_median_household_income", "poverty_level_overall", "poverty_level_65_over")

DP03_selected <- data.frame(DP03_selected, percent_unemployment)

# Join census variables into single dataframe
census_variables <- DP03_selected %>%
    full_join(P1_selected, by = "county_code") %>%
    full_join(S0101_selected, by = "county_code") %>%
    full_join(S1501_selected, by = "county_code") %>%
    full_join(S2101_selected, by = "county_code")

# Match dependent and census variables
census_variables[, 1] <- as.numeric(str_sub(census_variables[, 1], -5, -1))
suicide_data <- cdc_variables %>% full_join(census_variables, by = "county_code")


### Elevation data
# Import weighted elevation data
elevation_data <- read.csv("data/elevation/weighted_variables.csv")[, c(1, 3)]
elevation_state_code <- as.numeric(str_sub(elevation_data[, 1], 1, 2))
elevation_county_code <- as.numeric(str_sub(elevation_data[, 1], -3, -1))
elevation_county_code <- str_pad(elevation_county_code, 3, pad = "0")

elevation_data[, 1] <- as.numeric(paste(elevation_state_code, elevation_county_code, sep = ""))
colnames(elevation_data) <- c("county_code", "altitude")

# merge elevation to data set
suicide_data <- suicide_data %>% full_join(elevation_data, by = "county_code")


### Firearms licenses
# import federeal firearms licencse data & select relevant data
ffls <- read.csv("data/firearms/ctygundealercounts.csv")

ffls_2010 <- ffls %>% filter(year == 2010)
ffls_2010 <- ffls_2010 %>% select("county", "dealerlic")
colnames(ffls_2010) <- c("county_code", "gun_dealer_licenses")

# join ffl data
suicide_data <- suicide_data %>% full_join(ffls_2010, by = "county_code")


### Calculate variables expressed in rates
# Turn all columns to numbers
suicide_data <- suicide_data %>%
    mutate(across(-c(county_name, state_code, county_code), as.numeric)) %>%
    select(-county_name) %>%
    mutate(across(
        c(starts_with("suicide"), starts_with("homicide"), overdoses, liver_disease),
        ~ (.x / NO_YEARS) * 1e5 / population,
        .names = "{col}_rate"
    )) %>%
    select(-c(starts_with("suicide"), starts_with("homicide"), overdoses, liver_disease), ends_with("rate"))

# calculate gun dealer licenses rate
suicide_data <- suicide_data %>%
    mutate(gun_dealer_licenses_rate = gun_dealer_licenses * 1e5 / population) %>%
    select(-gun_dealer_licenses)

# Add firearms data
firearms_data_2021 <- read_xpt("data/cdc/LLCP2021XPT.ZIP", col_select = c("_STATE", "FIREARM5"))
firearms_data_2022 <- read_xpt("data/cdc/LLCP2022XPT.ZIP", col_select = c("_STATE", "FIREARM5"))
firearms_data_2023 <- read_xpt("data/cdc/LLCP2023XPT.ZIP", col_select = c("_STATE", "FIREARM5"))

# Merge firearms data and keep track of the number of datasets a state has data
firearms_data <- bind_rows(
    firearms_data_2021 %>% mutate(year = 2021),
    firearms_data_2022 %>% mutate(year = 2022),
    firearms_data_2023 %>% mutate(year = 2023)
)

# Merge the summary data with the main dataset
suicide_data <- firearms_data %>%
    transmute(state_code_firearms = as.character(`_STATE`), firearms = FIREARM5) %>%
    filter(!is.na(firearms)) %>%
    group_by(state_code_firearms) %>%
    summarise(firearms_rate = sum(firearms == 1, na.rm = TRUE) / sum(firearms %in% c(1, 2), na.rm = TRUE)) %>%
    right_join(suicide_data, by = c("state_code_firearms" = "state_code")) %>%
    select(-state_code_firearms)

# Calculate deaths of despair variables
suicide_data <- suicide_data %>%
    mutate(dod_excl_suicide_rate = overdoses_rate + liver_disease_rate, dod_rate = dod_excl_suicide_rate + suicide_overall_rate)


### Add state and county names for easier identification
# Add state names
suicide_data <- suicide_data %>%
    mutate(
        county_code_temp = county_code,
        county_code = str_sub(str_pad(as.character(county_code_temp), width = 5, pad = "0", side = "left"), start = 3, end = 5),
        state_code = str_sub(str_pad(as.character(county_code_temp), width = 5, pad = "0", side = "left"), start = 1, end = 2)
    ) %>%
    left_join(fips_codes, by = c("county_code", "state_code")) %>%
    mutate(state = state_name, county_code = county_code_temp) %>%
    select(-c(county_code_temp, state_name, state_code)) %>%
    unique()

### Write to consolidated CSV file
write.csv(suicide_data, "data/final_data.csv", row.names = FALSE)
