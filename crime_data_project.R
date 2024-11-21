library(tidyverse)

crime_df <- crime_data_LA %>%
  select(`DATE OCC`, `TIME OCC`, `Crm Cd Desc`, Status) %>%
  rename(date_crime = `DATE OCC`, time_crime = `TIME OCC`,
         type_crime = `Crm Cd Desc`, status = Status) %>%
  mutate(time_crime = as.numeric(time_crime),
         period_day = case_when(time_crime < 500 | time_crime >= 1700 ~ "evening",
                                time_crime >= 500 & time_crime <= 1100 ~ "morning",
                                time_crime > 1100 & time_crime < 1700 ~ "afternoon"),
         month = substr(date_crime, 1, 2),
         season = case_when(month %in% c("01", "02", "12") ~ "winter",
                            month %in% c("03", "04", "05") ~ "spring",
                            month %in% c("06", "07", "08") ~ "summer",
                            month %in% c("09", "10", "11") ~ "fall")) %>%
  select(month, season, period_day, type_crime, status)

## ANALYSIS ZERO (?)

period_count <- crime_df %>%
  group_by(period_day) %>%
  count()

### Result: There are more crimes in the evening than in the morning and afternoon combined.

## ANALYSIS ONE

crime_season <- crime_df %>%
  group_by(season, type_crime) %>%
  count() %>%
  ungroup() %>%
  group_by(season) %>%
  mutate(rank = dense_rank(1 / n))

crime_season_results <- crime_season %>%
  filter(rank <= 3)

### Result: The type of crime is generally constant by season.
###         Best explanation is that LA's weather is generally mild.

## ANALYSIS TWO

crime_period <- crime_df %>%
  group_by(period_day, type_crime) %>%
  count() %>%
  ungroup() %>%
  group_by(period_day) %>%
  mutate(rank = dense_rank(1 / n))

crime_period_results <- crime_period %>%
  filter(rank <= 3)

### Result: The most common crime in the afternoon is battery assault, while 
###         the most common crimes in the morning and evening are vehicle theft.

## ANALYSIS THREE

crime_status <- crime_df %>%
  group_by(type_crime) %>%
  filter(n() > 5000) %>%
  summarize(prop_resolved = sum(status != "IC") / sum(!is.na(status))) %>%
  arrange(-prop_resolved)

### Result: Crime investigations involving intimate partners and violations of restraining orders or court orders
###         are the most likely to be resolved. Crimes involving theft and burglary are the least likely 
###         to be resolved. 

