#----- Libraries imports -------

library(tidyverse)
library(zoo) 
library(readxl)
library(mgsub)
library(hydroGOF)
library(ggthemes)


#----- LCS data imports -------

# Run it to quickly have access to some important data of the first part:
# - Cleansed Zephyr data (zep_data)
# - Cleansed AQM data (aqm_data)
# - Cleansed Ref data (ref_data_no2)

## Importing the lcs 
lcs_raw = read.csv("quant_lcs_hourly.csv")   
inst_raw = read.csv("quant_instruments.csv")

## Data cleaning and preparation
# Merging the instrument data frame with the lcs data frame
lcs_raw = inner_join(lcs_raw, inst_raw, by = c("instrument"))
# function capitalising the first letter of each column
lcs_data_no2 = lcs_raw |>
  filter(measurand == "NO2", is.na(flag)) |> 
  mutate(date_time = as_datetime(time),
         month = as.yearmon(date_time),
         date = as_date(date_time),
         day = day(date_time)) |>
  select(-flag, -flagreason, -time, -measurand, -study, -sensornumber) |>
  tibble()

## Zephyr data

zep_data = lcs_data_no2 |>
  filter(company == "Zephyr") |>
  rename(ZEP = measurement) 

# London cleaning
zep_data_london =  zep_data |> filter(location == "London",
                                      !between(date_time, as_datetime("2020-03-11 00:00:00"),
                                               as_datetime("2020-03-27 23:00:00"))) 
# York cleaning
zep_data_york =  zep_data |> filter(location == "York",
                                    !between(date_time, as_datetime("2020-07-13 00:00:00"),
                                             as_datetime("2020-07-23 23:00:00")),
                                    !between(date_time, as_datetime("2020-09-11 00:00:00"), 
                                             as_datetime("2020-09-29 23:00:00")))
# Manchester cleaning
zep_data_man = zep_data |> filter(location == "Manchester")
# Putting everything together
data_zep = list(select(zep_data_man, date_time, ZEP, location, instrument, company),
                select(zep_data_london, date_time, ZEP, location, instrument, company),
                select(zep_data_york, date_time, ZEP, location, instrument, company))
zep_data = data_zep |> reduce(full_join, by = c("date_time", "ZEP", "location", 
                                                "instrument", "company")) |>
  mutate(month = as.yearmon(date_time),
         date = as_date(date_time),
         day = day(date_time)) |>
  tibble()

## AQM data

aqm_data = lcs_data_no2 |>
  filter(company == "AQMesh", version == 'cal2') |> 
  rename(AQM = measurement) 

## Reference data

ref_raw = read.csv("quant_reference_hourly.csv")  

ref_data_no2 = ref_raw |>
  rename(REF = measurement) |>
  filter(measurand == "NO2") |> 
  mutate(date_time = as_datetime(time),
         month = as.yearmon(date_time),
         date = as_date(date_time),
         day = day(date_time),
         company = "Reference") |>
  select(-version, -time, -measurand) |>
  tibble() 



#----- Tubes Data imports -------

sheets <- excel_sheets("NO2_Diffusion_Tubes.xlsx")

# Removing the unwanted sheets

sheets = sheets[! (str_detect(sheets, "Monitor") | str_detect(sheets, "Graphs"))]

# Combining the sheets in one data frame

read_sheet <- function(sheet_name) {
  read_excel("NO2_Diffusion_Tubes.xlsx", sheet = sheet_name) %>%
    add_column(month = as.yearmon(sheet_name)) |>
    filter(!is.na(`Sample Number`)) |> 
    filter(Type == "Traditional Shelter") |>
    mutate(
      Location = mgsub(Location,
                       pattern = c("Fishergate, ", "Honor Oak Park",
                                   "Manchester (SH)", "Supernite, ", 
                                   "Fallowfield, ", ", Fishergate", 
                                   "Fishergate - ", "Honor Oak Park, "),
                       replacement = c("", "London", "Manchester", "", "",
                                       "", "", ""))) |>
    group_by(Location, month) |> 
    summarise(Tubes = mean(`ugm-3`)) |> 
    mutate(Tubes = Tubes/1.9125) |>
    rename(location = Location) |>
    ungroup()
}

tubes_data <- bind_rows(map(sheets, read_sheet, .progress = TRUE))  



#----- Monthly averages ------
# Calculating the MEAN or MEDIAN of the LCS data 

zep_monthly = zep_data |>
  group_by(month, instrument, company, location) |> 
  summarise(ZEP = mean(ZEP)) |> 
  ungroup()

aqm_monthly = aqm_data |>
  group_by(month, instrument, company, location) |> 
  summarise(AQM = mean(AQM))|> 
  ungroup()

ref_monthly = ref_data_no2 |>
  group_by(month, location) |> 
  summarise(REF = mean(REF)) |>
  ungroup()



#----- Calculating offset -------
## Putting the monthly averages and tubes data together to calculate the offset

lcs = full_join(filter(zep_monthly,
                       between(month, as.yearmon("November 2020"), as.yearmon("March 2022"))),
                filter(aqm_monthly,
                       between(month, as.yearmon("November 2020"), as.yearmon("March 2022"))),
                by = c("month", "location", "company", "instrument")) |> 
  pivot_longer(c("AQM", "ZEP"), 
               names_to = NULL,
               values_to = "LCS") |>
  filter(!is.na(LCS))

data_lcs = list(tubes_data,
                lcs,
                ref_monthly)
tubes_data = data_lcs |> reduce(left_join,
                                 by = c("month", "location")) |> 
  tibble() |> 
  mutate(offset = Tubes - LCS)



#----- Tubes accuracy  -------

tubes_accuracy = loop_data_2 |> 
  group_by(Location) |> 
  summarise(Tubes_RMSE = rmse(Tubes, REF)) |>
  ungroup()



#----- Tubes accuracy plot -----
tubes_vs_ref = ggplot(tubes_data |> 
                        pivot_longer(c("Tubes", "REF"), 
                                     names_to = "Type", 
                                     values_to = "Measurements"),
                      mapping = aes(month, Measurements, color = Type, group = Type)) +
  geom_line() + scale_color_brewer(palette="Set1") +
  facet_wrap(~location, nrow = 3) + 
  theme_fivethirtyeight()



#----- Offset on hourly data  -------

hourly_lcs = full_join(filter(select(zep_data, -day), 
                              between(month, as.yearmon("November 2020"), 
                                      as.yearmon("March 2022"))),
                       filter(select(aqm_data, -day, -version), 
                              between(month, as.yearmon("November 2020"), 
                                      as.yearmon("March 2022"))),
                       by = c("date_time", "location", "company", "instrument",
                              "month", "date")) |>
  pivot_longer(c("AQM", "ZEP"), 
               names_to = NULL,
               values_to = "LCS") |>
  filter(!is.na(LCS)) 

# Creation of a data frame with the lagged offsets
# EXPLANATION: In order to add a month to a yearmon() variable, make it numeric, 
#              add 1/12 to the value and turn it back to yearmon().

lagged_offsets = tubes_data |> 
  group_by(month, location, instrument, offset) |> 
  summarise() |>
  mutate(month = as.yearmon(1/12 + as.numeric(month)))

# Combining lagged data with hourly data
hourly_lcs = left_join(hourly_lcs,
                       lagged_offsets, 
                       by = c("month", "location", "instrument")) |>
  mutate(NEW_LCS = LCS + offset) 

# Adding reference data
hourly_lcs = left_join(hourly_lcs,
                       select(ref_data_no2, -company, -day), 
                       by = c("date_time", "month", "location", "date"))



#----- NEW_LCS accuracy  -------

lcs_statistics = hourly_lcs |>
  filter(!is.na(NEW_LCS)) |> 
  group_by(location, instrument) |> 
  summarise(OLD_RMSE = rmse(REF, LCS),
            NEW_RMSE = rmse(REF, NEW_LCS))



#----- AQMesh Plots  -------

## 1. Comparing the effect of the tubes in the 3 locations

aqm_comparisons = hourly_lcs |>
  filter(company == "AQMesh", !is.na(NEW_LCS)) |>
  group_by(date, location) |>
  summarise(LCS = mean(LCS), 
            NEW_LCS = mean(NEW_LCS), 
            REF = mean(REF, na.rm = TRUE)) |>
  pivot_longer(c("LCS", "NEW_LCS", "REF"), 
               names_to = "Type",
               values_to = "Measurements")

aqm_locations = ggplot(aqm_comparisons,
                       mapping = aes(date, Measurements,
                                     color = Type, group = Type)) +
  geom_line() + scale_color_brewer(palette="Dark2") +
  facet_wrap(~location, nrow = 3)  +
  theme_fivethirtyeight()

## 2. OLD AND NEW RMSE with time 

new_rmse_aqm = ggplot(hourly_lcs |>
                    filter(company == "AQMesh", !is.na(NEW_LCS)) |>
                    mutate(month = as_date(month)) |> 
                    group_by(month, location) |>
                    summarise(LCS = mean(LCS), NEW_LCS = mean(NEW_LCS),
                              REF = mean(REF, na.rm = TRUE),
                              NEW_RMSE = rmse(REF, NEW_LCS),
                              OLD_RMSE = rmse(REF, LCS)) |>
                    pivot_longer(c("OLD_RMSE", "NEW_RMSE"), 
                                 names_to = "rmse_type", 
                                 values_to = "RMSE") |>
                    mutate(rmse_type = as.factor(rmse_type),
                           rmse_type = factor(rmse_type, 
                                            levels=c("OLD_RMSE", "NEW_RMSE"))),
                  mapping = aes(month, RMSE, color = location, group = location)) +
  geom_line() + scale_color_brewer(palette="Set1") +
  scale_x_date(date_breaks = "4 months") +
  facet_wrap(~rmse_type) + labs(colour = "Location") +
  theme_fivethirtyeight()

#----- Zephyr Plots  -------

## 1. Comparing the effect of the tubes in the 3 locations

zep_comparisons = hourly_lcs |>
  filter(company == "Zephyr", !is.na(NEW_LCS)) |>
  group_by(date, location) |>
  summarise(LCS = mean(LCS), 
            NEW_LCS = mean(NEW_LCS),
            REF = mean(REF, na.rm = TRUE)) |>
  pivot_longer(c("LCS", "NEW_LCS", "REF"), 
               names_to = "Type",
               values_to = "Measurements")

zep_locations = ggplot(zep_comparisons,
                       mapping = aes(date, Measurements, color = Type, group = Type)) +
  geom_line() + scale_color_brewer(palette="Dark2") +
  facet_wrap(~location, nrow = 3) + 
  theme_fivethirtyeight()

## 2. OLD AND NEW RMSE with time 

new_rmse_zep = ggplot(hourly_lcs |>
                    filter(company == "Zephyr", !is.na(NEW_LCS)) |>
                      mutate(month = as_date(month)) |> 
                    group_by(month, location) |>
                    summarise(LCS = mean(LCS), NEW_LCS = mean(NEW_LCS),
                              REF = mean(REF, na.rm = TRUE),
                              NEW_RMSE = rmse(REF, NEW_LCS),
                              OLD_RMSE = rmse(REF, LCS)) |>
                    pivot_longer(c("OLD_RMSE", "NEW_RMSE"), 
                                 names_to = "rmse_type", 
                                 values_to = "RMSE") |>
                    mutate(rmse_type = as.factor(rmse_type),
                           rmse_type = factor(rmse_type, 
                                              levels=c("OLD_RMSE", "NEW_RMSE"))),
                  mapping = aes(month, RMSE, color = location, group = location)) +
  geom_line() + scale_color_brewer(palette="Set1") +
  facet_wrap(~rmse_type) + labs(colour = "Location") +
  scale_x_date(date_breaks = "4 months") +
  theme_fivethirtyeight()

