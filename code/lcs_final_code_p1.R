#----- Libraries imports -------

library(tidyverse) 
library(zoo)
library(ggthemes)
library(ggeasy) 
library(Metrics) 
library(hydroGOF)
library(mgsub) 
library(plotly)



#----- Data imports -----

## Importing the lcs and reference data set
lcs_raw = read.csv("quant_lcs_hourly.csv")   
ref_raw = read.csv("quant_reference_hourly.csv")  
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



#----- Kunak data ------

# Creation of a data frame containing only AP data 
ap_data = lcs_data_no2 %>% 
  filter(company == "Kunak") %>% 
  rename(AP = measurement)
  tibble()
# Then, I grouped the measurements for each day and calculated the mean measurement
# for each day 
ap_data_daily = ap_data %>%
  group_by(date) %>%
  summarise(AP = mean(AP, na.rm = TRUE)) |>
  ungroup()
# Plot showing the evolution of avg measurements per day with time
ap_data_plt1 = ggplot(ap_data_daily, mapping = aes(x = date, y = AP)) +
  geom_line() + 
  geom_smooth(se = FALSE) +
  theme_fivethirtyeight()

# hours contains an  value for the hours of the day
ap_data = ap_data %>% mutate(hours = hour(date_time))
# Then, I grouped the measurements for each hour and calculated the mean measurement
# for each  hour
ap_data_hourly = ap_data %>%
  group_by(hours) %>%
  summarise(AP = mean(AP, na.rm = TRUE))
# Plot showing the evolution of avg measurements per hour 
ap_data_plt2 = ggplot(ap_data_hourly, mapping = aes(x = hours, y = AP)) +
  geom_line(linewidth = 1.2) +
  theme_fivethirtyeight()

# Plotting all three APs on the same axis as time series plots
# First need to make a new grouped data that contains instrument
ap_data_hourly2 = ap_data %>%
  group_by(date_time, instrument) %>%
  summarise(AP = mean(AP, na.rm = TRUE))
# We can now plot...
ap_data_plt3 = ggplot(ap_data_hourly2, mapping = aes(x = date_time, y = AP,
                                                    color = instrument)) +
  geom_line() + 
  geom_smooth(color = "black", se = FALSE) +
  facet_wrap(~ instrument, nrow = 3) +
  theme_fivethirtyeight()



#----- Zephyr data ------

## Because unflagged data were discovered in the data set later in the studies, 
## some period of the data need to be removed in certain locations

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

## Exploring how the data evolved in April 2020
zep_plot1 = ggplot(zep_data |> 
                     filter(month == as.yearmon("April 2020")) |>
                     group_by(date_time, location, instrument) |>
                     summarise(ZEP = mean(ZEP, na.rm = TRUE)) |>
                     ungroup(),
                   mapping = aes(x = date_time,
                                 y = ZEP, colour = instrument)) +
  geom_line() + 
  geom_smooth(se = FALSE, colour = "black") +
  facet_wrap(~ location, nrow = 3) +
  easy_center_title() + labs(colour = "Instrument") +
  theme_fivethirtyeight()

## Comparison between April 2020, April 2021, and April 2022 using Zep data
# Filtering of these months 
aprils = c(as.yearmon("April 2020"), 
           as.yearmon("April 2021"),
           as.yearmon("April 2022"))

zep_plot2 = ggplot(zep_data |>
                     filter(month %in% aprils) |>
                     group_by(day, location, month) |>
                     summarise(ZEP = mean(ZEP, na.rm = TRUE)) |>
                     ungroup(), 
                   mapping = aes(x = day,
                                 y = ZEP,
                                 colour = as.factor(month))) +
  geom_line() +
  ggtitle("PLOT COMPARING THE ZEP NO2 DATA RECORDED APRIL 2020, 2021 AND 2022
                         IN YORK, MANCHESTER AND LONDON") +
  facet_wrap(~ location, nrow = 3) + labs(colour = "Month") +
  theme_fivethirtyeight()



#----- AQMesh data ------

aqm_data = lcs_data_no2 |>
  filter(company == "AQMesh", version == 'cal2') |> 
  rename(AQM = measurement) 

# Creation of the plot
aqm_plot1 = ggplot(aqm_data |> filter(month %in% aprils) |>
                    group_by(day, location, month) |>
                    summarise(AQM = mean(AQM, na.rm = TRUE)) |>
                    tibble(), 
                   mapping = aes(x = day,
                                 y = AQM, 
                                 colour = as.factor(month))) +
  geom_line() +
  ggtitle("PLOT COMPARING THE AQMESH NO2 DATA RECORDED APRIL 2020, 2021 
                    AND 2022 IN YORK, MANCHESTER AND LONDON") +
  facet_wrap(~ location, nrow = 3) +
  labs(colour = "Month") +
  theme_fivethirtyeight()



#----- Reference data ------

# Creation of the plot
ref_plot1 = ggplot(ref_data_no2 |>
                     filter(month %in% aprils) |>
                     group_by(date, location, month, day) |>
                     summarise(REF = mean(REF, na.rm = TRUE)) |>
                     ungroup(),
                   mapping = aes(x = day,
                                 y = REF, 
                                 colour = as.factor(month), 
                                 )) +
  geom_line() +
  ggtitle("PLOT COMPARING THE REFERENCE NO2 DATA RECORDED APRIL 2020, 2021 
                  AND 2022 IN YORK, MANCHESTER AND LONDON") +
  facet_wrap(~ location, nrow = 3) +
  labs(colour = "Month") +
  theme_fivethirtyeight()
  


#----- Ref VS Zephyr ------

## Comparison between Zep data and the reference data in Manchester

ref_data_man = ref_data_no2 |>
  filter(location == "Manchester") |>
  group_by(date, company) |>
  summarise(REF = mean(REF)) |>
  ungroup()
 
  
zep_data_man2 = zep_data |>
  filter(location == "Manchester") |> 
  group_by(date, company) |> 
  summarise(ZEP = mean(ZEP)) |>
  ungroup()

zep_vs_ref = ggplot(inner_join(zep_data_man2, 
                               ref_data_man,
                               by = "date") |>
                      pivot_longer(c("ZEP", "REF"),
                                   names_to = "instrument", 
                                   values_to = "measurements") |>
                      tibble(), 
                    mapping = aes(date, measurements, color = instrument)) +
  geom_line() +
  scale_color_brewer(palette="Set1") +
  ggtitle("ZEPHYR VS REFERENCE NO2 DATA RECORDED
             FROM 2020 TO 2022 IN MANCHESTER") +
  labs(colour = "COMPANY") +
  theme_fivethirtyeight(base_size = 12)



#----- Ref VS AQM VS Zep 1 ------

# Comparing reference data with the AQM data 

aqm_data_man = aqm_data |> 
  filter(location == "Manchester") |>
  group_by(date, company) |> 
  summarise(AQM = mean(AQM))

avg_data = list(zep_data_man2, ref_data_man, aqm_data_man)

zep_vs_ref_vs_aqm1 = ggplot(avg_data |> 
                             reduce(inner_join, by = "date") |>
                             pivot_longer(c("ZEP", "REF", "AQM"),
                                          names_to = "instrument", 
                                          values_to = "measurements") |>
                             tibble(),
                           mapping = aes(date, measurements,
                                         color = instrument, group = instrument)) +
  geom_line() +
  scale_color_brewer(palette="Set1") +
  facet_wrap(~company, nrow = 3) +
  ggtitle("Plot comparing the AQMESH, ZEPHYR and Reference NO2
          Data recorded in Manchester") +
  xlab("Time in years") + ylab("NO2 in ppb") + labs(colour = "COMPANY") +
  theme_fivethirtyeight()

#----- Ref VS AQM VS Zep 2 ------

# ZEP
zep_data_aprils = zep_data |>
  filter(month %in% aprils) |>
  group_by(date, day, location, month) |>
  summarise(ZEP = mean(ZEP, na.rm = TRUE)) |>
  ungroup()
# REF
ref_data_aprils = ref_data_no2 |>
  filter(month %in% aprils) |>
  group_by(date, location, month, day) |>
  summarise(REF = mean(REF, na.rm = TRUE)) |>
  ungroup()
# AQM
aqm_data_aprils = aqm_data |> 
  filter(month %in% aprils) |>
  group_by(date, day, location, month) |>
  summarise(AQM = mean(AQM, na.rm = TRUE)) |>
  tibble()

# Merging and pivoting the data frames
avg_data2 = list(zep_data_aprils, ref_data_aprils, aqm_data_aprils)
combined_data = avg_data2 |> reduce(full_join, by = c("date", "day", "month", "location")) |> tibble()
combined_data = combined_data |> pivot_longer(c("ZEP", "AQM", "REF"),
                                                names_to = "company", values_to = "measurements" )

# Plot
zep_vs_ref_vs_aqm2 = ggplot(combined_data, mapping = aes(day, 
                                                         measurements, 
                                                         color = as.factor(month))) +
  geom_line() +
  scale_color_brewer(palette="Set1") +
  facet_grid(location~company) +
  ggtitle("COMPARISON OF ZEP, REF, AND AQM IN APRIL 2020, 2021 
              AND 2022 IN MANCHESTER, LONDON, AND YORK ") + 
  xlab("days in each month") + ylab("NO2 in ppb") + labs(colour = "COMPANY") +
  theme_fivethirtyeight()



#----- Sensors accuracy ------

combined_data2 = full_join(select(zep_data, date_time, instrument, location, company, month, ZEP),
                           select(aqm_data, date_time, instrument, location, company, month, AQM),
                           by = c("date_time", "location", 
                                  "instrument", "company", "month")) |>
  tibble() 

combined_data2 = combined_data2 |> pivot_longer(c("ZEP", "AQM"),
                                                names_to = NULL, values_to = "LCS" )

combined_data2 = full_join(combined_data2, 
                           select(ref_data_no2, date_time, date, location, month, REF),
                           by = c("date_time", "location","month")) |>
  filter(!is.na(LCS)) 

combined_data3 = combined_data2 |>
  group_by(instrument, company) |>
  summarise(
    RMSE = rmse(LCS, REF),
    R2 = cor(LCS, REF, use = "pairwise.complete.obs")**2
  ) |> ungroup()

stats_df <- combined_data3 |>
  mutate(x=10, y=150) |>
  group_by(company) |> 
  mutate(
    i = row_number(),
    y = 150 - (i * 10),
    lab = sprintf("RMSE = %.2f   R2 = %.2f", RMSE, R2)
  ) %>%
  ungroup()

sensors_accuracy = ggplot(filter(full_join(combined_data2, combined_data3), !is.na(LCS), !is.na(REF)),
                                  mapping = aes(x=REF, y=LCS, color = instrument, group = instrument)) +
  geom_point() + geom_smooth(method = "lm") + scale_color_brewer(palette="Dark2") +
  geom_text(aes(x=x, y=y, label=lab), data=stats_df, hjust=0, vjust=0.5) + 
  facet_grid(~company) + ggtitle("REF VS LCS") + 
  xlab("Reference data (ppb)") + ylab("LCS data (ppb)") + labs(colour = "COMPANY") + 
  theme_fivethirtyeight()



#----- RMSE with time -----

combined_data2 = combined_data2 |>
  group_by(month, instrument, company) |>
  summarise(
    RMSE = rmse(LCS, REF),
    R2 = cor(LCS, REF, use = "pairwise.complete.obs")**2
  )

rmse_with_time = ggplot(combined_data2, 
                                  mapping = aes(month, RMSE, color = instrument, group = instrument)) +
  geom_line() + scale_color_brewer(palette="Dark2") +
  facet_grid(~company) +
  ggtitle("EVOLUTION OF THE INSTRUMENTS RMSE WITH TIME") + 
  xlab("Time in months") + ylab("RMSE (ppb)") + labs(colour = "COMPANY") + 
  theme_fivethirtyeight()
