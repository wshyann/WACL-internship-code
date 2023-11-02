library(tidyverse)
library(zoo) # Thats for the yearmon() function
library(ggthemes)
library(ggeasy) 
library(Metrics) 
library(hydroGOF) # allow me to have an RMSE that takes NA values into consideration
library(mgsub) # allow me to use mgsub
library(plotly)



#library(lubridate) lubridate is actually also part of the tidyverse
#library(hms)

# Importing the lcs and reference data set

lcs_data = read.csv("quant_lcs_hourly.csv")   # base library
lcs_data2 = read_csv("quant_lcs_hourly.csv")  # readr from tidyverse
ref_data = read.csv("quant_reference_hourly.csv")  # base library
ref_data2 = read_csv("quant_reference_hourly.csv") # readr from tidyverse
inst_data = read.csv("quant_instruments.csv")

# Merging the instrument data frame with the lcs data frame
lcs_data = inner_join(lcs_data, inst_data, by = c("instrument"))




# TASK 1: Subsetting data to NO2 and save a separate copy

lcs_data_no2 = lcs_data %>% 
  filter(measurand == "NO2") %>% 
  tibble()

ref_data_no2 = ref_data |> 
  filter(measurand == "NO2") |>
  tibble()




# TASK 2: Why does 1579705200 = 2020-01-22 15:00:00 ?
# ANSWER: Is it the number of seconds from January 1, 1970 ?




# TASK 3: Plot all sensors from 1 company

# Creation of a data frame containing only AP data and suppression of error values
# A better way to do it is to create a vector with all the APs and use %in%

ap_data = lcs_data_no2 %>% 
  filter(company == "Kunak", is.na(flag)) %>% 
  tibble()

# Creation of a column called date_time
# It contains the time as an actual datetime variable
ap_data = ap_data %>% mutate(date_time = as_datetime(time))

# Creation of a column called day
ap_data = ap_data %>% mutate(day = dmy(format(date_time, format = "%d/%m/%y")))

# Then, I grouped the measurements for each day and calculated the mean measurement
# for each day 
group_by_day = ap_data %>%
  group_by(day) %>%
  summarise(avg_per_day = mean(measurement, na.rm = TRUE))

# Plot showing the evolution of avg measurements per day with time
avg_day_plot = ggplot(group_by_day, mapping = aes(x = day, y = avg_per_day)) +
  geom_line() + 
  geom_smooth(se = FALSE)

# hours contains an  value for the hours of the day
# How to get hours only?
# ap_data = ap_data %>% mutate(hours = as_hms(format(date_time, format = "%H:%M:%S")))
# There is a nicer way of getting the hour 
ap_data = ap_data %>% mutate(hours = hour(date_time))

# Then, I grouped the measurements for each hour and calculated the mean measurement
# for each  hour
group_by_hour = ap_data %>%
  group_by(hours) %>%
  summarise(avg_per_hour = mean(measurement, na.rm = TRUE))
# Plot showing the evolution of avg measurements per hour 
avg_hour_plot = ggplot(group_by_hour, mapping = aes(x = hours, y = avg_per_hour)) +
  geom_line(linewidth = 1.2)

# Plotting all three APs on the same axis as time series plots
# First need to make a new grouped data that contains instrument
group_by_day2 = ap_data %>%
  group_by(date_time, instrument) %>%
  summarise(avg_per_day = mean(measurement, na.rm = TRUE))
# We can now plot...
three_ap_plot = ggplot(group_by_day2, mapping = aes(x = date_time, y = avg_per_day,
                                              color = instrument)) +
  geom_line() + 
  geom_smooth(color = "black", se = FALSE) +
  facet_wrap(~ instrument, nrow = 3)




# TASK 4: Compare NO2 from different locations from the same company


# TASK 4-1: Creation of a data frame containing only Zep data and suppression of error values
zep_data = lcs_data_no2 %>% 
  filter(company == "Zephyr", is.na(flag)) %>% 
  tibble()
# Creation of columns date_time and month 
# It contains the time as an actual datetime variable
zep_data = zep_data |> mutate(date_time = as_datetime(time),
                              month = format(date_time, format = "%m/%y"),
                              date = dmy(format(date_time, format = "%d/%m/%y")))

# Task 6 revealed that there are some wrong values in zep_data so I created zep_data_new

zep_data = zep_data |> rename(ZEP = measurement)
zep_data_london =  zep_data |> filter(location == "London",
                                      !between(date_time, as_datetime("2020-03-11 00:00:00"), as_datetime("2020-03-27 23:00:00"))) 

zep_data_york =  zep_data |> filter(location == "York",
                                    !between(date_time, as_datetime("2020-07-13 00:00:00"), as_datetime("2020-07-23 23:00:00")),
                                    !between(date_time, as_datetime("2020-09-11 00:00:00"), as_datetime("2020-09-29 23:00:00")))

zep_data_man = zep_data |> filter(location == "Manchester")
data_zep = list(select(zep_data_man, date_time, ZEP, location, instrument, company),
                select(zep_data_london, date_time, ZEP, location, instrument, company),
                select(zep_data_york, date_time, ZEP, location, instrument, company))
zep_data_new = data_zep |> reduce(full_join, by = c("date_time", "ZEP", "location", "instrument", "company")) |> 
  tibble()

zep_data_new = zep_data_new |> mutate(month = format(date_time, format = "%m/%y"),
                              date = dmy(format(date_time, format = "%d/%m/%y")))

# TASK 4-2: Exploring how the data evolved in April 2020
# Choosing only the data from April 2020
zep_data_april20 = zep_data_new |> filter(month == "04/20")
# Daily average of the measurements 
group_by_day_zep = zep_data_april20 %>%
  group_by(date_time, location, instrument) %>%
  summarise(avg_per_day = mean(ZEP, na.rm = TRUE))
# Plot showing the evolution of avg measurements per day with time in April 2020
avg_day_plot_zep_2020 = ggplot(group_by_day_zep,
                               mapping = aes(x = date_time,
                                             y = avg_per_day, colour = instrument)) +
  geom_line() + 
  geom_smooth(se = FALSE, colour = "black") +
  facet_wrap(~ location, nrow = 3)


# TASK 4-3: Comparison between April 2020, April 2021, and April 2022 using Zep data
# Filtering of these months 
aprils = c("04/20", "04/21", "04/22")
zep_data_aprils = zep_data_new |> filter(month %in% aprils)
# Creation of the day column 
# Created it to use it as a x-axis so that the 3 plots can be in top of each other
zep_data_aprils = zep_data_aprils |> mutate(day = day(date_time))
# Daily average of the zep measurements 
group_by_day_zep_aprils = zep_data_aprils %>%
  group_by(day, location, month) %>%
  summarise(avg_per_day = mean(ZEP, na.rm = TRUE))
# Creation of the plot
avg_day_plot_zep_aprils = ggplot(group_by_day_zep_aprils, mapping = aes(x = day,
                                                               y = avg_per_day,
                                                               colour = month)) +
  geom_line() +
  ggtitle("PLOT COMPARING THE ZEP NO2 DATA RECORDED APRIL 2020, 2021 AND 2022
                \t \t  \t IN YORK, MANCHESTER AND LONDON") +
  facet_wrap(~ location, nrow = 3)


# TASK 4-4:Comparison between April 2020, April 2021, and April 2022 using reference data
# Create the day and month column 
# day is "12" for example but as an integer variable and month is "04/20" for example as strings
# Creating the day column so that I can do daily averages
ref_data_no2 = ref_data_no2 |> mutate(date_time = as_datetime(time))
ref_data_no2_aprils = ref_data_no2 |> mutate(month = format(date_time, format = "%m/%y"),
                                             day = day(date_time),
                                             date = dmy(format(date_time, format = "%d/%m/%y")))
# Filtering of these months 
ref_data_no2_aprils = ref_data_no2_aprils |> filter(month %in% aprils)
# Daily average of the reference measurements 
group_by_day_ref_aprils = ref_data_no2_aprils %>%
  group_by(date, location, month, day) %>%
  summarise(avg_per_day = mean(measurement, na.rm = TRUE))
# Creation of the plot
avg_day_plot_ref_aprils = ggplot(group_by_day_ref_aprils, 
                                 mapping = aes(x = day,
                                               y = avg_per_day, colour = month, 
                                               )) +
  geom_line() +
  ggtitle("PLOT COMPARING THE REFERENCE NO2 DATA RECORDED APRIL 2020, 2021 AND 2022
                \t \t  \t IN YORK, MANCHESTER AND LONDON") +
  facet_wrap(~ location, nrow = 3) +
  theme_few(base_size = 12)



# TASK 4-5:Comparison between April 2020, April 2021, and April 2022 using AQMesh data
# Creation of a data frame containing only AQMesh data and suppression of error values
aqm_data_no2 = lcs_data_no2 %>% 
  filter(company == "AQMesh", is.na(flag), version == 'cal2') %>% 
  tibble()
# Create an actual date_time column for aqm_data
aqm_data_no2 = aqm_data_no2 |> mutate(date_time = as_datetime(time))
# Create the day and month column 
aqm_data_no2 = aqm_data_no2 |> mutate(month = format(date_time, format = "%m/%y"),
                                             day = day(date_time))
# Filtering of aprils 
aqm_data_no2_aprils = aqm_data_no2 |> filter(month %in% aprils)
# Daily average of the reference measurements 
group_by_day_aqm_aprils = aqm_data_no2_aprils %>%
  group_by(day, location, month) %>%
  summarise(avg_per_day = mean(measurement, na.rm = TRUE))
# Creation of the plot
avg_day_plot_aqm_aprils = ggplot(group_by_day_aqm_aprils, 
                                 mapping = aes(x = day,
                                               y = avg_per_day, colour = month)) +
  geom_line() +
  ggtitle("PLOT COMPARING THE AQMESH NO2 DATA RECORDED APRIL 2020, 2021 AND 2022
                \t \t  \t IN YORK, MANCHESTER AND LONDON") +
  facet_wrap(~ location, nrow = 3)




# TASK 5: Comparison between Zep data and the reference data in Manchester

# TASK 5-1: Comparing reference data with the zep data 
# Creating the date column so that I can do daily averages
ref_data_no2 = ref_data_no2 |> mutate(date_time = as_datetime(time),
                                      #date = as_date(date_time) I could have used this
                                      date = dmy(format(date_time, format = "%d/%m/%y")))
# Filtering Manchester
ref_data_no2_man = ref_data_no2 |> filter(location == "Manchester") 
# Daily average of the reference measurements
ref_data_no2_avg_man = ref_data_no2_man |> group_by(date)|> 
  summarise(avg_per_day_ref = mean(measurement))
# Daily average of the zep measurements in Manchester
zep_data_day_avg_man = filter(zep_data_new, Location == "Manchester") |> group_by(date) |> 
  summarise(avg_per_day_zep = mean(ZEP))
# Combining the zep and reference daily averages 
combined_data = inner_join(zep_data_day_avg_man, ref_data_no2_avg_man, by = "date", 
                               #relationship = "many-to-many" | that's if you want to accept the warning,
                           ) |> 
  tibble() |> 
  rename(ZEP = avg_per_day_zep, REF = avg_per_day_ref) |> 
  pivot_longer(c(ZEP, REF), names_to = "Instrument", values_to = "Measurements")
  
# Creation of a plot comparing the averages measurements
zep_vs_ref = ggplot(combined_data, mapping = aes(date, Measurements, color = Instrument)) +
  geom_line() +
  scale_color_brewer(palette="Set1") +
  ggtitle("ZEPHYR VS REFERENCE NO2 DATA RECORDED
          FROM 2020 TO 2022 IN MANCHESTER") + easy_center_title() +
  theme(plot.title = element_text(size = 5, face = "bold")) + #Why this does not work ?
  theme_fivethirtyeight(base_size = 12)


# TASK 5-2: Comparing reference data with the AQM data 
# Adding the date column to aqm_data_no2
aqm_data_no2 = aqm_data_no2 |> mutate(date = as_date(date_time))
# Keep only Manchester AQM
aqm_data_no2_man = aqm_data_no2 |> filter(Location == "Manchester")
# Daily average of the AQM measurements
aqm_data_no2_avg_man = aqm_data_no2 |> filter(Location == "Manchester") |> group_by(date)|> 
  summarise(avg_per_day_aqm = mean(AQM))
# Combining the ZEP, reference and AQM daily averages 
avg_data = list(zep_data_day_avg_man, ref_data_no2_avg_man, aqm_data_no2_avg_man)
combined_data2 = avg_data |> reduce(inner_join, by = "date") |> tibble()
combined_data2 = combined_data2 |> pivot_longer(c("avg_per_day_zep", "avg_per_day_ref", "avg_per_day_aqm"),
                               names_to = "instrument", values_to = "measurements")
combined_data2 = combined_data2 |> mutate(company = toupper(gsub("avg_per_day_", "", instrument)))
# Creation of a plot comparing the averages measurements
zep_vs_ref_vs_aqm = ggplot(combined_data2, mapping = aes(date, measurements, color = company, group = company)) +
  geom_line() +
  #geom_smooth(se = FALSE) +
  # scale_color_manual(labels = c("AQM","REFERENCE","ZEP"), values = c("green", "red", "blue"))
  scale_color_brewer(palette="Set1") +
  facet_wrap(~company, nrow = 3) +
  ggtitle("Plot comparing the AQMESH, ZEPHYR and Reference NO2
          Data recorded in Manchester") + easy_center_title() +
  xlab("Time in years") + ylab("NO2 in ppb") + labs(colour = "COMPANY") +
  theme_fivethirtyeight()
  

  
# TASK 5-3: MAIN PLOT: COMPARISON OF ZEP, REF AND AQM IN APRIL 2020, 2021 AND 2022 IN 
# IN MANCHESTER, LONDON AND YORK

# TASK 5-3-1: Preparing the data
#ZEP: zep_data_aprils
zep_data_new = zep_data_new |> mutate(date = as_date(date_time),
                              month = format(date_time, format = "%m/%y"),
                              day = day(date_time))
zep_data_aprils = zep_data_new |> filter(month %in% aprils)
#REF: ref_data_no2_aprils
ref_data_no2 = ref_data_no2 |> mutate(date_time = as_datetime(time),
                                      date = as_date(date_time),
                                      month = format(date_time, format = "%m/%y"),
                                      day = day(date_time))
ref_data_no2 = ref_data_no2 |> select(-flag, -flagreason)
ref_data_no2_aprils = ref_data_no2 |> filter(month %in% aprils)
#AQM: aqm_data_no2_aprils
aqm_data_no2 = aqm_data_no2 |> mutate(date_time = as_datetime(time),
                                      date = as_date(date_time),
                                      month = format(date_time, format = "%m/%y"),
                                      day = day(date_time))
aqm_data_no2 = aqm_data_no2 |> select(date_time, month, location, instrument, company, date, day, measurement)
aqm_data_no2 = aqm_data_no2 |> rename(AQM = measurement)
aqm_data_no2_aprils = aqm_data_no2 |> filter(month %in% aprils)

# TASK 5-3-2: Averaging the measurements 
#ZEP
zep_data_aprils_avg = zep_data_aprils |> group_by(date, day, month, location)|> 
  summarise(ZEP = mean(ZEP))
#REF
ref_data_no2_aprils_avg = ref_data_no2_aprils |> group_by(date, day, month, location)|> 
  summarise(REF = mean(measurement))
#AQM
aqm_data_no2_aprils_avg = aqm_data_no2_aprils |> group_by(date, day, month, location)|> 
  summarise(AQM = mean(measurement))

# TASK 5-3-3: Merging and pivoting the data frames
avg_data2 = list(zep_data_aprils_avg, ref_data_no2_aprils_avg, aqm_data_no2_aprils_avg)
combined_data3 = avg_data2 |> reduce(full_join, by = c("date", "day", "month", "location")) |> tibble()
combined_data3 = combined_data3 |> pivot_longer(c("ZEP", "AQM", "REF"),
                                                names_to = "company", values_to = "measurements" )
  
# TASK 5-3-4: Creation of a plot
zep_vs_ref_vs_aqm_aprils = ggplot(combined_data3, mapping = aes(day, measurements, color = month)) +
  geom_line() +
  # scale_color_manual(labels = c("AQM","REFERENCE","ZEP"), values = c("green", "red", "blue"))
  scale_color_brewer(palette="Set1") +
  facet_grid(location~company) +
  ggtitle("\nCOMPARISON OF ZEP, REF, AND AQM IN APRIL 2020, 2021 AND 2022 IN MANCHESTER,
          LONDON, AND YORK \n") + 
  xlab("days in each month") + ylab("NO2 in ppb") + labs(colour = "COMPANY") +
  theme_fivethirtyeight()

  
  
# TASK 6: Quantify the accuracy of each sensor
# zep_data already only contains out_of_box version
# aqm_data_no2 also already only has cal2 version

aqm_data_no2 = aqm_data_no2 |> rename(AQM = measurement)

ref_essentials = ref_data_no2 |> group_by(date_time, date, location, measurement)|> 
  summarise()
ref_essentials = ref_essentials |> rename(REF = measurement)

combined_data4 = full_join(select(zep_data_new, date_time, instrument, location, company, ZEP),
                            select(aqm_data_no2, date_time, instrument, location, company, AQM), 
                            by = c("date_time", "location", "instrument", "company"))  |> tibble() 

combined_data4 = combined_data4 |> pivot_longer(c("ZEP", "AQM"),
                                                  names_to = NULL, values_to = "LCS" )

combined_data4 = full_join(combined_data4, ref_essentials, by = c("date_time", "location")) |>
  filter(!is.na(LCS)) 

combined_data4 = combined_data4 |>
  group_by(instrument, company) %>%
  summarise(
    RMSE = rmse(LCS, REF),
    R2 = cor(LCS, REF, use = "pairwise.complete.obs")**2
  ) %>% ungroup()

# Plots

# LCS vs REF plot
# First time I did the plot, I realised that some zep data were false so I had to fiter again
# I did the following lines of code to better observe these issues
## test = ggplot(zep_data, mapping = aes(x = date_time, y = ZEP)) + geom_line() + facet_wrap(~location, nrow = 3)
## ggplotly(test)


combined_data5 = full_join(zep_data_new,
                           select(aqm_data_no2, date_time, instrument, location, company, AQM), 
                           by = c("date_time", "location", "instrument", "company")) |>
  tibble()
combined_data5 = left_join(combined_data5, ref_essentials, by = c("date_time", "location"))
combined_data5 = combined_data5 |> pivot_longer(c("ZEP", "AQM"),
                                                names_to = NULL, values_to = "LCS" )

  
# Actually plotting the data

combined_data5 = full_join(combined_data5, combined_data4)

stats_df <- combined_data4 %>%
              mutate(x=10, y=150) %>%
              group_by(company) %>% 
              mutate(
                i = row_number(),
                y = 150 - (i * 10),
                lab = sprintf("RMSE = %.2f   R2 = %.2f", RMSE, R2)
              ) %>%
              ungroup()

# Ideally do this for combined_data4 rather than having to change it twice here!!
combined_data5 <- combined_data5 %>%
                    mutate(company = factor(company, 
                                            levels=c("Zephyr", "AQMesh"),
                                            labels=c("Company1", "Company 2")))
stats_df <- stats_df %>%
  mutate(company = factor(company, 
                          levels=c("Zephyr", "AQMesh"),
                          labels=c("Company1", "Company 2")))

zep_vs_ref_vs_aqm_stats1 = ggplot(filter(combined_data5, !is.na(LCS), !is.na(REF)),
                                  mapping = aes(x=REF, y=LCS, color = instrument, group = instrument)) +
  geom_point() + geom_smooth(method = "lm") + scale_color_brewer(palette="Set1") +
  geom_text(aes(x=x, y=y, label=lab), data=stats_df, hjust=0, vjust=0.5) + 
  facet_grid(~company) + ggtitle("REF VS LCS") + 
  xlab("Reference data (ppb)") + ylab("LCS data (ppb)") + labs(colour = "COMPANY") + 
  theme_fivethirtyeight()


# Date_time plot

zep_data_new = zep_data_new |> mutate(month = as.yearmon(date_time))|>
  rename(Location = location)

aqm_data_no2 = aqm_data_no2 |> mutate(month = as.yearmon(date_time)) |>
  rename(Location = location)

ref_essentials = ref_essentials |> mutate(month = as.yearmon(date_time)) |>
  rename(Location = location)

combined_data6 = full_join(select(zep_data_new, date_time, date, instrument, ZEP, month, company),
                           select(aqm_data_no2, date_time, date, instrument, AQM, month, company), 
                           by = c("instrument", "month", "company", "date_time", "date"))  |> tibble() 

combined_data6 = combined_data6 |> pivot_longer(c("ZEP", "AQM"),
                                                names_to = NULL , values_to = "LCS")
combined_data6 = combined_data6 |> filter(!is.na(LCS))

combined_data6 = combined_data6 |> group_by(date, month, instrument, company) |> 
  summarise(LCS = mean(LCS))

ref_essentials = ref_essentials |> group_by(date, month) |> 
  summarise(REF = mean(REF))


combined_data6 = full_join(combined_data6, ref_essentials,
                           by = c("month", "date")) |> filter(!is.na(LCS))

combined_data6 = combined_data6 |>
  group_by(month, instrument, company) |>
  summarise(
    RMSE = rmse(LCS, REF),
    R2 = cor(LCS, REF, use = "pairwise.complete.obs")**2
  )

zep_vs_ref_vs_aqm_stats2 = ggplot(combined_data6, 
                                  mapping = aes(month, RMSE, color = instrument, group = instrument)) +
  geom_line() + scale_color_brewer(palette="Dark2") +
  facet_grid(~company) + ggtitle("EVOLUTION OF THE INSTRUMENTS RMSE WITH TIME") + 
  xlab("Time in months") + ylab("RMSE (ppb)") + labs(colour = "COMPANY") + 
  theme_fivethirtyeight()






# TASK 5: Compare NO2 averages from 1 company to another by...
# TASK 5.1: Restrict to the 4 QUANT study companies during Dec 2019 - March 2020

# TASK 5.2: Calculate average per company each day of the out-of-box version

# TASK 5.3: Plot each company’s average as a time-series on the same axes (1 facet)


#The folllowing was done on the console
#combined_data6 = combined_data6 |> rename(instrument = instruments)
#combined_data6 = combined_data6 |> mutate(instrument = gsub("measurement", "REF", instrument))



#####
capitalise_df = function(df) {
  for (i in colnames(df)) {
    stringr::str_to_title(i)
  }
}
#####