library(tidyverse)
library(zoo) # Thats for the yearmon() function
library(readxl)
library(mgsub)


# Knowing the name of the sheets in the data 
excel_sheets("NO2_Diffusion_Tubes.xlsx")


sheets <- excel_sheets("NO2_Diffusion_Tubes.xlsx")

### Removing the unwanted sheets

# Attempt 1
for (i in sheets) {
  if (str_detect(i, " Monitor") |  str_detect(i, " Graphs") == TRUE) {
    sheets = sheets[-which(sheets == i)]
  }
}

# Attempt 2 (much quicker)
sheets = sheets[! (str_detect(sheets, "Monitor") | str_detect(sheets, "Graphs"))]



### Combining the sheets in one data frame

# Attempt 0 (worked but is too long)

#####
## November 2020

tubes_data_nov_20 = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = 1)

# Deal with NA values
tubes_data_nov_20 = tubes_data_nov_20 |> filter(!is.na(`Sample Number`))

# TASK 1: Filter to Traditional Shelter
tubes_data_nov_20 = tubes_data_nov_20 |> filter(Type == "Traditional Shelter")

# TASK 2: Average of the multiple replicate tubes per location
tubes_data_nov_20 = tubes_data_nov_20 |> group_by(Location) |> summarise(avg_masurement = mean(`ugm-3`)) 

# TASK 3: Convert average concentration into ppb by dividing by 1.9125
tubes_data_nov_20 = tubes_data_nov_20 |> mutate(avg_masurement = avg_masurement/1.9125)

# TASK 4: Calculate the average LCS concentration over the same time-period
# I will also add a date column and modify the names in the location column
# Adding NA values for Manchester and York
df = data.frame(Location = c("Manchester"), avg_masurement = c(NA))
tubes_data_nov_20 = tubes_data_nov_20 |> rows_insert(df)
tubes_data_nov_20 = tubes_data_nov_20 |> add_column(month = as.yearmon("Nov 2020"))
tubes_data_nov_20 = tubes_data_nov_20 |> mutate(Location = mgsub(Location,pattern = c("Fishergate, ", "Honor Oak Park, ", "Supernite, "),
                                                                 replacement = c("", "", "")))


## December 2020

tubes_data_dec_20 = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = "December 2020")

# Deal with NA values
tubes_data_dec_20 = tubes_data_dec_20 |> filter(!is.na(`Sample Number`))

# TASK 1: Filter to Traditional Shelter
tubes_data_dec_20 = tubes_data_dec_20 |> filter(Type == "Traditional Shelter")

# TASK 2: Average of the multiple replicate tubes per location
tubes_data_dec_20 = tubes_data_dec_20 |> group_by(Location) |> summarise(avg_masurement = mean(`ugm-3`)) 

# TASK 3: Convert average concentration into ppb by dividing by 1.9125
tubes_data_dec_20 = tubes_data_dec_20 |> mutate(avg_masurement = avg_masurement/1.9125)

# TASK 4: Calculate the average LCS concentration over the same time-period
# I will also add a date column and modify the names in the location column
tubes_data_dec_20 = tubes_data_dec_20 |> add_column(month = as.yearmon("dec 2020"))
tubes_data_dec_20 = tubes_data_dec_20 |> mutate(
  Location = mgsub(Location,pattern = c("Fishergate, ", "Honor Oak Park, ", "Supernite, ", "Fallowfield, "),
                   replacement = c("", "", "", "")))



## January 2021

tubes_data_jan_21 = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = "January 2021")

# Deal with NA values
tubes_data_jan_21 = tubes_data_jan_21 |> filter(!is.na(`Sample Number`))

# TASK 1: Filter to Traditional Shelter
tubes_data_jan_21 = tubes_data_jan_21 |> filter(Type == "Traditional Shelter")

# TASK 2: Average of the multiple replicate tubes per location
tubes_data_jan_21 = tubes_data_jan_21 |> group_by(Location) |> summarise(avg_masurement = mean(`ugm-3`)) 

# TASK 3: Convert average concentration into ppb by dividing by 1.9125
tubes_data_jan_21 = tubes_data_jan_21 |> mutate(avg_masurement = avg_masurement/1.9125)

# TASK 4: Calculate the average LCS concentration over the same time-period
# I will also add a date column and modify the names in the location column
# Adding NA values for Manchester and York
df = data.frame(Location = c("London"), avg_masurement = c(NA))
tubes_data_jan_21 = tubes_data_jan_21 |> rows_insert(df)
tubes_data_jan_21 = tubes_data_jan_21 |> add_column(month = as.yearmon("Jan 2021"))
tubes_data_jan_21 = tubes_data_jan_21 |> mutate(
  Location = mgsub(Location,pattern = c("Fishergate, ", "Honor Oak Park, ", "Supernite, ", "Fallowfield, ", ", Fishergate"),
                   replacement = c("", "", "", "", "")))



## February 2021

tubes_data_feb_21 = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = "February 2021")

# Deal with NA values
tubes_data_feb_21 = tubes_data_feb_21 |> filter(!is.na(`Sample Number`))

# TASK 1: Filter to Traditional Shelter
tubes_data_feb_21 = tubes_data_feb_21 |> filter(Type == "Traditional Shelter")

# TASK 2: Average of the multiple replicate tubes per location
tubes_data_feb_21 = tubes_data_feb_21 |> group_by(Location) |> summarise(avg_masurement = mean(`ugm-3`)) 

# TASK 3: Convert average concentration into ppb by dividing by 1.9125
tubes_data_feb_21 = tubes_data_feb_21 |> mutate(avg_masurement = avg_masurement/1.9125)

# TASK 4: Calculate the average LCS concentration over the same time-period
# I will also add a date column and modify the names in the location column
tubes_data_feb_21 = tubes_data_feb_21 |> add_column(month = as.yearmon("Feb 2021"))
tubes_data_feb_21 = tubes_data_feb_21 |> mutate(
  Location = mgsub(Location,pattern = c("Fishergate, ", "Honor Oak Park, ", "Supernite, ", "Fallowfield, ", ", Fishergate"),
                   replacement = c("", "", "", "", "")))


## March 2021

tubes_data_mar_21 = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = "March 2021")

# Deal with NA values
tubes_data_mar_21 = tubes_data_mar_21 |> filter(!is.na(`Sample Number`))

# TASK 1: Filter to Traditional Shelter
tubes_data_mar_21 = tubes_data_mar_21 |> filter(Type == "Traditional Shelter")

# TASK 2: Average of the multiple replicate tubes per location
tubes_data_mar_21 = tubes_data_mar_21 |> group_by(Location) |> summarise(avg_masurement = mean(`ugm-3`)) 

# TASK 3: Convert average concentration into ppb by dividing by 1.9125
tubes_data_mar_21 = tubes_data_mar_21 |> mutate(avg_masurement = avg_masurement/1.9125)

# TASK 4: Calculate the average LCS concentration over the same time-period
# I will also add a date column and modify the names in the location column
tubes_data_mar_21 = tubes_data_mar_21 |> add_column(month = as.yearmon("March 2021"))
tubes_data_mar_21 = tubes_data_mar_21 |> mutate(
  Location = mgsub(Location,pattern = c("Fishergate, ", "Honor Oak Park, ", "Supernite, ",
                                        "Fallowfield, ", ", Fishergate", "Fishergate - "),
                   replacement = c("", "", "", "", "", "")))



## April 2021

tubes_data_apr_21 = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = "April 2021")

# Deal with NA values
tubes_data_apr_21 = tubes_data_apr_21 |> filter(!is.na(`Sample Number`))

# TASK 1: Filter to Traditional Shelter
tubes_data_apr_21 = tubes_data_apr_21 |> filter(Type == "Traditional Shelter")

# TASK 2: Average of the multiple replicate tubes per location
tubes_data_apr_21 = tubes_data_apr_21 |> group_by(Location) |> summarise(avg_masurement = mean(`ugm-3`)) 

# TASK 3: Convert average concentration into ppb by dividing by 1.9125
tubes_data_apr_21 = tubes_data_apr_21 |> mutate(avg_masurement = avg_masurement/1.9125)

# TASK 4: Calculate the average LCS concentration over the same time-period
# I will also add a date column and modify the names in the location column
tubes_data_apr_21 = tubes_data_apr_21 |> add_column(month = as.yearmon("April 2021"))
tubes_data_apr_21 = tubes_data_apr_21 |> mutate(
  Location = mgsub(Location,pattern = c("Fishergate, ", "Honor Oak Park, ", "Supernite, ",
                                        "Fallowfield, ", ", Fishergate", "Fishergate - "),
                   replacement = c("", "", "", "", "", "")))


## May 2021

tubes_data_may_21 = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = "May 2021")

# Deal with NA values
tubes_data_may_21 = tubes_data_may_21 |> filter(!is.na(`Sample Number`))

# TASK 1: Filter to Traditional Shelter
tubes_data_may_21 = tubes_data_may_21 |> filter(Type == "Traditional Shelter")

# TASK 2: Average of the multiple replicate tubes per location
tubes_data_may_21 = tubes_data_may_21 |> group_by(Location) |> summarise(avg_masurement = mean(`ugm-3`)) 

# TASK 3: Convert average concentration into ppb by dividing by 1.9125
tubes_data_may_21 = tubes_data_may_21 |> mutate(avg_masurement = avg_masurement/1.9125)

# TASK 4: Calculate the average LCS concentration over the same time-period
# I will also add a date column and modify the names in the location column
tubes_data_may_21 = tubes_data_may_21 |> add_column(month = as.yearmon("May 2021"))
tubes_data_may_21 = tubes_data_may_21 |> mutate(
  Location = mgsub(Location,pattern = c("Fishergate, ", "Honor Oak Park, ", "Supernite, ",
                                        "Fallowfield, ", ", Fishergate", "Fishergate - "),
                   replacement = c("", "", "", "", "", "")))


# June 2021

tubes_data_jun_21 = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = "June 2021")

# Deal with NA values
tubes_data_jun_21 = tubes_data_jun_21 |> filter(!is.na(`Sample Number`))

# TASK 1: Filter to Traditional Shelter
tubes_data_jun_21 = tubes_data_jun_21 |> filter(Type == "Traditional Shelter")

# TASK 2: Average of the multiple replicate tubes per location
tubes_data_jun_21 = tubes_data_jun_21 |> group_by(Location) |> summarise(avg_masurement = mean(`ugm-3`)) 

# TASK 3: Convert average concentration into ppb by dividing by 1.9125
tubes_data_jun_21 = tubes_data_jun_21 |> mutate(avg_masurement = avg_masurement/1.9125)

# TASK 4: Calculate the average LCS concentration over the same time-period
# I will also add a date column and modify the names in the location column
tubes_data_jun_21 = tubes_data_jun_21 |> add_column(month = as.yearmon("June 2021"))
tubes_data_jun_21 = tubes_data_jun_21 |> mutate(
  Location = mgsub(Location,pattern = c("Fishergate, ", "Honor Oak Park, ", "Supernite, ",
                                        "Fallowfield, ", ", Fishergate", "Fishergate - "),
                   replacement = c("", "", "", "", "", "")))


# July 2021

tubes_data_jul_21 = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = "July 2021")

# Deal with NA values
tubes_data_jul_21 = tubes_data_jul_21 |> filter(!is.na(`Sample Number`))

# TASK 1: Filter to Traditional Shelter
tubes_data_jul_21 = tubes_data_jul_21 |> filter(Type == "Traditional Shelter")

# TASK 2: Average of the multiple replicate tubes per location
tubes_data_jul_21 = tubes_data_jul_21 |> group_by(Location) |> summarise(avg_masurement = mean(`ugm-3`)) 

# TASK 3: Convert average concentration into ppb by dividing by 1.9125
tubes_data_jul_21 = tubes_data_jul_21 |> mutate(avg_masurement = avg_masurement/1.9125)

# TASK 4: Calculate the average LCS concentration over the same time-period
# I will also add a date column and modify the names in the location column
tubes_data_jul_21 = tubes_data_jul_21 |> add_column(month = as.yearmon("July 2021"))
tubes_data_jul_21 = tubes_data_jul_21 |> mutate(
  Location = mgsub(Location, pattern = c("Fishergate, ", "Honor Oak Park", "Supernite, ",
                                         "Manchester (SH)", ", Fishergate", "Fishergate - "),
                   replacement = c("", "L", "", "Manchester", "", "")))


# August 2021 

tubes_data_aug_21 = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = "August 2021")

# Deal with NA values
tubes_data_aug_21 = tubes_data_aug_21 |> filter(!is.na(`Sample Number`))

# TASK 1: Filter to Traditional Shelter
tubes_data_aug_21 = tubes_data_aug_21 |> filter(Type == "Traditional Shelter")

# TASK 2: Average of the multiple replicate tubes per location
tubes_data_aug_21 = tubes_data_aug_21 |> group_by(Location) |> summarise(avg_masurement = mean(`ugm-3`)) 

# TASK 3: Convert average concentration into ppb by dividing by 1.9125
tubes_data_aug_21 = tubes_data_aug_21 |> mutate(avg_masurement = avg_masurement/1.9125)

# TASK 4: Calculate the average LCS concentration over the same time-period
# I will also add a date column and modify the names in the location column
tubes_data_aug_21 = tubes_data_aug_21 |> add_column(month = as.yearmon("August 2021"))
tubes_data_aug_21 = tubes_data_aug_21 |> mutate(
  Location = mgsub(Location,pattern = c("Fishergate, ", "Honor Oak Park", "Manchester (SH)",
                                        "Fallowfield, ", ", Fishergate", "Fishergate - "),
                   replacement = c("", "London", "Manchester", "", "", "")))


# September 2021 

tubes_data_sep_21 = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = "September 2021")

# Deal with NA values
tubes_data_sep_21 = tubes_data_sep_21 |> filter(!is.na(`Sample Number`))

# TASK 1: Filter to Traditional Shelter
tubes_data_sep_21 = tubes_data_sep_21 |> filter(Type == "Traditional Shelter")

# TASK 2: Average of the multiple replicate tubes per location
tubes_data_sep_21 = tubes_data_sep_21 |> group_by(Location) |> summarise(avg_masurement = mean(`ugm-3`)) 

# TASK 3: Convert average concentration into ppb by dividing by 1.9125
tubes_data_sep_21 = tubes_data_sep_21 |> mutate(avg_masurement = avg_masurement/1.9125)

# TASK 4: Calculate the average LCS concentration over the same time-period
# I will also add a date column and modify the names in the location column
tubes_data_sep_21 = tubes_data_sep_21 |> add_column(month = as.yearmon("September 2021"))
tubes_data_sep_21 = tubes_data_sep_21 |> mutate(
  Location = mgsub(Location,pattern = c("Fishergate, ", "Honor Oak Park", "Manchester (SH)",
                                        "Fallowfield, ", ", Fishergate", "Fishergate - "),
                   replacement = c("", "London", "Manchester", "", "", "")))


# October 2021 

tubes_data_oct_21 = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = "October 2021")

# Deal with NA values
tubes_data_oct_21 = tubes_data_oct_21 |> filter(!is.na(`Sample Number`))

# TASK 1: Filter to Traditional Shelter
tubes_data_oct_21 = tubes_data_oct_21 |> filter(Type == "Traditional Shelter")

# TASK 2: Average of the multiple replicate tubes per location
tubes_data_oct_21 = tubes_data_oct_21 |> group_by(Location) |> summarise(avg_masurement = mean(`ugm-3`)) 

# TASK 3: Convert average concentration into ppb by dividing by 1.9125
tubes_data_oct_21 = tubes_data_oct_21 |> mutate(avg_masurement = avg_masurement/1.9125)

# TASK 4: Calculate the average LCS concentration over the same time-period
# I will also add a date column and modify the names in the location column
tubes_data_oct_21 = tubes_data_oct_21 |> add_column(month = as.yearmon("October 2021"))
tubes_data_oct_21 = tubes_data_oct_21 |> mutate(
  Location = mgsub(Location,pattern = c("Fishergate, ", "Honor Oak Park", "Manchester (SH)",
                                        "Fallowfield, ", ", Fishergate", "Fishergate - "),
                   replacement = c("", "London", "Manchester", "", "", "")))


## November 2021

tubes_data_nov_21 = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = "November 2021")

# Deal with NA values
tubes_data_nov_21 = tubes_data_nov_21 |> filter(!is.na(`Sample Number`))

# TASK 1: Filter to Traditional Shelter
tubes_data_nov_21 = tubes_data_nov_21 |> filter(Type == "Traditional Shelter")

# TASK 2: Average of the multiple replicate tubes per location
tubes_data_nov_21 = tubes_data_nov_21 |> group_by(Location) |> summarise(avg_masurement = mean(`ugm-3`)) 

# TASK 3: Convert average concentration into ppb by dividing by 1.9125
tubes_data_nov_21 = tubes_data_nov_21 |> mutate(avg_masurement = avg_masurement/1.9125)

# TASK 4: Calculate the average LCS concentration over the same time-period
# I will also add a date column and modify the names in the location column
tubes_data_nov_21 = tubes_data_nov_21 |> add_column(month = as.yearmon("November 2021"))
tubes_data_nov_21 = tubes_data_nov_21 |> mutate(
  Location = mgsub(Location,pattern = c("Fishergate, ", "Honor Oak Park", "Manchester (SH)",
                                        "Fallowfield, ", ", Fishergate", "Fishergate - "),
                   replacement = c("", "London", "Manchester", "", "", "")))


## December 2021

tubes_data_dec_21 = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = "December 2021")

# Deal with NA values
tubes_data_dec_21 = tubes_data_dec_21 |> filter(!is.na(`Sample Number`))

# TASK 1: Filter to Traditional Shelter
tubes_data_dec_21 = tubes_data_dec_21 |> filter(Type == "Traditional Shelter")

# TASK 2: Average of the multiple replicate tubes per location
tubes_data_dec_21 = tubes_data_dec_21 |> group_by(Location) |> summarise(avg_masurement = mean(`ugm-3`)) 

# TASK 3: Convert average concentration into ppb by dividing by 1.9125
tubes_data_dec_21 = tubes_data_dec_21 |> mutate(avg_masurement = avg_masurement/1.9125)

# TASK 4: Calculate the average LCS concentration over the same time-period
# I will also add a date column and modify the names in the location column
# Adding NA values for Manchester and York
df = data.frame(Location = c("Manchester", "York"), avg_masurement = c(NA, NA))
tubes_data_dec_21 = tubes_data_dec_21 |> rows_insert(df)
tubes_data_dec_21 = tubes_data_dec_21 |> add_column(month = as.yearmon("December 2021"))
tubes_data_dec_21 = tubes_data_dec_21 |> mutate(
  Location = mgsub(Location,pattern = c("Fishergate, ", "Honor Oak Park", "Manchester (SH)",
                                        "Fallowfield, ", ", Fishergate", "Fishergate - "),
                   replacement = c("", "London", "Manchester", "", "", "")))


## January 2022

tubes_data_jan_22 = data.frame(Location = c("London", "Manchester", "York"),
                               avg_masurement = c(NA, NA, NA))  |> tibble()
tubes_data_jan_22 = tubes_data_jan_22 |> add_column(month = as.yearmon("January 2022"))


## February 2022

tubes_data_feb_22 = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = "February 2022")

# Deal with NA values
tubes_data_feb_22 = tubes_data_feb_22 |> filter(!is.na(`Sample Number`))

# TASK 1: Filter to Traditional Shelter
tubes_data_feb_22 = tubes_data_feb_22 |> filter(Type == "Traditional Shelter")

# TASK 2: Average of the multiple replicate tubes per location
tubes_data_feb_22 = tubes_data_feb_22 |> group_by(Location) |> summarise(avg_masurement = mean(`ugm-3`)) 

# TASK 3: Convert average concentration into ppb by dividing by 1.9125
tubes_data_feb_22 = tubes_data_feb_22 |> mutate(avg_masurement = avg_masurement/1.9125)

# TASK 4: Calculate the average LCS concentration over the same time-period
# I will also add a date column and modify the names in the location column
# Adding NA values for Manchester and York
df = data.frame(Location = c("Manchester", "York"), avg_masurement = c(NA, NA))
tubes_data_feb_22 = tubes_data_feb_22 |> rows_insert(df)
tubes_data_feb_22 = tubes_data_feb_22 |> add_column(month = as.yearmon("February 2022"))
tubes_data_feb_22 = tubes_data_feb_22 |> mutate(
  Location = mgsub(Location,pattern = c("Fishergate, ", "Honor Oak Park", "Manchester (SH)",
                                        "Fallowfield, ", ", Fishergate", "Fishergate - "),
                   replacement = c("", "London", "Manchester", "", "", "")))




## March 2022

tubes_data_mar_22 = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = "March 2022")

# Deal with NA values
tubes_data_mar_22 = tubes_data_mar_22 |> filter(!is.na(`Sample Number`))

# TASK 1: Filter to Traditional Shelter
tubes_data_mar_22 = tubes_data_mar_22 |> filter(Type == "Traditional Shelter")

# TASK 2: Average of the multiple replicate tubes per location
tubes_data_mar_22 = tubes_data_mar_22 |> group_by(Location) |> summarise(avg_masurement = mean(`ugm-3`)) 

# TASK 3: Convert average concentration into ppb by dividing by 1.9125
tubes_data_mar_22 = tubes_data_mar_22 |> mutate(avg_masurement = avg_masurement/1.9125)

# TASK 4: Calculate the average LCS concentration over the same time-period
# I will also add a date column and modify the names in the location column
# Adding NA values for Manchester and York
df = data.frame(Location = c("Manchester", "York"), avg_masurement = c(NA, NA))
tubes_data_mar_22 = tubes_data_mar_22 |> rows_insert(df)
tubes_data_mar_22 = tubes_data_mar_22 |> add_column(month = as.yearmon("March 2022"))
tubes_data_mar_22 = tubes_data_mar_22 |> mutate(
  Location = mgsub(Location,pattern = c("Fishergate, ", "Honor Oak Park", "Manchester (SH)",
                                        "Fallowfield, ", ", Fishergate", "Fishergate - "),
                   replacement = c("", "London", "Manchester", "", "", "")))




## Combining all the diffusion tubes data 
tubes_data_list = list(tubes_data_nov_20, tubes_data_dec_20, tubes_data_jan_21, 
                       tubes_data_feb_21, tubes_data_mar_21, tubes_data_apr_21,
                       tubes_data_may_21, tubes_data_jun_21, tubes_data_jul_21,
                       tubes_data_aug_21, tubes_data_sep_21, tubes_data_oct_21,
                       tubes_data_nov_21, tubes_data_dec_21, tubes_data_jan_22,
                       tubes_data_feb_22, tubes_data_mar_22)
tubes_data = tubes_data_list |> reduce(full_join, by = c("Location", "avg_masurement", 
                                                         "month"))

#####

#------ sectopn 2 -----



# Attempt 1: iteratively building (failure)
#####
loop_data = tibble()
for (s in sheets) {
  temp = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = s)
  temp = temp |> add_column(month = as.yearmon(s))
  loop_data = bind_rows(loop_data, temp)
}
#####


# Attempt 2: populating list
#####
loop_list <- list()
for (i in 1:length(sheets)) {
  temp = read_excel("NO2_Diffusion_Tubes.xlsx", sheet = sheets[i]) %>%
            add_column(month = as.yearmon(sheets[i]))
  loop_data = temp |> 
                filter(!is.na(`Sample Number`)) |> 
                filter(Type == "Traditional Shelter") |>
                mutate(
                  Location = mgsub(Location,
                                   pattern = c("Fishergate, ", "Honor Oak Park", "Manchester (SH)",
                                          "Fallowfield, ", ", Fishergate", "Fishergate - ", "Honor Oak Park, "),
                                   replacement = c("", "London", "Manchester", "", "", "", ""))) |>
                group_by(Location, month) |> 
                summarise(avg_masurement = mean(`ugm-3`)) |> 
                mutate(avg_masurement = avg_masurement/1.9125) |>
                ungroup()
  loop_list[[i]] <- loop_data
}
big_df <- bind_rows(loop_list)
#####


# Attempt 3: functions
read_sheet <- function(sheet_name) {
  read_excel("NO2_Diffusion_Tubes.xlsx", sheet = sheet_name) %>%
    add_column(month = as.yearmon(sheet_name)) |>
    filter(!is.na(`Sample Number`)) |> 
    filter(Type == "Traditional Shelter") |>
    mutate(
      Location = mgsub(Location,
                       pattern = c("Fishergate, ", "Honor Oak Park", "Manchester (SH)", "Supernite, ",
                                   "Fallowfield, ", ", Fishergate", "Fishergate - ", "Honor Oak Park, "),
                       replacement = c("", "London", "Manchester", "", "", "", "", ""))) |>
    group_by(Location, month) |> 
    summarise(Tubes = mean(`ugm-3`)) |> 
    mutate(Tubes = Tubes/1.9125) |>
    ungroup()
}
loop_data_1 <- bind_rows(lapply(sheets, read_sheet))  # base R
loop_data_2 <- bind_rows(map(sheets, read_sheet, .progress = TRUE))  # purrr style


# Monthly averages

zep_monthly = zep_data_new |> mutate(month = as.yearmon(date_time)) |>
  group_by(month, instrument, company, Location) |> 
  summarise(ZEP = median(ZEP)) |> 
  ungroup()

aqm_monthly = aqm_data_no2 |> mutate(month = as.yearmon(date_time)) |>
  group_by(month, instrument, company, Location) |> 
  summarise(AQM = median(AQM))|> 
  ungroup()

ref_essentials = ref_essentials |> mutate(month = as.yearmon(date_time))
ref_monthly = ref_essentials |> group_by(month, Location) |> 
  summarise(REF = mean(REF)) |>
  ungroup()


### Putting the monthly averages and loop data together and calculating offset
# Putting AQM and ZEP together and using pivot longer on it
lcs = full_join(filter(zep_monthly,
                       between(month, as.yearmon("November 2020"), as.yearmon("March 2022"))),
                filter(aqm_monthly,
                       between(month, as.yearmon("November 2020"), as.yearmon("March 2022"))),
                by = c("month", "Location", "company", "instrument")) |> 
  pivot_longer(c("AQM", "ZEP"), 
               names_to = NULL,
               values_to = "LCS") |>
  filter(!is.na(LCS))
# Uploading loop_data_2
data_lcs = list(loop_data_2,
                lcs,
                ref_monthly)
loop_data_2 = data_lcs |> reduce(left_join,
                                  by = c("month", "Location")) |> 
  tibble() |> 
  mutate(Offset = Tubes - LCS)   # Calculating offset


### Calculating Tubes and REF RMSE
tubes_accuracy = loop_data_2 |> group_by(Location) |> summarise(Tubes_RMSE = rmse(Tubes, REF)) |> ungroup()



### Calculating LCS and REF RMSE after the calibration
# Joining AQM and ZEP hourly data 
zep_data_new = zep_data_new |> mutate(month = as.yearmon(date_time))
aqm_data_no2 = aqm_data_no2 |> mutate(month = as.yearmon(date_time)) 
hourly_lcs = full_join(filter(select(zep_data_new, -date), 
                              between(month, as.yearmon("November 2020"), as.yearmon("March 2022"))),
                       filter(select(aqm_data_no2, -day, -date), 
                              between(month, as.yearmon("November 2020"), as.yearmon("March 2022"))),
                       by = c("date_time", "Location", "company", "instrument", "month")) |>
  pivot_longer(c("AQM", "ZEP"), 
               names_to = NULL,
               values_to = "LCS") |>
  filter(!is.na(LCS)) 
# Creation of a data frame with the lagged offsets
lagged_offsets = loop_data_2 |> 
  group_by(month, Location, instrument, Offset) |> 
  summarise() |>
  mutate(month = as.yearmon(1/12 + as.numeric(month)))
# Combining lagged data with hourly data
hourly_lcs = full_join(hourly_lcs,
                       lagged_offsets, 
                       by = c("month", "Location", "instrument")) |>
  mutate(NEW_LCS = LCS + Offset) 

hourly_lcs = left_join(hourly_lcs, ref_essentials, by = c("date_time", "month", "Location"))
  
lcs_statistics = hourly_lcs |>
  filter(!is.na(NEW_LCS)) |> 
  group_by(Location, instrument) |> 
  summarise(OLD_RMSE = rmse(REF, LCS),
            NEW_RMSE = rmse(REF, NEW_LCS))
  

### AQM locations comparisons plot

aqm_comparisons = hourly_lcs |>
  filter(company == "AQMesh", !is.na(NEW_LCS)) |>
  group_by(month, Location) |>
  summarise(LCS = mean(LCS), NEW_LCS = mean(NEW_LCS), REF = mean(REF, na.rm = TRUE)) |>
  pivot_longer(c("LCS", "NEW_LCS", "REF"), names_to = "Type", values_to = "Measurements")

aqm_locations = ggplot(aqm_comparisons,
                       mapping = aes(month, Measurements, color = Type, group = Type)) +
  geom_line() + scale_color_brewer(palette="Dark2") +
  facet_wrap(~Location, nrow = 3) + 
  #ggtitle("EVOLUTION OF THE INSTRUMENTS RMSE WITH TIME") + 
  #xlab("Time in months") + ylab("RMSE (ppb)") + labs(colour = "COMPANY") + 
  theme_fivethirtyeight()
  

### ZEP locations comparisons plot

zep_comparisons = hourly_lcs |>
  filter(company == "Zephyr", !is.na(NEW_LCS)) |>
  group_by(month, Location) |>
  summarise(LCS = mean(LCS), NEW_LCS = mean(NEW_LCS), REF = mean(REF, na.rm = TRUE)) |>
  pivot_longer(c("LCS", "NEW_LCS", "REF"), names_to = "Type", values_to = "Measurements")

zep_locations = ggplot(zep_comparisons,
                       mapping = aes(month, Measurements, color = Type, group = Type)) +
  geom_line() + scale_color_brewer(palette="Dark2") +
  facet_wrap(~Location, nrow = 3) + 
  #ggtitle("EVOLUTION OF THE INSTRUMENTS RMSE WITH TIME") + 
  #xlab("Time in months") + ylab("RMSE (ppb)") + labs(colour = "COMPANY") + 
  theme_fivethirtyeight()


### OLD AND NEW RMSE with time AQM plot
new_rmse_aqm = ggplot(hourly_lcs |>
                    filter(company == "AQMesh", !is.na(NEW_LCS)) |>
                    group_by(month, Location) |>
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
                  mapping = aes(month, RMSE, color = Location, group = Location)) +
  geom_line() + scale_color_brewer(palette="Set1") +
  facet_wrap(~rmse_type) + 
  #ggtitle("EVOLUTION OF THE INSTRUMENTS RMSE WITH TIME") + 
  #xlab("Time in months") + ylab("RMSE (ppb)") + labs(colour = "COMPANY") + 
  theme_fivethirtyeight()


### OLD AND NEW RMSE with time ZEP plot
new_rmse_zep = ggplot(hourly_lcs |>
                    filter(company == "Zephyr", !is.na(NEW_LCS)) |>
                    group_by(month, Location) |>
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
                  mapping = aes(month, RMSE, color = Location, group = Location)) +
  geom_line() + scale_color_brewer(palette="Set1") +
  facet_wrap(~rmse_type) + 
  #ggtitle("EVOLUTION OF THE INSTRUMENTS RMSE WITH TIME") + 
  #xlab("Time in months") + ylab("RMSE (ppb)") + labs(colour = "COMPANY") + 
  theme_fivethirtyeight()


### Tubes vs REF with time plot
tubes_vs_ref = ggplot(loop_data_2 |> 
                        pivot_longer(c("Tubes", "REF"), 
                                     names_to = "Type", 
                                     values_to = "Measurements"),
                      mapping = aes(month, Measurements, color = Type, group = Type)) +
  geom_line() + scale_color_brewer(palette="Set1") +
  facet_wrap(~Location, nrow = 3) + 
  #ggtitle("EVOLUTION OF THE INSTRUMENTS RMSE WITH TIME") + 
  #xlab("Time in months") + ylab("RMSE (ppb)") + labs(colour = "COMPANY") + 
  theme_fivethirtyeight()
#####
## Tubes and LCS data in York
# PART1: AQM391 measurements
tubes_data_york_aqm = left_join(tubes_data |> filter(Location == "York"), 
                               aqm_monthly |> filter(instrument == "AQM391", location == "York") |> 
                                 group_by(month, AQM) |> summarise(), 
                               by = "month")
tubes_data_york_aqm = tubes_data_york_aqm |> rename(Tubes = avg_masurement)
tubes_data_york_aqm = tubes_data_york_aqm |> mutate(offset_value = Tubes - AQM)
tubes_data_york_aqm = tubes_data_york_aqm |> mutate(offset_value = lag(offset_value))
tubes_data_york_aqm = tubes_data_york_aqm |> mutate(NEW_AQM = AQM + offset_value) 


# PART2: ZEP309 measurements
tubes_data_york_zep = left_join(tubes_data |> filter(Location == "York"), 
                               zep_monthly |> filter(instrument == "Zep309", location == "York") |> 
                                 group_by(month, ZEP) |> summarise(), 
                               by = "month")
tubes_data_york_zep = tubes_data_york_zep |> rename(Tubes = avg_masurement)
tubes_data_york_zep = tubes_data_york_zep |> mutate(offset_value = Tubes - ZEP)
tubes_data_york_zep = tubes_data_york_zep |> mutate(offset_value = lag(offset_value))
tubes_data_york_zep = tubes_data_york_zep |> mutate(NEW_ZEP = ZEP + offset_value) 

# PART3: ZEP AND AQM together
tubes_data_york = left_join(select(tubes_data_york_aqm, month, Tubes, AQM, NEW_AQM),
                                  select(tubes_data_york_zep, month, Tubes, ZEP, NEW_ZEP),
                                  by = c("month", "Tubes")) 
tubes_data_york = tubes_data_york |> pivot_longer(c("ZEP", "AQM"), 
                                                              names_to = "Instrument", 
                                                              values_to = "LCS")
tubes_data_york = tubes_data_york |> pivot_longer(c("NEW_ZEP", "NEW_AQM"), 
                                                              names_to = NULL, 
                                                              values_to = "NEW_LCS")
tubes_data_york = tubes_data_york |> filter(!is.na(NEW_LCS))



## Tubes and LCS data in Manchester
# PART1: AQM388 measurements
tubes_data_man_aqm = left_join(tubes_data |> filter(Location == "Manchester"), 
                            aqm_monthly |> filter(instrument == "AQM388", location == "Manchester") |> 
                              group_by(month, AQM) |> summarise(), 
                            by = "month")
tubes_data_man_aqm = tubes_data_man_aqm |> rename(Tubes = avg_masurement)
tubes_data_man_aqm = tubes_data_man_aqm |> mutate(offset_value = Tubes - AQM)
tubes_data_man_aqm = tubes_data_man_aqm |> mutate(offset_value = lag(offset_value))
tubes_data_man_aqm = tubes_data_man_aqm |> mutate(NEW_AQM = AQM + offset_value) 


# PART2: ZEP344 measurements
tubes_data_man_zep = left_join(tubes_data |> filter(Location == "Manchester"), 
                            zep_monthly |> filter(instrument == "Zep344", location == "Manchester") |> 
                              group_by(month, ZEP) |> summarise(), 
                            by = "month")
tubes_data_man_zep = tubes_data_man_zep |> rename(Tubes = avg_masurement)
tubes_data_man_zep = tubes_data_man_zep |> mutate(offset_value = Tubes - ZEP)
tubes_data_man_zep = tubes_data_man_zep |> mutate(offset_value = lag(offset_value))
tubes_data_man_zep = tubes_data_man_zep |> mutate(NEW_ZEP = ZEP + offset_value) 

# PART3: ZEP AND AQM together
tubes_data_manchester = left_join(select(tubes_data_man_aqm, month, Tubes, NEW_AQM),
                                  select(tubes_data_man_zep, month, Tubes, NEW_ZEP),
                                  by = c("month", "Tubes")) 
tubes_data_manchester = tubes_data_manchester |> rename(ZEP = NEW_ZEP)
tubes_data_manchester = tubes_data_manchester |> rename(AQM = NEW_AQM)
tubes_data_manchester = tubes_data_manchester |> pivot_longer(c("ZEP", "AQM"), 
                                                              names_to = "Instrument", 
                                                              values_to = "NEW_LCS")
tubes_data_manchester = tubes_data_manchester |> filter(!is.na(NEW_LCS))



## Calculate the RMSE/R2 per month for the corrected LCS
# Joining ref_monthly with the new LCS values
# YORK
comb_tubes_york = left_join(tubes_data_york, filter(ref_monthly, location == "York"), 
                            by = "month")
york_accuracy = comb_tubes_york |>  
  summarise(RMSE_NEW_LCS = rmse(NEW_LCS, REF), RMSE_TUBES = rmse(Tubes, REF)) 
# MANCHESTER
comb_tubes_manchester = left_join(tubes_data_manchester, filter(ref_monthly, location == "Manchester"), 
                                  by = "month")
manchester_accuracy = comb_tubes_manchester |>
  summarise(RMSE_NEW_LCS = rmse(NEW_LCS, REF), RMSE_TUBES = rmse(Tubes, REF)) 
#####
