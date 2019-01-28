library(zoo)
library(tidyverse)
library(googlesheets)
library(reshape2)
library(magrittr)
library(roll)
library(lubridate)

###Function to alter googlesheets download
gs_transform_func2 <- function(x) {
  x %>%
    mutate(
      Date = as.Date(as.POSIXct(Timestamp, format="%m/%d/%Y %H:%M:%S",tz=Sys.timezone()))
    ) %>%
    select(11, 2:10) %>%
    mutate_at(vars(4,5,8), funs(as.numeric(str_extract(., "[0-9]+")))) %>%
    group_by(Name) %>%
    mutate(
      Bodyweight_z =roll_scale(matrix(`Bodyweight (Kg)`), 30),
      Mood_z =roll_scale(matrix(Mood), 30),
      LBS_z =roll_scale(matrix(`Lower body soreness`), 30),
      UBS_z =roll_scale(matrix(`Upper body soreness`), 30),
      Sleep_z =roll_scale(matrix(`Sleep Quality`), 30),
      Fatigue_z =roll_scale(matrix(`Fatigue Levels`), 30),
      Health_z =roll_scale(matrix(Health), 30)
    ) %>% ungroup()
}


###Allow access
gs_auth(new_user = TRUE) ###ONLY NEEDS TO BE RUN ONCE

###View sheets in googlesheets account
sheets <- gs_ls()
glimpse(sheets)

###Create auth key --- CHANGE 2 AS NEEDED
key <- sheets$sheet_key[2]

###Allowing permanent access to wellness sheet without need to confirm privacy 
Wellness <- gs_key(key) %>%
  gs_read(.) %>%
  gs_transform_func2(.)

