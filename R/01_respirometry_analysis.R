### code for extracting and plotting respirometry data



# load pacakges -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(broom)
library(plotrix)

# read in data ------------------------------------------------------------

well_setup_apr30 <- read_csv("data-raw/respirometry/respirometer-well-setup-april-30-2017.csv")
well_setup <- read_csv("data-raw/respirometry/respirometer-well-setup-algal-evolution.csv")

resp_data_raw_apr30 <- read_excel("data-raw/respirometry/scen_photo_resp_20C_april30_Oxygen.xlsx", skip = 8)
resp_data_raw_may01 <- read_excel("data-raw/respirometry/scen_photo_resp_10C_may_1_2017_Oxygen.xlsx", skip = 8)
resp_data_raw_may02 <- read_excel("data-raw/respirometry/scen_photo_resp_16C_may2_2017_oxygen_Oxygen.xlsx", skip = 8)
resp_data_raw_may03 <- read_excel("data-raw/respirometry/scen_photo_resp_12C_may03_2017_oxygen_Oxygen.xlsx", skip = 8)
resp_data_raw_may04 <- read_excel("data-raw/respirometry/scen_photo_resp_24C_may04_2017_oxygen_Oxygen.xlsx", skip = 8)
resp_data_raw_may05 <- read_excel("data-raw/respirometry/scen_photo_resp_12C_may05_2017_oxygen_Oxygen.xlsx", skip = 8)


all_resp_data <- bind_rows(resp_data_raw_apr30, resp_data_raw_may01, resp_data_raw_may02, resp_data_raw_may03, resp_data_raw_may04, resp_data_raw_may05)


# tidy up the data --------------------------------------------------------

resp_data_20 <- resp_data_raw_apr30 %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(date_time)) %>% 
	select(-c2)

resp_data_10 <- resp_data_raw_may01 %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(date_time)) %>% 
	select(-c2)

resp_data_16 <- resp_data_raw_may02 %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(date_time)) %>% 
	select(-c2)

resp_data_12 <- resp_data_raw_may03 %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(date_time)) %>% 
	select(-c2)

resp_data_24 <- resp_data_raw_may04 %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(date_time)) %>% 
	select(-c2)

resp_data_12b <- resp_data_raw_may05 %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(date_time)) %>% 
	select(-c2)

## get the resp data together with the well data

##12b
resp_data_12blong <- resp_data_12b %>% 
	gather(key = well_id, value = oxygen, a1:d6) %>% 
	select(date_time, time_min, t_internal_c, well_id, oxygen)

wells_12b <- well_setup %>% 
	filter(date == "May 05 2017")

resp_data_wells_12b <- left_join(resp_data_12blong, wells_12b, by = "well_id")


##16
resp_data_16long <- resp_data_16 %>% 
	gather(key = well_id, value = oxygen, a1:d6) %>% 
	select(date_time, time_min, t_internal_c, well_id, oxygen)

wells_16 <- well_setup %>% 
	filter(date == "May 02 2017")

resp_data_wells_16 <- left_join(resp_data_16long, wells_16, by = "well_id")


##24
resp_data_24long <- resp_data_24 %>% 
	gather(key = well_id, value = oxygen, a1:d6) %>% 
	select(date_time, time_min, t_internal_c, well_id, oxygen)

wells_24 <- well_setup %>% 
	filter(date == "May 04 2017")

resp_data_wells_24 <- left_join(resp_data_24long, wells_24, by = "well_id")

##10
resp_data_10long <- resp_data_10 %>% 
	gather(key = well_id, value = oxygen, a1:d6) %>% 
	select(date_time, time_min, t_internal_c, well_id, oxygen)

wells_10 <- well_setup %>% 
	filter(date == "May 01 2017")

resp_data_wells_10 <- left_join(resp_data_10long, wells_10, by = "well_id")


# 20 ----------------------------------------------------------------------

##20
resp_data_20long <- resp_data_20 %>% 
	gather(key = well_id, value = oxygen, a1:d6) %>% 
	select(date_time, time_min, t_internal_c, well_id, oxygen)

wells_20 <- well_setup %>% 
	filter(date == "April 30 2017")

resp_data_wells_20 <- left_join(resp_data_20long, wells_20, by = "well_id")

resp_data_wells_20 %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

resp_data_wells_20 %>% 
	filter(time_min > 250) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

## for 20, looks like we can use 200-250 for photosynthesis and 275 to 325 for respiration


# 12 ----------------------------------------------------------------------

##12
resp_data_12long <- resp_data_12 %>% 
	gather(key = well_id, value = oxygen, a1:d6) %>% 
	select(date_time, time_min, t_internal_c, well_id, oxygen)

wells_12 <- well_setup %>% 
	filter(date == "May 03 2017")

resp_data_wells_12 <- left_join(resp_data_12long, wells_12, by = "well_id")

resp_data_wells_12 %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

resp_data_wells_12 %>% 
	filter(time_min > 400) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()


## for 12, looks like we can use 150-300 for photosynthesis and 450-550 for respiration



# make some plots ---------------------------------------------------------

## 1. check out temperature changes (we want to find the time period that corresponds to the most stable temperature)


# 12b ---------------------------------------------------------------------

resp_data_wells_12b %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

resp_data_wells_12b %>% 
	filter(time_min > 350) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

## for 12b, looks like we can use 160-300 for photosynthesis and 350 to 430 for respiration


# 16 ----------------------------------------------------------------------

resp_data_wells_16 %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

resp_data_wells_16$notes[[1]]

resp_data_wells_16 %>% 
	filter(time_min > 100) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

## for 16, looks like we can use 160-300 for photosynthesis and 350 to 430 for respiration


# 24 ----------------------------------------------------------------------


resp_data_wells_24 %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

resp_data_wells_24$notes[[1]]

resp_data_wells_24 %>% 
	filter(time_min > 450) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

## for 24, looks like we can use 200-400 for photosynthesis and 450 to 600 for respiration


# 10 ----------------------------------------------------------------------

resp_data_wells_10 %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

resp_data_wells_10$notes[[1]]

resp_data_wells_10 %>% 
	filter(time_min > 300) %>% 
	filter(time_min < 400) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

## for 10, looks like we can use 100-250 for photosynthesis and 300 to 400 for respiration





### ok so it looks like we can use from time 150 to time 244 for photosynthesis, b/c stable temps
## and 260 to 325 for respiration

## plot the respiration portion of the curve
resp_data_wells %>% 
	filter(time_min > 260) %>% 
	ggplot(aes(x = time_min, y = oxygen)) + geom_point() + 
	facet_wrap( ~ well_id)

## plot the photosynthesis portion of the curve
resp_data_wells %>% 
	filter(time_min > 150 & time_min < 244) %>% 
	ggplot(aes(x = time_min, y = oxygen)) + geom_point() + 
	facet_wrap( ~ well_id)

## plot both fluxes together

resp_data_wells %>% 
	filter(time_min > 150) %>% 
	mutate(metabolic_pathway = ifelse(time_min > 150 & time_min < 244, "photosynthesis", NA)) %>% 
	mutate(metabolic_pathway = ifelse(time_min > 260 , "respiration", metabolic_pathway)) %>% 
	filter(!is.na(metabolic_pathway)) %>%
	ggplot(aes(x = time_min, y = oxygen, color = metabolic_pathway)) + geom_point() + 
	facet_wrap( ~ well_id) + theme_bw() + ylab("oxygen concentration (mg/L)") + xlab("time (minutes)")



# calculate control slopes --------------------------------------------------------

control_slope_photo_20C <- resp_data_wells %>%
	filter(grepl("BLANK", treatment)) %>% 
	filter(time_min > 150 & time_min < 244) %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_photo = mean(estimate)) 

control_slope_photo_20C[[1]]


control_slope_resp_20C <- resp_data_wells %>%
	filter(grepl("BLANK", treatment)) %>% 
	filter(time_min > 260 & time_min < 325) %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_resp = mean(estimate)) 

control_slope_resp_20C[[1]]



# calculate phytoplankton slopes ------------------------------------------


photosynthesis20 <-  resp_data_wells %>%
	filter(time_min > 150 & time_min < 244) %>% 
	filter(!grepl("BLANK", treatment)) %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_photosynthesis_slope = estimate - control_slope_photo_20C[[1]]) %>% 
	mutate(temperature = "20")


respiration20 <- resp_data_wells %>%
	filter(!grepl("BLANK", treatment)) %>% 
	filter(time_min > 260 & time_min < 325) %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_respiration_slope = estimate - control_slope_resp_20C[[1]]) %>% 
	mutate(temperature = "20")


all_data_20 <- full_join(photosynthesis20, respiration20, by = "well_id") %>% 
	mutate(gross_photosynthesis = corrected_photosynthesis_slope + (-1*corrected_respiration_slope)) 


data_20 <- left_join(all_data_20, well_setup, by = "well_id")

# plot the fluxes ---------------------------------------------------------

data_20_groups <- data_20 %>% 
	mutate(group = ifelse(treatment %in% c("1", "2", "3", "4", "5", "6", "7", "8"), "group1", "group2")) %>% 
	mutate(group = ifelse(treatment %in% c("STOCK_1", "STOCK_2"), "stock", group)) 


data_20_groups %>% 
	ggplot(aes(x = treatment, y = corrected_respiration_slope, color = group)) + geom_point()

data_20_groups %>% 
	ggplot(aes(x = treatment, y = gross_photosynthesis, color = group)) + geom_point()


## how does the average photosynthesis slope look across treatments?
### note these aren't mass normalized yet, so I wouldn't interpret them too much!

data_20_groups %>% 
	group_by(group) %>% 
	summarise_each(funs(mean, std.error), corrected_respiration_slope, gross_photosynthesis) %>% 
	ggplot(aes(x = group, y = gross_photosynthesis_mean)) + geom_point() +
	geom_errorbar(aes(ymin = gross_photosynthesis_mean - gross_photosynthesis_std.error, ymax = gross_photosynthesis_mean + gross_photosynthesis_std.error), width = 0.1) +
	ylab("photosynthesis rate (mg O2/L/hr)")


## how does the average respiration slope look across treatments?
data_20_groups %>% 
	group_by(group) %>% 
	summarise_each(funs(mean, std.error), corrected_respiration_slope, gross_photosynthesis) %>%
	ggplot(aes(x = group, y = corrected_respiration_slope_mean)) + geom_point() +
	geom_errorbar(aes(ymin = corrected_respiration_slope_mean - corrected_respiration_slope_std.error, ymax = corrected_respiration_slope_mean + corrected_respiration_slope_std.error), width = 0.1) +
	ylab("respiration rate (mg O2/L/hr)")


