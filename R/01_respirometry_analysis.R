### code for extracting and plotting respirometry data



# load packages -----------------------------------------------------------

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


scen_cells <- read_csv("data-processed/scen_cells.csv")

## ones with character oxygen: 12b, 16, need to fix these

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


# 10 ----------------------------------------------------------------------

resp_data_10long <- resp_data_10 %>% 
	gather(key = well_id, value = oxygen, a1:d6) %>% 
	select(date_time, time_min, t_internal_c, well_id, oxygen)

wells_10 <- well_setup %>% 
	filter(date == "May 01 2017")

resp_data_wells_10 <- left_join(resp_data_10long, wells_10, by = "well_id")

resp_data_wells_10 %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

resp_data_wells_10$notes[[1]]

resp_data_wells_10 %>% 
	filter(time_min > 300) %>% 
	filter(time_min < 400) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()


View(resp_data_wells_10)
str(resp_data_wells_12)


## for 10, looks like we can use 100-250 for photosynthesis and 300 to 400 for respiration


# 20 ----------------------------------------------------------------------

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

View(resp_data_wells_20)

## for 20, looks like we can use 200-250 for photosynthesis and 275 to 325 for respiration


### OK now bring in the flow cam data

cells_20 <- scen_cells %>% 
	filter(grepl("apr30", file_name)) %>% 
	mutate(replicate = str_replace(replicate, "stock", "STOCK"))

all_20 <- left_join(resp_data_wells_20, cells_20, by = c("treatment" = "replicate"))


# calculate control slopes --------------------------------------------------------

control_slope_photo_20 <- all_20 %>%
	filter(grepl("COMBO", treatment)) %>% 
	filter(time_min > 150 & time_min < 244) %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_photo = mean(estimate)) 

control_slope_photo_20[[1]]


control_slope_resp_20 <- all_20 %>%
	filter(grepl("COMBO", treatment)) %>% 
	filter(time_min > 260 & time_min < 325) %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_resp = mean(estimate)) 

control_slope_resp_20[[1]]

## plot the respiration portion of the curve
all_20 %>% 
	filter(time_min > 260) %>% 
	ggplot(aes(x = time_min, y = oxygen)) + geom_point() + 
	facet_wrap( ~ well_id)

## plot the photosynthesis portion of the curve
all_20 %>% 
	filter(time_min > 150 & time_min < 244) %>% 
	ggplot(aes(x = time_min, y = oxygen)) + geom_point() + 
	facet_wrap( ~ well_id)

## plot both fluxes together

all_20 %>% 
	filter(time_min > 150) %>% 
	mutate(metabolic_pathway = ifelse(time_min > 150 & time_min < 244, "photosynthesis", NA)) %>% 
	mutate(metabolic_pathway = ifelse(time_min > 260 , "respiration", metabolic_pathway)) %>% 
	filter(!is.na(metabolic_pathway)) %>%
	ggplot(aes(x = time_min, y = oxygen, color = metabolic_pathway)) + geom_point() + 
	facet_wrap( ~ well_id) + theme_bw() + ylab("oxygen concentration (mg/L)") + xlab("time (minutes)")

# calculate phytoplankton slopes ------------------------------------------


photosynthesis20 <-  all_20 %>%
	filter(time_min > 150 & time_min < 244) %>% 
	filter(!grepl("COMBO", treatment)) %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_photosynthesis_slope = estimate - control_slope_photo_20[[1]]) %>% 
	mutate(temperature = "20")


respiration20 <- all_20 %>%
	filter(!grepl("COMBO", treatment)) %>% 
	filter(time_min > 260 & time_min < 325) %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_respiration_slope = estimate - control_slope_resp_20[[1]]) %>% 
	mutate(temperature = "20")


all_data_20 <- full_join(photosynthesis20, respiration20, by = "well_id") %>% 
	mutate(gross_photosynthesis = corrected_photosynthesis_slope + (-1*corrected_respiration_slope)) 


data_20 <- left_join(all_data_20, wells_20, by = "well_id")
data_20_percap <- left_join(data_20, cells_20, by = c("treatment" = "replicate"))

data_20_percap1 <- data_20_percap %>% 
	mutate(photosynthesis_percap = gross_photosynthesis/cell_density) %>% 
	mutate(respiration_percap = (-1*corrected_respiration_slope/cell_density))

# plot the fluxes ---------------------------------------------------------

data_20_groups <- data_20_percap1 %>% 
	mutate(group = ifelse(treatment %in% c("1", "2", "3", "4", "5", "6", "7", "8"), "12C", "18C")) %>% 
	mutate(group = ifelse(treatment == "STOCK",  "stock", group)) 

data_20_groups %>% 
	ggplot(aes(x = treatment, y = photosynthesis_percap, color = group)) + geom_point()

data_20_groups %>% 
	ggplot(aes(x = treatment, y = respiration_percap, color = group)) + geom_point()


## how does the average photosynthesis slope look across treatments?
### note these aren't mass normalized yet, so I wouldn't interpret them too much!

data_20_groups %>% 
	group_by(group) %>% 
	summarise_each(funs(mean, std.error), photosynthesis_percap, respiration_percap) %>% 
	ggplot(aes(x = group, y = photosynthesis_percap_mean)) + geom_point() +
	geom_errorbar(aes(ymin = photosynthesis_percap_mean - photosynthesis_percap_std.error, ymax = photosynthesis_percap_mean + photosynthesis_percap_std.error), width = 0.1) +
	ylab("photosynthesis rate (mg O2/L/hr)")


## how does the average respiration slope look across treatments?
data_20_groups %>% 
	group_by(group) %>% 
	summarise_each(funs(mean, std.error), respiration_percap, photosynthesis_percap) %>%
	ggplot(aes(x = group, y = respiration_percap_mean)) + geom_point() +
	geom_errorbar(aes(ymin = respiration_percap_mean - respiration_percap_std.error, ymax = respiration_percap_mean + respiration_percap_std.error), width = 0.1) +
	ylab("respiration rate (mg O2/L/hr)")






# 12 ----------------------------------------------------------------------

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

### OK now bring in the flow cam data

cells_12 <- scen_cells %>% 
	filter(grepl("may3", file_name)) %>% 
	mutate(replicate = str_replace(replicate, "stock", "STOCK"))

all_12 <- left_join(resp_data_wells_12, cells_12, by = c("treatment" = "replicate"))


# calculate control slopes --------------------------------------------------------

control_slope_photo_12 <- all_12 %>%
	filter(grepl("COMBO", treatment)) %>% 
	filter(time_min > 150 & time_min < 300) %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_photo = mean(estimate)) 

control_slope_photo_12[[1]]


control_slope_resp_12 <- all_12 %>%
	filter(grepl("COMBO", treatment)) %>% 
	filter(time_min > 450 & time_min < 550) %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_resp = mean(estimate)) 

control_slope_resp_12[[1]]

## plot the respiration portion of the curve
all_12 %>% 
	filter(time_min > 450) %>% 
	ggplot(aes(x = time_min, y = oxygen)) + geom_point() + 
	facet_wrap( ~ well_id)

## plot the photosynthesis portion of the curve
all_12 %>% 
	filter(time_min > 150 & time_min < 300) %>% 
	ggplot(aes(x = time_min, y = oxygen)) + geom_point() + 
	facet_wrap( ~ well_id)

## plot both fluxes together

all_12 %>% 
	filter(time_min > 150) %>% 
	mutate(metabolic_pathway = ifelse(time_min > 150 & time_min < 300, "photosynthesis", NA)) %>% 
	mutate(metabolic_pathway = ifelse(time_min > 450 , "respiration", metabolic_pathway)) %>% 
	filter(!is.na(metabolic_pathway)) %>%
	ggplot(aes(x = time_min, y = oxygen, color = metabolic_pathway)) + geom_point() + 
	facet_wrap( ~ well_id) + theme_bw() + ylab("oxygen concentration (mg/L)") + xlab("time (minutes)")

# calculate phytoplankton slopes ------------------------------------------


photosynthesis12 <-  all_12 %>%
	filter(time_min > 150 & time_min < 300) %>% 
	filter(!grepl("COMBO", treatment)) %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_photosynthesis_slope = estimate - control_slope_photo_12[[1]]) %>% 
	mutate(temperature = "12")


respiration12 <- all_12 %>%
	filter(!grepl("COMBO", treatment)) %>% 
	filter(time_min > 450 & time_min < 550) %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_respiration_slope = estimate - control_slope_resp_12[[1]]) %>% 
	mutate(temperature = "12")


all_data_12 <- full_join(photosynthesis12, respiration12, by = "well_id") %>% 
	mutate(gross_photosynthesis = corrected_photosynthesis_slope + (-1*corrected_respiration_slope)) 


data_12 <- left_join(all_data_12, wells_12, by = "well_id")
data_12_percap <- left_join(data_12, cells_12, by = c("treatment" = "replicate"))

data_12_percap1 <- data_12_percap %>% 
	mutate(photosynthesis_percap = gross_photosynthesis/cell_density) %>% 
	mutate(respiration_percap = (-1*corrected_respiration_slope/cell_density))

# plot the fluxes ---------------------------------------------------------

data_12_groups <- data_12_percap1 %>% 
	mutate(group = ifelse(treatment %in% c("1", "2", "3", "4", "5", "6", "7", "8"), "12C", "18C")) %>% 
	mutate(group = ifelse(treatment == "STOCK",  "stock", group)) 

data_12_groups %>% 
	ggplot(aes(x = treatment, y = photosynthesis_percap, color = group)) + geom_point()

data_12_groups %>% 
	ggplot(aes(x = treatment, y = respiration_percap, color = group)) + geom_point()


## how does the average photosynthesis slope look across treatments?
### note these aren't mass normalized yet, so I wouldn't interpret them too much!

data_12_groups %>% 
	group_by(group) %>% 
	summarise_each(funs(mean, std.error), photosynthesis_percap, respiration_percap) %>% 
	ggplot(aes(x = group, y = photosynthesis_percap_mean)) + geom_point() +
	geom_errorbar(aes(ymin = photosynthesis_percap_mean - photosynthesis_percap_std.error, ymax = photosynthesis_percap_mean + photosynthesis_percap_std.error), width = 0.1) +
	ylab("photosynthesis rate (mg O2/L/hr)")


## how does the average respiration slope look across treatments?
data_12_groups %>% 
	group_by(group) %>% 
	summarise_each(funs(mean, std.error), respiration_percap, photosynthesis_percap) %>%
	ggplot(aes(x = group, y = respiration_percap_mean)) + geom_point() +
	geom_errorbar(aes(ymin = respiration_percap_mean - respiration_percap_std.error, ymax = respiration_percap_mean + respiration_percap_std.error), width = 0.1) +
	ylab("respiration rate (mg O2/L/hr)")



# 12b ---------------------------------------------------------------------
resp_data_12blong <- resp_data_12b %>% 
	gather(key = well_id, value = oxygen, a1:d6) %>% 
	select(date_time, time_min, t_internal_c, well_id, oxygen)

wells_12b <- well_setup %>% 
	filter(date == "May 05 2017")

resp_data_wells_12b <- left_join(resp_data_12blong, wells_12b, by = "well_id")


resp_data_wells_12b %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

resp_data_wells_12b %>% 
	filter(time_min > 350) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

## for 12b, looks like we can use 160-300 for photosynthesis and 350 to 430 for respiration


# 16 ----------------------------------------------------------------------
## ugh 16 appears to be in the wrong units -- air sat? need to re-export in mg/L

resp_data_16long <- resp_data_16 %>% 
	gather(key = well_id, value = oxygen, a1:d6) %>% 
	select(date_time, time_min, t_internal_c, well_id, oxygen)

wells_16 <- well_setup %>% 
	filter(date == "May 02 2017")

resp_data_wells_16 <- left_join(resp_data_16long, wells_16, by = "well_id")

resp_data_wells_16 %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

resp_data_wells_16$notes[[1]]

resp_data_wells_16 %>% 
	filter(time_min > 100) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

## for 16, looks like we can use 160-300 for photosynthesis and 350 to 430 for respiration


# 24 ----------------------------------------------------------------------
resp_data_24long <- resp_data_24 %>% 
	gather(key = well_id, value = oxygen, a1:d6) %>% 
	select(date_time, time_min, t_internal_c, well_id, oxygen)

wells_24 <- well_setup %>% 
	filter(date == "May 04 2017")

resp_data_wells_24 <- left_join(resp_data_24long, wells_24, by = "well_id")

resp_data_wells_24 %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

resp_data_wells_24$notes[[1]]

resp_data_wells_24 %>% 
	filter(time_min > 450) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_point()

## for 24, looks like we can use 200-400 for photosynthesis and 450 to 600 for respiration










