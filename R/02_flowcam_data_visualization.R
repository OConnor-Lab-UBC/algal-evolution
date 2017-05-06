### plot data

library(tidyverse)
library(lubridate)

sdata_raw <- read_csv("data-processed/scen_cells.csv")


sdata <- sdata_raw %>% 
	mutate(start_time = ymd_hms(start_time))


sdata %>% 
	filter(replicate != "stock") %>% 
	mutate(temperature = NA) %>% 
	mutate(temperature = ifelse(replicate %in% c("1", "2", "3", "4", "5", "6", "7", "8"), "12C", "18C")) %>% 
ggplot(aes(x = start_time, y = cell_density, color = replicate)) + geom_point(size = 2) + theme_bw() +
	facet_wrap( ~ temperature)
