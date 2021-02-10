library(tidyverse)
library(ggthemes)
library(lubridate)

## Doing the project first for data for LH
# import data

electricity_producer_LH <- read_csv2("data/electricity-producer-59348.csv", 
                                     col_names = c("MaStR", "name", "status", "operation_start", "registration",
                                                   "powersource", "brutto_power", "netto_power", "zip_code", "city",
                                                   "operator", "x1", "x2"), 
                                     col_types = cols(
                                       MaStR = col_character(),
                                       name = col_character(),
                                       status = col_factor(),
                                       operation_start = col_date(format = "%m/%d/%Y"),
                                       registration = col_date(format = "%m/%d/%Y"),
                                       powersource = col_factor(),
                                       brutto_power = col_double(),
                                       netto_power = col_double(),
                                       zip_code = col_factor(),
                                       city = col_factor(),
                                       operator = col_factor(),
                                       x1 = col_character(),
                                       x2 = col_character()),
                                     skip = 1
)

electricity_producer_LH <- electricity_producer_LH %>% select(-x2, -x1)

# tidy data


# transform data

electricity_producer_LH %>% group_by(powersource) %>% summarise(count = n())

electricity_producer_LH %>% filter(powersource == "Solare Strahlungsenergie") %>% arrange(operation_start) %>% 
  mutate(cummulated_power = cumsum(netto_power)) %>% view() 

data_1 <- electricity_producer_LH %>% group_by(powersource) %>% arrange(powersource, operation_start) %>% 
  mutate(cummulated_power = cumsum(netto_power)) %>% filter(powersource %in% c("Solare Strahlungsenergie", "Speicher", "Biomasse", "Wind"))

# visualize data
data_1 %>% filter(powersource %in% c("Solare Strahlungsenergie", "Speicher")) %>% 
  ggplot(aes(x = operation_start, y = cummulated_power, color = powersource)) + geom_line() +
  scale_x_date() + scale_y_log10() + theme_economist()
    
# model data

# communicate data
