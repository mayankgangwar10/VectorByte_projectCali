library(tidyverse)
library(lubridate)

mosq <- read.csv('California NW Data.csv') %>%
  mutate(mo_yr = format(as.Date(sample_end_date), '%m/%y'))

print(unique(mosq$species)) # some species are duplicated (if genus listed in some cases but not all)
length(unique(mosq$species)) # check how many species are in dataset

dedup <- mosq %>% # deduplicating species of interest
  mutate(
    species = if_else(
      species == "tarsalis", 
      str_replace(species, "tarsalis", 'Culex tarsalis'),
      species),
    species = if_else(
      species == "aegypti", 
      str_replace(species, "aegypti", 'Aedes aegypti'),
      species),
    species = if_else(
      species == "quinquefasciatus", 
      str_replace(species, "quinquefasciatus", 'Culex quinquefasciatus'),
      species)
  )
length(unique(dedup$species)) # check how many species are in deduplicated dataset 

# aggregate to month by species, noting number of sample days per month
mosq_agg <- dedup %>%
  group_by(species, mo_yr) %>%
  summarise(indiv_sampled = sum(sample_value),
            ndays_sampled = n()
            ) %>%
  mutate(mo_yr = my(mo_yr)) # creates a dummy day

# when filtering by species, how to deal with dates being dropped? 
# also think about how many trap locations there were 
# look at culex tarsalis, culex quincfascicatus, aedes aegypti abundance over time
c_tarsalis <- mosq_agg %>% 
  filter(species == 'Culex tarsalis') 
ggplot(data = c_tarsalis) + 
  geom_line(aes(x = mo_yr, y = indiv_sampled)) + 
  labs(title = 'Monthly counts of Culex tarsalis', x = 'Year', y = 'Individuals sampled') + 
  theme_bw()

c_quinque <- mosq_agg %>% 
  filter(species == 'Culex quinquefasciatus') 
ggplot(data = c_quinque) + 
  geom_point(aes(x = mo_yr, y = indiv_sampled)) + 
  labs(title = 'Monthly counts of Culex quinquefasciatus', x = 'Year', y = 'Individuals sampled') + 
  theme_bw()

ae_aegypti <- mosq_agg %>% 
  filter(species == 'Aedes aegypti') 
ggplot(data = ae_aegypti) + 
  geom_point(aes(x = mo_yr, y = indiv_sampled)) + 
  labs(title = 'Monthly counts of Aedes aegypti', x = 'Year', y = 'Individuals sampled') + 
  theme_bw()

ggplot(data = mosq_agg) + 
  geom_point(aes(x = mo_yr, y = indiv_sampled)) + 
  labs(title = 'Monthly counts of all mosquito species', x = 'Year', y = 'Individuals sampled') + 
  theme_bw()

spp_of_interest <- c('Aedes aegypti', 'Culex quinquefasciatus', 'Culex tarsalis')
vect <- mosq_agg %>%
  filter(species %in% spp_of_interest)

ggplot() + 
  geom_line(data = ae_aegypti, aes(x = mo_yr, y = indiv_sampled), color='blue') +
  geom_line(data = c_tarsalis, aes(x = mo_yr, y = indiv_sampled), color='black') +
  geom_line(data = c_quinque, aes(x = mo_yr, y = indiv_sampled), color='red') +
  labs(title = 'Monthly counts', x = 'Year', y = 'Individuals sampled') + 
  theme_bw()

# check autocorrelation
acf(ae_aegypti$indiv_sampled)
acf(c_tarsalis$indiv_sampled)
acf(c_quinque$indiv_sampled)

# Build a data set with the sample data and the lag-1 response
autoDat <- data.frame(time = seq(1, length(c_tarsalis$indiv_sampled),1),
                      indiv = as.vector(c_tarsalis$indiv_sampled),
                      lag_indiv = lag(as.vector(c_tarsalis$indiv_sampled),1))

# Remove the NA value that is introduced
autoDat <- autoDat %>% na.omit(lag_indiv)

# Calculate the autocorrelation
corr <- round(cor(autoDat$indiv, autoDat$lag_indiv), 2)

# Plot the relationship between deaths this month and last month
ggplot(data = autoDat, aes(x = indiv, y = lag_indiv))+
  geom_point()+
  annotate("text",
           x = 1000, y = 30000, label = paste("corr = ", corr ),
           color = "red")

# time series with temp and precip vars also aggregated to month