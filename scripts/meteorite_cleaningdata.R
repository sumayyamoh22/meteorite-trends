# loading packages
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(naniar)

# loading datasets:
   # NASA Meteorite dataset
# from here: https://catalog.data.gov/dataset/meteorite-landings
nasa_data <- read_csv("C:/Users/sumay/Downloads/Meteorite_Landings.csv")
glimpse(nasa_data)
summary(nasa_data)

   # World cities dataset
# acquired from this link: https://simplemaps.com/data/world-cities
city_country <- read_excel("C:/Users/sumay/Downloads/simplemaps_worldcities_basicv1.77/worldcities.xlsx")
glimpse(city_country)
summary(city_country)


#clean things up and deal with missing info
nasa_data <- nasa_data %>%
  clean_names()

city_country <- city_country %>%
  clean_names() %>%
  select(-capital)

nasa_data <- nasa_data %>%
  filter(!is.na(reclong) & !is.na(reclat) & !is.na(geo_location)) %>%
  filter(reclat >= -90 & reclat <= 90, reclong >= -180 & reclong <= 180) # filters out invalid lats&longs

city_country <- city_country %>%
  filter(!is.na(population) & !is.na(admin_name)) %>%
  filter(lat >= -90 & lat <= 90, lng >= -180 & lng <= 180)

#colSums(is.na(city_country))

#gg_miss_var(city_country)


class(nasa_data$year)
class(nasa_data$mass_g)  #just checking that they are numeric



# merging the lat and lng column in city_country to one geolocation to make join easier:
city_country <- city_country %>%
  mutate(geo_location = paste("(", lat, ", ", lng, ")", sep = "")) %>%
  select(-lat, -lng)
  
 
# experimental join
meteorite_landings <- nasa_data %>%   
  left_join(city_country,by = c("name" = "city_ascii"))    

table(is.na(meteorite_landings$country))

unmatched_names <- meteorite_landings %>%
  filter(is.na(country)) %>%
  distinct(name) %>%
  arrange(name)  

matched_names <- meteorite_landings %>%
  filter(!is.na(country)) %>%
  distinct(name, country) %>%
  arrange(name)
# join failure, need to find another key. not city and name.

# let's try to join by geolocation:
meteorite_landings <- nasa_data %>%   
  left_join(city_country,by = "geo_location")

table(is.na(meteorite_landings$country))

# way too many misses since exact geolocations are very difficult to match.


# lets assign meteorites to countries/regions based on their coordinates

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")

# turn the lat and long into spatial object
meteorites_sf <- st_as_sf(nasa_data, coords = c("reclong", "reclat"), crs = 4326)

# spatial join
meteorites_with_country <- st_join(meteorites_sf, world["name"])

n_distinct(meteorites_with_country) #every result is unique
table(is.na(meteorites_with_country$country))

#lets fix the column names and rename the df
meteorite_landings <- meteorites_with_country %>%
  rename(name = name.x) %>%
  rename(country = name.y)
 # get rid of the nulls in country
meteorite_landings <- meteorite_landings %>%
  filter(!is.na(country)) # obs. decreased from 38400 to 32135

# LETS START ANALYSIS NOW!

# meteorite landings per country
per_country <- meteorite_landings %>%
  st_drop_geometry() %>%
  count(country, sort = TRUE)

per_country %>%
  ggplot(aes(x = country, y = n)) +
  geom_col(fill = "pink",position = "stack") +
  labs(title = "Meteorite Landings Per Country",
       x = "Country", y = "Number of Meteorites")

 # how many fell versus were found in each country?

fall_country <- meteorite_landings %>%
  st_drop_geometry() %>%
  group_by(country, fall) %>%
  summarise(n = n())

# change to wide format
fall_country_wide <- fall_country %>%
  pivot_wider(names_from = fall, values_from = n, values_fill = 0)

# sort most by fell
fell_most <- fall_country_wide %>%
  arrange(desc(Fell))

# sort most by found
found_most <- fall_country_wide %>%
  arrange(desc(Found))



# is there any trend by years?
by_year <- meteorite_landings %>%
  st_drop_geometry() %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) # may make a plot for this in markdown

by_year %>%
  ggplot(aes(x = year, y = count)) +
  geom_col(fill = "navyblue", position = "stack", ) +
  labs( title = "Meteorite Landings Per Year", x = "Year", y = "Number of Meteorites") +
  theme_light()
  




# let's check for trends with the heaviest/most massive landings now and see if there are country trends

heaviest_hits <- meteorite_landings %>%
  st_drop_geometry() %>%
  filter(!is.na(mass_g)) %>%
  group_by(country) %>%
  summarise(avg_mass = mean(mass_g, na.rm = TRUE),
            total = n()) %>%
  arrange(desc(avg_mass)) %>%
  head(10)# hmm doesn't look like any explicit country trend, but first two heaviest are both African countries

#lets work with recclass now, to learn more about TYPES of meteorites

top_types <- meteorite_landings %>%
  st_drop_geometry() %>%
  group_by(recclass) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)
  
# some recclass trends by country
recclass_country <- meteorite_landings %>%
  st_drop_geometry() %>%
  count(country, recclass) %>%
  group_by(country) %>%
  top_n(1, n) %>%    #most frequent type per country
  arrange(desc(n))

#lets visualize it using a SF viz!
map_viz <- ggplot() +
  geom_sf(data = world, fill = "grey80") +
  geom_sf(data = meteorite_landings, color = "blue", size = 0.8, alpha = 0.5) +
  labs(title = "Global Meteorite Landings",
       caption = "Data: NASA Meteorite Landings + Country Borders") +
  theme_minimal()

# is there a connection between the type of meteorite and mass?
mass_by_type <- meteorite_landings %>%
  st_drop_geometry() %>%
  filter(!is.na(mass_g), !is.na(recclass)) %>%
  group_by(recclass) %>%
  summarise(
    count = n(),
    avg_mass = mean(mass_g, na.rm = TRUE),
    median_mass = median(mass_g, na.rm = TRUE),
    max_mass = max(mass_g, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_mass)) #%>%
  #head(10)

