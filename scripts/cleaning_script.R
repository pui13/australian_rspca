# Packages ----------

library(tidyverse)
library(janitor)
library(lubridate)
library(zoo)

# Read in data and clean names ------

animals <- read_csv("raw_data/animal_outcomes.csv") %>%
  clean_names()
brisbane <- read_csv("raw_data/brisbane_complaints.csv") %>%
  clean_names()
townsville <- read_csv("raw_data/animal_complaints.csv") %>%
  clean_names()

# For Townsville dataset - renaming all the "Unallocated" electoral divisions to the correct one -----

div_1 <- c("Mount Louisa", "Bohle Plains", "Cosgrove", "Toolakea", "Jensen", "Rangewood", "Shaw", "Alice River", "Balgal Beach", "Black River", "Bluewater", "Saunders Beach", "Rollingstone"  )

div_2 <- c("Bushland Beach", "Deeragun", "Mount Low", "Beach Holm"  )

div_3 <- c("Townsville City", "Rowes Bay", "Burdell", "Town Common", "Horseshoe Bay", "North Ward", "South Townsville", "Nelly Bay", "Belgian Gardens", "Castle Hill", "West Point")

div_4 <- c("Condon", "Kelso", "Rasmussen")

div_5 <- c("Kirwan", "Thuringowa Central")

div_6 <- c("Douglas", "Annandale")

div_7 <- c("Cranbrook", "Heatley")

div_8 <- c("Gulliver", "Currajong", "Mundingburra", "Garbutt", "Vincent", "Aitkenvale"  )

div_9 <- c("Hermit Park", "Hyde Park", "Pimlico", "Mysterton", "Rosslea", "West End"  )

div_10 <- c("Railway Estate", "Stuart", "Oonoonba", "Cluden", "Cungulla", "Roseneath", "Wulguru", "Mt Elliot","Oak Valley", "Nome")


# For brisbane dataframe, adding an area variable for future binding with townsville data

inner <- c(
  "bowen hills", "brisbane city", "east brisbane", "fortitude valley",
  "herston", "highgate hill", "kangaroo point", "kelvin grove", "new farm",
  "newstead", "paddington", "petrie terrace", "red hill", "south brisbane",
  "spring hill", "teneriffe", "west end", "woolloongabba", "enoggera"
  )

northern <- c(
  "albion", "alderley", "ascot", "aspley", "bald hills", "banyo", "boondall",
  "bracken ridge", "bridgeman downs", "brighton", "brisbane airport", 
  "carseldine", "chermside", "chermside west", "clayfield", "deagon", 
  "eagle farm", "everton park", "ferny grove", "fitzgibbon", "gaythorne",
  "geebung", "gordon park", "grange", "hamilton", "hendra", "kalinga", "kedron",
  "keperra", "lutwyche", "mcdowall", "mitchelton", "myrtletown", "newmarket", 
  "northgate", "nudgee", "nudgee beach", "nundah", "pinkenba", "sandgate", 
  "shorncliffe", "stafford", "stafford heights", "taigum", "virginia", 
  "wavell heights", "wilston", "windsor", "wooloowin", "zillmere")

southern <- c(
  "acacia ridge", "algester", "annerley", "archerfield", "burbank", "calamvale",
  "coopers plains", "darra", "doolandella", "drewvale", "durack", "dutton park",
  "eight mile plains", "ellen grove", "fairfield", "forest lake", "greenslopes",
  "heathwood", "holland park", "holland park west", "inala", "karawatha",
  "kuraby", "larapinta", "macgregor", "mackenzie", "mansfield", "moorooka", 
  "mount gravatt", "mount gravatt east", "nathan", "pallara", "parkinson", 
  "richlands", "robertson", "rochedale", "rocklea", "runcorn", "salisbury", 
  "seventeen mile rocks", "sinnamon park", "stones corner", "stretton", 
  "sumner", "sunnybank", "sunnybank hills", "tarragindi", "tennyson", 
  "upper mount gravatt", "wacol", "willawong", "wishart", "yeerongpilly", "yeronga"
  )

eastern <- c(
  "balmoral", "belmont", "bulimba", "camp hill", "cannon hill", "carina", 
  "carina heights", "carindale", "chandler", "coorparoo", "gumdale", "hawthorne",
  "hemmant", "lota", "lytton", "manly", "manly west", "moreton island",
  "morningside", "murarrie", "norman park", "port of brisbane", "ransome", 
  "seven hills", "tingalpa", "wakerley", "wynnum", "wynnum west", "bulwer", 
  "kooringal")

western <- c(
  "anstead", "ashgrove", "auchenflower", "bardon", "bellbowrie", "brookfield", 
  "chapel hill", "chelmer", "chuwar", "corinda", "england creek– enoggera", 
  "enoggera reservoir", "fig tree pocket", "graceville", "indooroopilly", 
  "jamboree heights", "jindalee", "karana downs", "kenmore", "kenmore hills", 
  "kholo", "lake manchester", "middle park", "milton", "moggill", "mount coot-tha",
  "mount crosby", "mount ommaney", "oxley", "pinjarra hills", "pullenvale", 
  "riverhills", "seventeen mile rocks", "sherwood", "sinnamon park", "st lucia",
  "taringa", "the gap", "toowong", "upper brookfield", "upper kedron", "westlake"
  )

# Clean animals data ------
## Region mutate is for a fault I noticed in the dataset, values have been swapped

animals_clean <- animals %>%
  rename(
    "Australian Capital Territory" = act,
    "New South Wales" = nsw,
    "Northern Territory" = nt,
    "Queensland" = qld,
    "South Australia" = sa,
    "Tasmania" = tas,
    "Victoria" = vic,
    "Western Australia" = wa
  ) %>%
  mutate(total = ifelse(is.na(total),
                        select(.,(4:11)) %>%
                          rowSums(na.rm = T),
                        total
          ),
        outcome = case_when(
          outcome == "In Stock" ~ "Currently In Care",
          T ~ outcome
        )
  ) %>%
  pivot_longer(c(4:11),
               names_to = "region",
               values_to = "region_total") %>%
  mutate(region = case_when(
    year > 2015 & region == "New South Wales" ~ "Northern Territory",
    year > 2015 & region == "Northern Territory" ~ "New South Wales",
    T ~ region
  )) %>%
  select(-total)

# Clean Townsville data ----

townsville_clean <- townsville %>%
  mutate(date_range = my(date_received),
         city = "Townsville",
         electoral_division = case_when(
           electoral_division == "Unallocated" & suburb %in% div_1 ~ "Division 1",
           electoral_division == "Unallocated" & suburb %in% div_2 ~ "Division 2",
           electoral_division == "Unallocated" & suburb %in% div_3 ~ "Division 3",
           electoral_division == "Unallocated" & suburb %in% div_4 ~ "Division 4",
           electoral_division == "Unallocated" & suburb %in% div_5 ~ "Division 5",
           electoral_division == "Unallocated" & suburb %in% div_6 ~ "Division 6",
           electoral_division == "Unallocated" & suburb %in% div_7 ~ "Division 7",
           electoral_division == "Unallocated" & suburb %in% div_8 ~ "Division 8",
           electoral_division == "Unallocated" & suburb %in% div_9 ~ "Division 9",
           electoral_division == "Unallocated" & suburb %in% div_10 ~ "Division 10",
           T ~ electoral_division),
         animal_type = case_when(
           animal_type == "cat" ~ "Cat",
           animal_type == "dog" ~ "Dog"
         )) %>%
  rename(area = electoral_division,
         category = complaint_type) %>%
  select(-date_received)
  
# clean brisbane ------

brisbane_clean <- brisbane %>%
  select(-c(responsible_office, nature)) %>%
  mutate(date_range = ymd(case_when(
    date_range == "1st-quarter-2016-17.csv" ~ "2016-01-01",
    date_range == "april-june-2016.csv" ~ "2016-04-01",
    date_range == "apr-jun-2019.csv" ~ "2019-04-01",
    date_range == "apr-to-jun-2018.csv" ~ "2018-04-01",                                             
    date_range == "april-to-june-2017.csv" ~ "2017-04-01",                          
    date_range == "jan-mar-2019.csv" ~ "2019-01-01",         
    date_range == "jan-to-mar-2018.csv" ~ "2018-01-01",                                          
    date_range == "january-to-march-2017.csv" ~ "2017-01-01",                                       
    date_range == "jul-to-sep-2018.csv" ~ "2018-07-01",                                             
    date_range == "jul-to-sep-2019.csv" ~ "2019-07-01",                                            
    date_range == "july-to-september-2017.csv" ~ "2017-07-01",                                     
    date_range == "oct-to-dec-2018.csv" ~ "2018-10-01",                                            
    date_range == "october-to-december-2016.csv" ~ "2016-10-01",                                 
    date_range == "october-to-december-2017.csv" ~ "2017-10-01",                     
    date_range == "cars-srsa-open-data-animal-related-complaints-apr-to-jun-2020.csv" ~ "2020-04-01",
    date_range == "cars-srsa-open-data-animal-related-complaints-jan-to-mar-2020.csv" ~ "2020-01-01",
    date_range == "cars-srsa-open-data-animal-related-complaints-oct-to-dec-2019.csv" ~ "2019-10-01"
  )
  ),
  suburb = tolower(suburb),
  area = case_when(
    suburb %in% inner ~ "Central Brisbane",
    suburb %in% northern ~ "North Brisbane",
    suburb %in% southern ~ "South Brisbane",
    suburb %in% eastern ~ "East Brisbane",
    suburb %in% western ~ "West Brisbane",
    T ~ NA_character_
  ),
  category = case_when(
    animal_type == "Cat Trapping" ~ "Trapping",
    T ~ category
  ),
  animal_type = case_when(
    animal_type == "Cat Trapping" ~ "Cat",
    animal_type == "Attack" ~ NA_character_,
    T ~ animal_type
  )
  ) 

# bind brisbane and townsville together ------

bind_townsville_brisbane <- bind_rows(brisbane_clean, townsville_clean) %>%
  mutate(qtr = as.yearqtr(date_range, format = "%Y-%m-%d"))

# save cleaned data

animals_clean %>%
  write_csv("clean_data/animals_clean.csv")

brisbane_clean %>%
  write_csv("clean_data/brisbane.csv")

townsville_clean %>%
  write_csv("clean_data/townsville.csv")

bind_townsville_brisbane %>%
  write_csv("clean_data/bind_townsville_brisbane.csv")