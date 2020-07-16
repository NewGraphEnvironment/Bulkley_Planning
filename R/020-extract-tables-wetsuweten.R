##pull the cv info from the Bulkley File

{
  library(tidyverse)
  library(readwritesqlite)
  library(data.table)
  library(sf)
  # library(readxl)
  library(tabulizer)
}

path <- "data/Assessing Barriers to fish passage within the WFN traditional territory 2018.pdf"


##define the area to extract table from for first page

#you would run with this the first time
tab_trim_17 <- tabulizer::locate_areas(path, 17)

##since we have done this before though - numbers below are results
# top      left    bottom     right 
# 108.89680  75.11744 536.49822 719.06050 
tab_trim_17 = list(c(108.89680,  75.11744, 536.49822, 719.06050 ))


##define the area to extract table from the rest of the even # pages
tab_trim_18 <- tabulizer::locate_areas(path, 18)


# top      left    bottom     right 
# 100.18505  73.66548 528.51246 720.51246 
tab_trim_18  = list(c(100.18505,  73.66548, 528.51246, 720.51246 ))


##define the area to extract table from the rest of the odd # pages
tab_trim_19 <- tabulizer::locate_areas(path, 19)  ##for some reason the last row is shifted over one

# top      left    bottom     right 
# 100.85650  74.90583 534.47085 716.40807  
tab_trim_19  = list(c(100.85650, 74.90583, 534.47085, 716.40807))

##extract the tables useing the areas you defined
table_17 <- tabulizer::extract_tables(path,
                                       pages = 17:17,
                                       method = "lattice",
                                       area = tab_trim_17) %>% 
  set_names(17)

table_18 <- tabulizer::extract_tables(path,
                                        pages = seq(18,18),
                                        method = "lattice",
                                        area = tab_trim_18) %>% 
  set_names(18) ##should do this as input from "pages" part of the function

table_19 <- tabulizer::extract_tables(path,
                                       pages = seq(19,19),
                                       method = "lattice",
                                       area = tab_trim_19) %>% 
  set_names(19)

# rm(tab_trim_17, tab_trim_18, tab_trim_19)

##look at them as lists of tibbles to see inconsistencies
##we want to be sure that there is the right number of columns = 12
test_view <- table_17 %>% 
  map(as_tibble)

test_view <- table_18 %>% 
  map(as_tibble)

test_view <- table_19 %>% 
  map(as_tibble)

# ##this is how we make a clean dataframe
# table_17_df <- table_17 %>% 
#   pluck(1) %>% 
#   as_tibble() %>% 
#   janitor::row_to_names(1) %>% 
#   janitor::clean_names() 

#turn above into function and do it for all 3 pages then bind_rows  
make_table <- function(lis){
  lis %>% 
    pluck(1) %>% 
    as_tibble() %>% 
    janitor::row_to_names(1) %>% 
    janitor::clean_names()
}

##we need to do table_19 different b/c of that one shifted row
list <- list(table_17, table_18)

table <- list %>% 
  map_df(make_table) %>% 
  rename(map_id = x) %>% ##lets give the crs in a column
  mutate(crs = case_when(zone %like% '9' ~ 26909,
                         T ~ 26910))

table_19_df <- table_19 %>% 
  pluck(1) %>% 
  as_tibble() %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()

##shift last row over one 
nc  <- ncol(table_19_df)
table_19_df[10,2:nc] <- table_19_df[10,1:(nc-1)]

table_19_df <-  table_19_df %>% 
  rename(map_id = x) %>% 
  mutate(crs = case_when(zone %like% '9' ~ 26909,
                         T ~ 26910),
         map_id = case_when(map_id %ilike% 'none' ~'29',
                            TRUE ~ map_id))
  
table <- bind_rows(table, table_19_df)

##turn the table into an sf object and reassign the UTms  -  a bit tricky because the last stream is utm zone 10 and the rest are 9
table_sf <- sf::st_as_sf(table, coords = c("easting", "northing"), 
                         crs = 26909, remove = F) %>% 
  st_transform(crs = 4326) %>% 
  mutate(longitude = st_coordinates(.)[,1],
         latitude = st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  st_as_sf(., coords = c("easting", "northing"), ##do it again for the utm 10
           crs = 26910, remove = F) %>% 
  st_transform(crs = 4326) %>% 
  mutate(longitude2 = st_coordinates(.)[,1],
         latitude2 = st_coordinates(.)[,2],
         longitude = case_when(crs == 26910 ~ longitude2, ##this is for the 10U case
                               T ~ longitude),
         latitude = case_when(crs == 26910 ~ latitude2,
                               T ~ latitude)) %>% 
  st_drop_geometry() %>%
  sf::st_as_sf(., coords = c("longitude", "latitude"), 
               crs = 4326, remove = F) %>% 
  select(-longitude2, -latitude2)
  

##burn to a geopackage for now
sf::st_write(table_sf, "gis/bulkley_fish_passage_background.gpkg", "wetsuweten")

