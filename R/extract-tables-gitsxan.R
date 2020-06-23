##pull the cv info from the Bulkley File

{
  library(tidyverse)
  library(readwritesqlite)
  library(data.table)
  library(sf)
  library(readxl)
  library(tabulizer)
}

##-------now for the Gitxsanreport

##we could not digitize the tables from the pdf so Alicia from Gitsxan forwarded a bunch of files including the raw excel file!

##this is the path to the 2015 data
path <- "C:/Users/allan/OneDrive/New_Graph/Current/2019-023_Bulkley_fish_passage/data/FHRI 2015 Priority Fish Passage Barrier Reporting/Copy of Final_List of High priority FP sites_May 21_2015_MM.xls"

gitxsan_reporting_all_2015 <-  path %>% 
  readxl::excel_sheets() %>% 
  purrr::set_names() %>% 
  purrr::map(read_excel, 
             path = path, 
             .name_repair = janitor::make_clean_names) 

gitxsan_summary_2015 <- gitxsan_reporting_all_2015 %>% 
  pluck(1)
  # sf::st_as_sf(coords = c("easting", "northing"), ##hashed out for now
  #              crs = 26909, remove = F)


##burn to a geopackage for now
# sf::st_write(gitxsan_summary_2015, "C:/Users/allan/OneDrive/New_Graph/Current/GIS/projects/2019-011_SERNbc_capacity/layers/bulkley.gpkg", "gitsxan_2015")

##I used this to delete the layer because I did not like the name (was just gitsxan before)
# library(RSQLite)
# db = SQLite()
# con = dbConnect(db,"C:/Users/allan/OneDrive/New_Graph/Current/GIS/projects/2019-011_SERNbc_capacity/layers/bulkley.gpkg")
# dbRemoveTable(con, "gitsxan")

st_layers("C:/Users/allan/OneDrive/New_Graph/Current/GIS/projects/2019-011_SERNbc_capacity/layers/bulkley.gpkg")


###-----------------2016
##this is the path to the 2016 data
path <- "C:/Users/allan/OneDrive/New_Graph/Current/2019-023_Bulkley_fish_passage/data/GWA Priority Site List_May 13, 2016.xlsx"

gitxsan_reporting_all_2016 <-  path %>% 
  readxl::excel_sheets() %>% 
  purrr::set_names() %>% 
  purrr::map(read_excel, 
             path = path, 
             .name_repair = janitor::make_clean_names) 
  # purrr::set_names(janitor::make_clean_names(names(.))) %>% ##results are better without these steps
  # purrr::map(at_trim_xlsheet2) %>% 
  # purrr::map(plyr::colwise(type.convert))


##lets see which columns differ between the spreadsheets
##diff between sheet 2 and sheet 1
setdiff(names(gitxsan_reporting_all_2016 %>% pluck(1)),names(gitxsan_summary_2015)) ##a few to add

setdiff(names(gitxsan_reporting_all_2016 %>% pluck(3)),names(gitxsan_reporting_all_2016 %>% pluck(1))) ##only road_tenure" but is a duplicate of tenure_holder

##see diff between 2015 and 2016
setdiff(names(gitxsan_reporting_all_2016 %>% pluck(2)),names(gitxsan_reporting_all_2016 %>% pluck(1)))

##we want to bind all the tables from 2016 besides the ones that start with S_ , McD and FISS.  Then we add just the columns we don't have to the 2015 file and that is our spatial layer.

gitxsan_reporting_2016_join <- gitxsan_reporting_all_2016[1:8] %>% ##grab the first 8 sheets
  purrr::list_modify('TBD 16_17' = NULL) %>%  ##remove this sheet for now as it would need to have the est_habitat_gain_m column to join, that is proving to be a pain and looks like has no new info
  map_dfr(., ~{
    .x %>% 
      mutate(easting = as.integer(easting),
             northing = as.integer(northing),
             map_ref_number = as.integer(map_ref_number),
             est_habitat_gain_m = as.numeric(est_habitat_gain_m))
  })

##join the 2015 and the 2016 data together but only keep new columns from 2016
col_2016 <- setdiff(names(gitxsan_reporting_all_2016 %>% pluck(1)),names(gitxsan_summary_2015)) ##a few columns to add

gitxan_joined <- left_join(
  gitxsan_summary_2015,
  select(gitxsan_reporting_2016_join, map_ref_number, all_of(col_2016)),
  by = 'map_ref_number'
)


##turn the table into an sf object 
gitxsan_summary <- gitxan_joined %>% 
  sf::st_as_sf(coords = c("easting", "northing"), 
                         crs = 26909, remove = F)
  

##burn to a geopackage for now
##burn to a geopackage for now
sf::st_write(gitxsan_summary, "gis/bulkley_fish_passage_background.gpkg", "gitxsan")

##we need to join to the pscis data then to the fish habitat model ouputs - leave for now - enough for today

