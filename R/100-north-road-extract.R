##extract the data from the 2009 triton work done on north rd for Canfor

library(tidyverse)
library(readxl)
library(sf)


##needed to convert from xls to xlsx with excel 
path = "./data/north_road_fish_passage_2009_SM09.xlsx"


# path <- "./data/bulkley_Attachment2_20190424.xlsx"

search_list <-  path %>% 
  readxl::excel_sheets() 

##function to trim up sheet and get names (was previously source from altools package)
at_trim_xlsheet2 <- function(df, column_last = ncol(df)) {
  df %>%
    dplyr::select(1:column_last) %>% ##get rid of the extra columns.  should be more abstract
    janitor::row_to_names(which.max(complete.cases(.))) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(., which = "rows")
}


fish_data_submission = path %>% 
  readxl::excel_sheets() %>% 
  purrr::set_names() %>% 
  purrr::map(read_excel, 
             path = path, 
             .name_repair = janitor::make_clean_names) %>% 
  purrr::set_names(janitor::make_clean_names(names(.))) %>% 
  purrr::map(at_trim_xlsheet2) %>% #moved to functions from https://github.com/NewGraphEnvironment/altools to reduce dependencies
  purrr::map(plyr::colwise(type.convert))

# fish_data_submission <- path %>% 
#   readxl::excel_sheets() %>% 
#   purrr::set_names() %>% 
#   purrr::map(read_excel, 
#              path = path, 
#              .name_repair = janitor::make_clean_names) %>% 
#   purrr::set_names(janitor::make_clean_names(names(.))) %>% 
#   purrr::map(at_trim_xlsheet2)


site_location_data <- fish_data_submission %>% 
  purrr::pluck("step_1_ref_and_loc_info") %>% 
  dplyr::filter(!is.na(site_number)) %>% 
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(date_yyyy_mm_day)))

##we need to join the site data with the fish info

fish_coll_data <- fish_data_submission %>% 
  purrr::pluck("step_2_fish_coll_data")

fish_sampling_data <- left_join(site_location_data,
                                select(fish_coll_data, reference_number,species_code,total_number, comments),
                                by = c('reference_number')) %>% 
  st_as_sf(coords = c('utm_easting','utm_northing'), remove = F, crs = 26909)
  
##burn to th geopackage for now
sf::st_write(fish_sampling_data, "data/gis/bulkley_fish_passage_background.gpkg", "north_road_2009", delete_layer = T)
