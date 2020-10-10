{
  library(tidyverse)
  library(data.table)
  library(sf)
  library(readxl)
}




##this is the path to the 2018 data
path <- "data/bulkley_Attachment2_20190424.xlsx"

search_list <-  path %>% 
  readxl::excel_sheets() 

search_list <- search_list[1:4]


irvine_2018 <-  search_list  %>% 
  purrr::set_names() %>% 
  purrr::map(read_excel, 
             path = path, 
             .name_repair = janitor::make_clean_names) %>% 
  bind_rows(.id = 'priority') %>% 
  mutate(priority = stringr::str_replace_all(priority, '_priority', '')) %>% 
  select(-stream_name) %>% 
  st_as_sf(coords = c('easting_9u','northing_9u'), remove = F, crs = 26909)

##burn to a geopackage for now
sf::st_write(irvine_2018, "data/gis/bulkley_fish_passage_background.gpkg", "irvine_2018", delete_layer = T)


#burn version for simon