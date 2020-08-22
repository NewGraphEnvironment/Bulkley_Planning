library(DBI)
library(tidyverse)
library(sf)
library(RPostgres)
library(data.table)

# db connection
conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = 'postgis',
  host = 'localhost',
  port = 5432,
  user = 'postgres',
  password = 'postgres'
)

##list tables in a schema  
dbGetQuery(conn,
           "SELECT table_name 
           FROM information_schema.tables 
           WHERE table_schema='fish_passage'")

##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type 
           FROM information_schema.columns 
           WHERE table_name='pscis_model_combined'")

dbListTables(conn)

st_layers("data/gis/fish_passage_skeena_20200714.gpkg")

##bring in the data
df <- sf::st_read(
  "data/gis/fish_passage_skeena_20200714.gpkg",
  layer = "xings_bulk_morr_wet") 


# ##make your sf object and change crs
# df_gpx <- df %>% 
#   mutate(long = st_coordinates(.)[,1],
#          lat = st_coordinates(.)[,2])



##make a gpx file
df %>% 
  mutate(name = paste0(pscis_stream_crossing_id, ' PSCIS_mod_comb-',pscis_model_combined_id,
                      ' modelled-', model_crossing_id), 
         desc = paste(dbm_mof_50k_grid_map_tile, map_ref_number_sfc, 'moe-', priority_moe,
                                                 comments_moe, original_report_recommendations_sfc,
                      comments_completion_details_sfc, comments_wetsuweten, sep = " "),
         geometry = geom) %>% 
  select(name, desc,  geometry)  %>% 
  write_sf(dsn = "data/gis/field_sites_20200821.gpx", driver="GPX",
           dataset_options="GPX_USE_EXTENSIONS=yes", delete_dsn = TRUE)


##always disconnect from your database!!!!!!!!!
dbDisconnect(conn)
