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
           WHERE table_schema='whse_admin_boundaries'")

##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type 
           FROM information_schema.columns 
           WHERE table_name='fwa_watershed_groups_poly'")

##bring in the priority info
# read the geopackage
df <- sf::st_read(
  "data/gis/fish_passage_skeena_20200714.gpkg",
  layer = "xings_bulk_morr_wet") %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>% 
  sf::st_transform(crs = 3005) ##put in same crs as our moe layer

# add a unique id
df$misc_point_id <- seq.int(nrow(df))

# load to database
st_write(obj = df, dsn = conn, Id(schema= "working", table = "misc_points"))

# sf doesn't automagically create a spatial index or a primary key
res <- dbSendQuery(conn, "CREATE INDEX ON working.misc_points USING GIST (geom)")
dbClearResult(res)
res <- dbSendQuery(conn, "ALTER TABLE working.misc_points ADD PRIMARY KEY (misc_point_id)")
dbClearResult(res)

df_info <- dbGetQuery(conn, "SELECT a.misc_point_id, moe.region_name, wsg.watershed_group_code
FROM working.misc_points a
INNER JOIN whse_admin_boundaries.eadm_wlap_region_bnd_area_svw moe
ON ST_Intersects(a.geom, moe.geom)
                 INNER JOIN whse_basemapping.fwa_watershed_groups_poly wsg
                             ON ST_Intersects(a.geom, wsg.geom)")


##looks like everything is in the Skeena region and bulk or morr which makes sense.
#add the watershed group info because some are missing it (did not match modeled ids)

df <- left_join(
  select(df, -watershed_group_code),
  df_info,
  by = 'misc_point_id'
) 



permit_table <- df %>% 
  mutate(stream_moe = as.character(stream_moe),
         stream_name_sfc = as.character(stream_name_sfc),
         stream_name = as.character(stream_name),
         waterbody = case_when(is.na(stream_moe) ~ stream_name_sfc,
                               T ~ stream_moe), 
         waterbody = case_when(is.na(waterbody) ~ stream_name,
                               T ~ waterbody)) %>% 
  mutate(comments = paste(comments_wetsuweten, comments_completion_details_sfc,
                               comments_moe),
         comments = stringr::str_replace_all(comments, 'NA', '')) %>% 
  select(MoE_region = region_name, waterbody, watershed_code_50k, watershed_code_20k,
         lat, long, uphab_gross_sub15_perc_km = uphab_gross_sub15_km, watershed_area_ha, upstr_species, comments,
         image_view_url, pscis_model_combined_id:map_ref_number_sfc, misc_point_id)
 

# write.table(permit_table, "C:/Users/allan/OneDrive/New_Graph/Current/2019-023_Bulkley_fish_passage/permit/permit_streams.txt", 
#             sep = ",", quote = FALSE, row.names = F)
  
write.xlsx(permit_table, file="permit_details.xlsx", sheetName="sample_streams", append=FALSE, row.names=FALSE)
##this one needs to come from the "01_modelled_Part3.... file
write.xlsx(as.data.frame(fish_watershed), file="permit_details.xlsx", sheetName="potential_species", append=TRUE, row.names=FALSE)


setwd("C:/Users/allan/OneDrive/New_Graph/Current/2019-010_FWCP_Parsnip/permit")

