##looking to add in the Owen Ck crossings to potential candidates to have a look at. particularly site 2359
##i will also grab the other "high" hab value site for our fish permit appication

source('R/packages.R')
source('R/functions.R')


# db connection
conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = 'postgis',
  host = 'localhost',
  port = 5432,
  user = 'postgres',
  password = 'postgres'
)



##turn it into a spatial object and grab the fh model outputs
df <- import_pscis() %>% 
  sf::st_as_sf(coords = c("easting", "northing"), 
               crs = 26909, remove = F) %>% 
  rename(geom = geometry) %>% 
  sf::st_transform(crs = 3005) ##put in same crs as our pscis_modelled_combined layer

# add a unique id
df$misc_point_id <- seq.int(nrow(df))

# load to database
st_write(obj = df, dsn = conn, Id(schema= "working", table = "misc_points"))

# sf doesn't automagically create a spatial index or a primary key
res <- dbSendQuery(conn, "CREATE INDEX ON working.misc_points USING GIST (geom)")
dbClearResult(res)
res <- dbSendQuery(conn, "ALTER TABLE working.misc_points ADD PRIMARY KEY (misc_point_id)")
dbClearResult(res)

pscis_info <- dbGetQuery(conn, "SELECT
  a.misc_point_id,
  b.*,
  ST_Distance(ST_Transform(a.geom,3005), b.geom) AS distance
FROM
  working.misc_points AS a
CROSS JOIN LATERAL
  (SELECT *
   FROM fish_passage.pscis_model_combined
   ORDER BY
     a.geom <-> geom
   LIMIT 1) AS b")

##join pscis id back to the dataframe
df_joined<- left_join(df,
                      select(pscis_info, -geom, -stream_name),
                      by = c('misc_point_id')) %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>% 
  mutate(downstream_route_measure = as.integer(downstream_route_measure),
         blue_line_key = as.integer(blue_line_key)) 

####--------------watershed area - function is int he functions file
crossing_watersheds <-  get_watershed(fish_habitat_info = df_joined %>% filter(!is.na(blue_line_key)))  ##needed to get rid of the xings with no fh info

df_joined <- left_join(
  df_joined,
  crossing_watersheds %>% 
    st_set_geometry(NULL) %>% select(pscis_model_combined_id, area_ha, refine_method) %>% 
    mutate(pscis_model_combined_id = as.integer(pscis_model_combined_id)),
  by = 'pscis_model_combined_id')
  # st_set_geometry(NULL)


##add the 1:50k watershed code and modify the 1:20k so it has hyphens rather than periods
df_joined <- left_join(
  df_joined,
  select(wsheds,pscis_model_combined_id, watershed_code_50k = watershed_code_50k_parsed),
  by = 'pscis_model_combined_id'
) %>% 
  mutate(watershed_code_20k = stringr::str_replace_all(wscode, '\\.', '-'))


##grab the data ready for the permit application
##bring in the priority info
# read the geopackage
df <- sf::st_read(
  "data/gis/fish_passage_skeena_20200714.gpkg",  ##this path does not work anymore b/c rmd files ref diff
  layer = "xings_bulk_morr_wet") %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>% 
  sf::st_transform(crs = 3005) ##not necessary

##just keep the columns that we have already that are contained in the owen data
col_same <- intersect(names(df), names(df_joined))

df_joined_high <- df_joined %>% 
  select(all_of(col_same), habitat_value) %>% 
  filter(habitat_value %ilike% 'high')

bulk_morr_owen <- list(df,df_joined_high) %>% 
  data.table::rbindlist(idcol="pscis_model_combined_id", fill = T) %>% 
  st_as_sf()

##burn to the geopackage
sf::st_write(bulk_morr_owen, "./data/gis/fish_passage_skeena_20200714.gpkg", "xings_skeena_bulk_high_owen", delete_layer = T)
sf::st_write(select(df_joined, -fid), "./data/gis/fish_passage_skeena_20200714.gpkg", "xings_owen", delete_layer = T)


##grab the watersheds to have a look
crossing_watersheds_joined <- left_join(
  select(crossing_watersheds, -wscode, -localcode),
  df_joined %>% st_set_geometry(NULL) %>% mutate(pscis_model_combined_id = as.character(pscis_model_combined_id)),
  by = 'pscis_model_combined_id')

sf::st_write(select(crossing_watersheds_joined, -fid), "./data/gis/fish_passage_skeena_20200714.gpkg", "watersheds_owen", delete_layer = F)
