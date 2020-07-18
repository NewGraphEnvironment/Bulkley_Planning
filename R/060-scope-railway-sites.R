##want to see which crossings and wtersheds are on the railway

##https://gist.github.com/smnorris/65c3db70eccfed9cf0d7f2bc10b3d058 - Simon is a massive boss

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

##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type 
           FROM information_schema.columns 
           WHERE table_name='misc_points'")

railway <- dbGetQuery(conn, "
SELECT
  pmc.pscis_model_combined_id,
  x.crossing_id as model_crossing_id,
  pmc.pscis_stream_crossing_id,
  x.track_name,
  x.track_classification,
  x.use_type,
  x.gauge,
  x.status,
  x.operator_english_name,
  x.owner_name,
  x.fish_habitat,
  pmc.uphab_gross_sub15,
  pmc.uphab_gross_sub22,
  pmc.dnstr_crossing_ids,
  pmc.upstr_crossing_ids,
  0 as distance_to_railway,
  x.geom
FROM fish_passage.modelled_crossings_closed_bottom x
INNER JOIN fish_passage.pscis_model_combined pmc
ON x.crossing_id = pmc.model_crossing_id
AND x.track_segment_id is not null
AND x.fish_habitat NOT IN ('NON FISH HABITAT', 'FISH HABITAT - INFERRED - 220-300PCT')

UNION ALL


SELECT
  pmc.pscis_model_combined_id,
  pmc.model_crossing_id,
  pmc.pscis_stream_crossing_id,
  r.track_name,
  r.track_classification,
  r.use_type,
  r.gauge,
  r.status,
  r.operator_english_name,
  r.owner_name,
  pmc.fish_habitat,
  pmc.uphab_gross_sub15,
  pmc.uphab_gross_sub22,
  pmc.dnstr_crossing_ids,
  pmc.upstr_crossing_ids,
  r.distance_to_railway,
  pmc.geom
FROM fish_passage.pscis_model_combined pmc
CROSS JOIN LATERAL
(SELECT
track_name,
track_classification,
use_type,
gauge,
status,
operator_english_name,
owner_name,
ST_Distance(rwy.geom, pmc.geom) as distance_to_railway
FROM whse_basemapping.gba_railway_tracks_sp AS rwy
ORDER BY rwy.geom <-> pmc.geom
LIMIT 1) as r
WHERE pmc.model_crossing_id IS NULL
AND r.distance_to_railway < 25")

##bring in the priority info
# read the geopackage
df <- sf::st_read(
  "data/gis/fish_passage_skeena_20200714.gpkg",
  layer = "xings_bulk_morr_wet") %>% 
  sf::st_transform(crs = 3005) ##put in same crs as our moe layer

##join the railway info to the geopackage 
df_joined <- left_join(
  df,
  select(railway, pscis_model_combined_id, track_name:status, distance_to_railway),
  by = 'pscis_model_combined_id'
)

df_joined <- left_join(
  df_joined,
  select(railway, pscis_stream_crossing_id, distance_to_railway_pscis = distance_to_railway),
  by = 'pscis_stream_crossing_id', na_matches = "never"
)

test <- df_joined %>% 
  select(pscis_model_combined_id, distance_to_railway,distance_to_railway_pscis) %>% 
  filter(!is.na(distance_to_railway) | !is.na(distance_to_railway_pscis))

##here is a list of crossings that have cn rail associated
##33264 - Barren Ck - sfc - site A#6

test <- df %>% filter(watershed_area_ha > 4000)

##going to joion to ecosections but should do in the 040 - join file
# add a unique id
df$misc_point_id <- seq.int(nrow(df))

# load to database
st_write(obj = df, dsn = conn, Id(schema= "working", table = "misc_points"))

# sf doesn't automagically create a spatial index or a primary key
res <- dbSendQuery(conn, "CREATE INDEX ON working.misc_points USING GIST (geom)")
dbClearResult(res)
res <- dbSendQuery(conn, "ALTER TABLE working.misc_points ADD PRIMARY KEY (misc_point_id)")
dbClearResult(res)

df_info <- dbGetQuery(conn, "SELECT a.misc_point_id, moe.ecosection_name
FROM working.misc_points a, whse_terrestrial_ecology.erc_ecosections_sp moe
WHERE ST_Intersects(a.geom, moe.geom);")

dbGetQuery(conn, "select UpdateGeometrySRID('whse_terrestrial_ecology', 'erc_ecosections_sp', 'geom', 3005);")
dbGetQuery(conn, "SELECT Find_SRID('whse_terrestrial_ecology', 'erc_ecosections_sp', 'geom');")




df %>% select(contains('misc'))
