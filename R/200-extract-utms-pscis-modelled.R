##this is a new hack file to get the utms for modelled crossings from the postgres database

source('R/packages.R') 
source('R/Functions.R')

# library(DBI)
# library(tidyverse)
# library(sf)
# library(RPostgres)
# library(data.table)

# db connection
conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = 'postgis',
  host = 'localhost',
  port = 5432,
  user = 'postgres',
  password = 'postgres'
)

##bring in your pscis sheet - phase 1
pscis <- import_pscis(workbook_name = 'pscis_phase1.xlsm') %>% 
  mutate(my_crossing_reference = as.integer(my_crossing_reference),
         easting = as.numeric(easting),
         northing = as.numeric(northing))

# ##bring in your pscis sheet - phase 2
# pscis <- import_pscis(workbook_name = 'pscis_phase2.xlsm') %>% 
#   mutate(my_crossing_reference = as.integer(pscis_crossing_id),
#          easting = as.numeric(easting),
#          northing = as.numeric(northing))

##isolate the id's that we want info for
id <- pscis %>% 
  filter(is.na(easting)) %>% 
  pull(my_crossing_reference)

##this is for phase 1
sql <- glue::glue_sql("SELECT * FROM fish_passage.modelled_crossings_closed_bottom x WHERE x.crossing_id IN ({id*})",
                      .con = conn)


##we need to tweak it a bit for the phase 2
##we are using the pscis model combined layer from way back but will work for now
# sql <- glue::glue_sql("SELECT x.*, ST_X(ST_TRANSFORM(x.geom, 26909)) as utm_easting, ST_Y(ST_TRANSFORM(x.geom, 26909)) as utm_northing FROM fish_passage.pscis_model_combined x WHERE x.pscis_stream_crossing_id IN ({id*})",
#                       .con = conn)


query <- DBI::dbSendQuery(conn, sql)
df <- DBI::dbFetch(query)
dbClearResult(query)

##now join our ids to the utm info - phase 1
id_joined <- left_join(
  as_tibble(id),
  select(df,
         crossing_id, utm_easting, utm_northing),
  by = c('value' = 'crossing_id'))

# ##now join our ids to the utm info - phase 2
# id_joined <- left_join(
#   as_tibble(id),
#   select(df, 
#          pscis_stream_crossing_id, utm_easting, utm_northing),
#   by = c('value' = 'pscis_stream_crossing_id'))

##join it back to the original spreadsheet and fill in the easting northing columns where not already filled
##burn to a csv so you can cut and paste into your spreadsheet
utms <- left_join(
  pscis,
  id_joined,
  # by = c('my_crossing_reference' = 'value') ##this is for phase 1
  by = c('my_crossing_reference' = 'value') ##this is for phase 2
) %>% 
  mutate(easting = case_when(is.na(easting) ~ utm_easting,
                             T ~ easting),
         northing = case_when(is.na(northing) ~ utm_northing,
                              T ~ northing))  %>% 
  select(my_crossing_reference, easting, northing) %>% ##this works for both phases although my_crossing_reference is mislabeled and should be pscis_crossing_id
  write_csv("data/utms_modelled.csv")  ##burn to a csv so you can cut and paste into your spreadsheet


##always disconnect from the database
dbDisconnect(conn = conn)
