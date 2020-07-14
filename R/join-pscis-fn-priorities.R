
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

# read the geopackage
df <- sf::st_read(
  "data/gis/bulkley_fish_passage_background.gpkg",
  layer = "gitxsan") %>% 
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
  b.pscis_model_combined_id,
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
df_joined_git <- left_join(df,
                       pscis_info,
                       by = c('misc_point_id')) %>% 
  rename(distance_sfc = distance)

####----------------------repeat for wetsuweten---------------------------------
##repeat for wetsuweten
df <- sf::st_read(
  "data/gis/bulkley_fish_passage_background.gpkg",
  layer = "wetsuweten") %>% 
  sf::st_transform(crs = 3005) 


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
  b.pscis_model_combined_id, 
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
df_joined_wet <- left_join(df,
                           pscis_info,
                           by = c('misc_point_id')) %>% 
  rename(distance_wetsuweten = distance) %>% 
  mutate(geom2 = geom) %>% 
  st_set_geometry(NULL)  ##remove the geometry so we can join - need to add it back for ones that don't have it in git though!!!


##there are many shared columns so we should just keep unique columns - removed b/c not all xings in the wet/lgl file are included in the sfc/git
# col_same <- intersect(names(df_joined_git), names(df_joined_wet))
# col_same <- col_same[!col_same %in% 'pscis_model_combined_id']  ##we have to remove the key that will join them though

##remove the columns from the wet file that are the same as the ones in the git df
df_joined_wet <- df_joined_wet %>% 
  # select(-all_of(col_same)) %>% ##this is not the right thing to do for the crossings that are not in the sfc/gitsxan file
  dplyr::rename_all(function(x) paste0(x, "_wetsuweten")) %>% ##add identifier to columns so we know where the data came from
  rename(pscis_model_combined_id = pscis_model_combined_id_wetsuweten)

##add identifier to git columns so we know where the data came from
df_joined_git <- df_joined_git %>% 
  dplyr::rename_all(function(x) paste0(x, "_sfc")) %>% 
  rename(pscis_model_combined_id = pscis_model_combined_id_sfc, geom = geom_sfc)
  

##manually look at crossings that are more than 50 - 100 m from your target - this has slightly diff names so check first
# far <-df_joined %>% 
#   filter(git_distance_git > 50 & git_major_watershed %ilike% 'BULK') %>% 
#   select(pscis_model_combined_id, git_distance_git, wet_distance_wet, everything())


##join together the git and wet info based on the crossing they match.  
df_joined1 <- full_join(df_joined_git,df_joined_wet,
                       by = 'pscis_model_combined_id') %>% 
  filter(sf::st_is_empty(geom) == F) %>% ##filter out non-geometry results
  select(-geom2_wetsuweten)
  
  # mutate(test = case_when(sf::st_is_empty(geom) == T ~ wet_geom2,  ##not sure why this does not work
  #                         T ~ geom)) 

##these are the ones that need the geometry specified
df_joined2 <- full_join(df_joined_git,df_joined_wet,
                       by = 'pscis_model_combined_id') %>%  
  filter(sf::st_is_empty(geom) == T) %>% 
  mutate(geom = geom2_wetsuweten) %>% 
  select(-geom2_wetsuweten)

##we need to add back the geom column for the crossings that are empty (i.e. wetsuweten crossings with no gitsxan equivalent)
df_joined <-  bind_rows(df_joined1, df_joined2) %>% 
  st_transform(crs = 4326) %>% 
  distinct(geom, .keep_all = T) ##there are a couple of duplicate geometries for some reason so we drop the whole row

# ##see which crossings are shared
# shared <-  df_joined %>% filter(!is.na(git_distance_git) & !is.na(wet_distance_wet))
# plot(shared$geom)

##get id which fn priorities have duplicated xings
dups <- df_joined %>%
  group_by(pscis_model_combined_id) %>%
  summarize(n=n()) %>%
  filter(n > 1) %>% 
  pull(pscis_model_combined_id) %>%
  as_vector()

##look at the duplicated crossings - none in the bulkley or Morr so should not matter
dups <- df_joined %>%
  filter(pscis_model_combined_id %in% dups) %>%
  arrange(pscis_model_combined_id) %>%
  select(pscis_model_combined_id, everything())
# 
rm(dups)

##this is useful for viewing results
# test <- df_joined2 %>% 
#   filter(is.na(pscis_model_combined_id_retrieve)) %>% 
#   select(pscis_model_combined_id, pscis_model_combined_id_retrieve, git_distance_git, wet_distance_wet, everything())

n_distinct(df_joined$pscis_model_combined_id)

##looking at the crossings it seems like it is reasonable to say that the crossings are correct with anything less than 150m so
##modify the pscis_model_combined_id column to only give the pscis_model_combined_id if the number is less than 150 for the git and wet distances
df_joined <- df_joined %>%
  mutate(pscis_model_combined_id = case_when(distance_sfc_sfc > 150 | distance_wetsuweten_wetsuweten > 150 ~ NA_integer_,
                                                      T ~ pscis_model_combined_id))

n_distinct(df_joined$pscis_model_combined_id) ##14 crossings (156 of 170) had matches more than 150m away so do not have associated modeling info 


####-------------------repeat for 2018 moe report - Irvine
df <- sf::st_read(
  "data/gis/bulkley_fish_passage_background.gpkg",
  layer = "irvine_2018") %>%
  sf::st_transform(crs = 4326) %>%
  rename(geom2 = geom) %>%
  as_tibble() %>%
  mutate(pscis_id = as.integer(pscis_id))


##need to get the pscis_model_combined_id for my pscis crossings
ids <-  df %>%
  pull(pscis_id) %>%
  unique() %>%
  as_vector() %>%
  na.omit()


sql <- glue::glue_sql(
  "
                                Select pscis_model_combined_id, pscis_stream_crossing_id
                                FROM fish_passage.pscis_model_combined b
                                WHERE b.pscis_stream_crossing_id IN
                                ({ids*})
                                ",
  .con = conn
)
query <- DBI::dbSendQuery(conn, sql)
pscis_info <- DBI::dbFetch(query)
dbClearResult(query)


##for some reason we do not have all the points (379 of 396)
missing <- as.numeric(setdiff(ids, pscis_info$pscis_stream_crossing_id))
# 
##have a look at the missing values
missing <- df %>% filter(pscis_id %in% missing) ##REviewed them and only 3 are 'high' priorities and none look that great compared to our new FN xings

rm(missing)

##join pscis id back to the dataframe
df_joined_irvine <- left_join(df,
                           pscis_info,
                           by = c('pscis_id' = 'pscis_stream_crossing_id')) %>% 
  rename(pscis_stream_crossing_id = pscis_id) %>% 
  select(priority:northing_9u, road_tenure, comments, pscis_model_combined_id, geom2) %>% ##drop columns from pscis
  dplyr::rename_all(function(x) paste0(x, "_moe")) %>% 
  rename(pscis_model_combined_id = pscis_model_combined_id_moe)

##get id which fn priorities have duplicated xings
dups <- df_joined_irvine %>%
  group_by(pscis_model_combined_id) %>%
  summarize(n=n()) %>%
  filter(n > 1) %>% 
  pull(pscis_model_combined_id) %>%
  as_vector()

##look at the duplicated crossings - none in the bulkley or Morr so should not matter
dups <- df_joined_irvine %>%
  filter(pscis_model_combined_id %in% dups) %>%
  arrange(pscis_model_combined_id) %>%
  select(pscis_model_combined_id, everything())
# 
rm(dups)  

##join to the fn dataframe
df_joined1 <- full_join(df_joined,df_joined_irvine,
                        by = 'pscis_model_combined_id', na_matches = "never") %>% 
  filter(sf::st_is_empty(geom) == F) %>% ##filter out non-geometry results
  select(-geom2_moe)


# mutate(test = case_when(sf::st_is_empty(geom) == T ~ wet_geom2,  ##not sure why this does not work
#                         T ~ geom)) 

##these are the ones that need the geometry specified
df_joined2 <- full_join(df_joined,df_joined_irvine,
                        by = 'pscis_model_combined_id', na_matches = "never") %>%  
  filter(sf::st_is_empty(geom) == T) %>% 
  mutate(geom = geom2_moe) %>% 
  select(-geom2_moe)


##we need to add back the geom column for the crossings that are empty (i.e. wetsuweten crossings with no gitsxan equivalent)
df_joined <-  bind_rows(df_joined1, df_joined2) %>% 
  select(pscis_model_combined_id, ends_with('_wetsuweten'), ends_with('_sfc'), everything())


##looks like 16 shared crossings - confirm and keep as is a great reference for later
shared <-  df_joined %>% filter((!is.na(distance_sfc_sfc) | !is.na(distance_wetsuweten_wetsuweten)) & 
                                  !is.na(priority_moe)) %>% 
  select(pscis_model_combined_id,pscis_stream_crossing_id_moe, map_id_wetsuweten,
         map_ref_number_sfc, everything())

rm(shared)

n_distinct(df_joined$pscis_model_combined_id) ##519                                   

##----------back to the database for the modeled info
##need to get the pscis_model_combined_id for my pscis crossings
ids <-  df_joined %>%
  pull(pscis_model_combined_id) %>%
  unique() %>%
  as_vector() %>%
  na.omit()


sql <- glue::glue_sql(
  "
                                Select *
                                FROM fish_passage.pscis_model_combined b
                                WHERE b.pscis_model_combined_id IN
                                ({ids*})
                                ",
  .con = conn
)
query <- DBI::dbSendQuery(conn, sql)
pscis_info <- DBI::dbFetch(query)
dbClearResult(query)

n_distinct(pscis_info$blue_line_key) ##365 blue line keys

test <- pscis_info %>% 
  filter(!is.na(blue_line_key))


##remove a few uneeded columns
pscis_info <- pscis_info %>% 
  select(pscis_model_combined_id:bcgs_20k_grid_map_tile)


######------add the fish habitat model back to the data from the 3 reports
df_joined <- left_join(
  df_joined,
  pscis_info,
  by = 'pscis_model_combined_id'
) 

####-----------add a few columns from pscis
ids <-  df_joined %>%
  pull(pscis_stream_crossing_id) %>%
  unique() %>%
  as_vector() %>%
  na.omit()


sql <- glue::glue_sql(
  "
                                Select stream_crossing_id, downstream_channel_width, image_view_url,road_km_mark, assessment_comment
                                FROM whse_fish.pscis_assessment_svw
                                WHERE whse_fish.pscis_assessment_svw.stream_crossing_id IN
                                ({ids*})
                                ",
  .con = conn
)
query <- DBI::dbSendQuery(conn, sql)
pscis_info <- DBI::dbFetch(query)
dbClearResult(query)

##add back to the df and tidy up
df_joined <- left_join(
  df_joined,
  pscis_info,
  by = c('pscis_stream_crossing_id' = 'stream_crossing_id')
) %>% mutate(uphab_gross_sub15_km = round(uphab_gross_sub15/1000, 1)) %>% 
  mutate(downstream_route_measure = as.integer(downstream_route_measure),
         blue_line_key = as.integer(blue_line_key))  ##this might not be necesary - check

n_distinct(df_joined$blue_line_key) ##366 crossings with blue_line_key - why is this not 365????
n_distinct(df_joined$pscis_model_combined_id)
n_distinct(df_joined$downstream_route_measure)

##for the deliverables to the FN and for the permit application and to save time we want high priorities only
##we can filter out sites outside the BULK Morr later
df_joined_high <- df_joined %>% 
  filter(priority_moe %in% c('high') |
           !is.na(map_id_wetsuweten) |
           !is.na(map_ref_number_sfc)) 


####--------------watershed info
##make a function to retrieve the watershed info 
get_watershed <- function(fish_habitat_info){
  mapply(fwapgr::fwa_watershed, blue_line_key = fish_habitat_info$blue_line_key,
         downstream_route_measure = fish_habitat_info$downstream_route_measure) %>%
    purrr::set_names(nm = fish_habitat_info$pscis_model_combined_id) %>%
    discard(function(x) nrow(x) == 0) %>% ##remove zero row tibbles with https://stackoverflow.com/questions/49696392/remove-list-elements-that-are-zero-row-tibbles
    data.table::rbindlist(idcol="pscis_model_combined_id") %>%
    distinct(pscis_model_combined_id, .keep_all = T) %>% ##there are duplicates we should get rid of
    st_as_sf()
}

##for the FN first - function hits fwa api and retrieves a spatial polygon so is very slow.  
##Could be done locally to speed up. sql on simons fwapg github
##Should rerun with all the watersheds when I have a bit more time available
##
crossing_watersheds <-  get_watershed(fish_habitat_info = df_joined_high %>% filter(!is.na(blue_line_key)))  ##needed to get rid of the xings with no fh info


##add the watershed info to the point layer and tidy up the column order
df_joined <- left_join(
  df_joined,
  crossing_watersheds %>% 
    st_set_geometry(NULL) %>% select(pscis_model_combined_id, area_ha, refine_method) %>% 
    mutate(pscis_model_combined_id = as.integer(pscis_model_combined_id)),
  by = 'pscis_model_combined_id'
)  %>% select(pscis_model_combined_id, pscis_stream_crossing_id, model_crossing_id, map_id_wetsuweten,
          map_ref_number_sfc, watershed_group_code, remediation_priority_wetsuweten, priority_rank_for_restoration_sfc,
          priority_moe, road_name_corridor_wetsuweten, road_name_sfc, road_moe, road_km_mark, stream_name_sfc,
          stream_moe, comments_wetsuweten, comments_completion_details_sfc, original_report_recommendations_sfc,
          comments_moe, assessment_comment, uphab_gross_sub15_km, watershed_area_ha = area_ha, downstream_channel_width, 
          species_presence_sfc, upstr_species, everything())



# ##this is for running a test on a small dataset since it is time consuming - for some reason it does not work.  I cannot figure out why - maybe a dplyr thing
# df_joined_fh_test <- df_joined_fh %>%
#   select(pscis_model_combined_id, blue_line_key, downstream_route_measure) %>%
#   slice(2:3)
# 

##weird little workaround to get index for the crossings because it takes so long to run the query..
# df_joined2 <- df_joined_fn %>% 
#   mutate(idx = seq.int(nrow(df_joined_fn)),
#          pscis_model_combined_id = as.character(pscis_model_combined_id)) %>% 
#   filter(!is.na(blue_line_key)) %>% 
#   # select(idx, pscis_model_combined_id, starts_with('git_')) %>% 
#   st_set_geometry(NULL)

# ##this is the workaround version!!!!!
# crossing_watersheds_joined <- crossing_watersheds %>% 
#   purrr::set_names(nm = df_joined2$pscis_model_combined_id) %>% ##note that we use the altered one here
#   discard(function(x) nrow(x) == 0) %>% 
#   data.table::rbindlist(idcol="pscis_model_combined_id") %>% 
#   distinct(pscis_model_combined_id, .keep_all = T) %>% ##there are duplicates we should get rid of
#   st_as_sf() ##for some reason it looses its sf class when we rbindlist. it might be because of duplicate geometries


##here is how we get the watershed codes - thanks Simon!!!!
wsheds <- dbGetQuery(conn, "SELECT DISTINCT ON (pscis_model_combined_id)
    a.pscis_model_combined_id, 
    a.linear_feature_id,
    b.watershed_code_50k,
    substring(b.watershed_code_50k from 1 for 3)
      ||'-'||substring(b.watershed_code_50k from 4 for 6)
      ||'-'||substring(b.watershed_code_50k from 10 for 6)
      ||'-'||substring(b.watershed_code_50k from 16 for 6)
      ||'-'||substring(b.watershed_code_50k from 24 for 6)
      ||'-'||substring(b.watershed_code_50k from 30 for 6)
      ||'-'||substring(b.watershed_code_50k from 36 for 6) as watershed_code_50k_parsed,
    b.blue_line_key_20k,
    b.watershed_key_20k,
    b.blue_line_key_50k,
    b.watershed_key_50k,
    b.match_type
FROM fish_passage.pscis_model_combined a
LEFT OUTER JOIN whse_basemapping.fwa_streams_20k_50k b
ON a.linear_feature_id = b.linear_feature_id_20k
WHERE a.watershed_group_code IN ('BULK','MORR')
ORDER BY a.pscis_model_combined_id, b.match_type;")

##add the 1:50k watershed code and modify the 1:20k so it has hyphens rather than periods
df_joined <- left_join(
  df_joined,
  select(wsheds,pscis_model_combined_id, watershed_code_50k = watershed_code_50k_parsed),
  by = 'pscis_model_combined_id'
) %>% 
  mutate(watershed_code_20k = stringr::str_replace_all(wscode, '\\.', '-'))

##join the fn point layer info back to the watershed layer.    
crossing_watersheds_joined <- left_join(
  select(crossing_watersheds, -wscode, -localcode),
  df_joined %>% st_set_geometry(NULL) %>% mutate(pscis_model_combined_id = as.character(pscis_model_combined_id)),
  by = 'pscis_model_combined_id')

##get id which fn priorities have duplicated xings
dups <- crossing_watersheds_joined %>%
  group_by(pscis_model_combined_id) %>%
  summarize(n=n()) %>%
  filter(n > 1) %>% 
  pull(pscis_model_combined_id) %>%
  as_vector()

##look at the duplicated crossings - none in the bulkley or Morr so should not matter
dups <- crossing_watersheds_joined %>%
  filter(pscis_model_combined_id %in% dups) %>%
  arrange(pscis_model_combined_id) %>%
  select(pscis_model_combined_id, everything())
# 
rm(dups) 


##make a file for the permit and FN
df_joined_high <- df_joined %>% 
  filter(priority_moe %in% c('high') |
           (!is.na(map_id_wetsuweten) & 
              (!watershed_group_code %ilike% 'FRAN'))|
           (!is.na(map_ref_number_sfc) & !watershed_group_code %ilike% 'FRAN' &
              (major_watershed_sfc %ilike% 'Bulk' | major_watershed_sfc %ilike% 'Mor'))) 


##make a file for the permit and FN
crossing_watersheds_joined_high <- crossing_watersheds_joined %>% 
  filter(priority_moe %in% c('high') |
           (!is.na(map_id_wetsuweten) & 
              (!watershed_group_code %ilike% 'FRAN'))|
           (!is.na(map_ref_number_sfc) & !watershed_group_code %ilike% 'FRAN' &
              (major_watershed_sfc %ilike% 'Bulk' | major_watershed_sfc %ilike% 'Mor'))) 

##add the points and the watersheds to the database to view
st_write(obj = df_joined, dsn = conn, Id(schema= "working", table = "my_pscis_fn_joined"))
st_write(obj = df_joined_high, dsn = conn, Id(schema= "working", table = "my_pscis_fn_joined_high"))
st_write(obj = crossing_watersheds_joined, dsn = conn, Id(schema= "working", table = "sfc_watersheds"))
st_write(obj = crossing_watersheds_joined_high, dsn = conn, Id(schema= "working", table = "sfc_watersheds_high"))  


##make a geopackage for Alicia and Dave


sf::st_write(crossing_watersheds_joined, "./data/gis/fish_passage_skeena_20200713.gpkg", "watersheds_all", delete_layer = T) 
sf::st_write(crossing_watersheds_joined_high, "./data/gis/fish_passage_skeena_20200713.gpkg", "watersheds_bulk_morr_wet", delete_layer = T) 
# sf::st_write(df_joined_fn, "./data/gis/fish_passage_skeena_20200713.gpkg", "xings_gtixsan_wetsuweten") 
sf::st_write(df_joined, "./data/gis/fish_passage_skeena_20200713.gpkg", "xings_all", delete_layer = T) 
sf::st_write(df_joined_high, "./data/gis/fish_passage_skeena_20200713.gpkg", "xings_bulk_morr_wet", delete_layer = T)

##we need a summary for the fish permit application
##we need to make sure we get the moe region correct so check


