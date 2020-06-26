##join the gitsxan ad wesuweten to pscis

##best way to do this is maybe
#load gitxsan and wet points to postgres
#query pscis_model_combined layer for 50m buffered gitxsan and wet points
#join pscis crossing info using stream_crossing_id
#join my_priorities, my_text info using stream_crossing_id

##connect to database
library(RPostgreSQL)
library(tidyverse)
library(sf)
library(bcdata)

#Enter the values for you database connection and connect
{dsn_database = "postgis"            
  dsn_hostname = "localhost"
  dsn_port = "5432"               
  dsn_uid = "postgres"        
  dsn_pwd = "postgres"
  
  
  #connect and see if the connection to the database is working
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    print("Connecting to database")
    conn <- dbConnect(drv, 
                      dbname = dsn_database,
                      host = dsn_hostname, 
                      port = dsn_port,
                      user = dsn_uid, 
                      password = dsn_pwd)
    print("Connected!")
  },
  error=function(cond) {
    print("Unable to connect to database.")
  })
}



#load git and wet to R
# gitxsan <- sf::st_read("./data/gis/bulkley_fish_passage_background.gpkg", layer = "gitxsan") %>%
#   st_transform(crs = 26909)
# wetsuweten <- sf::st_read("./data/gis/bulkley_fish_passage_background.gpkg", layer = "wetsuweten") %>%
#   st_transform(crs = 26909)
  
gitxsan <- sf::st_read("./data/gis/bulkley_fish_passage_background.gpkg", layer = "gitxsan") %>%
  st_transform(crs = 3005) 
  # rename(geometry = geom)
wetsuweten <- sf::st_read("./data/gis/bulkley_fish_passage_background.gpkg", layer = "wetsuweten") %>%
  st_transform(crs = 3005)  
  # rename(geometry = geom)

##load them to postgres

classes <-  c("sf", "tbl_df", "tbl", "data.frame")  ##need to remove bcdata_sf as class
class(gitxsan) <- classes
schema_name <- 'working'
table_name <- 'gitxsan'
dbWriteTable(conn, c(schema_name, table_name), value = gitxsan, overwrite = TRUE)
dbSendQuery(conn, paste0("ALTER TABLE ", schema_name, ".", table_name, " ALTER COLUMN geom TYPE geometry;"))
# dbSendQuery(conn, paste0("ALTER TABLE ", schema_name, ".", table_name, " ADD PRIMARY KEY (",'"objectid"',");")) ##could add key I guess

table_name <- 'wetsuweten'
dbWriteTable(conn, c(schema_name, table_name), value = wetsuweten, overwrite = TRUE)
dbSendQuery(conn, paste0("ALTER TABLE ", schema_name, ".", table_name, " ALTER COLUMN geom TYPE geometry;"))


##now pull them out again joined with the pscis_modelled_combined layer

#join them together - this is too slow!!!!!!!!!
query <- "Select fh.*, ST_Transform(fh.geom, 4326) as geom
FROM fish_passage.pscis_model_combined fh
INNER JOIN working.gitxsan fn
ON ST_Intersects(fh.geom, ST_Buffer(fn.geom,50))"
# 
pscis_model_combined_gitxsan <- st_read(conn, query = query)


##this one needs to be adapted to work but out of time now
pscis_railway <- dbGetQuery(conn,
                            "SELECT x.stream_crossing_id,x.watershed_group_code,y.owner_name,ROUND((ST_Distance(x.geom, y.geom)):: numeric,1) as dist 
                                       FROM working.my_pscis_20180207 x, 
                                       whse_basemapping.gba_railway_tracks_sp y
                                       WHERE ST_dWithin(x.geom, y.geom,10) 
                          ORDER BY x.watershed_group_code, dist")











##-------------------------------this is old and not the way to go
##join the layers together
joined <- st_join(gitxsan, st_buffer(wetsuweten, dist = 50),
                suffix = c("", "_wet")) 

#see which overlap
match <- joined %>% filter(!is.na(easting_wet))


##use bcdata to look for layers. Could use bcdc_browse() 
info <- bcdc_search("pscis", type = "Geographic", n = 83)  #pscis-assessments (ID: 7ecfafa6-5e18-48cd-8d9b-eae5b5ea2881 )

##name the layer you want to download
get_this <- "7ecfafa6-5e18-48cd-8d9b-eae5b5ea2881"

##download the layer
dl <- bcdc_get_data(get_this) %>% 
  purrr::set_names(nm = tolower(names(.))) ##fix column names

##transform to utm zone 9 - might be something wrong here....
dl2 <- dl %>% 
  st_transform(crs = 26909)
  # mutate(crs = as.numeric(utm_zone) + 26900) %>% 
  # sf::st_as_sf(coords = c("lon_map", "lat_map"), crs = 4326, remove = F)


##see which points are pscis points... only 2?
pscis_joined <- st_join(dl2, st_buffer(joined, dist = 50),
              suffix = c("", "_fn"))

##look at matches
match <- pscis_joined %>% filter(!is.na(easting_wet))

##see which points are pscis points... only 2?
pscis_joined_wet <- st_join(dl2, st_buffer(wetsuweten, dist = 50),
                        suffix = c("", "_wet"))

##look at matches
match_wet <- pscis_joined_wet %>% filter(!is.na(map_id))

pscis_joined_git <- st_join(dl2, st_buffer(gitxsan, dist = 50),
                            suffix = c("", "_wet"))

##look at matches
match_git <- pscis_joined_git %>% filter(!is.na(map_ref_number))


 


