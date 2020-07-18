##update 20191028
##author Al Irvine
##tweaks download only specific chunks of PSCIS data and visualize
##add the "last updated" info to the data so we can query whether to bother updating at a later date


# remotes::install_github("bcgov/bcdata")
# remotes::install_github("bcgov/bcmaps")
# library(bcmaps)
{
  library(bcdata)
  library(sf)
  library(RPostgreSQL)
  library(tidyverse)  
  options(timeout=180)##increase your timeout limit to allow download of bigger files
}

##here we type our search term in to look for layers. Could use bcdc_browse() 
info <- bcdc_search("ecosections-ecoregion-ecosystem-classification-of-british-columbia", type = "Geographic", n = 83)

#ecosections-ecoregion-ecosystem-classification-of-british-columbia : ccc01f43-860d-4583-8ba4-e72d8379441e


##name the layer you want to download
get_this <- "ccc01f43-860d-4583-8ba4-e72d8379441e"

##now lets bring the layer into R and drop it into the database in one step
bcdatapg <- function(get_this)
  {
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
  dl <- bcdc_get_data(get_this)
  layer_name <- dl$id[1]
  schema_name <- tolower(word(layer_name,1,sep = "\\."))
  table_name <- tolower(word(layer_name,2,sep = "\\."))
  names(dl) <- tolower(names(dl))
  dl <- dl %>% rename(geom = geometry)
  classes <-  c("sf", "tbl_df", "tbl", "data.frame")
  class(dl) <- classes
  dbSendQuery(conn, paste0("CREATE SCHEMA IF NOT EXISTS ", schema_name,";"))
  dbWriteTable(conn, c(schema_name, table_name), value = dl, overwrite = TRUE)
  dbSendQuery(conn, paste0("ALTER TABLE ", schema_name, ".", table_name, " ALTER COLUMN geom TYPE geometry;"))
  dbSendQuery(conn, paste0("ALTER TABLE ", schema_name, ".", table_name, " ADD PRIMARY KEY (",'"objectid"',");"))
  dbDisconnect(conn)
  rm(conn, drv, dl, classes, dsn_database, dsn_hostname, dsn_port, dsn_pwd,dsn_uid,
     layer_name) 
} 

bcdatapg(get_this = get_this)

##check to see that you have a unique identifier
# names(dl)
# n_distinct(dl$objectid) == nrow(dl)
# n_distinct(dl$objectid)
# nrow(dl)

# ##look at your duplicates
# duplicates <- dl %>%
#   group_by(objectid) %>%
#   filter(n() > 1) 
# 
# ##remove duplicates in a timely fashion
# dl <- dl %>%
#   group_by(objectid) %>%
#   slice(1) 
# 
# ##remove duplicated rows (way slow)
# dl <- dl %>%
#   distinct(id, .keep_all = TRUE)

##add a column for the FID to use as the primary key if you don't have a unique ID (i.e. objectid)
# dl <- mutate(dl, fid = as.integer(rownames(dl))) %>%
#   select(fid, everything())
# names(dl)


##need to drop the bcdc_sf class from dl in order to save it to postgres
#class(dl)
classes <-  c("sf", "tbl_df", "tbl", "data.frame")
class(dl) <- classes

dbSendQuery(conn, paste0("CREATE SCHEMA IF NOT EXISTS ", schema_name,";"))

##write the table to the database ##should row.names = TRUE then make that the primary key?
dbWriteTable(conn, c(schema_name, table_name), value = dl, overwrite = TRUE)

##use postGIStools to assign geometry type and primary key column
dbSendQuery(conn, paste0("ALTER TABLE ", schema_name, ".", table_name, " ALTER COLUMN geom TYPE geometry;"))

##ALTER TABLE tableName ADD PRIMARY KEY (id)
dbSendQuery(conn, paste0("ALTER TABLE ", schema_name, ".", table_name, " ADD PRIMARY KEY (",'"objectid"',");"))

##always disconnect from your database!!!!!!!!!
dbDisconnect(conn)

schema_name
table_name


##_________looking for layers and getting info about them____________________________________
library(jsonlite)

search_term <- "env-regional-boundaries"

bcdc_api_url = "https://catalogue.data.gov.bc.ca/api/3/action/"
wfs_url = "https://openmaps.gov.bc.ca/geo/pub/wfs"

##build the search url for the api
search_url <- paste0(bcdc_api_url, "package_search?q=", search_term)

##this is requesting the info about the files 
search_return <- fromJSON(readLines(search_url), flatten = TRUE)


##let's isolate the layer we are looking for 
search_return <- search_return$result 
search_return <- search_return$results
#matches$layer_name
names(search_return)
search_return$name
target_layer_info <- filter(search_return, name == get_this) ##this refers to the search term at the top - might fuck you up a bit....
target_layer_info$object_name
#layer$id

##see when the data was modified last
target_layer_info$record_last_modified

##here we can preview on imap
bcdc_get_record("pscis-habitat-confirmations") [["preview_map_service_url"]] ##get resource id
test <- bcdc_query_geodata("1915c8e2-3c8f-494f-bfc8-ecc013d494c5") %>%  collect()