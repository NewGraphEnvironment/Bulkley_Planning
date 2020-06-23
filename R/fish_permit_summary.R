library("RPostgreSQL")
library(tidyverse)
library(xlsx)

#Enter the values for you database connection
{
  dsn_database = "postgis"            
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

##list tables in a schema  
dbGetQuery(conn,
           "SELECT table_name 
           FROM information_schema.tables 
           WHERE table_schema='whse_admin_boundaries'")

##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type 
           FROM information_schema.columns 
           WHERE table_name='pscis_assessment_svw'")


##make the query to pull out pscis info
query <- "SELECT p.stream_crossing_id, p.utm_zone, p.utm_easting, p.utm_northing, p.image_view_url, moe.region_name, ST_Transform(p.geom, 4326) as geom  
FROM whse_fish.pscis_assessment_svw p
INNER JOIN whse_admin_boundaries.eadm_wlap_region_bnd_area_svw moe
ON ST_Intersects(p.geom, moe.geom)"


##read in the pscis layer
pscis <- st_read(conn, query = query) 
  # select(-id)

# #grab things I need from pscis - lets add the MoE region to these as we pull them out
# pscis <- dbGetQuery(conn, "SELECT stream_crossing_id, utm_zone,utm_easting, utm_northing, image_view_url  
#                     FROM whse_fish.pscis_assessment_svw")


##read in the priority for follow up info
pscis_working <- dbGetQuery(conn, 
                            "SELECT *
                     FROM working.my_pscis_20190709")

names(pscis_working)

permit_table <- pscis_working %>% 
  filter(watershed_group_code == 'BULK' &
           (my_priority == 'high' | my_priority == 'mod')) %>% 
  select(stream_crossing_id, my_stream_name, stream_name, wscode, upstr_species, my_priority, my_text) 




permit_table <- left_join(permit_table, pscis, by = "stream_crossing_id") %>% 
  select(region_name, my_stream_name, stream_name, wscode, stream_crossing_id,
         utm_zone,utm_easting, utm_northing, image_view_url,upstr_species, my_priority, my_text) %>% 
  mutate('wscode' = gsub("\\.","-", wscode),
         my_stream_name = case_when( is.na(my_stream_name) ~ stream_name,
                   TRUE ~ my_stream_name)) %>% 
  rename(MoE_Region = region_name, 'Watershed Code (1:20,000)' = wscode, Waterbody = my_stream_name, 
         PSCIS_stream_crossing_id = stream_crossing_id, Priority = my_priority, Comments = my_text) %>% 
  select(-stream_name)

write.table(permit_table, "C:/Users/allan/OneDrive/New_Graph/Current/2019-023_Bulkley_fish_passage/permit/permit_streams.txt", 
            sep = ",", quote = FALSE, row.names = F)
  

setwd("C:/Users/allan/OneDrive/New_Graph/Current/2019-010_FWCP_Parsnip/permit")
write.xlsx(permit_table, file="permit_details.xlsx", sheetName="sample_streams", append=FALSE, row.names=FALSE)
##this one needs to come from the "01_modelled_Part3.... file
write.xlsx(as.data.frame(fish_watershed), file="permit_details.xlsx", sheetName="potential_species", append=TRUE, row.names=FALSE)
