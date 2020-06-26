## Load a set of misc crossings to postgres, then
## join each point to nearest pscis points / modelled crossing point
## Return everything to pscis_info df, filter out points that are too far away
## (maybe 50-100m) in R  - manual review of matches that are not within a few
## metres should also be done

library(DBI)
library(tidyverse)
library(sf)

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
  "../data_shared/gis/bulkley_fish_passage_background.gpkg",
  layer = "gitxsan")

# add a unique id
df$misc_point_id <- seq.int(nrow(df))

# load to database
st_write(obj = df, dsn = conn, Id(schema= "working", table = "misc_points"))

# sf doesn't automagically create a spatial index or a primary key
res <- dbSendQuery(conn, "CREATE INDEX ON working.misc_points USING GIST (geom)")
dbClearResult(res)
res <- dbSendQuery(conn, "ALTER TABLE working.misc_points ADD PRIMARY KEY (misc_point_id)")
dbClearResult(res)

# if fish_passage.pscis_model_combined does not have an index on the geom, run this
#res <- dbSendQuery(conn, "CREATE INDEX ON fish_passage.pscis_model_combined USING GIST (geom)", n=0)
#dbClearResult(res)

# get the *nearest* modelled crossing / pscis crossing and the distance apart
pscis_info <- dbGetQuery(conn, "SELECT
  a.misc_point_id,
  b.*,
  ST_Distance(a.geom, b.geom) AS distance
FROM
  working.misc_points AS a
CROSS JOIN LATERAL
  (SELECT *
   FROM fish_passage.pscis_model_combined
   ORDER BY
     a.geom <-> geom
   LIMIT 1) AS b")