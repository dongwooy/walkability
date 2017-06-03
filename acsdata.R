# install.packages("RPostgreSQL")
require("RPostgreSQL")

# create a connection
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "acs",
                 host = "localhost", port = 5432,
                 user = "postgres", password = '6344vppv')


# query the data from postgreSQL
geo_im <- dbGetQuery(con, "SELECT * from geo_ca WHERE geoid LIKE '%4000US06025%' AND geoname LIKE 'Census-Tract%'")
geo_la <- dbGetQuery(con, "SELECT * from geo_ca WHERE geoid LIKE '%4000US06037%' AND geoname LIKE 'Census-Tract%'")
geo_rs <- dbGetQuery(con, "SELECT * from geo_ca WHERE geoid LIKE '%4000US06065%' AND geoname LIKE 'Census-Tract%'")
geo_oc <- dbGetQuery(con, "SELECT * from geo_ca WHERE geoid LIKE '%4000US06059%' AND geoname LIKE 'Census-Tract%'")
geo_sb <- dbGetQuery(con, "SELECT * from geo_ca WHERE geoid LIKE '%4000US06071%' AND geoname LIKE 'Census-Tract%'")
geo_vn <- dbGetQuery(con, "SELECT * from geo_ca WHERE geoid LIKE '%4000US06111%' AND geoname LIKE 'Census-Tract%'")
geo_scag <- rbind(geo_im, geo_la, geo_rs, geo_oc, geo_sb, geo_vn)


seq028_ca <- dbGetQuery(con, "SELECT * from seq028")


seq028_scag <- merge(geo_scag, seq028_ca, by="logrecno")
