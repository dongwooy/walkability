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

mot <- seq028_scag[,160:180]
mot$pcar <- 100*(mot$b08301_002/mot$b08301_001)
mot$ppt <- 100*(mot$b08301_010/mot$b08301_001)
mot$ptx <- 100*(mot$b08301_016/mot$b08301_001)
mot$pmc <- 100*(mot$b08301_017/mot$b08301_001)
mot$pbc <- 100*(mot$b08301_018/mot$b08301_001)
mot$pwk <- 100*(mot$b08301_018/mot$b08301_001)




seq059_ca <- dbGetQuery(con, "SELECT * from seq059")
seq059_scag <- merge(geo_scag, seq059_ca, by="logrecno")

mhi <- seq059_scag[,180]




seq005_ca <- dbGetQuery(con, "SELECT * from seq005")
seq005_scag <- merge(geo_scag, seq005_ca, by="logrecno")

ethn <- seq005_scag[,10:12]
ethn$plat <- 100*(ethn$b03001_003/ethn$b03001_001)




seq002_ca <- dbGetQuery(con, "SELECT * from seq002")
seq002_scag <- merge(geo_scag, seq002_ca, by="logrecno")

gender <- seq002_scag[,1:58]
gender$pfemale <- 100*(gender$b01001_026/gender$b01001_001)
age <- seq002_scag[,1:58]
age$p65y <- 100*((age$b01001_020+age$b01001_021+age$b01001_022+age$b01001_023+age$b01001_025+age$b01001_044+age$b01001_045+age$b01001_046+age$b01001_047+age$b01001_048+age$b01001_049)/age$b01001_001)
