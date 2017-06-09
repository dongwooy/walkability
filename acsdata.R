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


# query the geoid data for SCAG from postgreSQL database
geo_im <- dbGetQuery(con, "SELECT * from geo_ca WHERE geoid LIKE '%4000US06025%' AND geoname LIKE 'Census-Tract%'")
geo_la <- dbGetQuery(con, "SELECT * from geo_ca WHERE geoid LIKE '%4000US06037%' AND geoname LIKE 'Census-Tract%'")
geo_rs <- dbGetQuery(con, "SELECT * from geo_ca WHERE geoid LIKE '%4000US06065%' AND geoname LIKE 'Census-Tract%'")
geo_oc <- dbGetQuery(con, "SELECT * from geo_ca WHERE geoid LIKE '%4000US06059%' AND geoname LIKE 'Census-Tract%'")
geo_sb <- dbGetQuery(con, "SELECT * from geo_ca WHERE geoid LIKE '%4000US06071%' AND geoname LIKE 'Census-Tract%'")
geo_vn <- dbGetQuery(con, "SELECT * from geo_ca WHERE geoid LIKE '%4000US06111%' AND geoname LIKE 'Census-Tract%'")
geo_scag <- rbind(geo_im, geo_la, geo_rs, geo_oc, geo_sb, geo_vn)



##############   Means of Transportation   #####################################

# query transportation-related ACS SF data from postgresql database
    seq028_ca <- dbGetQuery(con, "SELECT * from seq028")
# join geoid table and seq028 table
    seq028_scag <- merge(geo_scag, seq028_ca, by="logrecno")
# get means of transportation data
    mot <- seq028_scag[,c(3:4,160:180)]
    mot$pcar <- 100*(mot$b08301_002/mot$b08301_001)
    mot$ppt <- 100*(mot$b08301_010/mot$b08301_001)
    mot$ptx <- 100*(mot$b08301_016/mot$b08301_001)
    mot$pmc <- 100*(mot$b08301_017/mot$b08301_001)
    mot$pbc <- 100*(mot$b08301_018/mot$b08301_001)
    mot$pwk <- 100*(mot$b08301_018/mot$b08301_001)



##############   Median Annual Household Income   ##############################

# query economy-related ACS SF data from postgresql database
    seq059_ca <- dbGetQuery(con, "SELECT * from seq059")
# join geoid table and seq058 table
    seq059_scag <- merge(geo_scag, seq059_ca, by="logrecno")
# get median household income
    mhi <- seq059_scag[,c(3:4,180)]



##############   % Hispanic/Latino Population   ################################

# query Hispanic/Latino pop ACS SF data from postgresql database
    seq005_ca <- dbGetQuery(con, "SELECT * from seq005")
# join geoid table and seq005 table
    seq005_scag <- merge(geo_scag, seq005_ca, by="logrecno")
# get % Hispanic/Latino pop
    ethn <- seq005_scag[,c(3:4,10:12)]
    ethn$plat <- 100*(ethn$b03001_003/ethn$b03001_001)


##############   % Population by Gender and Age  ###############################

# Age ACS SF data from postgresql database
    seq002_ca <- dbGetQuery(con, "SELECT * from seq002")
# join geoid table and seq002 table
    seq002_scag <- merge(geo_scag, seq002_ca, by="logrecno")
# get % female and senior pop
    gender <- seq002_scag[,c(3:4,1:58)]
    gender$pfemale <- 100*(gender$b01001_026/gender$b01001_001)
    age <- seq002_scag[,c(3:4,1:58)]
    age$p65y <- 100*((age$b01001_020+age$b01001_021+age$b01001_022+age$b01001_023+age$b01001_025+age$b01001_044+age$b01001_045+age$b01001_046+age$b01001_047+age$b01001_048+age$b01001_049)/age$b01001_001)



##############   Housing Tenure / Avg. Household Size  #########################

# Housing Unit ACS SF data from postgresql database
    seq103_ca <- dbGetQuery(con, "SELECT * from seq103")
# join geoid table and seq103 table
    seq103_scag <- merge(geo_scag, seq103_ca, by="logrecno")
# get % renter occupied and average HH size
    tenure <- seq103_scag[,c(3:4,14:16,106)]
    tenure$prenter <- 100*(tenure$b25003_003/tenure$b25003_001)
    tenure$hhsize <- tenure$b25010_000_5


# Merge all datasets

l1 <- merge(mot, mhi, by="geoid")
l2 <- merge(l1, ethn, by="geoid")
l3 <- merge(l2, gender, by="geoid")
l4 <- merge(l3, age, by="geoid")
l5 <- merge(l4, tenure, by="geoid")
dataset <- l5[,c(1:2,24:29,31,36,96,156,161:163)]


model1<-lm(data=dataset, pwk~b19013_001+plat+pfemale+p65y+b25010_000_5+prenter)
summary(model1)
