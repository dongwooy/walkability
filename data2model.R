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


geo_scag <- dbGetQuery(con, "SELECT * from geo_ca
                       WHERE (geoid LIKE '%4000US06025%' OR
                              geoid LIKE '%4000US06037%' OR
                              geoid LIKE '%4000US06065%' OR
                              geoid LIKE '%4000US06059%' OR
                              geoid LIKE '%4000US06071%' OR
                              geoid LIKE '%4000US06111%') AND geoname LIKE 'Census-Tract%'")

d <- c("seq028","seq059","seq005","seq002","seq103") #(transport, economic, demo, age, housing)
e <- c("seq028_ca","seq059_ca","seq005_ca","seq002_ca","seq103_ca") #CA
# getting transport, economic, demo, age, housing acs data for CA
for (i in 1:5) {
  y <- d[i]
  c <- paste("SELECT","*","from",y,sep=" ")
  assign(e[i], dbGetQuery(con, c))
}

seq028_scag <- merge(geo_scag, seq028_ca, by="logrecno") # join geoid table and seq028 table
seq059_scag <- merge(geo_scag, seq059_ca, by="logrecno") # join geoid table and seq058 table
seq005_scag <- merge(geo_scag, seq005_ca, by="logrecno") # join geoid table and seq005 table
seq002_scag <- merge(geo_scag, seq002_ca, by="logrecno") # join geoid table and seq002 table
seq103_scag <- merge(geo_scag, seq103_ca, by="logrecno") # join geoid table and seq103 table


##############   Means of Transportation   #####################################

# query transportation-related ACS SF data from postgresql database
    seq028_ca <- dbGetQuery(con, "SELECT * from seq028")
# join geoid table and seq028 table
    seq028_scag <- merge(geo_scag, seq028_ca, by="logrecno")
# get means of transportation data
    mot <- seq028_scag[,c(3:4,160,161,169,175:180)]
    colnames(mot)[3] <- "mot_total"
    colnames(mot)[4] <- "car"
    colnames(mot)[5] <- "transit"
    colnames(mot)[6] <- "taxi"
    colnames(mot)[7] <- "mcycle"
    colnames(mot)[8] <- "bike"
    colnames(mot)[9] <- "walk"
    colnames(mot)[10] <- "mot_other"
    colnames(mot)[11] <- "mot_home"

    mot$motor <- mot$car + mot$transit + mot$taxi + mot$mcycle
    mot$nonmotor <- mot$bike + mot$walk
    mot$car_n <- mot$mot_total - mot$car
    mot$transit_n <- mot$mot_total - mot$transit
    mot$taxi_n <- mot$mot_total - mot$taxi
    mot$bike_n <- mot$mot_total - mot$bike
    mot$walk_n <- mot$mot_total - mot$bike
    mot$motor_n <- mot$mot_total - mot$motor
    mot$nonmotor_n <- mot$mot_total - mot$nonmotor



    mot$pcar <- 100*(mot$car/mot$mot_total)
    mot$pcar[mot$pcar == "NaN"] <- 0
    mot$ppt <- 100*(mot$transit/mot$mot_total)
    mot$ppt[mot$ppt == "NaN"] <- 0
    mot$ptx <- 100*(mot$taxi/mot$mot_total)
    mot$ptx[mot$ptx == "NaN"] <- 0
    mot$pmc <- 100*(mot$mcycle/mot$mot_total)
    mot$pmc[mot$pmc == "NaN"] <- 0
    mot$pbc <- 100*(mot$bike/mot$mot_total)
    mot$pbc[mot$pbc == "NaN"] <- 0
    mot$pwk <- 100*(mot$walk/mot$mot_total)
    mot$pwk[mot$pwk == "NaN"] <- 0



##############   Median Annual Household Income   ##############################

# query economy-related ACS SF data from postgresql database
    seq059_ca <- dbGetQuery(con, "SELECT * from seq059")
# join geoid table and seq058 table
    seq059_scag <- merge(geo_scag, seq059_ca, by="logrecno")
# get median household income
    mhi <- seq059_scag[,c(3:4,180)]
    colnames(mhi)[3] <- "mhi"


##############   % Hispanic/Latino Population   ################################

# query Hispanic/Latino pop ACS SF data from postgresql database
    seq005_ca <- dbGetQuery(con, "SELECT * from seq005")
# join geoid table and seq005 table
    seq005_scag <- merge(geo_scag, seq005_ca, by="logrecno")
# get % Hispanic/Latino pop
    ethn <- seq005_scag[,c(3:4,10:12)]
    ethn$plat <- 100*(ethn$b03001_003/ethn$b03001_001)
    ethn$plat[ethn$plat == "NaN"] <- 0
    colnames(ethn)[3] <- "pop"
    colnames(ethn)[4] <- "nonhisp"
    colnames(ethn)[5] <- "hisp"

##############   % Population by Gender and Age  ###############################

# Age ACS SF data from postgresql database
    seq002_ca <- dbGetQuery(con, "SELECT * from seq002")
# join geoid table and seq002 table
    seq002_scag <- merge(geo_scag, seq002_ca, by="logrecno")
# get % female and senior pop
    gender <- seq002_scag[,c(3:4,10,35)]
    gender$pfemale <- 100*(gender$b01001_026/gender$b01001_001)
    gender <- gender[-c(3,4)]
    gender$pfemale[gender$pfemale == "NaN"] <- 0
    age <- seq002_scag[,c(3:4,10,29:34,53:58)]
    age$p65y <- 100*((age$b01001_020+age$b01001_021+age$b01001_022+
                      age$b01001_023+age$b01001_024+age$b01001_025+
                      age$b01001_044+age$b01001_045+age$b01001_046+
                      age$b01001_047+age$b01001_048+age$b01001_049)/
                      age$b01001_001)
    age$p65y[age$p65y == "NaN"] <- 0
    age <- age[-c(3:15)]



##############   Housing Tenure / Avg. Household Size  #########################

# Housing Unit ACS SF data from postgresql database
    seq103_ca <- dbGetQuery(con, "SELECT * from seq103")
# join geoid table and seq103 table
    seq103_scag <- merge(geo_scag, seq103_ca, by="logrecno")
# get % renter occupied and average HH size
    tenure <- seq103_scag[,c(3:4,14:16,106)]
    tenure$prenter <- 100*(tenure$b25003_003/tenure$b25003_001)
    tenure$prenter[tenure$prenter == "NaN"] <- 0
    tenure$hhsize <- tenure$b25010_000_5
    tenure <- tenure[-c(3:6)]



##############  walk/transit/bikescores by BG to tract  ########################
# Walk/transit/walkscores data from postgresql databse
    ws_bg <- dbGetQuery(con, "SELECT * from walkscore")
# create a column representing census tract
    ws_bg$tract <- paste("14",substr(ws_bg$geoid,3,18),sep="")
# get aggregated scores by tract (walk/transit/bikescores at tract level)
    ws_tr <- aggregate(ws_bg[,5:7], list(ws_bg$tract), mean)
# column name for tract id
    colnames(ws_tr)[1] <- "geoid"
# subset walkscore information for the SCAG area
    ws_tr_scag <- subset(ws_tr, substr(geoid,9,12)=="6025"|
                                substr(geoid,9,12)=="6037"|
                                substr(geoid,9,12)=="6065"|
                                substr(geoid,9,12)=="6059"|
                                substr(geoid,9,12)=="6071"|
                                substr(geoid,9,12)=="6111")
    colnames(ws_tr_scag)[2] <- "walkscore"
    colnames(ws_tr_scag)[3] <- "transitscore"
    colnames(ws_tr_scag)[4] <- "bikescore"


##############  land slop by cesus tract  ########################
# slope data from postgresql databse
    slope <- dbGetQuery(con, "SELECT * from slope")



# Merge all datasets

l1 <- merge(mot, mhi, by="geoid")
  l1 <- l1[-c(27)]
l2 <- merge(l1, ethn, by="geoid")
  l2 <- l2[-c(28:31)]
l3 <- merge(l2, gender, by="geoid")
  l3 <- l3[-c(29)]
l4 <- merge(l3, age, by="geoid")
  l4 <- l4[-c(30)]
l5 <- merge(l4, tenure, by="geoid")
  l5 <- l5[-c(31)]
l6 <- merge(l5, ws_tr_scag, by="geoid")
l7 <- merge(l6, slope, by="geoid")
dataset <- l7

write.table(dataset, "/home/dongwoo/Documents/census/acs.csv", sep="\t")




library(spdep)
library(maptools)


scag.poly <- readShapePoly("/home/dongwoo/Documents/gis/tract/ca/tl_2016_06_tract_scag_final_utm.shp")
proj4string(scag.poly) <- CRS("+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
scag.wgs <- spTransform(scag.poly, CRS("+init=epsg:4326"))


###########   Spatial Dependency   #############################################
list.queen<-poly2nb(scag.wgs, queen=TRUE)
W<-nb2listw(list.queen, style="W", zero.policy=TRUE)
W
plot(W,coordinates(scag.wgs))
coords<-coordinates(scag.wgs)
W_dist<-dnearneigh(coords,0,1,longlat = FALSE)




###########   SAR Model   ######################################################
# Car
sar.scag_car<-lagsarlm(acs_pcar~acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                       ,data=scag.wgs@data, W)
summary(sar.scag_car)

# Transit
sar.scag_transit<-lagsarlm(acs_ppt~acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                       ,data=scag.wgs@data, W)
summary(sar.scag_transit)

# Bicycle
sar.scag_bike<-lagsarlm(acs_pbc~acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                       ,data=scag.wgs@data, W)
summary(sar.scag_bike)

# Walk
sar.scag_walk<-lagsarlm(acs_pwk~acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                       ,data=scag.wgs@data, W)
summary(sar.scag_walk)

















cr <- cbind(dataset$car_n,dataset$car)
glm.scag_car<-glm(formula = cr~dataset$slope+
                                 dataset$walkscore+dataset$transitscore+
                                 dataset$mhi+dataset$plat+dataset$pfemale+dataset$p65y+
                                 dataset$prenter+dataset$hhsize, family = binomial)
summary(glm.scag_car)



trst <- cbind(dataset$transit_n,dataset$transit)
glm.scag_transit<-glm(formula = trst~dataset$slope+
                                     dataset$walkscore+dataset$transitscore+
                                     dataset$mhi+dataset$plat+dataset$pfemale+dataset$p65y+
                                     dataset$prenter+dataset$hhsize, family = binomial)
summary(glm.scag_transit)



bk <- cbind(dataset$bike_n,dataset$bike)
glm.scag_bike<-glm(formula = bk~dataset$slope+
                                 dataset$walkscore+dataset$transitscore+
                                 dataset$mhi+dataset$plat+dataset$pfemale+dataset$p65y+
                                 dataset$prenter+dataset$hhsize, family = binomial)
summary(glm.scag_bike)



wlk <- cbind(dataset$walk_n,dataset$walk)
glm.scag_walk<-glm(formula = wlk~dataset$slope+
                                 dataset$walkscore+dataset$transitscore+
                                 dataset$mhi+dataset$plat+dataset$pfemale+dataset$p65y+
                                 dataset$prenter+dataset$hhsize, family = binomial)
summary(glm.scag_walk)



motor <- cbind(dataset$motor_n, dataset$motor)
glm.scag_mt<-glm(formula = motor~dataset$slope+
                                  dataset$walkscore+dataset$transitscore+
                                  dataset$mhi+dataset$plat+dataset$pfemale+dataset$p65y+
                                  dataset$prenter+dataset$hhsize, family = binomial)
summary(glm.scag_mt)



nonmotor <- cbind(dataset$nonmotor_n,dataset$nonmotor)
glm.scag_nmt<-glm(formula = nonmotor~dataset$slope+
                                     dataset$walkscore+dataset$transitscore+
                                     dataset$mhi+dataset$plat+dataset$pfemale+dataset$p65y+
                                     dataset$prenter+dataset$hhsize, family = binomial)
summary(glm.scag_nmt)
