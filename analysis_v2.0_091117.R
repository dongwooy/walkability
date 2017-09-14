# install.packages("RPostgreSQL")
require("RPostgreSQL")

# create a connection
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "acs",
                 host = "localhost", port = 5432,
                 user = "postgres", password = '*********')


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
  # pmot1(% Car), pmot2(% Transit), pmot3(% Taxi)
  # pmot4(% Motorcycle), pmot5(% Bike), pmot6(% Walk)

    mot <- seq028_scag[,c(3:4,160:180)]
    for (i in c(4, 12, 18:21)) {
            a <- 100*(mot[,i]/mot[,3])
            a[a == "NaN"] <- 0
            mot <- cbind(mot,a)
        }
    for (j in 24:29) {
            colnames(mot)[j] <- paste("pmot",j-23,sep="")
    }


##############   Median Annual Household Income   ##############################

    mhi <- seq059_scag[,c(3:4,180)] # get median household income


##############   % Hispanic/Latino Population   ################################

    ethn <- seq005_scag[,c(3:4,10:12)]
    ethn$plat <- 100*(ethn$b03001_003/ethn$b03001_001)
    ethn$plat[ethn$plat == "NaN"] <- 0


##############   % Population by Gender and Age  ###############################

# get % female and senior pop
    gender <- seq002_scag[,c(3:4,1:58)]
    gender$pfemale <- 100*(gender$b01001_026/gender$b01001_001)
    gender$pfemale[gender$pfemale == "NaN"] <- 0
    age <- seq002_scag[,c(3:4,1:58)]
    age$p65y <- 100*((age$b01001_020+age$b01001_021+
                      age$b01001_022+age$b01001_023+age$b01001_025+
                      age$b01001_044+age$b01001_045+age$b01001_046+
                      age$b01001_047+age$b01001_048+age$b01001_049)/
                      age$b01001_001)
    age$p65y[age$p65y == "NaN"] <- 0



##############   Housing Tenure / Avg. Household Size  #########################

# get % renter occupied and average HH size
    tenure <- seq103_scag[,c(3:4,14:16,106)]
    tenure$prenter <- 100*(tenure$b25003_003/tenure$b25003_001)
    tenure$prenter[tenure$prenter == "NaN"] <- 0
    tenure$hhsize <- tenure$b25010_000_5



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


##############  land slop by cesus tract  ########################
# slope data from postgresql databse
    slope <- dbGetQuery(con, "SELECT * from slope")



# Merge all datasets

l1 <- merge(mot, mhi, by="geoid")
l2 <- merge(l1, ethn, by="geoid")
l3 <- merge(l2, gender, by="geoid")
l4 <- merge(l3, age, by="geoid")
l5 <- merge(l4, tenure, by="geoid")
l6 <- merge(l5, ws_tr_scag, by="geoid")
l7 <- merge(l6, slope, by="geoid")
dataset <- l7[,c(1:2,24:29,31,36,96,156,161:163,164:166,169)]





#############################    SCAG Region    #############################
library(spdep)
library(maptools)

scag.poly <- readShapePoly("/home/path/tl_2016_06_tract_scag_final_utm.shp")
proj4string(scag.poly) <- CRS("+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
scag.wgs <- spTransform(scag.poly, CRS("+init=epsg:4326"))

##############################################################################################
#library(leaflet)                                                                            #
#leaflet(scag.wgs)  %>%                                                                      #
#  addPolygons(stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5) %>%                    #
#  addTiles()                                                                                #
#                                                                                            #
#equire(RColorBrewer)                                                                        #
#pal<-colorQuantile("OrRd", scag.wgs@data$allscore_2, n=8)                                   #
#eaflet(scag.wgs) %>%                                                                        #
# addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal(allscore_2)#
# ) %>%                                                                                      #
#  addTiles()                                                                                #
##############################################################################################

###########   Spatial Dependency   #############################################
list.queen<-poly2nb(scag.wgs, queen=TRUE)
W<-nb2listw(list.queen, style="W", zero.policy=TRUE)
W
#plot(W,coordinates(scag.wgs))
#coords<-coordinates(scag.wgs)
#W_dist<-dnearneigh(coords,0,1,longlat = FALSE)


###########   SAR Model   ######################################################
# Car
#sar.scag_car<-lagsarlm(acs_pcar~acs_slope+
#                                acs_walksc+acs_tran_2+
#                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
#                        ,data=scag.wgs@data, W)
#summary(sar.scag_car)
scag.wgs@data$lag_rate_c <- lag.listw(x=W, var=(scag.wgs@data$acs_car/(scag.wgs@data$acs_mot_to+0.00001)))
sar.scag_car<-glm((acs_car/acs_mot_to)~lag_rate_c+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=scag.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.scag_car)


# Transit
#sar.scag_transit<-lagsarlm(acs_ppt~acs_slope+
#                                acs_walksc+acs_tran_2+
#                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
#                        ,data=scag.wgs@data, W)
#summary(sar.scag_transit)
scag.wgs@data$lag_rate_t <- lag.listw(x=W, var=(scag.wgs@data$acs_transi/(scag.wgs@data$acs_mot_to+0.00001)))
sar.scag_transit<-glm((acs_transi/acs_mot_to)~lag_rate_t+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=scag.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.scag_transit)

# Bicycle
#sar.scag_bike<-lagsarlm(acs_pbc~acs_slope+
#                                acs_walksc+acs_tran_2+
#                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
#                       ,data=scag.wgs@data, W)
#summary(sar.scag_bike)
scag.wgs@data$lag_rate_b <- lag.listw(x=W, var=(scag.wgs@data$acs_bike/(scag.wgs@data$acs_mot_to+0.00001)))
sar.scag_bike<-glm((acs_bike/acs_mot_to)~lag_rate_b+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=scag.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.scag_bike)

# Walk
#sar.scag_walk<-lagsarlm(acs_pwk~acs_slope+
#                                acs_walksc+acs_tran_2+
#                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
#                       ,data=scag.wgs@data, W)
#summary(sar.scag_walk)
scag.wgs@data$lag_rate_w <- lag.listw(x=W, var=(scag.wgs@data$acs_walk/(scag.wgs@data$acs_mot_to+0.00001)))
sar.scag_walk<-glm((acs_walk/acs_mot_to)~lag_rate_w+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=scag.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.scag_walk)



# Non-motorized
scag.wgs@data$lag_rate_nm <- lag.listw(x=W, var=((scag.wgs@data$acs_walk+scag.wgs@data$acs_bike)/(scag.wgs@data$acs_mot_to+0.00001)))
sar.scag_nonmotor<-glm(((acs_walk+acs_bike)/acs_mot_to)~lag_rate_nm+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=scag.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.scag_nonmotor)






# Bike vs Car
scag.wgs@data$lag_rate_bc <- lag.listw(x=W, var=((scag.wgs@data$acs_car+scag.wgs@data$acs_bike)/(scag.wgs@data$acs_mot_to+0.00001)))
sar.scag_bikecar<-glm(((acs_bike)/(acs_car+acs_bike))~lag_rate_cb+acs_slope+
                         acs_walksc+acs_tran_2+
                         acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                       ,data=scag.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.scag_bikecar)

# Bike vs Transit
scag.wgs@data$lag_rate_bt <- lag.listw(x=W, var=((scag.wgs@data$acs_transi+scag.wgs@data$acs_bike)/(scag.wgs@data$acs_mot_to+0.00001)))
sar.scag_biketransit<-glm(((acs_bike)/(acs_transi+acs_bike))~lag_rate_bt+acs_slope+
                        acs_walksc+acs_tran_2+
                        acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                      ,data=scag.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.scag_biketransit)


# Bike vs Walk
scag.wgs@data$lag_rate_bw <- lag.listw(x=W, var=((scag.wgs@data$acs_walk+scag.wgs@data$acs_bike)/(scag.wgs@data$acs_mot_to+0.00001)))
sar.scag_bikewalk<-glm(((acs_bike)/(acs_walk+acs_bike))~lag_rate_bw+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=scag.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.scag_bikewalk)




# Walk vs Transit
scag.wgs@data$lag_rate_wt <- lag.listw(x=W, var=((scag.wgs@data$acs_walk+scag.wgs@data$acs_transi)/(scag.wgs@data$acs_mot_to+0.00001)))
sar.scag_walktransit<-glm(((acs_walk)/(acs_walk+acs_transi))~lag_rate_wt+acs_slope+
                         acs_walksc+acs_tran_2+
                         acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                       ,data=scag.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.scag_walktransit)


# Walk vs Car
scag.wgs@data$lag_rate_wc <- lag.listw(x=W, var=((scag.wgs@data$acs_walk+scag.wgs@data$acs_car)/(scag.wgs@data$acs_mot_to+0.00001)))
sar.scag_walkcar<-glm(((acs_walk)/(acs_walk+acs_car))~lag_rate_wc+acs_slope+
                            acs_walksc+acs_tran_2+
                            acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                          ,data=scag.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.scag_walkcar)






# Car vs Transit
scag.wgs@data$lag_rate_ct <- lag.listw(x=W, var=((scag.wgs@data$acs_car+scag.wgs@data$acs_transi)/(scag.wgs@data$acs_mot_to+0.00001)))
sar.scag_cartransit<-glm(((acs_car)/(acs_car+acs_transi))~lag_rate_ct+acs_slope+
                            acs_walksc+acs_tran_2+
                            acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                          ,data=scag.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.scag_cartransit)
