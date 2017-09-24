#############################    SCAG Region    #############################
library(spdep)
library(maptools)

scag.poly <- readShapePoly("/home/dongwoo/Documents/gis/tract/ca/tl_2016_06_tract_scag_final_utm.shp")
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













###########   SAR Model -LA County  ######################################################
library(spdep)
library(maptools)

la.poly <- readShapePoly("/home/dongwoo/Documents/gis/tract/ca/tl_2016_06_tract_scag_final_utm_la.shp")
proj4string(la.poly) <- CRS("+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
la.wgs <- spTransform(la.poly, CRS("+init=epsg:4326"))

###########   Spatial Dependency   #############################################
list.queen<-poly2nb(la.wgs, queen=TRUE)
W<-nb2listw(list.queen, style="W", zero.policy=TRUE)

# Car
la.wgs@data$lag_rate_c <- lag.listw(x=W, var=(la.wgs@data$acs_car/(la.wgs@data$acs_mot_to+0.00001)))
sar.la_car<-glm((acs_car/acs_mot_to)~lag_rate_c+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=la.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.la_car)

# Transit
la.wgs@data$lag_rate_t <- lag.listw(x=W, var=(la.wgs@data$acs_transi/(la.wgs@data$acs_mot_to+0.00001)))
sar.la_transit<-glm((acs_transi/acs_mot_to)~lag_rate_t+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=la.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.la_transit)

# Bicycle
la.wgs@data$lag_rate_b <- lag.listw(x=W, var=(la.wgs@data$acs_bike/(la.wgs@data$acs_mot_to+0.00001)))
sar.la_bike<-glm((acs_bike/acs_mot_to)~lag_rate_b+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=la.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.la_bike)

# Walk
la.wgs@data$lag_rate_w <- lag.listw(x=W, var=(la.wgs@data$acs_walk/(la.wgs@data$acs_mot_to+0.00001)))
sar.la_walk<-glm((acs_walk/acs_mot_to)~lag_rate_w+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=la.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.la_walk)


# Non-motorized
la.wgs@data$lag_rate_nm <- lag.listw(x=W, var=((la.wgs@data$acs_walk+la.wgs@data$acs_bike)/(la.wgs@data$acs_mot_to+0.00001)))
sar.la_nonmotor<-glm(((acs_walk+acs_bike)/acs_mot_to)~lag_rate_nm+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=la.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.la_nonmotor)



































###########   SAR Model -Orange County  ######################################################
library(spdep)
library(maptools)

oc.poly <- readShapePoly("/home/dongwoo/Documents/gis/tract/ca/tl_2016_06_tract_scag_final_utm_oc.shp")
proj4string(oc.poly) <- CRS("+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
oc.wgs <- spTransform(oc.poly, CRS("+init=epsg:4326"))


###########   Spatial Dependency   #############################################
list.queen<-poly2nb(oc.wgs, queen=TRUE)
W<-nb2listw(list.queen, style="W", zero.policy=TRUE)

# Car
oc.wgs@data$lag_rate_c <- lag.listw(x=W, var=(oc.wgs@data$acs_car/(oc.wgs@data$acs_mot_to+0.00001)))
sar.oc_car<-glm((acs_car/acs_mot_to)~lag_rate_c+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=oc.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.oc_car)

# Transit
oc.wgs@data$lag_rate_t <- lag.listw(x=W, var=(oc.wgs@data$acs_transi/(oc.wgs@data$acs_mot_to+0.00001)))
sar.oc_transit<-glm((acs_transi/acs_mot_to)~lag_rate_t+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=oc.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.oc_transit)

# Bicycle
oc.wgs@data$lag_rate_b <- lag.listw(x=W, var=(oc.wgs@data$acs_bike/(oc.wgs@data$acs_mot_to+0.00001)))
sar.oc_bike<-glm((acs_bike/acs_mot_to)~lag_rate_b+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=oc.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.oc_bike)

# Walk
oc.wgs@data$lag_rate_w <- lag.listw(x=W, var=(oc.wgs@data$acs_walk/(oc.wgs@data$acs_mot_to+0.00001)))
sar.oc_walk<-glm((acs_walk/acs_mot_to)~lag_rate_w+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=oc.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.oc_walk)


# Non-motorized
oc.wgs@data$lag_rate_nm <- lag.listw(x=W, var=((oc.wgs@data$acs_walk+oc.wgs@data$acs_bike)/(oc.wgs@data$acs_mot_to+0.00001)))
sar.oc_nonmotor<-glm(((acs_walk+acs_bike)/acs_mot_to)~lag_rate_nm+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=oc.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.oc_nonmotor)


































###########   SAR Model -Riverside County  ######################################################
library(spdep)
library(maptools)

rs.poly <- readShapePoly("/home/dongwoo/Documents/gis/tract/ca/tl_2016_06_tract_scag_final_utm_rs.shp")
proj4string(rs.poly) <- CRS("+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
rs.wgs <- spTransform(rs.poly, CRS("+init=epsg:4326"))


###########   Spatial Dependency   #############################################
list.queen<-poly2nb(rs.wgs, queen=TRUE)
W<-nb2listw(list.queen, style="W", zero.policy=TRUE)

# Car
rs.wgs@data$lag_rate_c <- lag.listw(x=W, var=(rs.wgs@data$acs_car/(rs.wgs@data$acs_mot_to+0.00001)))
sar.rs_car<-glm((acs_car/acs_mot_to)~lag_rate_c+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=rs.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.rs_car)

# Transit
rs.wgs@data$lag_rate_t <- lag.listw(x=W, var=(rs.wgs@data$acs_transi/(rs.wgs@data$acs_mot_to+0.00001)))
sar.rs_transit<-glm((acs_transi/acs_mot_to)~lag_rate_t+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=rs.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.rs_transit)

# Bicycle
rs.wgs@data$lag_rate_b <- lag.listw(x=W, var=(rs.wgs@data$acs_bike/(rs.wgs@data$acs_mot_to+0.00001)))
sar.rs_bike<-glm((acs_bike/acs_mot_to)~lag_rate_b+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=rs.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.rs_bike)

# Walk
rs.wgs@data$lag_rate_w <- lag.listw(x=W, var=(rs.wgs@data$acs_walk/(rs.wgs@data$acs_mot_to+0.00001)))
sar.rs_walk<-glm((acs_walk/acs_mot_to)~lag_rate_w+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=rs.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.rs_walk)


# Non-motorized
rs.wgs@data$lag_rate_nm <- lag.listw(x=W, var=((rs.wgs@data$acs_walk+rs.wgs@data$acs_bike)/(rs.wgs@data$acs_mot_to+0.00001)))
sar.rs_nonmotor<-glm(((acs_walk+acs_bike)/acs_mot_to)~lag_rate_nm+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=rs.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.rs_nonmotor)


































###########   SAR Model -San Bernardino County  ######################################################
library(spdep)
library(maptools)

sb.poly <- readShapePoly("/home/dongwoo/Documents/gis/tract/ca/tl_2016_06_tract_scag_final_utm_sb.shp")
proj4string(sb.poly) <- CRS("+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
sb.wgs <- spTransform(sb.poly, CRS("+init=epsg:4326"))


###########   Spatial Dependency   #############################################
list.queen<-poly2nb(sb.wgs, queen=TRUE)
W<-nb2listw(list.queen, style="W", zero.policy=TRUE)

# Car
sb.wgs@data$lag_rate_c <- lag.listw(x=W, var=(sb.wgs@data$acs_car/(sb.wgs@data$acs_mot_to+0.00001)))
sar.sb_car<-glm((acs_car/acs_mot_to)~lag_rate_c+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=sb.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.sb_car)

# Transit
sb.wgs@data$lag_rate_t <- lag.listw(x=W, var=(sb.wgs@data$acs_transi/(sb.wgs@data$acs_mot_to+0.00001)))
sar.sb_transit<-glm((acs_transi/acs_mot_to)~lag_rate_t+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=sb.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.sb_transit)

# Bicycle
sb.wgs@data$lag_rate_b <- lag.listw(x=W, var=(sb.wgs@data$acs_bike/(sb.wgs@data$acs_mot_to+0.00001)))
sar.sb_bike<-glm((acs_bike/acs_mot_to)~lag_rate_b+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=sb.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.sb_bike)

# Walk
sb.wgs@data$lag_rate_w <- lag.listw(x=W, var=(sb.wgs@data$acs_walk/(sb.wgs@data$acs_mot_to+0.00001)))
sar.sb_walk<-glm((acs_walk/acs_mot_to)~lag_rate_w+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=sb.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.sb_walk)


# Non-motorized
sb.wgs@data$lag_rate_nm <- lag.listw(x=W, var=((sb.wgs@data$acs_walk+sb.wgs@data$acs_bike)/(sb.wgs@data$acs_mot_to+0.00001)))
sar.sb_nonmotor<-glm(((acs_walk+acs_bike)/acs_mot_to)~lag_rate_nm+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=sb.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.sb_nonmotor)






































###########   SAR Model -Ventura County  ######################################################
library(spdep)
library(maptools)

vt.poly <- readShapePoly("/home/dongwoo/Documents/gis/tract/ca/tl_2016_06_tract_scag_final_utm_vt.shp")
proj4string(vt.poly) <- CRS("+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
vt.wgs <- spTransform(vt.poly, CRS("+init=epsg:4326"))


###########   Spatial Dependency   #############################################
list.queen<-poly2nb(vt.wgs, queen=TRUE)
W<-nb2listw(list.queen, style="W", zero.policy=TRUE)
plot(W,coordinates(vt.wgs))
# Car
vt.wgs@data$lag_rate_c <- lag.listw(x=W, var=(vt.wgs@data$acs_car/(vt.wgs@data$acs_mot_to+0.00001)))
sar.vt_car<-glm((acs_car/acs_mot_to)~lag_rate_c+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=vt.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.vt_car)

# Transit
vt.wgs@data$lag_rate_t <- lag.listw(x=W, var=(vt.wgs@data$acs_transi/(vt.wgs@data$acs_mot_to+0.00001)))
sar.vt_transit<-glm((acs_transi/acs_mot_to)~lag_rate_t+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=vt.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.vt_transit)

# Bicycle
vt.wgs@data$lag_rate_b <- lag.listw(x=W, var=(vt.wgs@data$acs_bike/(vt.wgs@data$acs_mot_to+0.00001)))
sar.vt_bike<-glm((acs_bike/acs_mot_to)~lag_rate_b+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=vt.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.vt_bike)

# Walk
vt.wgs@data$lag_rate_w <- lag.listw(x=W, var=(vt.wgs@data$acs_walk/(vt.wgs@data$acs_mot_to+0.00001)))
sar.vt_walk<-glm((acs_walk/acs_mot_to)~lag_rate_w+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=vt.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.vt_walk)


# Non-motorized
vt.wgs@data$lag_rate_nm <- lag.listw(x=W, var=((vt.wgs@data$acs_walk+vt.wgs@data$acs_bike)/(vt.wgs@data$acs_mot_to+0.00001)))
sar.vt_nonmotor<-glm(((acs_walk+acs_bike)/acs_mot_to)~lag_rate_nm+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=vt.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.vt_nonmotor)
































###########   SAR Model -Imperial County  ######################################################
library(spdep)
library(maptools)

im.poly <- readShapePoly("/home/dongwoo/Documents/gis/tract/ca/tl_2016_06_tract_scag_final_utm_im.shp")
proj4string(im.poly) <- CRS("+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
im.wgs <- spTransform(im.poly, CRS("+init=epsg:4326"))


###########   Spatial Dependency   #############################################
list.queen<-poly2nb(im.wgs, queen=TRUE)
W<-nb2listw(list.queen, style="W", zero.policy=TRUE)
plot(W,coordinates(im.wgs))
# Car
im.wgs@data$lag_rate_c <- lag.listw(x=W, var=(im.wgs@data$acs_car/(im.wgs@data$acs_mot_to+0.00001)))
sar.im_car<-glm((acs_car/acs_mot_to)~lag_rate_c+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=im.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.im_car)

# Transit
im.wgs@data$lag_rate_t <- lag.listw(x=W, var=(im.wgs@data$acs_transi/(im.wgs@data$acs_mot_to+0.00001)))
sar.im_transit<-glm((acs_transi/acs_mot_to)~lag_rate_t+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=im.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.im_transit)

# Bicycle
im.wgs@data$lag_rate_b <- lag.listw(x=W, var=(im.wgs@data$acs_bike/(im.wgs@data$acs_mot_to+0.00001)))
sar.im_bike<-glm((acs_bike/acs_mot_to)~lag_rate_b+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=im.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.im_bike)

# Walk
im.wgs@data$lag_rate_w <- lag.listw(x=W, var=(im.wgs@data$acs_walk/(im.wgs@data$acs_mot_to+0.00001)))
sar.im_walk<-glm((acs_walk/acs_mot_to)~lag_rate_w+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=im.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.im_walk)


# Non-motorized
im.wgs@data$lag_rate_nm <- lag.listw(x=W, var=((im.wgs@data$acs_walk+im.wgs@data$acs_bike)/(im.wgs@data$acs_mot_to+0.00001)))
sar.im_nonmotor<-glm(((acs_walk+acs_bike)/acs_mot_to)~lag_rate_nm+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=im.wgs@data, weights=acs_mot_to, family = binomial)
summary(sar.im_nonmotor)

































###########   SAR Model -SCAG /w tracts <20,000acres  ######################################################
library(spdep)
library(maptools)

scag2.poly <- readShapePoly("/home/dongwoo/Documents/gis/tract/ca/tl_2016_06_tract_scag_final_urban.shp")
proj4string(scag2.poly) <- CRS("+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
scag2.wgs <- spTransform(scag2.poly, CRS("+init=epsg:4326"))


###########   Spatial Dependency   #############################################
list.queen<-poly2nb(scag2.wgs, queen=TRUE)
W<-nb2listw(list.queen, style="W", zero.policy=TRUE)
plot(W,coordinates(scag2.wgs))
# Car
scag2.wgs@data$lag_rate_c <- lag.listw(x=W, var=(scag2.wgs@data$acs_car/(scag2.wgs@data$acs_mot_to+0.00001)))
sar.scag2_car<-glm((acs_transi/acs_mot_to)~lag_rate_c+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=scag2.wgs@data, weights=acs_mot_to, family=binomial)
summary(sar.scag2_car)
# Transit
scag2.wgs@data$lag_rate_t <- lag.listw(x=W, var=(scag2.wgs@data$acs_transi/(scag2.wgs@data$acs_mot_to+0.00001)))
sar.scag2_transit<-glm((acs_transi/acs_mot_to)~lag_rate_t+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=scag2.wgs@data, weights=acs_mot_to, family=binomial)
summary(sar.scag2_transit)

# Bicycle
scag2.wgs@data$lag_rate_b <- lag.listw(x=W, var=(scag2.wgs@data$acs_bike/(scag2.wgs@data$acs_mot_to+0.00001)))
sar.scag2_bike<-glm((acs_bike/acs_mot_to)~lag_rate_b+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=scag2.wgs@data, weights=acs_mot_to, family=binomial)
summary(sar.scag2_bike)

# Walk
scag2.wgs@data$lag_rate_w <- lag.listw(x=W, var=(scag2.wgs@data$acs_walk/(scag2.wgs@data$acs_mot_to+0.00001)))
sar.scag2_walk<-glm((acs_walk/acs_mot_to)~lag_rate_w+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=scag2.wgs@data, weights=acs_mot_to, family=binomial)
summary(sar.scag2_walk)


# Non-motorized
scag2.wgs@data$lag_rate_nm <- lag.listw(x=W, var=((scag2.wgs@data$acs_walk+scag2.wgs@data$acs_bike)/(scag2.wgs@data$acs_mot_to+0.00001)))
sar.scag2_nonmotor<-glm(((acs_walk+acs_bike)/acs_mot_to)~lag_rate_nm+acs_slope+
                                acs_walksc+acs_tran_2+
                                acs_mhi+acs_plat+acs_pfemal+acs_p65y+acs_prente+acs_hhsize
                        ,data=scag2.wgs@data, weights=acs_mot_to, family=binomial)
summary(sar.scag2_nonmotor)



















# Bike vs Car
scag.wgs@data$lag_rate_bc <- lag.listw(x=W, var=((scag.wgs@data$acs_car+scag.wgs@data$acs_bike)/(scag.wgs@data$acs_mot_to+0.00001)))
sar.scag_bikecar<-glm(((acs_bike)/(acs_car+acs_bike))~lag_rate_bc+acs_slope+
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
