install.packages("ctv", repos="http://healthstat.snu.ac.kr/CRAN")
library(ctv)
install.views("Spatial", repos="http://healthstat.snu.ac.kr/CRAN")

library(maptools)
library(rgdal)
library(spdep)




getinfo.shape("/home/dongwoo/Documents/gis/tract/ca/tl_2016_06_tract_scag_utm.shp")
sids <- readShapePoly("/home/dongwoo/Documents/gis/tract/ca/tl_2016_06_tract_scag_utm.shp")
class(sids)


sids_nbr <- poly2nb(sids, queen=FALSE)
coords<-coordinates(sids)
plot(sids)
plot(sids_nbq, coords, add=T)
