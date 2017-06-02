install.packages("rvest")
install.packages("stringr")
install.packages("RDSTK")

library(stringr)
library(rvest)
library(XML)
library(RDSTK)

add<-read.table("paths of a file", header=TRUE)
add$score<-NA
#add$fadd<-NA
#add$lat<-0
#add$lon<-0



for (i in (1:nrow(add))) {
  tryCatch({
  a<-paste("http://www.walkscore.com/score/", as.character(add[i,1]), sep="")
  walkscore<-read_html(a, timeout=200)
  scores <- xml_find_all(walkscore, "//span[contains(string(.), 'Walk Score of')]", xml_ns(walkscore))
  b<-html_text(scores)
  b1<-gsub("[[:alpha:]]", "", b)
  b2<-gsub("100", "", b1)
  b3<-gsub("[[:punct:]]", "", b2)
  b4<-gsub("\\s", "", b3)
  b5<-str_sub(b4, -2)
  b6<-as.numeric(b5)
#  c1<-gsub("[[:punct:]]"," ",add[i,1])
  add[i,2]<-b6
#  add[i,3]<-c1
#  latlong<- street2coordinates(c1)
#  d1<-as.numeric(latlong[1,3])
#  d2<-as.numeric(latlong[1,5])
#  add[i,4]<-d1
#  add[i,5]<-d2
  },error=function(e){}
)
}


write.table(add, "path of a file", sep="\t")


library(sp)
cran_mat <- cbind(add$lon, add$lat)
llcrs<-CRS("+proj=longlat +ellps=WGS84")
ws_sp<-SpatialPoints(cran_mat, proj4string=llcrs)
ws_spdf<-SpatialPointsDataFrame(ws_sp,add,proj4string=llcrs,match.ID=TRUE)
