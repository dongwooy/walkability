install.packages("rvest")
install.packages("stringr")
install.packages("RDSTK")

library(stringr)
library(rvest)
library(XML)
library(RDSTK)

add<-read.table("//home//dongwoo//Desktop//address2.csv", header=TRUE)
add$score<-NA
#add$fadd<-NA
#add$lat<-0
#add$lon<-0



for (i in (3430:nrow(add))) {
  tryCatch({
  a<-paste("http://www.walkscore.com/score/", as.character(add[i,1]), sep="")
  walkscore <- read_html(a, timeout=200)
  b <- html_nodes(walkscore, "img")
  c <- html_attr(b, "src")
  c1 <- c[grep("//pp.walk.sc/badge/bike/",c)]
  c2 <- gsub("//pp.walk.sc/badge/bike/score/","",c1)
  c3 <- c2[1]
  c4 <- gsub(".svg","",c3)
  c5 <- as.numeric(c4)
  add[i,2]<-c5
  },error=function(e){}
)
}


write.table(add, "/home/dongwoo/Desktop/bikescore_ca2.csv", sep="\t")
