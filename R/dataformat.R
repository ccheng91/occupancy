## making long data format 
library(lubridate)
###

rm(list=ls(all=TRUE))
data.photo <- read.csv("data/All_photo.csv", header=TRUE)
SE_date <- read.csv("data/start_end_date.csv", header = TRUE, stringsAsFactors=FALSE) # enter start end date
SE_date$NO <- tolower(SE_date$NO) # change camera name to lower case 
str(SE_date)
# data.long <- data.frame(station=character(), year=double(), month=double(),days=double(),individuals=double(),
#                        camhours=double(),stringsAsFactors=FALSE)

## making first col(station col) of datalong 
sumcam<-cumsum(SE_date$Cam_days) # cumsum give you accumulate sum of days, to indicate where to add next station name 
data.long <- matrix(999, sum(SE_date$Cam_days), 6, byrow=T) # make a matrix fill with 999
k <- 1 # initial k 
for(i in 1:length(data.long[,1])) {
  if(i < sumcam[k]){ # if sumcam > i, start to add next station name
    data.long[i,1] <- SE_date$NO[k]
      } else {
        data.long[i,1] <- SE_date$NO[k]
        k <- k+1 
      }
    }
# make a vector that contain all date 
all.date <-data.frame() # make a empty data.frame
for(i in 1:length(SE_date[,1])) { 
  c.date <-data.frame(seq(as.Date(toString(SE_date$START_DATE[i])), as.Date(toString(SE_date$END_DATE[i])), by = "day"))
  all.date <- dplyr::bind_rows(all.date, c.date ) # adding dates to empty data frame
}
names(all.date)<-c("date")
all.date <- as.Date(all.date$date)

# make 2,3,4 col of long data, basically seprate year,month,day from a date
for(i in 1:length(data.long[,1])) {  
  data.long[i,2] <- year(all.date[i])
  data.long[i,3] <- month(all.date[i])
  data.long[i,4] <- day(all.date[i])
}

#### data for individual 
data.wildboar <- dplyr::filter(data.photo, species == "leopardcat") # filter one spp 
df.dl <- as.data.frame(data.long, stringsAsFactors=FALSE)
df.dl$ymd.dt <- all.date # need a new variable to match
names(df.dl)[1:6] <- c("station","year","month","day","individuals","camhours")
df.dl$individuals <- 0 
datetime.x <- as.Date(data.wildboar$datetime)
data.wildboar <- cbind(data.wildboar,datetime.x)
#‘data.wildboar <- ddply(data.wildboar, .(camera, datetime.x) , summarize, n = max(n))
data.wbsum <- aggregate(n ~ camera + datetime.x, FUN = max, data = data.wildboar) # funtion chose max/sum depends
data.wbsum$camera <- as.character(data.wbsum$camera)
nrow(data.wbsum)
data.wildboar <- data.wbsum

# if camera & date all matched then write in indivadule number
for(i in 1:nrow(data.wildboar)){
  index <- which(df.dl$ymd.dt == data.wildboar$datetime.x[i] & df.dl$station == data.wildboar$camera[i])
#  if(df.dl$individuals[index] != 0) {
#    df.dl$individuals[index] <- df.dl$individuals[index] + data.wildboar$n[i]
#  } else {
#  }
   df.dl$individuals[index] <- data.wildboar$n[i]
}

sum(data.wildboar$n) # to check whether have right indivdule numbers
sum(df.dl$individuals)#

## fill in camhours 
df.dl$camhours <- 24
for(i in 1:nrow(SE_date)) {
  ds <- which(df.dl$ymd.dt == SE_date$START_DATE[i] & df.dl$station == SE_date$NO[i])
  df.dl$camhours[ds] <- SE_date$DAY1[i]
  de <- which(df.dl$ymd.dt == SE_date$END_DATE[i] & df.dl$station == SE_date$NO[i])
  df.dl$camhours[de] <- SE_date$END_TIME[i]
}

## checking 
sum(df.dl$camhours)-((nrow(df.dl) - (nrow(SE_date)*2))*24)
sum(SE_date$DAY1)+sum(SE_date$END_TIME)
sum(df.dl$camhours)
## enter the intervals for BLS1005 & BLS 3005 if there are lots of interval need re-write 
c2 <- which(df.dl$ymd.dt >= as.Date("2014-05-29") & df.dl$ymd.dt <= as.Date("2014-06-30") & df.dl$station == "bls3005")
inters <- which(df.dl$ymd.dt == as.Date("2014-05-29") & df.dl$station == "bls3005")
intere <- which(df.dl$ymd.dt == as.Date("2014-06-30") & df.dl$station == "bls3005")
df.dl$camhours[c2] <- 0
df.dl$camhours[inters] <- 16.5
df.dl$camhours[intere] <- 19.5

### remove redundance and write long data sheet ###
df.dl$ymd.dt <- NULL

write.csv(df.dl, file="data/leopardcat_long.csv", row.names = FALSE )









###### jsut for checking  dont run #####

#if(df.dl$individuals[2222] == 0) {print("yes") 
#} else {
#  print("no")
#}

sum(df.dl$camhours)
#df.dl$ymd.dt[1]

#sum(data.wildboar$n)
#sum(df.dl$individuals)

#which(df.dl$ymd.dt == data.wildboar$datetime.x[289] & df.dl$station == data.wildboar$camera[289])
#data.wildboar[289,]
#df.dl$individuals[1135] 
#which(data.wildboar$camera == "ml3001")

#data.wildboar[245,]
#range(df.dl$ymd.dt[df.dl$station == "ms5004"])
#ml1018
#bls2006
#ml3001
#ms4002
#ms5004: 2011-05-21
#q <- c(110, 114,122,134,135,141,142,143,145,152)#

#for(k in q) {
#  dd <- which(df.dl$ymd.dt == data.wildboar$datetime.x[k] & df.dl$station == data.wildboar$camera[k])
#  print(k)
#  print(dd)
#}#

#for(k in q) {
#  print(data.wildboar[k,])
#}

#which(data.wildboar$camera == "bls3006")
#data.wildboar[145,]

### cheking ###







