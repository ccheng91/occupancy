### accumlative curve of camdays
library(vegan)
##
rm(list=ls(all=TRUE))
data.photo <- read.csv("data/All_photo.csv")
str(data.photo)
data.photo <- dplyr::filter(data.photo, data.photo$species != "bat" & data.photo$species != "commongreenmagpie" & 
                              data.photo$species != "greateryellownape"& data.photo$species !="greenmagpie"& 
                              data.photo$species!="treeshrew"& data.photo$species != "junglefowl"& data.photo$species!="silverpheasant"& 
                              data.photo$species!="squirrel"& data.photo$species!="bird" & 
                              data.photo$species!="rat" & data.photo$species != "unknown" & data.photo$species != "human" )

data.photo <- data.photo[which(data.photo$species != "bat" & data.photo$species != "commongreenmagpie" & 
                                 data.photo$species != "greateryellownape"& data.photo$species !="greenmagpie"& 
                                 data.photo$species!="treeshrew"& data.photo$species != "junglefowl"& data.photo$species!="silverpheasant"& 
                                 data.photo$species!="squirrel"& data.photo$species!="bird" & 
                                 data.photo$species!="rat" & data.photo$species != "unknown" & data.photo$species != "human"), ]

## richness of each site 
list <- as.data.frame.matrix(table(data.photo$site,data.photo$species))
list <- list[, colSums(list) != 0] ### delet col that are all zero 
list <- subset( list, select = -human2 )
list <- subset( list, select = -hunter )
list <- subset( list, select = -watermonitor )
list <- subset( list, select = -dog )
list <- subset( list, select = -cattle )
allmammal <- colnames(list)

############# make long data for all mammal ##############

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
data.wildboar <- dplyr::filter(data.photo, species == "guar") # filter one spp 
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

write.csv(df.dl, file="data/guar_long.csv", row.names = FALSE )
allmammal

#########
spp1 <- read.csv("data/blackbear_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp2 <- read.csv("data/brushtailedporcupine_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp3 <- read.csv("data/chineseferretbadger_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp4 <- read.csv("data/commonmacaque_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp5 <- read.csv("data/commonpalmcivet_long.csv", header = TRUE, stringsAsFactors=FALSE)

spp6 <- read.csv("data/crabeatingmongoose_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp7 <- read.csv("data/dhole_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp8 <- read.csv("data/goral_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp9 <- read.csv("data/guar_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp10 <- read.csv("data/hogbadger_long.csv", header = TRUE, stringsAsFactors=FALSE)

spp11 <- read.csv("data/leopardcat_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp12 <- read.csv("data/maskedpalmcivet_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp13 <- read.csv("data/muntjac_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp14 <- read.csv("data/pigtailedmacaque_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp15 <- read.csv("data/porcupine_long.csv", header = TRUE, stringsAsFactors=FALSE)

spp16 <- read.csv("data/sambar_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp17 <- read.csv("data/serow_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp18 <- read.csv("data/smallindiancivet_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp19 <- read.csv("data/spotedlinsang_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp20 <- read.csv("data/weasel_long.csv", header = TRUE, stringsAsFactors=FALSE)

spp21 <- read.csv("data/wildboar_long.csv", header = TRUE, stringsAsFactors=FALSE)
spp22 <- read.csv("data/yellowthroatedmarten_long.csv", header = TRUE, stringsAsFactors=FALSE)

long_list <- list(spp1,spp2,spp3,spp4,spp5,spp6,spp7,spp8,spp9,spp10,spp11,spp12,spp13,spp14,spp15,spp16
                  ,spp17,spp18,spp19,spp21,spp22)

richness <- data.frame(1:12296)
for(i in long_list) {
  a <- data.frame(i[,5])
  richness <- dplyr::bind_cols(richness, a) #add cols each col is indivadual for each spp
}
colnames(richness) <-allmammal
richness[,1] <- spp1[,5]
 
#data(BCI)

data(BCI)
S <- specnumber(BCI) # observed number of species
(raremax <- min(rowSums(BCI)))
Srare <- rarefy(BCI, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)
spa <- specaccum(richness, method = "rarefaction")
#data(BCI)

raremax<-min(rowSums((richness)))
Srare <- rarefy(richness, raremax)
rarecurve(richness)
head(richness)
plot(n,Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
sp1 <- specaccum(richness)
sp2 <- specaccum(richness, "random")
#sp2
summary(sp2)
n <- specnumber(richness)

plot(sp2, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")
### Fit Lomolino model to the exact accumulation
mod1 <- fitspecaccum(sp1, "lomolino")
coef(mod1)
fitted(mod1)
plot(sp1)
#### Add Lomolino model using argument 'add'
plot(mod1, add = TRUE, col=2, lwd=2)
### Fit Arrhenius models to all random accumulations
mods <- fitspecaccum(sp2, "arrh")
plot(mods, col="hotpink")
#boxplot(sp2, col = "yellow", border = "blue", lty=1, cex=0.3, add= TRUE)
### Use nls() methods to the list of models
sapply(mods$models, AIC)
head(richness)
head(spp1)

c1 <- max(which(spp1$station == "ml3006"))
c2 <- max(which(spp1$station == "mg2007"))
c3 <- max(which(spp1$station == "lsl2006"))
c4 <- max(which(spp1$station == "bls7003"))
c5 <- max(which(spp1$station == "ms5004"))
c6 <- max(which(spp1$station == "nbh5005"))

richness.ml  <- richness[1:c1,] 
richness.mg  <- richness[c1:c2,]
richness.lsl <- richness[c2:c3,] 
richness.bls <- richness[c3:c4,] 
richness.ms  <- richness[c4:c5,] 
richness.nbh <- richness[c5:c6,] 

sp.ml <- specaccum(richness.ml, "rarefaction")
sp.mg <- specaccum(richness.mg, "rarefaction")
sp.lsl <- specaccum(richness.lsl, "rarefaction")
sp.bls <- specaccum(richness.bls, "rarefaction")
sp.ms <- specaccum(richness.ms, "rarefaction")
sp.nbh <- specaccum(richness.nbh, "rarefaction")

str(sp.ml)

plot(sp.ml,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(sp.mg,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(sp.lsl,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(sp.bls,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(sp.ms,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(sp.nbh,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")




