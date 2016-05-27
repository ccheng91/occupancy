# Converts from "Long" data format (one row per site-day) to "Wide" format for both individuals and camhours
# (one row per site)

rm(list=ls(all=TRUE)) 
datalong<-read.csv("data/leopardcat_long.csv",header=TRUE) 
stationTemp <- datalong$station 
station <- unique(stationTemp) #remove duplicates of station

# count total camdays for each station or input from excel 
stationNameTable<-matrix(999, length(station), 2, byrow=T)
for(i in 1:length(station)) {      
  stationNameTable[i,1]<-toString(station[i])
}
stationN <- station[1]
dayCount <- 0
j <- 1

for(i in 1:length(stationTemp)) {
  if (stationN == stationTemp[i]) {
    dayCount <- dayCount + 1
  } else {
    stationN <- stationTemp[i]
    stationNameTable[j,2] <- dayCount
    j <- j + 1
    dayCount <- 1
  }
  if (i == length(stationTemp)) {
    stationNameTable[j,2] <- dayCount
  }
}

# remaining variables
station <- stationNameTable[,1]
camdays <- as.numeric(stationNameTable[,2])
individuals<-datalong$individuals
camhours<-datalong$camhours
sumcam<-cumsum(camdays)

sumcamShifted<-as.vector(rep(998,length(camdays)))
sumcamShifted[1]<-0
for(i in 1:(length(camdays)-1)) {
  sumcamShifted[i+1]<-as.numeric(sumcam[i])
}

# makes the wide format of individuals
dataWide<-matrix(999, length(station), max(camdays), byrow=T)

for (i in 1:length(station)) {
  for (j in 1:max(camdays)) {
    if (j > camdays[i]) {
      dataWide[i,j] <- NA
    } else {
      dataWide[i,j] <- individuals[j + sumcamShifted[i]]
    }
  }
}

# row & col names
dataWid.f <- as.data.frame(dataWide)
dataWid.f <- cbind(station, dataWid.f)
colNames <- c("station")
for (i in 1:max(camdays)) {
  colNames <- append(colNames, paste("day", toString(i), sep=""), after = length(colNames))
}
names(dataWid.f) <- colNames

write.csv(dataWid.f, file="data/leopardcat_datawide_n.csv", row.names = FALSE)

# makes the wide format of camhours

for (i in 1:length(station)) {
  for (j in 1:max(camdays)) {
    if (j > camdays[i]) {
      dataWide[i,j] <- NA
    } else {
      dataWide[i,j] <- camhours[j + sumcamShifted[i]]
    }
  }
}

# row & col names
dataWid.f <- as.data.frame(dataWide)
dataWid.f <- cbind(station, dataWid.f)
colNames <- c("station")
for (i in 1:max(camdays)) {
  colNames <- append(colNames, paste("day", toString(i), sep=""), after = length(colNames))
}
names(dataWid.f) <- colNames

write.csv(dataWid.f, file="data/leopardcat_datawide_camhour.csv", row.names = FALSE)

