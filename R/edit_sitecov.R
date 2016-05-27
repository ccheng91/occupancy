library(ggplot2)
library(dplyr)

rm(list=ls(all=TRUE))
qs <- read.csv("data/QS_102.csv", stringsAsFactors = FALSE)
sitecovs<-read.csv("data/sitecov_temp.csv",header=TRUE, stringsAsFactors = FALSE)
head(qs)
## making enforcement level variables ##
enforce <-data.frame(cbind(qs$NO, qs$PAs, qs$Village, qs$Time_of_outreach, qs$No_of_Punishment, qs$Relationship, qs$Score_of_Punishment_Of_P,
                      qs$Score_of_Punishment_Of_H))
names(enforce) <- c("NO", "PAs", "Village", "Time_of_outreach", "No_of_Punishment", "Relationship", "Score_of_Punishment_Of_P",
             "Score_of_Punishment_Of_H")
pa <- tolower(c("BULANGSHAN", "MANGAO", "MENGLA", "MENGLUN", "MENGSONG", "NABANHE"))

## 
reach <- aggregate(qs$Time_of_outreach ~ qs$PAs, FUN = "mean")
punish <- aggregate(qs$No_of_Punishment ~ qs$PAs, FUN = "mean")
relationship <- aggregate(qs$Relationship ~ qs$PAs, FUN = "mean")
Score.P <-aggregate(qs$Score_of_Punishment_Of_P ~ qs$PAs, FUN = "mean")
Score.H <-aggregate(qs$Score_of_Punishment_Of_H ~ qs$PAs, FUN = "mean")

# demographic variable
# education 
edu <- table(qs$PAs, qs$Education)
edu <- as.data.frame.matrix(edu)
names(edu) <- c("Elementary", "High", "Middle", "None", "Secondary", "Temple")
head(edu)
edu <- edu[c(4,1,3,2,5,6)]
edu <- edu[,0:5]
wt <- c(0, 4, 7, 10, 13)
wtedu <- data.frame()
for(i in 1:6) {
  j <- as.data.frame(sum(edu[i,] * wt)/sum(edu[i,]))
  wtedu <- dplyr::bind_rows(wtedu, j)
}
wtedu <- cbind(pa, wtedu)
names(wtedu)<-c("pas", "wtedu")

# income 
income <- aggregate(qs$Income ~ qs$PAs, FUN = "mean")
# new data frame
anthrop <- data.frame(pa, reach[,2], punish[,2], relationship[,2], Score.P[,2], Score.H[,2], wtedu[,2],income[,2])
names(anthrop) <- c("pas","reach","punish","relationship", "score.P","score.H", "edu", "income")

## change lvshilin to menglun
lv <- which(sitecovs$PAS == "Lvshilin")
sitecovs$PAS[lv] <- "menglun"
sitecovs$PAS <- tolower(sitecovs$PAS)

# adding new variables
anthrop.mx <- matrix(999, ncol = 8, nrow = nrow(sitecovs))
for(i in 2:8) {
  for(j in 1:6){
  n <- which(sitecovs$PAS == anthrop$pas[j])
  anthrop.mx[n,i] <- anthrop[j,i]
   }
}

anthrop <- data.frame(anthrop.mx)
anthrop[,1] <- sitecovs$PAS
names(anthrop) <- c( "pas","reach","punish","relationship", "score.P","score.H", "edu", "income")




#sitecovs<-read.csv("data/sitecov_temp.csv",header=TRUE, stringsAsFactors = FALSE)
#vill.pop <- read.csv("data/Village_GPS_CC.csv",header=TRUE)
#str(SP)
#head(sitecovs)
#qplot(wtedu.z * pop.s, size, data=sitecovs, colour=PAS)
#qplot(dis,popP , data=sitecovs, colour=PAS)
#table(sitecovs$PAS)
#no.ml <- which(sitecovs$PAS == "Mengla")
#no.ml <- which(sitecovs$PAS == "Mengla")
#no.ml <- which(sitecovs$PAS == "Mengla")
#o.ml <- which(sitecovs$PAS == "Mengla")
#no.ml <- which(sitecovs$PAS == "Mengla")

#sitecovs$pun[no.ml] <- spsum$`SP$SP`[3]
#index <- -sitecovs$dis * sitecovs$pop3000m *  sitecovs$S_H
#sitecovs$index <- index

#table(qs$No_of_Punishment)
#nrow(qs)
#which(qs$No_of_Punishment == NA)


