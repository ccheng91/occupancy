library(ggplot2)
library(dplyr)

####### lm for speices richness and anthropnical factor between parks #########
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
list <- list[, colSums(list) != 0]  ### delet col that are all zero 
list <- select(list, -human2)
list <- select(list, -hunter)
list <- select(list, -watermonitor)
list <- select(list, -dog)
list <- select(list, -cattle)

list <- data.frame(apply(list, 2, function(x) ifelse(x > 0, 1, x)))
richness <- data.frame(rowSums(list))
rownames(richness)[2] <- "menglun"
rownames(richness)[1] <- "bulangshan"
names(richness) <- "richness"


### caculate speices richness for each camera
list.cam <- as.data.frame.matrix(table(data.photo$camera,data.photo$species))
list.cam <- list.cam[, colSums(list.cam) != 0]  ### delet col that are all zero 
list.cam <- select(list.cam, -human2)
list.cam <- select(list.cam, -hunter)
list.cam <- select(list.cam, -watermonitor)
list.cam <- select(list.cam, -dog)
list.cam <- select(list.cam, -cattle)

list.cam <- data.frame(apply(list.cam, 2, function(x) ifelse(x > 0, 1, x)))
richness.cam <- data.frame(rowSums(list.cam))
names(richness.cam) <- "richness.cam"

####
## get anthropogenic factors ###
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
## assign richness into anthrop
richness <- richness[c(1,3,4,2,5,6),]
anthrop$richness <- richness

## add size to anthrop 
sitecovs<-read.csv("data/sitecov_temp.csv",header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)
size <- unique(sitecovs$size.m)
pa <-  tolower(unique(sitecovs$PAS))
size <-data.frame(pa, size)
size <- size[c(4,2,1,3,5,6),]
anthrop$size <- size$size

## Linear model
z <- lm(richness ~ size, data=anthrop)
qplot(size ,richness, data=anthrop, colour=pas) + geom_abline(intercept = 6.230926, slope = 1.135311)
z2 <- lm(richness ~ reach, data=anthrop)
coef(z2)
qplot(reach ,richness, data=anthrop, colour=pas) + geom_abline(intercept = 2.097989, slope = 2.212644) 
qplot(punish ,richness, data=anthrop, colour=pas)

z <- lm(richness ~ punish, data=anthrop)
z3 <- lm(richness ~ relationship, data=anthrop)
qplot(relationship ,richness, data=anthrop, colour=pas)
qplot(score.P ,score.H, data=anthrop, colour=pas)
zz <- lm(score.P ~ score.H, data=anthrop)
summary(zz)
z4 <-lm(richness ~ score.P)
summary(z)
anthrop

## linear model by site 
sitecovs$NO <- tolower(sitecovs$NO)
lv <- which(sitecovs$PAS == "Lvshilin")
sitecovs$PAS[lv] <- "menglun"
sitecovs$PAS <- tolower(sitecovs$PAS)

anthrop.mx <- matrix(999, ncol = 10, nrow = nrow(sitecovs))
for(i in 2:10) {
  for(j in 1:6){
    n <- which(sitecovs$PAS == anthrop$pas[j])
    anthrop.mx[n,i] <- anthrop[j,i]
  }
}

anthrop.df <- data.frame(anthrop.mx)
anthrop.df[,1] <- sitecovs$NO
names(anthrop.df) <- names(anthrop)
names(anthrop.df)[1] <- "NO"

##### 
richness.mx <- matrix(999, ncol = 1, nrow=nrow(sitecovs))
for(i in 1:nrow(richness.cam)) {
  m <- which(rownames(richness.cam)[i] == sitecovs$NO)
  richness.mx[m] <- richness.cam[i,1]
}
richness.cam.df <- data.frame(sitecovs$NO, richness.mx)
k <- which(richness.cam.df$richness.mx == 999)
richness.cam.df$richness.mx[k] <- 0
anthrop.df$richness <- richness.cam.df$richness.mx
anthrop.df$pas <- sitecovs$PAS


z1 <- lm(richness ~ reach + size + score.H + edu , data=anthrop.df)


step(glm(richness ~  edu +pop3000m+size+dis+ reach  + punish + score.H + income, data=anthrop.df, 
         family = "gaussian"), direction = "backward")

summary(z)
AIC(z)


Anova(z, type=3)
visreg::visreg(z)
qplot(dis, richness, data=anthrop.df, colour=pas)

head(anthrop.df)
head(sitecovs)
anthrop.df$ele <- sitecovs$ele
anthrop.df$dis <- sitecovs$dis
anthrop.df$pop3000m <- sitecovs$pop3000m
z <-glm(formula = richness ~ pop3000m + size + reach + punish, family = "gaussian", 
        data = anthrop.df)

summary(z)
head(sitecovs)
