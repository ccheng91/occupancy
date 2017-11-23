library(ggplot2)
library(dplyr)
library(nlme)
library(MASS)
library(car)
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
list <- list[, colSums(list) != 0] ### delet col that are all zero 
list <- subset( list, select = -human2 )
list <- subset( list, select = -hunter )
list <- subset( list, select = -watermonitor )
list <- subset( list, select = -dog )
list <- subset( list, select = -cattle )

list <- data.frame(apply(list, 2, function(x) ifelse(x > 0, 1, x)))
richness <- data.frame(rowSums(list))
rownames(richness)[2] <- "menglun"
rownames(richness)[1] <- "bulangshan"
names(richness) <- "richness"


### caculate speices richness for each camera
list.cam <- as.data.frame.matrix(table(data.photo$camera,data.photo$species))
list.cam <- list.cam[, colSums(list.cam) != 0]  ### delet col that are all zero 
list.cam <- subset(list.cam, select = -human2)
list.cam <- subset(list.cam, select = -hunter)
list.cam <- subset(list.cam, select = -watermonitor)
list.cam <- subset(list.cam, select = -dog)
list.cam <- subset(list.cam, select = -cattle)

list.cam <- data.frame(apply(list.cam, 2, function(x) ifelse(x > 0, 1, x)))
richness.cam <- data.frame(rowSums(list.cam))
names(richness.cam) <- "richness.cam"

str(list.cam)

####### richness of species classified by redlist/protected level ###
class <- read.csv("data/mammal_class.csv", header=TRUE, stringsAsFactors = FALSE,strip.white = TRUE)
## locate the which spp
threat.cn <- which(class$CN_redlist == "CR" | class$CN_redlist == "VU" | class$CN_redlist == "EN")
okay.cn <- which(class$CN_redlist == "LC" | class$CN_redlist == "NT")

threat.IUCN <- which(class$ICUN_redlist == "CR" | class$ICUN_redlist == "VU" | class$ICUN_redlist == "EN")
okay.IUCN<- which(class$ICUN_redlist == "LC" | class$ICUN_redlist == "NT")

threat.class <- which(class$CN_class == 1 | class$CN_class == 2 )
okay.class <- which(class$CN_class == 3 )

#### making subset ###

list.threat.cn <- subset(list.cam, select=threat.cn)
list.okay.cn <- subset(list.cam, select=okay.cn)

list.threat.IUCN <- subset(list.cam, select=threat.IUCN)
list.okay.IUCN <- subset(list.cam, select=threat.IUCN)

list.threat.class <- subset(list.cam, select=threat.class)
list.okay.class <- subset(list.cam, select=okay.class)
######
##### make richness index ####
##### using CN redlist ####
list.threat.cn <- data.frame(apply(list.threat.cn, 2, function(x) ifelse(x > 0, 1, x)))
richness.threat.cn <- data.frame(rowSums(list.threat.cn))
names(richness.threat.cn) <- "richness.threat.cn"

list.okay.cn <- data.frame(apply(list.okay.cn, 2, function(x) ifelse(x > 0, 1, x)))
richness.okay.cn <- data.frame(rowSums(list.okay.cn))
names(richness.okay.cn) <- "richness.okay.cn"

###using IUCN redlist ####
list.threat.IUCN <- data.frame(apply(list.threat.IUCN, 2, function(x) ifelse(x > 0, 1, x)))
richness.threat.IUCN <- data.frame(rowSums(list.threat.IUCN))
names(richness.threat.IUCN) <- "richness.threat.IUCN"

list.okay.IUCN <- data.frame(apply(list.okay.IUCN, 2, function(x) ifelse(x > 0, 1, x)))
richness.okay.IUCN <- data.frame(rowSums(list.okay.IUCN))
names(richness.okay.IUCN) <- "richness.okay.IUCN"

### using Chinese proection level
list.threat.class <- data.frame(apply(list.threat.class, 2, function(x) ifelse(x > 0, 1, x)))
richness.threat.class <- data.frame(rowSums(list.threat.class))
names(richness.threat.class) <- "richness.threat.class"

list.okay.class <- data.frame(apply(list.okay.class, 2, function(x) ifelse(x > 0, 1, x)))
richness.okay.class <- data.frame(rowSums(list.okay.class))
names(richness.okay.class) <- "richness.okay.class"


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
k <- which(richness.cam.df$richness.mx == 999) ### 3 camera have 0 spp is not went through matt's code
richness.cam.df$richness.mx[k] <- 0  ### add there camera
anthrop.df$richness <- richness.cam.df$richness.mx

v.threat.cn <- as.vector(richness.threat.cn[,1])
v.threat.cn <- append(v.threat.cn, 0, after = k[3]-1)
v.threat.cn <- append(v.threat.cn, 0, after = k[2]-1)
v.threat.cn <- append(v.threat.cn, 0, after = k[1]-1)
anthrop.df$richness.threat.cn <- v.threat.cn

v.okay.cn <- as.vector(richness.okay.cn[,1])
v.okay.cn <- append(v.okay.cn, 0, after = k[3]-1)
v.okay.cn <- append(v.okay.cn, 0, after = k[2]-1)
v.okay.cn <- append(v.okay.cn, 0, after = k[1]-1)
anthrop.df$richness.okay.cn <- v.okay.cn

v.threat.IUCN <- as.vector(richness.threat.IUCN[,1])
v.threat.IUCN <- append(v.threat.IUCN, 0, after = k[3]-1)
v.threat.IUCN <- append(v.threat.IUCN, 0, after = k[2]-1)
v.threat.IUCN <- append(v.threat.IUCN, 0, after = k[1]-1)
anthrop.df$richness.threat.IUCN <- v.threat.IUCN

v.okay.IUCN <- as.vector(richness.okay.IUCN[,1])
v.okay.IUCN <- append(v.okay.IUCN, 0, after = k[3]-1)
v.okay.IUCN <- append(v.okay.IUCN, 0, after = k[2]-1)
v.okay.IUCN <- append(v.okay.IUCN, 0, after = k[1]-1)
anthrop.df$richness.okay.IUCN <- v.okay.IUCN


v.threat.class <- as.vector(richness.threat.class[,1])
v.threat.class <- append(v.threat.class, 0, after = k[3]-1)
v.threat.class <- append(v.threat.class, 0, after = k[2]-1)
v.threat.class <- append(v.threat.class, 0, after = k[1]-1)
anthrop.df$richness.threat.class <- v.threat.class


v.okay.class <- as.vector(richness.okay.class[,1])
v.okay.class <- append(v.okay.class, 0, after = k[3]-1)
v.okay.class <- append(v.okay.class, 0, after = k[2]-1)
v.okay.class <- append(v.okay.class, 0, after = k[1]-1)
anthrop.df$richness.okay.class <- v.okay.class

anthrop.df$pas <- sitecovs$PAS
anthrop.df$ele <- sitecovs$ele
anthrop.df$dis <- sitecovs$dis
anthrop.df$pop3000m <- sitecovs$pop3000m



stepAIC(glm(richness ~ edu +pop3000m+size+dis+ reach  + punish + score.H + income, data=anthrop.df, 
            family = "poisson"), direction = "both")
stepAIC(lm(richness ~ edu +pop3000m+size+dis+ reach  + punish + score.H + income, data=anthrop.df), direction = "both")
summary(z1)
z1 <- lm(richness ~ pop3000m + size + reach + punish, data = anthrop.df)
z2 <-glm(formula = richness ~ size + punish + score.H, family = "poisson", 
         data = anthrop.df)
summary(z2)
Anova(z2,type = 3)
visreg::visreg(z2)
AIC(z1,z2)

stepAIC(glm(richness.threat.cn ~ edu +pop3000m+size+dis+ reach  + punish + score.H + income, data=anthrop.df, 
         family = "poisson"), direction = "both")
stepAIC(lm(richness.threat.cn ~ edu +pop3000m+size+dis+ reach  + punish + score.H + income, data=anthrop.df), direction = "both")

z3 <- lm(richness.threat.cn ~ edu + score.H, data = anthrop.df)
z4 <- glm(richness.threat.cn ~ edu + pop3000m + punish + 
            score.H, family = "poisson", data = anthrop.df)
summary(z4)
Anova(z4,type = 3)
visreg::visreg(z4)

stepAIC(glm(richness.okay.cn ~ edu +pop3000m+size+dis+ reach  + punish + score.H + income, data=anthrop.df, 
            family = "poisson"), direction = "both")
stepAIC(lm(richness.okay.cn ~ edu +pop3000m+size+dis+ reach  + punish + score.H + income, data=anthrop.df), direction = "both")

z5 <- lm(richness.okay.cn ~ edu + pop3000m + dis + reach + 
           score.H, data = anthrop.df)
z6 <- glm(formula = richness.okay.cn ~ edu + pop3000m + dis + reach + 
            score.H, family = "poisson", data = anthrop.df)
summary(z6)
Anova(z5,type = 3)
visreg::visreg(z6)

stepAIC(glm(richness.threat.class ~ edu +pop3000m+size+dis+ reach  + punish + score.H + income, data=anthrop.df, 
            family = "poisson"), direction = "both")

stepAIC(lm(richness.threat.class ~ edu + pop3000m+size+dis+ reach  + punish + score.H + income, data=anthrop.df), direction = "both")

z7 <- glm(richness.threat.class ~ pop3000m + size + score.H, 
         family = "poisson", data = anthrop.df)
z8 <- lm(richness.threat.class ~ pop3000m + size + reach, 
         data = anthrop.df)
summary(z8)





z1 <- lm(richness.threat.cn ~ reach + size + score.H + edu , data=anthrop.df)


step(glm(richness.okay.cn ~ edu +pop3000m+size+dis+ reach  + punish + score.H + income, data=anthrop.df, 
         family = "poisson"), direction = "backward")
z <- glm(formula = richness.okay.cn ~ edu + pop3000m + dis + reach + 
           score.H, family = "poisson", data = anthrop.df)
summary(z)
AIC(z)


Anova(z, type=3)
#visreg::visreg(z)
#qplot(log(size), richness, data=anthrop.df, colour=pas)

head(anthrop.df)
head(sitecovs)


z <-lme(richness ~ pop3000m + size + reach + punish,  random = ~1 | pas, data = anthrop.df)
z2 <- lm(richness ~ pas + pop3000m + size + reach + punish, data = anthrop.df)
z <- lme(richness.threat.cn ~ edu +pop3000m+size+dis+ reach  + punish + score.H + income, random = ~1 | pas, data=anthrop.df)


anthrop.df$pas <- as.factor(anthrop.df$pas)
is.factor(anthrop.df$pas)

VarCorr(z)
coef(z)
summary(z)
head(sitecovs)
visreg::visreg(z)

z <- lm(richness.threat.cn ~ edu +pop3000m+size+dis+ reach  + punish + score.H + income, data=anthrop.df)
z <- lm(richness.threat.cn ~ edu + score.H,data=anthrop.df)

step <- stepAIC(lme(richness.threat.cn ~ edu +pop3000m+size+dis+ reach  + punish + score.H + income, random = ~1 | pas, data=anthrop.df))

qplot(dis ,richness.okay.cn, data=anthrop.df, colour=pas)

head(data.photo)
wildboar <- read.csv("data/wildboar_long.csv")

