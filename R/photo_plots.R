library(ggplot2)

rm(list=ls(all=TRUE))
### read all photo and check name of all spp ######### 

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

q <- which(data.photo$species == "bat" | data.photo$species == "commongreenmagpie" | 
             data.photo$species == "greateryellownape" | data.photo$species =="greenmagpie"| 
             data.photo$species=="treeshrew"| data.photo$species == "junglefowl"| data.photo$species =="silverpheasant"| 
             data.photo$species=="squirrel"| data.photo$species=="bird" | 
             data.photo$species=="rat" | data.photo$species == "unknown" | data.photo$species == "human")

############## plot species need reavise ##################
list <- data.frame(aggregate(data.photo$n ~ data.photo$species + data.photo$site, FUN =sum))
# list <- list[list$Freq != 0, ]
names(list) <- c("spp", "pa", "n")
str(list)
head(list)
list<-list[order(list$pa, list$n),]
table(list$spp)


list2 <-data.frame(aggregate(data.photo$n ~ data.photo$species, FUN =sum))
names(list2) <- c( "spp","count")
list2 <-list2[order(list2$count),]
list2$spp3 <-factor(list2$spp, levels = order(list2$count))
p <- ggplot(list2, aes(x=spp2, y = count, order=count)) 
p + geom_bar(stat = "identity") 



xx <- barplot(list2$count, names.arg=list2$spp, las=2, ylim=c(0,500))
par(mar=c(12,3,5,1))
text(x = xx, y = list2$count, label = list2$count, pos = 3, cex = 0.8, col = "black")


# text(x = list2$spp, srt = 60, adj= 1, xpd = TRUE,labels = paste(rownames(mtcars)), cex=0.65)
# axis(2,at=seq(from = 0, to=450, by=50))

sum(df3$n)
1331

y <- ggplot(list, aes(x=spp, y=n)) + geom_bar(stat="identity")
y

data.photo$spp <-factor(data.photo$spp, levels=data.photo$n[order(mtcars$n)])

#####
data.new <- plyr::ddply(data.photo, .(site), sum, prob=prop.table(table(species)), species=names(table(species)))
str(data.photo)
a <- prop.table((table(data.photo$species)))

list$spp2 = as.character(list$spp2)
list$spp3 <- list$spp2
list$spp3 = factor(list$spp3, levels=factor(list$n))
p <- ggplot(list, aes(x=spp2, y = n, order=n)) 
p + geom_bar(stat = "identity") + facet_wrap(~ pa2 , scale="free_x") + 
    theme(axis.text.x = element_text(angle = 35, size=12, hjust = 0.9), strip.text.x = element_text(size = 15, colour = "black"),
          axis.title.y = element_text(hjust=0.55, size=15)) + xlab("Species")  + ylab("Count") 
                                                                           
ggplot(list, aes(reorder(spp2,n),n)) + geom_bar(stat = "identity") + facet_wrap(~ pa2 , scale="free_x")



list$spp <- factor(list$spp, levels=(sort(list$freq, decreasing=TRUE)))

list$spp2 <- factor(list$spp, levels=rev(levels(factor(list$spp))))
list$pa2 <- factor(list$pa, levels=rev(levels(factor(list$pa))))
list$spp2 <-reorder(list$spp, list$n)
list$pa2 <- reorder(list$pa, list$n)

d$Team3 <- reorder(d$Team1, d$Win)


mtcars$carb2 <- factor(mtcars$carb, levels=rev(levels(factor(mtcars$carb))))
list<- transform(list, spp=reorder(spp, freq))
list<- transform(list, spp=reorder(pa, freq))

theTable <- within(theTable, 
                   Position <- factor(Position, 
                                      levels=names(sort(table(Position), 
                                                        decreasing=TRUE))))

############## plot species need reavise ##################


