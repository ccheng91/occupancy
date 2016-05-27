## to get a list of end time through all photo ##

rm(list=ls(all=TRUE))
all_photo<-read.csv("data/All_photo.csv", header=TRUE)
all_photo$datetime.x <- as.Date(all_photo$datetime)
data.es <- aggregate(datetime.x ~ camera, FUN = max, data = all_photo)

write.csv(data.es, file="data/All_End_data.csv", row.names = FALSE)
