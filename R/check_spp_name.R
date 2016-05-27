photo_path <- c("/Users/chencheng/Desktop/data/Camdata/Data/All_camera_trap_data")
data <- cam_data(photo_path)

##### check spp name & Independence ####
table(data$species)
head(data)
data$species[data$species == "bear"] <- "blackbear"
data$species[data$species == "crabeatingmoogose"] <- "crabeatingmongoose"
data$species[data$species == "hogdadger"] <- "hogbadger"
data$species[data$species == "cow"] <- "cattle"

data$species[data$species == "maskedcommoncivet"] <- "maskedpalmcivet"
data$species[data$species == "maskedplamcivet"] <- "maskedpalmcivet"
data$species[data$species == "procupine"] <- "porcupine"
data$species[data$species == "sliverpheasant"] <- "silverpheasant"
data$species[data$species == "squirral"] <- "squirrel"
data$species[data$species == "squrriel"] <- "squirrel"
data$species[data$species == "unknow"] <- "unknown"
data$species[data$species == "yellowthroadedmarten"] <- "yellowthroatedmarten"
data$species[data$species == "pigtailedmacque"] <- "pigtailedmacaque"

cam_independent(data, window = lubridate::dhours(1))

data <- cam_independent(data, window = lubridate::dhours(1))
table(data$species)
write.csv(data, file="/Users/chencheng/Desktop/data/Camdata/Data/All_photo.csv", row.names = FALSE )

## fix wrong exif date 
## there are some cameras in Bulong set 2014 into 2011
## camera ml3001 set 2015 into 2014
## code to fix that

a <- read.csv("/Users/chencheng/Desktop/data/Camdata/Data/All_photo.csv", head = TRUE, stringsAsFactors = FALSE)
a$datetime <- as.Date(a$datetime)
c <- which(year(a$datetime) < 2014)

for(i in c ) {
  year(a$datetime[i]) <- 2014
}

q <- which(a$camera == "ml3001")

for(i in q ) {
  year(a$datetime[i]) <- 2015
}


write.csv(a, file="/Users/chencheng/Desktop/data/Camdata/Data/All_photo.csv", row.names = FALSE )
write.csv(a, file="/Users/chencheng/Desktop/data/occupancy/data/All_photo.csv", row.names = FALSE )
