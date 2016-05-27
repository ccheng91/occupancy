photo_path <- c("/Volumes/Seagate Backup Plus Drive/Nabanhe201401_04")
checks <- cam_check(photo_path)
photo <- cam_data(photo_path)
checks$name_problem
head(photo)
table(data$species)

data <- photo
data$species[data$species == "guar"] <- "gaur"
data$species[data$species == "masked_plam_civet"] <- "maskedpalmcivet"
data$species[data$species == "maskedplamcivet"] <- "maskedpalmcivet"
data$species[data$species == "munijac"] <- "muntjac"
data$species[data$species == "muntajc"] <- "muntjac"
data$species[data$species == "pigtailedmacaque"] <- "pigtailmacaque"
data$species[data$species == "procupine"] <- "porcupine"
data$species[data$species == "silver_pheasant"] <- "silverpheasant"
data$species[data$species == "common_macaque"] <- "commonmacaque"
data$species[data$species == "jungle_fowl"] <- "junglefowl"

data <- cam_independent(data, window = lubridate::dhours(1))

write.csv(data, file="/Users/chencheng/Desktop/Nabanhe201401_04_allphoto.csv", row.names = FALSE)

str(date.s)

data$date.x <-  as.Date(data$datetime) 
date.e <- aggregate(data$datetime ~ camera, FUN = max, data=data)
names(date.e) <- c("camera","End_date")
date.s <- aggregate(data$datetime ~ camera, FUN = min , data=data)
names(date.s) <- c("camera","Start_date")
data.sh <- lubridate::hour(date.s$Start_date)
data.eh <- lubridate::hour(date.e$End_date)
S_E_date <- data.frame(date.e$camera, as.Date(date.s$Start_date), data.sh, as.Date(date.e$End_date), data.eh)
names(S_E_date) <- c("camera","start_date","start_hour","end_date", "end_hour")
str(S_E_date)

write.csv(S_E_date, file="/Users/chencheng/Desktop/Nabanhe201401_04_date.csv", row.names = FALSE)


### guar ##

guar <- data[which(data$species == "gaur"), ]
table(guar$camera)
