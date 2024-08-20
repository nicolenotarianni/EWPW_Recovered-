# this is the prep code for recovered tags for 20 randoms

library(raster); library(sf) #required packages

ewpw <- read.csv("./EWPW_MASTER.csv")

head(ewpw) #looks good, Bird.ID; Latitude; Longitude; CID

#setting the projection to Albers 
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#creating a blank dataframe 
blankDF <- data.frame("SID" = 0, "CID" = 0, "Alts" = 0, "Use" = 0, "Lat" = 0, "Long" = 0)

#data prep: creating randoms 19 for every 1 used point within in a 200 m radius of the used point
for(i in 1:length(unique(ewpw$Bird.ID))){ 
  ewpw_i <- subset(ewpw, Bird.ID == unique(ewpw$Bird.ID)[i]) 
  
  for(j in 1:nrow(ewpw_i)){ 
    ewpw_i_CID_j <- ewpw_i[j,]
    
    ewpw_i_CID_j_sp <- SpatialPoints(coords = data.frame("x" = ewpw_i_CID_j$Longitude, "y" = ewpw_i_CID_j$Latitude)) # create spatial object
    crs(ewpw_i_CID_j_sp) <- CRS("+init=epsg:4326") 
    plot(ewpw_i_CID_j_sp, axes = TRUE)
    ewpw_i_CID_j_sp <- spTransform(ewpw_i_CID_j_sp, CRS(albers)) 
    buff1 <- raster::buffer(ewpw_i_CID_j_sp, width = 200)
    plot(buff1, axes = TRUE); plot(ewpw_i_CID_j_sp, add = TRUE)
    
    buff2 <- st_as_sf(buff1) 
    points1 = st_sample(buff2, size = 19) 
    points1 <- as_Spatial(points1) 
    plot(buff1, lwd = 2); plot(points1, add = TRUE, col = "red", cex = 5, pch = "."); plot(ewpw_i_CID_j_sp, add = TRUE, col = "blue", cex = 10, pch = ".") # plot to confir pch = 3m it worked
    points2 <- spTransform(points1, CRS("+init=epsg:4326"))  
    randomcoords <- data.frame(points2@coords) 
    names(randomcoords) <- c("long", "lat") 
    choice_j_data <- data.frame("SID" = rep(i, 5),
                                "CID" = rep(max(blankDF$CID)+1, 20),
                                "Alts" = c(1:20),
                                "Use" = c(rep(0,19),1),
                                "Lat" = c(randomcoords$lat, ewpw_i_CID_j$Latitude),
                                "Long" = c(randomcoords$long, ewpw_i_CID_j$Longitude))
    blankDF <- rbind(blankDF, choice_j_data)
  }
}

#filling in the blank data frame with our newly generated randoms and SID 
nrow(blankDF)
blankDF <- blankDF[2:nrow(blankDF),]
write.csv(blankDF, "./RecoveredEWPWPtsw20randoms.csv", row.names = FALSE)



