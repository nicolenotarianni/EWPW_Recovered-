## calculating the breeding and non breeding home range sizes of EWPW using MCP & KDE ##

library(sp); library(adehabitatHR)
breeding <- read.csv("./EWPW_MASTER_breedingseason.csv",header = TRUE) #reading in breeding season data

#creating spatial points 
data.xy <- breeding[c("Latitude","Longitude")] 
breeding.sp <- SpatialPoints(data.xy)

#setting the CRS
proj4string(breeding.sp) <- CRS( "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

#setting up the data frame 
sppt<- data.frame(breeding.sp)
idsp <- data.frame(breeding[1])
breedingMCP <- data.frame(idsp,sppt)


# calculate the MCPS for each bird during the breeding season 
breeding.mcp <- mcp(breeding.sp[,1], percent = 95)
breeding.mcp
