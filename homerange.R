## calculating the breeding and non breeding home range sizes of EWPW using MCP & KDE ##
library(sp); library(adehabitatHR); library(sf)
breeding <- read.csv("./EWPW_MASTER_breedingseason.csv",header = TRUE) #reading in breeding season data

# formatting data to become spatial points 
data.xy <- breeding[c("Longitude", "Latitude", "Bird.ID")] 

# remove outliers
data.xy <- subset(data.xy, Longitude > -78.2)
data.xy <- subset(data.xy, Latitude > 40.8)
breeding.sp <- SpatialPoints(data.xy)
plot(breeding.sp, axes = T, col = "green")

for(i in 1:length(unique(breeding$Bird.ID))){
  
  print(unique(breeding$Bird.ID)[i])
  
  # subset for bird i
  data.xy_i <- subset(data.xy, Bird.ID == unique(breeding$Bird.ID)[i])
  
  # create spatial points and plot
  breeding.sp_i <- SpatialPoints(data.xy_i[,1:2])
  plot(breeding.sp_i, axes = T)
  
  
  #setting the CRS (latitude longitude)
  crs(breeding.sp_i) <- CRS( "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  plot(breeding.sp_i, axes = T, col = "green")
  
  # reproject into planar coordinates
  breeding.sp_i <- spTransform(breeding.sp_i, CRS("+init=epsg:32617"))
  plot(breeding.sp_i, axes = T, col = "blue", main = paste0("Bird ID = ", unique(breeding$Bird.ID)[i]))
  crs(breeding.sp_i)
  
  # calculate the MCPS for each bird during the breeding season 
  breeding.mcp <- mcp(breeding.sp_i, percent = 100)
  class(breeding.mcp)
  plot(breeding.mcp, add = T)
   
  
  # pause
  Sys.sleep(1)
  
}

### attempting to calculate the area of each breeding MCP #
