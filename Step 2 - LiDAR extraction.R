# this is the LIDAR Extraction code for the 2024 recovered tags

library(raster) #load in packages

#load in CSV file with randoms (generated from step 1) and plot
ewpw1 <- read.csv("./RecoveredEWPWptsw20randoms.csv")
plot(ewpw1$Long, ewpw1$Lat)


#creating spatial points, defining CRS and reprojecting to match LiDAR
Locs <- SpatialPoints(coords = data.frame("x" = ewpw1$Long, "y" = ewpw1$Lat)) 
crs(Locs) <- CRS("+init=epsg:4269") 
Locs <- spTransform(Locs, crs(iqr))


#read in LiDAR layers and extract at each point
iqr_100m <- extract(x = raster("C:/Users/User/Desktop/dc2/IQR.tif"), y = Locs)
p75_100m <- extract(raster("C:/Users/User/Desktop/dc2/p75.tif"), y = Locs)
p90_100m <- extract(raster("C:/Users/User/Desktop/dc2/p90.tif"), y = Locs)
perc5to1_100m <- extract(raster("C:/Users/User/Desktop/dc2/Perc_5m_to_1m.tif"), y = Locs)
percfirst5to1_100m <- extract(raster("C:/Users/User/Desktop/dc2/Perc_First_5m_to_1m.tif"), y = Locs)
TopRug30m_p95_100m <- extract(raster("C:/Users/User/Desktop/dc2/TopRug30m_p95.tif"), y = Locs)

#add extracted points to the dataset
ewpw1$iqr_100 <- iqr_100m
ewpw1$p75_100 <- p75_100m
ewpw1$p90_100 <- p90_100m
ewpw1$perc5to1_100 <- perc5to1_100m
ewpw1$percfirst5to1_100 <- percfirst5to1_100m
ewpw1$rugosity_100 <- TopRug30m_p95_100m

#export CSV
write.csv(ewpw1, "./RecoveredEWPWptsw20randsandCOVS.csv", row.names = FALSE)

