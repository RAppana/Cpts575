
library(ncdf4)
library(rgdal)
library(raster)

# setwd("C:/Users/kalli/OneDrive - Washington State University (email.wsu.edu)/Data Science - Cpt S 575/Project/Code/miniproject_nc4_to_raster_and clip_average")
setwd("C:/Users/auser/OneDrive - Washington State University (email.wsu.edu)/Data Science - Cpt S 575/Project/Code/miniproject_nc4_to_raster_and clip_average")
setwd("C:/wget_download/files/")

# lising .nc4 files from the directory
nc4_files <- list.files(pattern = ".nc4")

# reading the watershed file
watersheds <- list.files(pattern = ".tif")
# watershed <- raster(watersheds[2])
# plot(watershed)



# Variables
vars <- c("SWnet", "LWnet", "Qle", "Qh",
          "Qg", "Snowf", "Rainf", "Evap", 
          "Qs", "Qsb", "Qsm", "RadT", "SWE", 
          "SnowDepth", "SnowFrac", "SoilMoist0_10cm",
          "SoilMoist10_40cm", "SoilMoist40_100cm",
          "SoilMoist100_200cm", "SoilTemp0_10cm", 
          "SoilTemp10_40cm", "SoilTemp40_100cm", 
          "SoilTemp100_200cm", "PotEvap", "ECanop", 
          "TVeg", "ESoil", "SubSnow", "CanopInt", 
          "Streamflow", "FloodedFrac", "FloodedArea",
          "IrrigatedWater", "Wind_f", "Rainf_f", "Tair_f", 
          "Tair_f_min", "Tair_f_max", "Qair_f", "Psurf_f", 
          "SWdown_f", "LWdown_f")


df <- data.frame(matrix(ncol = 4, nrow = 0))

x <- c("Watershed","Date", "Variable", "Value")

colnames(df) <- x
i=1

for (watshed in watersheds){
  watershed <- raster(watshed)
  for (file in nc4_files){
    for (var in vars){
      nc4_file <- raster(file,varname=var)
      resampled = resample(nc4_file, watershed, "bilinear")
      masked <- mask(resampled,watershed)
      mean <- cellStats(masked, stat='mean', na.rm=TRUE, asSample=TRUE)
      date <- substr(file, 21, 28)
      watshed_name <- strsplit(watshed,split = "_")[[1]][2]
      df[i,]=c(watshed_name,date,var,mean)
      i=i+1
      
      print(paste(watshed_name,date,var,sep = "_"))
    }
    
  }
  
  
}


# file=nc4_files[1]
# var=vars[1]
# nc4_file <- raster(file,varname=var)
# plot(nc4_file)
# 
# resampled = resample(nc4_file, watershed, "bilinear")
# plot(resampled)
# 
# masked <- mask(resampled,watershed)
# plot(masked)
# 
# mean <- cellStats(masked, stat='mean', na.rm=TRUE, asSample=TRUE)
# date <- substr(file, 21, 28)
# df[i,]=c(date,var,mean)


# lising .nc4 files from the directory
nc4_files <- list.files(pattern = ".nc4")

# reading the watershed file
watersheds <- list.files(pattern = ".tif")

# watershed <- raster(watersheds[2])
# plot(watershed)


#####################################

nc4_file <- nc4_files[1]
watershed <- watersheds[2]
watershed <- raster(watershed)
watershed[watershed>=0] <- 1

path_for_plots <-"C:/Users/auser/OneDrive - Washington State University (email.wsu.edu)/Data Science - Cpt S 575/Project/plots/" 
png(filename = paste(path_for_plots,"0. watershed.png",sep = ""),width = 3000,height = 3000,res = 300)
plot(watershed,main="Greenwater Watershed in raster format",xlab="Longitude (dd)", ylab="Latitude (dd)",legend=FALSE)
dev.off()
#####################################

rain_f <-vars[7] 
rain_f_raster <- raster(nc4_file,varname=rain_f)
writeRaster(rain_f_raster, filename = paste(path_for_plots,"rain_f_raster",".tif",sep = ""))

png(filename = paste(path_for_plots,"1.1. rain_f_raster.png",sep = ""),width = 3000,height = 3000,res = 300)
plot(rain_f_raster,main="Rainfall in raster format. Date of observation: 03/26/2014",xlab="Longitude (dd)", ylab="Latitude (dd)")
dev.off()

t_air <-vars[36] 
t_air_raster <- raster(nc4_file,varname=t_air)
png(filename = paste(path_for_plots,"2.1. t_air_raster.png",sep = ""),width = 3000,height = 3000,res = 300)
plot(t_air_raster,main="Mean Air Temperature in raster format. Date of observation: 03/26/2014",xlab="Longitude (dd)", ylab="Latitude (dd)")
dev.off()
#####################################

# Interpolation
rain_f_resampled = resample(rain_f_raster,watershed, "bilinear")
writeRaster(rain_f_resampled, filename = paste(path_for_plots,"rain_f_resampled.tif",sep = ))
png(filename = paste(path_for_plots,"1.2. rain_f_resampled.png",sep = ""),width = 3000,height = 3000,res = 300)
plot(rain_f_resampled,main="Rainfall. Interpolated raster format. Date of observation: 03/26/2014",xlab="Longitude (dd)", ylab="Latitude (dd)")
dev.off()

t_air_resampled = resample(t_air_raster, watershed, "bilinear")
png(filename = paste(path_for_plots,"2.2. t_air_resampled.png",sep = ""),width = 3000,height = 3000,res = 300)
plot(t_air_resampled,main="Mean Air Temperature. Interpolated raster format. Date of observation: 03/26/2014",xlab="Longitude (dd)", ylab="Latitude (dd)")
dev.off()
################################################################

# Clipping

rain_f_masked <- mask(rain_f_resampled,watershed)
writeRaster(rain_f_masked, filename = paste(path_for_plots,"rain_f_masked",".tif",sep = ""))

png(filename = paste(path_for_plots,"1.3. rain_f_masked.png",sep = ""),width = 3000,height = 3000,res = 300)
plot(rain_f_masked,main="Rainfall. Clipped using the Greenwater Watershed . Date of observation: 03/26/2014",xlab="Longitude (dd)", ylab="Latitude (dd)")
dev.off()


t_air_masked <- mask(t_air_resampled,watershed)
png(filename = paste(path_for_plots,"2.3. t_air_masked.png",sep = ""),width = 3000,height = 3000,res = 300)
plot(t_air_masked,main="Mean Air Temperature. Clipped using the Greenwater Watershed . Date of observation: 03/26/2014",xlab="Longitude (dd)", ylab="Latitude (dd)")
dev.off()

################################################################

# Mean values

rain_f_mean <- cellStats(rain_f_masked, stat='mean', na.rm=TRUE, asSample=TRUE)
text_rain <- paste("Average = ",rain_f_mean,sep = "")
png(filename = paste(path_for_plots,"1.4. rain_f_average.png",sep = ""),width = 3000,height = 3000,res = 300)
plot(rain_f_masked, main="Average value of Rainfall for the whole watershed. Date of observation: 03/26/2014",xlab="Longitude (dd)", ylab="Latitude (dd)")
text(text_rain,x=-121.5,y=47.07,cex=2)
dev.off()


t_air_mean <- cellStats(t_air_masked, stat='mean', na.rm=TRUE, asSample=TRUE)
text_air_t <- paste("Average = ",t_air_mean,sep = "")
png(filename = paste(path_for_plots,"2.4. air_t_average.png",sep = ""),width = 3000,height = 3000,res = 300)
plot(t_air_masked, main="Average value of Air Temperature for the whole watershed. Date of observation: 03/26/2014",xlab="Longitude (dd)", ylab="Latitude (dd)")
text(text_air_t,x=-121.5,y=47.07,cex=2)
dev.off()
