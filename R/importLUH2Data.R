library(ncdf4)
library(raster)
library(maptools)
library(moveVis)
library(move)
library(data.table)
# library(RCurl)
library(curl)
library(rasterVis)
library(rgdal)
library(animation)
library(lattice)
library(chron)

# much of the processing code is taken from http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html

#Introduction
# Land use harmonization data are take from http://luh.umd.edu.
# The url with data downloads is http://luh.umd.edu/data.shtml
# The data available at this url are states, transitions, and management, stored in netcdf format.
# There are 8 versions of these data sets based on combinations of RCPs and SSPs. Each data set is generated
# by a single IAM.
# 6 were released on Dec 21, 2017. Two additional data sets (RCP1.9 SSP1, RCP3.4OS SSP5) were released on Nov 29, 2018.
# The file names are contained in the fileChoices variable.
# The variable names are identical in each file - primf, primn, secdf, secdn, urban, c3ann, c4ann, c3per, c4per, 
# c3nfx, pastr, range, secmb, secma, lat_bounds, lon_bounds, time_bnds.
# The slightly more explanatory name for each is - forested primary land, non-forested primary land, 
# potentially forested secondary land, potentially non-forested secondary land, urban land, C3 annual crops, 
# C4 annual crops, C3 perennial crops, C4 perennial crops, C3 nitrogen-fixing crops, managed pasture, rangeland, secondary mean biomass carbon density, secondary mean age, lat_bounds, lon_bounds, time_bnds

# The data are from 2015 to 2100
# check to see if project directory has correct subdirectories. These are contained in defaultDirList.
defaultDirList <- c("data", "data-raw", "references", "graphics", "results")
# Code below assumes these directories are available
# create any directories in the defaultDirList that do not alreay exist
for (i in defaultDirList) {
  if (!dir.exists(i)) dir.create(i)
}
#url at U of Maryland
baseURL <- "http://gsweb1vh2.umd.edu/LUH2/LUH2_v2f/"
fileChoices <- c("IMAGE_SSP1_RCP19/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp119-2-1-f_gn_2015-2100.nc",
                 "MAGPIE_SSP5_RCP34OS/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MAGPIE-ssp534-2-1-f_gn_2015-2100.nc",
                 "IMAGE/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp126-2-1-f_gn_2015-2100.nc",
                 "GCAM34/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-GCAM-ssp434-2-1-f_gn_2015-2100.nc",
                 "MESSAGE/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MESSAGE-ssp245-2-1-f_gn_2015-2100.nc",
                 "GCAM60/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-GCAM-ssp460-2-1-f_gn_2015-2100.nc",
                 "AIM/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-AIM-ssp370-2-1-f_gn_2015-2100.nc",
                 "MAGPIE/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MAGPIE-ssp585-2-1-f_gn_2015-2100.nc")

# remove the subdirectory names in the fileChoices names
outfileNames <- gsub(".*/", "", fileChoices)

readmefile <- "http://gsweb1vh2.umd.edu/LUH2/LUH2_v2f_README_v6.pdf"
outreadmefile <- "references/LUH2_v2f_README_v6.pdf"

destDir <- paste0(getwd(), "/data-raw/")

# download files if they haven't already been downloaded
for (i in 1:length(fileChoices)) {
  url <- paste0(baseURL, fileChoices[i])
  destfile <- paste0(destDir, outfileNames[i])
  if (!file.exists(destfile)) curl_download(url, destfile)
}
if (!file.exists(outreadmefile)) curl_download(readmefile, destfile = outreadmefile)

# display some useful information
for (i in 1:length(outfileNames)) {
  print(paste0("Reporting on: ", outfileNames[i]))
  temp <- paste0("data-raw/", outfileNames[i])
  ncin <- ncdf4::nc_open(temp)
  # list variable names in ncin
  varNames <- names(ncin[['var']])
  print(varNames)
  # print number of years in ncin
  ncin$dim$time$len
  
  # print start year
  ncin$dim$time$units
  
  # get the long names of variables and create a data table with short and long names
  dt.names <- data.table(shortName = character(), longName = character())
  for (j in 1:length(varNames)) {
    tmp <- ncin[["var"]][[varNames[j]]][["longname"]]
    dt.names <- rbind(dt.names, list(varNames[j], tmp))
  }
  filename.names <- paste0("data/varNames.", outfileNames[i], ".csv")
  write.csv(dt.names, filename.names, row.names = FALSE)
}

# do some crunching on one of the variables  - primf from the first of the netcdf files
fileNumber <- 1
varToGet <- "primf"
temp <- paste0("data-raw/", outfileNames[fileNumber])
ncin <- ncdf4::nc_open(temp)

# get global attributes
title <- ncatt_get(ncin, 0, "title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

# create some variables used for processing below
# get longitude and latitude
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
# get time
time <- ncvar_get(ncin,"time")
tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)

ncin.1 <- raster(temp, varname = varToGet)
array_var1 <- ncvar_get(ncin,varToGet)
dlname <- ncatt_get(ncin,varToGet, "long_name")
dunits <- ncatt_get(ncin,varToGet, "units")
fillvalue <- ncatt_get(ncin,varToGet, "_FillValue")
dim(array_var1)

ncin.brick <- brick(temp, varname = varToGet)
ncin.stack <- stack(temp, varname = varToGet)

# Close the netCDF file
nc_close(ncin)

# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time, origin = c(tmonth, tday, tyear))

# if there are missing values, fill with NAs
array_var1[array_var1 == fillvalue$value] <- NA

# Total number of non-missing (i.e. land, except for Antarctica) grid cells 
length(na.omit(as.vector(array_var1[,,1])))

# get a single year
m <- 1
slice_var1 <- array_var1[,,m]

# do a nice looking graph
# levelplot of the slice
grid <- expand.grid(lon = lon, lat = lat)
cutpts <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
lattice::levelplot(slice_var1 ~ lon * lat, data = grid, at = cutpts, cuts = 11, pretty = T, 
          col.regions = (rev(brewer.pal(10, "RdBu"))))

# outdir <- "graphics"
# rasterOptions(tmpdir = outdir)
#plot years 1:10
raster::plot(ncin.stack, 1:10, main = ncin.stack@title, xlab = "long", ylab = "lat", ylim = c(-100, 100))

# extract the 15th year from ncin.stack
singleYear <- 15
oneYear <- subset(ncin.stack, subset = 15:25)
plotTitle <-  ncin.stack@title
plotTitle <- gsub("\\.", " ", ncin.stack@title)

# capitalize the first word
plotTitle <- paste0(toupper(substr(plotTitle, 1, 1)), substr(plotTitle, 2, nchar((plotTitle))))

plot(oneYear, main = plotTitle, xlab = "long", ylab = "lat")
raster::animate(ncin.stack, main = ncin.stack@title)

saveGIF(animate(ncin.stack, main = ncin.stack@title), movie.name = "animation.gif")
saveGIF(animate(oneYear, main = ncin.stack@title), movie.name = "animation2.gif")


