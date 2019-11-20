# first four lines of code below are to install packages automatically for people who don't have them installed
packageList <- installed.packages()
neededPackages <- c("animation", "ncdf4", "raster", "rgdal", "maps", "maptools", "rgeos", "sp", "sf", "moveVis", "data.table", "curl", 
                   "gganimate", "ggplot2", "magick", "colorspace")
packagesToInstall <- neededPackages[!neededPackages %in% packageList]
install.packages(packagesToInstall)
library(ncdf4)
library(raster)
library(maps)
library(maptools)
# library(rgeos)
library(sp)
library(sf)
#  library(moveVis)
library(data.table)
library(tmap)
# library(curl)
# library(rasterVis)
library(rgdal)
library(animation)
library(gganimate)
library(ggplot2)
library(magick)
# #library(lattice)
library("colorspace")

# much of the processing code is taken from http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html

#Introduction
# Land use harmonization data are take from http://luh.umd.edu.
# The url with data downloads is http://luh.umd.edu/data.shtml
# The data available at this url are states, transitions, and management, stored in netcdf format.
# There are 8 versions of these data sets based on combinations of RCPs and SSPs. Each data set is generated
# by a single IAM.
# 6 were released on Dec 21, 2017. Two additional data sets (RCP1.9 SSP1, RCP3.4OS SSP5) were released on Nov 29, 2018.
# The file names are stored in the fileName variable.
# The variable names are identical in each file - primf, primn, secdf, secdn, urban, c3ann, c4ann, c3per, c4per, 
# c3nfx, pastr, range, secmb, secma, lat_bounds, lon_bounds, time_bnds.
# The slightly more explanatory name for each is - forested primary land, non-forested primary land, 
# potentially forested secondary land, potentially non-forested secondary land, urban land, C3 annual crops, 
# C4 annual crops, C3 perennial crops, C4 perennial crops, C3 nitrogen-fixing crops, managed pasture, rangeland, 
# secondary mean biomass carbon density, secondary mean age, lat_bounds, lon_bounds, time_bnds
# The data are annual from 2015 to 2100

# Please use the following citation in any presentations or publications that result from, or include, the use of the LUH2 datasets:
#   Hurtt, G., L. Chini, R. Sahajpal, S. Frolking, et al. “Harmonization of global land-use change and management for the period 850-2100”. Geoscientific Model Development (In prep).
# An alternative might be Ma, L., Hurtt, G. C., Chini, L. P., Sahajpal, R., Pongratz, J., Frolking, S., et al. (2019). Global Transition Rules for Translating Land-use Change (LUH2) 
# To Land-cover Change for CMIP6 using GLM2. Geosci. Model Dev. Discuss., 1–30. doi:10.5194/gmd-2019-146.

# check to see if project directory has correct subdirectories. These are contained in defaultDirList.
defaultDirList <- c("data", "data-raw", "references", "graphics", "results")
# Code below assumes these directories are available
# create any directories in the defaultDirList that do not alreay exist
for (i in defaultDirList) {
  if (!dir.exists(i)) dir.create(i)
}
#url at U of Maryland
baseURL <- "http://gsweb1vh2.umd.edu/LUH2/LUH2_v2f/"
fileName <- c("IMAGE_SSP1_RCP19/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp119-2-1-f_gn_2015-2100.nc",
              "MAGPIE_SSP5_RCP34OS/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MAGPIE-ssp534-2-1-f_gn_2015-2100.nc",
              "IMAGE/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp126-2-1-f_gn_2015-2100.nc",
              "GCAM34/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-GCAM-ssp434-2-1-f_gn_2015-2100.nc",
              "MESSAGE/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MESSAGE-ssp245-2-1-f_gn_2015-2100.nc",
              "GCAM60/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-GCAM-ssp460-2-1-f_gn_2015-2100.nc",
              "AIM/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-AIM-ssp370-2-1-f_gn_2015-2100.nc",
              "MAGPIE/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MAGPIE-ssp585-2-1-f_gn_2015-2100.nc")

# these file names include the SSP and RCP combos after the model name and before f_gn. For example, ssp119 is ssp1 and rcp1.9. 

# create variable for file content
fileContent <- gsub("multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-", "", fileName)
fileContent <- gsub(".*/", "", fileContent)
fileContent <- gsub(".nc", "", fileContent)
fileContent <- gsub("-2-1-f_gn_2015-2100", "", fileContent)

# get ssps and rcps
test <- as.data.table(tstrsplit(fileContent, "-"))
setnames(test, old = names(test), new = c("model", "ssprcpcombo"))
test[, ssp := toupper(substr(ssprcpcombo, 1,4))][,rcp := paste0("RCP", substr(ssprcpcombo, 5,6))][, ssprcpcombo := NULL]
fileInfo <- cbind(test, fileName)

# remove the subdirectory names in the fileName names
fileInfo[, outfileName := gsub(".*/", "", fileName)]

readmefile <- "http://gsweb1vh2.umd.edu/LUH2/LUH2_v2f_README_v6.pdf"
outreadmefile <- "references/LUH2_v2f_README_v6.pdf"

destDir <- paste0(getwd(), "/data-raw/")
destDirFiles <- list.files(destDir) # might be better than file.exists below
# download files if they haven't already been downloaded

for (i in 1:nrow(fileInfo)) {
  url <- paste0(baseURL, fileInfo$fileName[i])
  destfile <- paste0(destDir, fileInfo$outfileName[i])
  print(destfile)
  if (!file.exists(destfile)) curl_download(url, destfile)
}
if (!file.exists(outreadmefile)) curl_download(readmefile, destfile = outreadmefile)

# display some useful information. It is identical for all these files but I'll leave the code in for now with the print statements commented out
for (i in 1:length(fileInfo)) {
  temp <- paste0("data-raw/", fileInfo$outfileName[i])
  ncin <- ncdf4::nc_open(temp)
  # list variable names in ncin
  varNames <- names(ncin[['var']])
  ncin$dim$time$len
  
  # print start year
  ncin$dim$time$units
  
  # get the long names of variables and create a data table with short and long names
  dt.varNames <- data.table(shortName = character(), longName = character())
  for (j in 1:length(varNames)) {
    tmp <- ncin[["var"]][[varNames[j]]][["longname"]]
    dt.varNames <- rbind(dt.varNames, list(varNames[j], tmp))
  }
  filename.names <- paste0("data/varNames.", fileInfo$outfileName[i], ".csv")
  write.csv(dt.varNames, filename.names, row.names = FALSE)
}

# do some crunching on one of the variables  - c3ann from the first of the netcdf files - change varToGet for other variables
fileNumber <- 8
varToGet <- "c3ann"
temp <- paste0("data-raw/", fileInfo$outfileName[fileNumber])
ncin <- ncdf4::nc_open(temp)
model <- fileInfo[fileName %in% fileName[fileNumber], model]
ssp <- fileInfo[fileName %in% fileName[fileNumber], ssp]
rcp <- fileInfo[fileName %in% fileName[fileNumber], rcp]

# get global attributes
title <- ncatt_get(ncin, 0, "title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

# create some variables used for processing below
# get longitude and latitude
# not currently used so commented out
# lon <- ncvar_get(ncin,"lon")
# nlon <- dim(lon)
# lat <- ncvar_get(ncin,"lat")
# nlat <- dim(lat)
# latunits <- ncatt_get(ncin,"lat","units")$value # should always be "degrees_north"
# lonunits <- ncatt_get(ncin,"lon","units")$value # should always be "degrees_east"

# get time
time <- ncvar_get(ncin,"time")
tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)

# get raster version of the data for a particular variable. This makes it easy to do raster math on the data.
ncin.brick <- brick(temp, varname = varToGet) # because there is no explicit projection info in the netcdf files, this is assumed - +proj=longlat +datum=WGS84"
brick.crs <- crs(ncin.brick)
# Close the netCDF file, flushes unwritten data to the disk
nc_close(ncin)

# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1]) # starting year

# get a map of country borders
borders <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))

# prepare a data frame from the raster brick version of the netcdf data. This can be used in ggplot which requires a data frame for its data input
# I use data.table because it is a data frame and I like the syntax for manipulating it.
mydf.complete <- purrr::map_dfr(
  as.list(ncin.brick), 
  ~setNames(as.data.table(as(., "SpatialPixelsDataFrame")), c('value', 'x', 'y')), 
  .id = 'year'
)
mydf.complete[, year := as.numeric(year)]

# get 7 color palatte
p <- colorspace::sequential_hcl(n = 7, palette = "Terrain 2",
                                rev = TRUE)

legendText <- "fraction of grid cell"
plotTitle <-  ncin.brick@title
plotTitle <- gsub("\\.", " ", plotTitle)
plotTitle <- paste0(toupper(substr(plotTitle, 1, 1)), substr(plotTitle, 2, nchar((plotTitle))))
plotTitle <- paste0(plotTitle, ", ", rcp, ", ", ssp, ", Model: ", model)

# the raster::plot command is a quick way to see what you have
yearVal = 2100
yearVal.converted = yearVal -  tyear - 1
plotTitle.oneYear <- paste0(plotTitle, ", Year: ", yearVal)
raster::plot(ncin.brick[[yearVal.converted]], main = plotTitle.oneYear, col = p)

# using ggplot gives more control over what gets plotted but can be slow. It requires a data.frame
# pull out one year of the data for plotting
# convert to netCDF year numbers
yearNum = yearVal - tyear - 1
mydf.oneyear <- mydf.complete[year %in% yearNum]

gg <- ggplot()
gg <- gg + geom_sf(data = borders, fill = "transparent", color = "black") # +
gg <- gg + ggthemes::theme_map()
gg <- gg + geom_tile(data = mydf.oneyear, aes(x = x, y = y, fill = value), alpha = 0.4)
gg <- gg + scale_fill_gradientn(colors = p, name = legendText,
                                na.value = "grey50",
                                guide = "colorbar") #, values = bb, breaks = f, limits = f, labels = f, aesthetics = "fill")
gg <- gg + theme(legend.position = "right")
gg <- gg + labs(title = plotTitle.oneYear)
gg <- gg + theme(plot.title = element_text(hjust = 0.5)) #center the title
print(gg)

# Three (or more) approaches to animation - using gganimate, tmap, and animate's saveGIF and saveMovie

# gganimate - works with ggplot.

# reduce number of years to speed up testing

yearsToDisplay <- c(10, 20, 30, 40, 50, 60, 70, 80)

mydf.multiYears <- mydf.complete[year %in% yearsToDisplay]
# convert relative years to absolute years
mydf.multiYears[, year := year + tyear - 1]
gg <- ggplot(mydf.multiYears, aes(x = x, y = y, fill = value))
gg <- gg +  geom_sf(data = borders, color = "black", inherit.aes = FALSE)
gg <- gg +  geom_tile()

gg <- gg + scale_fill_gradientn(colors = p, name = legendText,
                                na.value = "grey50",
                                guide = "colorbar") #, values = bb, breaks = f, limits = f, labels = f, aesthetics = "fill")
gg <- gg + ggthemes::theme_map()
gg <- gg + theme(plot.title = element_text(hjust = 0.5))
print(gg)


# if the animation doesn't show much change, do some raster math to see what happens between the first and last year
firstYear <- 1
lastYear <- nlayers(ncin.brick)

deltaLastMinusFirst <- ncin.brick[[lastYear]] -  ncin.brick[[firstYear]]

# some experiments with tmap for animations
# Source of info is https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html

# from tmap
data("World")
# set some tmap session options
tmap_options(max.raster = c(plot = 1036800, view = 1036800))
tmap_mode("plot") #use "view" for interactive maps; "plot" for static maps

#plot one year with tmap, the first year X0
tmMap <- tm_shape(ncin.brick) + tm_raster("X0", palette = terrain.colors(10, rev = TRUE)) +
  tm_shape(World) +
  tm_borders("white", lwd = .5) +
  tm_text("iso_a3", size = .5)  + tm_view(text.size.variable = TRUE)

tmMap

# animations using the animation package

filename <- "animationSaveGIF.gif"
animation::saveGIF(
  {
    for (m in 1:nlayers(ncin.brick)) { 
      raster::plot(ncin.brick[[m]], main = paste0(plotTitle, ", year ", m + tyear - 1) )
    }
  }, movie.name = filename, img.name = "Rplot", convert = "magick", path = "graphics")

gganimate::anim_save(filename, animation = last_animation(), path = "graphics", renderer = gifski_renderer())

filename <- "graphics/animationSavemovie.mp4"
animation::saveVideo({
  for (m in 1:nlayers(ncin.brick)) {        
    raster::plot(ncin.brick[[m]], main = paste0(plotTitle, ", year ", m + tyear - 1) )       # min, max of the legend
  }
}, video.name = filename, img.name = "Rplot", ffmpeg = ani.options("ffmpeg"))

