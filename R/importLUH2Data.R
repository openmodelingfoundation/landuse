library(ncdf4)
library(raster)
library(maps)
library(maptools)
library(rgeos)
library(sp)
library(sf)
# library(moveVis)
# library(move)
library(data.table)
library(curl)
library(rasterVis)
library(rgdal)
#library(animation)
library(gganimate)
library(magick)
#library(lattice)
library("colorspace")

# much of the processing code is taken from http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html

#Introduction
# Land use harmonization data are take from http://luh.umd.edu.
# The url with data downloads is http://luh.umd.edu/data.shtml
# The data available at this url are states, transitions, and management, stored in netcdf format.
# There are 8 versions of these data sets based on combinations of RCPs and SSPs. Each data set is generated
# by a single IAM.
# 6 were released on Dec 21, 2017. Two additional data sets (RCP1.9 SSP1, RCP3.4OS SSP5) were released on Nov 29, 2018.
# The file names are stored in the fileChoices variable.
# The variable names are identical in each file - primf, primn, secdf, secdn, urban, c3ann, c4ann, c3per, c4per, 
# c3nfx, pastr, range, secmb, secma, lat_bounds, lon_bounds, time_bnds.
# The slightly more explanatory name for each is - forested primary land, non-forested primary land, 
# potentially forested secondary land, potentially non-forested secondary land, urban land, C3 annual crops, 
# C4 annual crops, C3 perennial crops, C4 perennial crops, C3 nitrogen-fixing crops, managed pasture, rangeland, 
# secondary mean biomass carbon density, secondary mean age, lat_bounds, lon_bounds, time_bnds
# The data are annual from 2015 to 2100

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

# create variable for file content
fileContent <- gsub("multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-", "", fileChoices)
fileContent <- gsub(".*/", "", fileContent)
fileContent <- gsub(".nc", "", fileContent)
# remove the subdirectory names in the fileChoices names
outfileNames <- gsub(".*/", "", fileChoices)

readmefile <- "http://gsweb1vh2.umd.edu/LUH2/LUH2_v2f_README_v6.pdf"
outreadmefile <- "references/LUH2_v2f_README_v6.pdf"

destDir <- paste0(getwd(), "/data-raw/")
destDirFiles <- list.files(destDir) # might be better than file.exists below
# download files if they haven't already been downloaded
for (i in 1:length(fileChoices)) {
  url <- paste0(baseURL, fileChoices[i])
  destfile <- paste0(destDir, outfileNames[i])
  if (!file.exists(destfile)) curl_download(url, destfile)
}
if (!file.exists(outreadmefile)) curl_download(readmefile, destfile = outreadmefile)

# display some useful information. It is identical for all these files but I'll leave the code in for now with the print statements commented out
for (i in 1:length(outfileNames)) {
#  print(paste0("Reporting on: ", outfileNames[i]))
  temp <- paste0("data-raw/", outfileNames[i])
  ncin <- ncdf4::nc_open(temp)
  # list variable names in ncin
  varNames <- names(ncin[['var']])
#  print(varNames)
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

# do some crunching on one of the variables  - c3ann from the first of the netcdf files - change varToGet for other variables
fileNumber <- 1
varToGet <- "c3ann"
temp <- paste0("data-raw/", outfileNames[fileNumber])
ncin <- ncdf4::nc_open(temp)
content <- fileContent[fileNumber]

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
latunits <- ncatt_get(ncin,"lat","units")$value # should always be "degrees_north"
lonunits <- ncatt_get(ncin,"lon","units")$value # should always be "degrees_east"

# get time
time <- ncvar_get(ncin,"time")
tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)

ncin.brick <- brick(temp, varname = varToGet) # because there is no explicit projection info in the netcdf files, this is assumed - +proj=longlat +datum=WGS84"
brick.crs <- crs(ncin.brick)
# Close the netCDF file, flushes unwritten data to the disk
nc_close(ncin)

# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])

# do a nice looking graph of year x between 2015 and 2100
world <- readRDS("data-raw/worldMap.RDS") # has a latlong projection. This can be changed in the functions.R code.
# an alternative
borders <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))

# prepare a data frame from the raster brick version of the netcdf data. This can be used in ggplot which requires a data frame for it's data input
# I use data.table because it is a data frame and I like the syntax for manipulating it.
mydf.complete <- purrr::map_dfr(
  as.list(ncin.brick), 
  ~setNames(as.data.table(as(., "SpatialPixelsDataFrame")), c('value', 'x', 'y')), 
  .id = 'year'
)
mydf.complete[, year :=  as.numeric(year)]

# pull out one year of the data for plotting
yearToDisplay <- 2040
# convert to netCDF year numbers
yearNum = yearToDisplay - tyear - 1
mydf.oneyear <- mydf.complete[year %in% yearNum]

# the plot command is a quick way to see what you have

plot(mydf.oneyear)

# using ggplot gives more control over what gets plotted

# get 7 color palatte
p <- colorspace::sequential_hcl(n = 7, palette = "Green-Yellow",
                    rev = TRUE)

legendText <- dt.names[shortName %in% varToGet, longName]
plotTitle <-  ncin.brick@title
plotTitle <- gsub("\\.", " ", plotTitle)
plotTitle <- paste0(toupper(substr(plotTitle, 1, 1)), substr(plotTitle, 2, nchar((plotTitle))))
plotTitle <- paste0(plotTitle, ", ", yearToDisplay, ", ", content)

gg <- ggplot()
gg <- gg + geom_sf(data = borders, fill = "transparent", color = "black") # +
gg <- gg + ggthemes::theme_map()
gg <- gg + geom_tile(data = mydf.oneyear, aes(x = x, y = y, fill = value), alpha = 0.4)
gg <- gg + scale_fill_gradientn(colors = p, name = legendText,
                                na.value = "grey50",
                                guide = "colorbar") #, values = bb, breaks = f, limits = f, labels = f, aesthetics = "fill")
gg <- gg + theme(legend.position = "right")
gg <- gg + labs(title = plotTitle)
#center the title
gg <- gg + theme(plot.title = element_text(hjust = 0.5))
gg

# animation 

# reduce number of years to speed up testing

yearsToDisplay <- c(10, 20, 30, 40, 50, 60, 70, 80)
plotTitle <-  ncin.brick@title
plotTitle <- gsub("\\.", " ", plotTitle)
plotTitle <- paste0(toupper(substr(plotTitle, 1, 1)), substr(plotTitle, 2, nchar((plotTitle))))
plotTitle <- paste0(plotTitle, " source: ", content)

mydf.multiYears <- mydf.complete[year %in% yearsToDisplay]

gg <- ggplot(mydf.multiYears, aes(x = x, y = y, fill = value))
gg <- gg +  geom_sf(data = borders, color = "black", inherit.aes = FALSE)
gg <- gg +  geom_tile()

gg <- gg + scale_fill_gradientn(colors = p, name = legendText,
                                na.value = "grey50",
                                guide = "colorbar") #, values = bb, breaks = f, limits = f, labels = f, aesthetics = "fill")
gg <- gg + ggthemes::theme_map()
gg <- gg + theme(plot.title = element_text(hjust = 0.5))

gganim <- gg + gganimate::transition_time(year) + labs(title = plotTitle, subtitle = "Year: {frame_time}")
gganim

gganimate::animate(gganim, nframes = length(yearsToDisplay), fps = 1, renderer = ffmpeg_renderer())
gganimate::anim_save("animateOutput", animation = last_animation(), path = "graphics")

# if the animation doesn't show much change, do some raster math to see what happens between the first and last year
firstYear <- 1
lastYear <- nlayers(ncin.brick)

deltaLastMinusFirst <- ncin.brick[[lastYear]] -  ncin.brick[[firstYear]]

