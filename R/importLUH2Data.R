library(ncdf4)
# library(RNetCDF)
library(raster)
library(maptools)
data(wrld_simpl)
plot(wrld_simpl,add = TRUE)
library(moveVis)
library(move)
library(data.table)
library(RCurl)
library(curl)

#Introduction
# Land use harmonization data are take from http://luh.umd.edu.
# The url with data downloads is http://luh.umd.edu/data.shtml
# The data available at this url are states, transitions, and management, stored in netcdf format.
# There are 8 versions of these data sets based on combinations of RCPs and SSPs. Each data set is generated
# by a single IAM.
# 6 were released on Dec 21, 2017. Two additional data sets (RCP1.9 SSP1, RCP3.4OS SSP5) were released on Nov 29, 2018.
# The file names are contained in the fileChoices variable.

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

# remove the directory in the fileChoices names
outfileNames <- gsub(".*/", "", fileChoices)
readmefile <- "http://gsweb1vh2.umd.edu/LUH2/LUH2_v2f_README_v6.pdf"
outreadmefile <- "http://gsweb1vh2.umd.edu/LUH2/LUH2_v2f_README_v6.pdf"

destDir <- paste0(getwd(), "/data-raw/")
for (i in fileChoices) {
  url <- paste0(baseURL, i)
  destfile <- paste0(destDir, i)
  curl_download(url, destfile)
}
temp <- paste0("data-raw/", states_ssp585)
ncin <- ncdf4::nc_open(temp)
# list variable names in ncin
names(ncin[['var']])
# print number of years in ncin
ncin$dim$time$len

# print start year
ncin$dim$time$units

# get the long name of variable primf
ncin[["var"]][["primf"]][["longname"]]
ncin.1 <- raster(temp, varname = "primf")
ncin.brick <- brick(temp, varname = "primf")
ncin.stack <- stack(temp, varname = "primf")
outdir <- "graphics"
rasterOptions(tmpdir = outdir)
#plot years 1:10
raster::plot(ncin.stack, 1:10, main = ncin.stack@title, xlab = "long", ylab = "lat")

# extract the 15th year from ncin.stack
singleYear <- 15
oneYear <- subset(ncin.stack, subset = 15)
plotTitle <-  ncin.stack@title
plotTitle <- gsub("\\.", " ", ncin.stack@title)

# capitalize the first word
plotTitle <- paste0(toupper(substr(plotTitle, 1, 1)), substr(plotTitle, 2, nchar((plotTitle))))
      
plot(oneYear, main = plotTitle, xlab = "long", ylab = "lat")
raster::animate(ncin.stack, main = ncin.stack@title, zlim = -90:90)


curl_download("http://gsweb1vh2.umd.edu/LUH2/LUH2_v2f/IMAGE_SSP1_RCP19/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp119-2-1-f_gn_2015-2100.nc", destfile = "test.nc")

