Import and Process Land Use Data used as Input to CMIP6
================
Gerald Nelson
11/20/2019

The goal of this project is to provide code to import the land use data
files used by the CMIP6 project. It includes code to automatically
import all the files from their official storage location
[1](http://luh.umd.edu)

# Directory structure

The directory hierarchy of the project is as follows. Second level
directories are in parentheses:

  - data — contains .xlsx and .rds files that are generated from data in
    the data-raw folder and used to produce results
  - data-raw — data files from original data sources
  - graphics — graphic outputs from the analysis
  - R — R scripts
  - Results — results from the analysis, contains .xlsx and .rds files

This directory structure is created automatically in the script.

The script also automatically imports libraries that are needed and not
on the user’s computer.

More detailed documentation is contained in the R script
importLUH2Data.R. The script should be run in sections to see what where
particular outputs are generated.

This is a work in progress. Comments, suggestions, improvements
appreciated\!
