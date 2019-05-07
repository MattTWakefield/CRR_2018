
# CRR_2018
CRR Profile Redesign for 2018

-------

##Developing the Source Data
Note that these only need to be run once per year (or whatever other cycle the profiles are updated).

### readcensus.R
Aggregates census data by fire department using TIGER geodatabase.

### ArcGIS.py
Called by readcensus.R. This is responsible for the spatial join of Fire Departments to blocks. 

### oldfatalities.R
Gathers fatality statistics from Access database and NFIRS data warehouse files.

### datapull.R
Gathers call volume data from sql server table, and creates R object (.rds)

###ResponseTimes.R

Uses the cv.RMD file to generate response time data for fire calls. 



------

##Developing the report. 

### generate.report.R
This is the r file that is used to generate a report.

### report.rmd
This is the styling of the report. generate.report.R executes this. 

### WaffleProp.R
Contains the code to create waffle visualizations and bar charts. 

##Other r scripts

###Convert.R 
Converts the names and formats of the images exported by ArcGIS. 

------

## Folder Structure

###CRR_2018

Contains all of the r scripts and subfolders. 

###Data

Black group, fire department boundaries,old fatality spreadsheet, and the various .RDS files are stored here. 

###Output

This is where the CRR Profiles are output to. 

###Report

The rmarkdown file report.rmd is here.

###Graphics

Static images such as maps statewide risk etc are stored here. 

###


