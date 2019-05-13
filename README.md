
# CRR_2018
CRR Profile Redesign for 2018

-------

##Primary Source Data:

1. SQL Server table with all NFIRS incidents. 
2. Tiger Database with Census polygons and demographic data. 
2. 10 Year Census blocks with population and housing numbers. 
3. Fire Fatality Datadatabase. 
4. Geocoded structure fires. 
5. Risk map with risk assigned to each block. 
6. Fire Fatality NFIRS extract to supplement offcial record. 

-------

## Developing the secondary Source Data (should be run in this order)
Note that these only need to be run once per year (or whatever other cycle the profiles are updated).

### readcensus.R
Aggregates census data by fire department using TIGER geodatabase.

### ArcGIS.py
Called by readcensus.R. This is responsible for the spatial join of Fire Departments to blocks. 

### datapull.R
Gathers call volume data from sql server table, and creates R object (.rds)

### oldfatalities.R
Gathers fatality statistics from Access database and NFIRS data warehouse files.

### ResponseTimes.R
Uses the cv.RMD file to generate response time data for fire calls. 

### FireRates.R
Develops data regarding structure fires. 



------

## Developing the report. 

### generate.report.R
This is the r file that is used to generate a report.

### report.rmd
This is the styling of the report. generate.report.R executes this. 

### WaffleProp.R
Contains the code to create waffle visualizations and bar charts. 

## Other r scripts

### Convert.R 
Converts the names and formats of the images exported by ArcGIS. 

------

## Folder Structure

### CRR_2018

Contains all of the r scripts and subfolders. 

### Data

Black group, fire department boundaries,old fatality spreadsheet, and the various .RDS files are stored here. 

### Output

This is where the CRR Profiles are output to. 

### Report

The rmarkdown file report.rmd is here.

### Graphics

Static images such as maps statewide risk etc are stored here. 


