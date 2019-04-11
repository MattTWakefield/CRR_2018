
# CRR_2018
CRR Profile Redesign for 2018

# Presenet Challenges

* Compare call volume results to fdtn.(After Report Creation)
* Create maps for boundaries, structure fires, and risk.
* Have team approve of loss mitigation strategies. 
* Seek input on additional ideas. 
* Create bubble graphs for loss and count. 
* Create styling sheet. 


-------

##Developing the Source Data
Note that these only needs to be run once per year (or whatever other cycle the profiles are updated)

### readcensus.R
Aggregates census data by fire department using TIGER geodatabase.

### ArcGIS.py
Called by readcensus.R. This is responsible for the spatial join of Fire Departments to blocks. 

### oldfatalities.R
Gathers fatality statistics from Access database and NFIRS data warehouse files.

### datapull.R
Gathers call volume data from sql server table, and creates R object (.rds)

------

##Developing the report. 

### callvolume.R
This is the r file that is used to generate a report.

### report.rmd
This is the styling of the report. callvolume.R executes this. 

### WaffleProp.R
Contains the code to create waffle visualizations and bar charts. 