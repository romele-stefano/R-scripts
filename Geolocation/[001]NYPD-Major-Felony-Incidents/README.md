# -R-NYPD-7-Major-Felony-Incidents

Analysis of the data set [NYPD-7-Major-Felony-Incidents](https://data.cityofnewyork.us/Public-Safety/NYPD-7-Major-Felony-Incidents/hyij-8hr7)   
Check the blog post [NYC crime map] (https://romele-stefano.github.io/coding-blog/r/2016/10/24/NYC.html)     

The data set includes 1123465 observations of 20 variables:
	- id
	- identifier
	- occurrence date
	- day of the week
	- occurrence month
	- occurrence day
	- occurrence year
	- occurrence hour
	- compStat month
	- compStat day
	- compStat year
	- offense
	- offense classification
	- sector
	- precinct
	- borough
	- jurisdition
	- Xcoordinate
	- Ycoordinate
	- location

	
| Script | What it does |
|--------|--------------|
| 01 | Call libraries, set working directory, load data, manipulate data |
| 02 | Plot distribution (barplot and pie chart) of felonies by day of the week |
| 03 | Plot distribution (barplot and pie chart) of felonies by month |
| 04 | Plot (barplot and pie chart) distribution of felonies by year |
| 05 | Plot (barplot and pie chart) distribution of felonies by type |
| 06 | Plot (barplot and pie chart) distribution of felonies by borough |
| 07 | Plot (barplot and pie chart) distribution of felonies by borough and type |
| 08 | Plot (barplot) distribution of felonies by borough per 1000 people |
| 09 | Plot (barplot) distribution of felonies by borough per square mile |
| 10 | Plot (barplot) distribution of felonies by jurisdition |
| 11 | Create map by borough for Offense = Rape |
| 12 | Create map by borough for Offense = Murder |
| 13 | Create nyc map for Offense = Rape and Murder |
