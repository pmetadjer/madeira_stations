########################################
#### Extract Flow and Precip Data   ####
#### From Amazon Dams Network Files ####
#### Pamela Senesi, Shar Siddiqui   ####
#### 11/06/2018                     ####
########################################

# Goal: Make list of Fluviometric & Pluviometric Stations Available in ADN Database (Has LAT & LON)

## Needed data files
# Madeira Metadata (lat/lon/id info)
# ADN flow data
# ADN precipitation

## User Defined Parameters
data_dir = "C:/Users/Shar/OneDrive - University of Florida/Data/Spatial" #i.e. wherever you keep the Madeira_Stations.csv metadata
pluvio_dir = "C:/Users/Shar/OneDrive - University of Florida/Data/ADN_DB/WATERSHED_1_AMAZON/Rainfall_WS1"
fluvio_dir = "C:/Users/Shar/OneDrive - University of Florida/Data/ADN_DB/WATERSHED_1_AMAZON/Flow_WS1"
madeira_flow_dir = "C:/Users/Shar/OneDrive - University of Florida/Data/Madeira/ADN/Fluvio"
madeira_precip_dir = "C:/Users/Shar/OneDrive - University of Florida/Data/Madeira/ADN/Pluvio"

#Load Packages/Library
if (!require("rgdal")) { install.packages("rgdal", dependencies = TRUE); library(rgdal) }
if (!require("data.table")) { install.packages("data.table", dependencies = TRUE); library(data.table) }
if (!require("dplyr")) { install.packages("dplyr", dependencies = TRUE); library(dplyr) }
if (!require("readxl")) { install.packages("readxl", dependencies = TRUE); library(readxl) }

#########---MADEIRA INFO---###########
#Download (Shar's) Excel file with all stations needed (contains location info).
setwd(data_dir)
all_madeira <- read.csv("Madeira_Stations.csv") 
#Extract Madeira station code numbers.
madeira_id <- all_madeira[ , c("station")]
#Turn this into a list of stations needed (Madeira Basin Stations)
stations_needed <- as.data.frame(madeira_id)

## note: may want to move this section after pluvio file names is created
## Matching Test: match returns a vector of the positions of (first) matches of its first argument in its second.
madeira_flow_match = match(madeira_id,as.numeric(fluvio.names),nomatch=0)#all files in ADN that correspond to Madeira stn ids
madeira_precip_match = match(madeira_id,as.numeric(pluvio.names),nomatch=0)
## this is looking *through* Madeira ID's for matches

##We want to take the ones that match and move them to Madeira/ADN/Fluvio or Pluvio
flow_match = unique(madeira_flow_match) #126 through 190
precip_match = unique(madeira_precip_match) #not as neat of an order

setwd(fluvio_dir)
file.copy(fluvio.filenames[flow_match],madeira_flow_dir) #works!

setwd(pluvio_dir)
file.copy(pluvio.filenames[precip_match],madeira_precip_dir)


#########---Setting NAMES OF FILES---###########
##Read files; i.e. filenames <- list.files(path="PATH/FOLDER OF FILES NEEDED") 
pluvio.filenames <- list.files(path = pluvio_dir)
fluvio.filenames <- list.files(path = fluvio_dir)

#Create list of new names (In this case we remove ".csv" part & select wanted length)
#Example: names <- substr(filenames, lower limit , upper limit)
#NOTE: we only want the station name (i.e. fluvio - 10100000, pluvio - 00469000)
pluvio.names <- substr(pluvio.filenames, 13, 20)
fluvio.names <- substr(fluvio.filenames, 13, 20)

####---IMPORTING EXCEL FILES---####
#1 Make a list of all excel files
#select folder/path containing excel files as temporary directory
setwd(fluvio_dir) 
fluvio.file.list <- list.files(pattern='*.xlsx', recursive = TRUE)
#2 Convert list of excel files into dataframe (store actual files there)
fluvio.df.list <- lapply(fluvio.file.list, read_excel) 
names(fluvio.df.list) <- fluvio.names #sets name of Tibble to station code #

setwd(pluvio_dir)
pluvio.file.list <- list.files(pattern='*.xlsx', recursive = TRUE)
#2 Convert list of excel files into dataframe (store actual files there)
pluvio.df.list <- lapply(pluvio.file.list, read_excel)
names(pluvio.df.list) <- pluvio.names #sets name of Tibble to station code #

#Bind excel files in ONE dataframe 
fluvio.df <- bind_rows(fluvio.df.list, .id = "id")

# pluvio.df <- bind_rows(pluvio.df.list, .id = "id") #DID NOT WORK FOR PLUVIO
#INSERT CODE HERE ## pretty much the same method, just skipping the stations that are all NA's
pluvio.df <- data.frame()
for (i in 1:length(pluvio.df.list)) {
  if(i==228) next
  if(i==233) next
  if(i==460) next
  if(i==500) next
  a <- pluvio.df.list[i] 
  pluvio.df <- bind_rows(pluvio.df,a)
  #print(i) ## so 228,233,460 is the problem...
}

#########---MATCHING STATIONS---FIRST ATTEMPT FAILED###########

# #Check where in the all_pluvio data set the stations we want are located [position].
# 
# #We put this info in a vector named "match_pluvio".
# 
# match_fluvio <- match(fluvio.df$id, stations_needed$madeira_stations)
# match_pluvio <- match(stations_needed$madeira_stations,all_pluvio$STATION_CODE)
# 
# #We remove all NA to clear the results.
# match_fluvio <- match_fluvio[!is.na(match_fluvio)]
# match_pluvio <- match_pluvio[!is.na(match_pluvio)]
# 
# #Now we subset by square brakets "[]" to get the code name.
#   # NOTE: this part can be done with ANY of the columns (i.e. LAT, LON...)
# 
# all_fluvio$STATION_CODE[match_fluvio] 
# all_pluvio$STATION_CODE[match_pluvio] 
# 
# #Finally, we create a new column.
# stations_needed$names =
#   all_pluvio$STATION_NAME[match(stations_needed$madeira_stations,all_pluvio$STATION_CODE)]
# 
# stations_needed$names =
#   all_pluvio$STATION_NAME[match(stations_needed$madeira_stations,all_pluvio$STATION_CODE)]# 

# Change column name for stations into 'id' and make numeric
stations_needed <- data.table(stations_needed)
setnames(stations_needed, "madeira_stations", "id")
stations_needed <- data.frame(stations_needed)
stations_needed$id <- as.numeric(stations_needed$id)
fluvio.df$id <- as.numeric(fluvio.df$id)

#Merge stations needed and stations available by station 'id'
new_for_fluvio <- merge(fluvio.df, stations_needed, by = "id")

# change position of columns with NA 
# convert NA to 999999
new_for_fluvio$DATE <- as.character(new_for_fluvio$DATE)
new_for_fluvio$Date <- as.character(new_for_fluvio$Date)

new_for_fluvio$DATE[is.na(new_for_fluvio$DATE)] <- 999999
new_for_fluvio$Date[is.na(new_for_fluvio$Date)] <- 999999

new_for_fluvio_test <- new_for_fluvio[DATE == "999999"]
new_for_fluvio <- new_for_fluvio[DATE != "999999"]

for(i in 1:nrow(new_for_fluvio_test)){
  if(new_for_fluvio_test$DATE[i] == "999999"){
    new_for_fluvio_test$DATE[i] <- new_for_fluvio_test$Date[i]
  }
}

new_for_fluvio <- rbind(subset(new_for_fluvio, DATE != 999999), new_for_fluvio_test)

# Change flow columns with different names to 'FLOW' and turn all NA to 999999
# Make sure all of them are numeric.

new_for_fluvio <- data.table(new_for_fluvio)
setnames(new_for_fluvio, c("FLOW (m???/s)","Flow (m3/s)", "Flow(m3/s)"), 
         c("FLOW","flow1", "flow2"))
new_for_fluvio <- data.frame(new_for_fluvio)

# new_for_fluvio$FLOW  <- as.character(new_for_fluvio$FLOW)
# new_for_fluvio$flow1 <- as.character(new_for_fluvio$flow1)
# new_for_fluvio$flow2 <- as.character(new_for_fluvio$flow2)

new_for_fluvio$FLOW[is.na(new_for_fluvio$FLOW)]   <- 999999
new_for_fluvio$flow1[is.na(new_for_fluvio$flow1)] <- 999999
new_for_fluvio$flow2[is.na(new_for_fluvio$flow2)] <- 999999

# new_for_fluvio$FLOW <- as.numeric(new_for_fluvio$FLOW)
# new_for_fluvio$flow1 <- as.numeric(new_for_fluvio$flow1)
# new_for_fluvio$flow2 <- as.numeric(new_for_fluvio$flow2)

new_for_fluvio <- data.frame(new_for_fluvio)
new_for_fluvio_test1 <- subset(new_for_fluvio, FLOW == 999999)

for(i in 1:nrow(new_for_fluvio_test1)){
  if(new_for_fluvio_test1$FLOW[i] == '999999'){
    new_for_fluvio_test1$FLOW[i] <- new_for_fluvio_test1$flow1[i]
  }
}

new_for_fluvio <- rbind(subset(new_for_fluvio, FLOW != 999999), new_for_fluvio_test1)


new_for_fluvio_test2 <- subset(new_for_fluvio, FLOW == 999999)

for(i in 1:nrow(new_for_fluvio_test2)){
  if(new_for_fluvio_test2$FLOW[i] == '999999'){
    new_for_fluvio_test2$FLOW[i] <- new_for_fluvio_test2$flow2[i]
  }
}


new_for_fluvio <- rbind(subset(new_for_fluvio, FLOW != 999999), new_for_fluvio_test2)

#Extract three needed rows into a new data frame

FINALLY.FLOW <- new_for_fluvio

FINALLY.FLOW$Date <- NULL
FINALLY.FLOW$flow1 <- NULL
FINALLY.FLOW$flow2 <- NULL

write.csv(FINALLY.FLOW, 'finally.fluvio.csv')

# IDEA: (in progress) 
# Add columns with names of the stations, lat, long, and other info.

# clean_fluvio <- data.frame()
# 
# for(i in 1:nrow(fluviometric)){
#   if(fluviometric$FLOW[i] != 999999){
#     fluviometric[i] <- fluviometric[i]
#   }
# }


