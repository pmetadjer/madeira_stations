#Load Library:
  
library(dplyr)
library(rgdal)

#Make list of Fluviometric & Pluviometric Stations Available in NDA Database (Has LAT & LON)

all_fluvio <- read.csv("FINAL_AllAmazon_Fluvio1.csv")
all_fluvio

all_pluvio <- read.csv("FINAL_AllAmazon_Pluvio1.csv")
all_pluvio

 #NOTE: These come from the metadata files.

#Make list of stations needed (Madeira Basin Stations)

stations_needed <- as.data.frame(madeira_stations)

#########---Setting NAMES OF FILES---###########

setwd("/Users/mac/Desktop/Madeira_Basins/")

##Read files 
  #Example: filenames <- list.files(path="PATH/FOLDER OF FILES NEEDED") 

  pluvio.filenames <- list.files(path = "Rainfall_WS1")
  fluvio.filenames <- list.files(path="Flow_WS1")
  
#Create list of new names (In this case we remove ".csv" part & select wanted length)
  #Example: names <- substr(filenames, lower limit , upper limit)
  #NOTE: we only want the station name (i.e. fluvio - 10100000, pluvio - 00469000)

  pluvio.names <- substr(pluvio.filenames, 4, 11)
  fluvio.names <- substr(fluvio.filenames, 3, 10)

##################################################
###----------IMPORTING EXCEL FILES-------------###
###---------ONE OF MANY WAYS TO DO SO----------###
##################################################

#1 Make a list of all excel files

library(readxl)

  #select folder/path containing excel files as temporary directory
  setwd("Flow_WS1") 
  fluvio.file.list <- list.files(pattern='*.xlsx', recursive = TRUE)
  fluvio.file.list

  setwd("Rainfall_WS1")
  pluvio.file.list <- list.files(pattern='*.xlsx', recursive = TRUE)
  pluvio.file.list

#2 Convert list of excel files into dataframe (store actual files there)

fluvio.df.list <- lapply(fluvio.file.list, read_excel) 
names(fluvio.df.list) <- fluvio.names #sets name of Tibble to station code #
fluvio.df.list

pluvio.df.list <- lapply(pluvio.file.list, read_excel)
names(pluvio.df.list) <- pluvio.names #sets name of Tibble to station code #
pluvio.df.list

pluvio.df.list

#Bind excel files in ONE dataframe 

library(dplyr)
fluvio.df <- bind_rows(fluvio.df.list, .id = "id")
pluvio.df <- bind_rows(pluvio.df.list, .id = "id")

pluvio.df <- bind_rows(pluvio.df.list)


#########---MATCHING STATIONS---###########

#Check where in the all_pluvio data set the stations we want are located [position].

#We put this info in a vector named "match_pluvio".

match_fluvio <- match(fluvio.df$id, stations_needed$madeira_stations)
match_fluvio

match_pluvio <- match(stations_needed$madeira_stations,all_pluvio$STATION_CODE)
match_pluvio

#We remove all NA to clear the results.
match_fluvio <- match_fluvio[!is.na(match_fluvio)]
match_fluvio

match_pluvio <- match_pluvio[!is.na(match_pluvio)]
match_pluvio

#Now we subset by square brakets "[]" to get the code name.
  # NOTE: this part can be done with ANY of the columns (i.e. LAT, LON...)

all_fluvio$STATION_CODE[match_fluvio] 
all_pluvio$STATION_CODE[match_pluvio] 

#Finally, we create a new column.
stations_needed$names =
  all_pluvio$STATION_NAME[match(stations_needed$madeira_stations,all_pluvio$STATION_CODE)]

stations_needed$names =
  all_pluvio$STATION_NAME[match(stations_needed$madeira_stations,all_pluvio$STATION_CODE)]


###SHAR SUGGESTION###

#make a list before the loop and then call it back inside to save the resulting files.


flow.data <- list()
name <- all_stations$name

for (i in 1:length(pluvio.df.list)){
  read_xlsx(paste("0", madeira_stations[i],name[i], ".xlsx", sep="_"))
}

for (i in 1:length(pluvio.df.list)){
  read_xlsx(paste("0", madeira_stations[i],name[i], ".xlsx", sep="_"))
}

df[row, column]

for (i in fluvio.df){
  flow.data <- fluvio.df[i,1] %in% madeira_stations
  print(flow.data)
}

new <- matrix(0, nrow = nrow(fluvio.df), ncol = 6)


# Change column name for stations

stations_needed <- data.table(stations_needed)
setnames(stations_needed, "madeira_stations", "id")
stations_needed <- data.frame(stations_needed)
stations_needed$id <- as.numeric(stations_needed$id)
fluvio.df$id <- as.numeric(fluvio.df$id)

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

# take care of the flows 

new_for_fluvio <- data.table(new_for_fluvio)
setnames(new_for_fluvio, c("FLOW (mᶟ/s)","Flow (m3/s)", "Flow(m3/s)"), 
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






# For future reference

for(i in 1:nrow(fluvio.df)){
  for(j in 1:nrow(stations_needed)){
    if(fluvio.df$id[i] == stations_needed$madeira_stations[j]){
      new[i,] <- fluvio.df[i,]
    }
  }
}

