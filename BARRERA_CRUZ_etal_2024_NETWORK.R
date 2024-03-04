###################################################################################################################

#### Author: Maria Barrera-Cruz.

#### License: Permission is granted to use and adapt this code. Please 
#### acknowledge authorship when appropriate.

#### This code allows full reproducibility of the article "Weaving Social Networks from Cultural Similarities 
#### on the Neolithization process in the Western Mediterranean: Evolutionary trajectories using projectile tools
#### by M. Barrera-Cruz, O. García-Puchol, J. Jiménez-Puerto, A. Cortell-Nicolau and J. Bernabeu-Aubán

###################################################################################################################

###########################
##  Prepare the dataset  ##
###########################

##Load the library
##################
library(xlsx)
library(rcarbon)
library(vegan)
library(cluster)
library(dplyr)
library(stringr)
library(formattable)
library(ggplot2)
library(tidyr)
library(igraph)

#####################
##Load the dataset ##
#####################

dates.redes <- read.csv("Dates_Redes_PUBLISH.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
nrow(dates.redes) #116


########################################
## 200 years Time window creation ######
########################################

#Create a column to define the cal.curve used according to the sample
######################################################################
dates.redes$calCurves <- ifelse(dates.redes$Muestra == "Marine", "marine20", "intcal20")

## Calibrate
############

datacal <- rcarbon::calibrate(dates.redes$BP, dates.redes$Sd_short, 
                              calCurves = dates.redes$calCurves)


##################################################################################
##################             TIME BINS            ##############################
##################################################################################

###
#Create the empty data.frame
###


# Step 1: Define a function to create the empty data.frame where the simulations will be saved (several times, one by time-window)
create_timebins_frame <- function(dates.redes) {
  timebins <- as.data.frame(matrix(NA, nrow(dates.redes), ncol(dates.redes)))
  names(timebins) <- names(dates.redes)
  return(timebins)
}

# Step 2: With the previous function, create an empty data.frame for every time-bin.

v8600.8400.new <- create_timebins_frame(dates.redes)
v8400.8200.new <- create_timebins_frame(dates.redes)
v8200.8000.new <- create_timebins_frame(dates.redes)
v8000.7800.new <- create_timebins_frame(dates.redes)
v7800.7600.new <- create_timebins_frame(dates.redes)
v7600.7400.new <- create_timebins_frame(dates.redes)
v7400.7200.new <- create_timebins_frame(dates.redes)
v7200.7000.new <- create_timebins_frame(dates.redes)
v7000.6800.new <- create_timebins_frame(dates.redes)


# Step 3: Save it into a list to iterate when the simulation run
time.bins <- list("v7000.6800.new" = v7000.6800.new,
                  "v7200.7000.new" = v7200.7000.new,
                  "v7400.7200.new" = v7400.7200.new,
                  "v7600.7400.new" = v7600.7400.new,
                  "v7800.7600.new" = v7800.7600.new,
                  "v8000.7800.new" = v8000.7800.new,
                  "v8200.8000.new" = v8200.8000.new,
                  "v8400.8200.new" = v8400.8200.new,
                  "v8600.8400.new" = v8600.8400.new)

rm(v8600.8400.new, v8400.8200.new, v8200.8000.new, v8000.7800.new, v7800.7600.new, 
   v7600.7400.new, v7400.7200.new, v7200.7000.new, v7000.6800.new)

###
## Now save dates in every time bin according to the prDens and save results in the previously created data.frames
###

upbond <- seq(7000, 8600, by = 200) 
lowbond <- seq(6801, 8401, by = 200)

for (i in 1:length(datacal)){
  for (j in 1:length(upbond)) {
    a <- sum(datacal$grids[[i]]$PrDens[datacal$grids[[i]]$calBP %in% c(upbond[j]:lowbond[j])])
    if(a >= 0.35) {
      time.bins[[j]][i,] <- dates.redes[i,]
    } else {print(i)}  
  }  
}

time.bins <- lapply(time.bins, function(df) na.omit(df)) # Delete rows with all NA



###
## Create a summary table with info about the context and dates present in every time bin
###


# Step 1: Duplicate the original
v8600.8400.Context <- time.bins[[9]]
v8400.8200.Context <- time.bins[[8]]
v8200.8000.Context <- time.bins[[7]]
v8000.7800.Context <- time.bins[[6]]
v7800.7600.Context <- time.bins[[5]]
v7600.7400.Context <- time.bins[[4]]
v7400.7200.Context <- time.bins[[3]]
v7200.7000.Context <- time.bins[[2]]
v7000.6800.Context <- time.bins[[1]]


# Step 3: Save it into a list to iterate when the simulation run
time.context <- list("v8600.8400.Context" = v8600.8400.Context,
                     "v8400.8200.Context" = v8400.8200.Context,
                     "v8200.8000.Context" = v8200.8000.Context,
                     "v8000.7800.Context" = v8000.7800.Context,
                     "v7800.7600.Context" = v7800.7600.Context,
                     "v7600.7400.Context" = v7600.7400.Context,
                     "v7400.7200.Context" = v7400.7200.Context,
                     "v7200.7000.Context" = v7200.7000.Context,
                     "v7000.6800.Context" = v7000.6800.Context)

rm(v8600.8400.Context, v8400.8200.Context, v8200.8000.Context, v8000.7800.Context, v7800.7600.Context, 
   v7600.7400.Context, v7400.7200.Context, v7200.7000.Context, v7000.6800.Context)

# Step 3: Run the loop to clean the news data frames with only info about Context and Id_short; this will be the base for the match in step 3 
for (i in 1:length(time.context)) {
  time.context[[i]]$Context <- paste(time.context[[i]]$ID_Yac, time.context[[i]]$Nivel, time.context[[i]]$Fase)
  time.context[[i]] <- time.context[[i]] %>%
    select(Context, Id_short)
}

# Step 4: Create the structure of the main table where you will draw what context (row) appears in every time bin (columns)

dates.redes.visual <- dates.redes
dates.redes.visual$Context <- paste(dates.redes.visual$ID_Yac, dates.redes.visual$Nivel, dates.redes.visual$Fase)

time.bins.visual <- as.data.frame(matrix(NA,nrow(table(dates.redes.visual$Context)), ncol = 9))
names(time.bins.visual) <- paste(c("v8600.8400", "v8400.8200", "v8200.8000", "v8000.7800", "v7800.7600", "v7600.7400", "v7400.7200", "v7200.7000", "v7000.6800"))
row.names(time.bins.visual) <- as.data.frame(table(dates.redes.visual$Context))[,1]

# Step 5: Import the information by matching the empty main table with the info in the context tables

for (i in 1:nrow(time.bins.visual)) {   
  for (j in 1:ncol(time.bins.visual)) { 
    match <- grepl(paste0("\\b", row.names(time.bins.visual[i,]), "\\b"), c(time.context[[j]][["Context"]]))  # Check if "i" context is present in the time bin we are interested in [[j]]
    n = sum(match, na.rm=TRUE)  #sum how many times is present
    if(n > 0) {  #if is present...
      time.bins.visual[i,][[j]] <- 1 #then save the rows which contain that context
    } else {time.bins.visual[i,][[j]] <- 0}
  }
}  

# Step 6: Visualice

formattable(time.bins.visual, list(area(col = v8600.8400:v7000.6800) ~ color_tile("transparent", "pink")))


#######################################################################
#####     GEO-DATA TO TIME-BINS
#######################################################################

###########################
## Prepare the dataset ##
###########################

library(xlsx)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)


geo.redes.filtered <- read.csv("Geo_Redes_PUBLISH.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")


##########################################################################################
##########################################################################################
## ATTENTION !!!!!! CONDITIONAL LINES
## ONLY RUN THE NEXT LINES In case we need to DELETES those TYPES with only 1 appearance
##########################################################################################
##########################################################################################

##Create a table with information about the total number of types and the % of appearance.
sum(table(geo.redes.filtered$Type) == 1) # n types with 1 appearance
types <- table(geo.redes.filtered$Type)
nrow(types) # n types
types.info <- (table(geo.redes.filtered$Type)*100)/nrow(geo.redes.filtered)
sum(types.info)  # If it is correct, the sum must be = 100

##Localize the types with only 1 appearance
types.df <- as.data.frame(types)
types.one.appear <- filter(types.df, Freq == 1)

##Now create the new data.frame with geos with more than 1 appearance
geo.redes.filtered.more.t.1.appearance <- as.data.frame(matrix(NA,nrow(geo.redes.filtered), ncol = ncol(geo.redes.filtered)))
names(geo.redes.filtered.more.t.1.appearance) <- colnames(geo.redes.filtered)

for (i in 1:nrow(geo.redes.filtered)) {
  match <- grepl(paste0("\\b", geo.redes.filtered[i,]$Type, "\\b"), c(types.one.appear$Var1))  # Check if "i" context is present in the time bin we are interested in
  n = sum(match, na.rm=TRUE)   #sum how many times is present
  if(n == 0) {  #if is present...
    geo.redes.filtered.more.t.1.appearance[i,] <- geo.redes.filtered[i,] #then save the rows which contain that context
  }
}

geo.redes.filtered.more.t.1.appearance <- geo.redes.filtered.more.t.1.appearance[!is.na(geo.redes.filtered.more.t.1.appearance$ID_Yac),] #Delete NA rows
geo.redes.filtered.original <- geo.redes.filtered
geo.redes.filtered <- geo.redes.filtered.more.t.1.appearance
rm(geo.redes.filtered.more.t.1.appearance)
nrow(table(geo.redes.filtered$Type)) #final ntypes 

##############################
##############################
### FIN OF CONDITIONAL RUN
##############################


###########################################################################
# Once you have time-window, filter by phase and create distance matrices #
###########################################################################

# Step 1: Create a column with context info

geo.redes.filtered$Context <- paste(geo.redes.filtered$ID_Yac, geo.redes.filtered$Nivel, geo.redes.filtered$Fase)
sample.level <- table(geo.redes.filtered$Context) #nsample by site and level

# Step 2: Define a function to create the empty data.frame where the geometrics will be saved 
create_geo_frame <- function(geo.redes.filtered) {
  geotime <- as.data.frame(matrix(NA,nrow(geo.redes.filtered), ncol = ncol(geo.redes.filtered)))
  names(geotime) <- colnames(geo.redes.filtered)
  return(geotime)
}

# ...and With the previously function, create a empty data.frame for every time bin.

geo.v8600.8400.new <- create_geo_frame(geo.redes.filtered)
geo.v8400.8200.new <- create_geo_frame(geo.redes.filtered)
geo.v8200.8000.new <- create_geo_frame(geo.redes.filtered)
geo.v8000.7800.new <- create_geo_frame(geo.redes.filtered)
geo.v7800.7600.new <- create_geo_frame(geo.redes.filtered)
geo.v7600.7400.new <- create_geo_frame(geo.redes.filtered)
geo.v7400.7200.new <- create_geo_frame(geo.redes.filtered)
geo.v7200.7000.new <- create_geo_frame(geo.redes.filtered)
geo.v7000.6800.new <- create_geo_frame(geo.redes.filtered)


# Step 3: Save its into a list to iterate when the loop run
geo.bins <- list("geo.v8600.8400.new" = geo.v8600.8400.new,
                 "geo.v8400.8200.new" = geo.v8400.8200.new,
                 "geo.v8200.8000.new" = geo.v8200.8000.new,
                 "geo.v8000.7800.new" = geo.v8000.7800.new,
                 "geo.v7800.7600.new" = geo.v7800.7600.new,
                 "geo.v7600.7400.new" = geo.v7600.7400.new,
                 "geo.v7400.7200.new" = geo.v7400.7200.new,
                 "geo.v7200.7000.new" = geo.v7200.7000.new,
                 "geo.v7000.6800.new" = geo.v7000.6800.new)

rm(geo.v8600.8400.new, geo.v8400.8200.new, geo.v8200.8000.new, geo.v8000.7800.new, geo.v7800.7600.new,
   geo.v7600.7400.new, geo.v7400.7200.new, geo.v7200.7000.new, geo.v7000.6800.new)

###
## Now save geos in every time bin according to the info from "time.context"
###

for (i in 1:nrow(geo.redes.filtered)) {
  for (j in 1:length(geo.bins)) {
    match <- grepl(paste0("\\b", geo.redes.filtered[i,]$Context, "\\b"), c(time.context[[j]][["Context"]]))  # Check if "i" context is present in the time bin we are interested in
    n = sum(match, na.rm=TRUE)   #sum how many times is present
    if(n > 0) {  #if it is present...
      geo.bins[[j]][i,] <- geo.redes.filtered[i,] #then save the row which contain that context
    } 
  } 
}

geo.bins <- lapply(geo.bins, function(df) {
  # Remove rows with all NA cells
  df[!rowSums(is.na(df)) == ncol(df), ] 
})


#-----------------------------------------------------------------------------------------------------

### Visualization samples result
####################################################


# Types present by phase and time-bin
#####################################

nrow(table(geo.bins[["geo.v8600.8400.new"]]$Context)) #To indicate in area(col = 1:n)
formattable(as.data.frame.matrix(table(geo.bins[["geo.v8600.8400.new"]]$Type, geo.bins[["geo.v8600.8400.new"]]$Context)), list(area(col = 1:4) ~ color_bar("#FA614B66")))
## Repeat for every df inside geo.bins in case necessary 


############################################
##### Calculate distance matrix ############
############################################

## Prepare the dataset 
######################

## Save geo.bins as geo.data to keep the original info, after that, transform the df from geo.bins and calculate adjacency matrices
geo.data <- geo.bins

## Prepare and save contingency matrices to test how robust it is the sample in every time-bin for the different network measures
# 1. Create an empty list to save results
geo.sample.bins <- vector(mode='list', length=length(geo.data))

# 2. Create the contingency tables
for (i in 1:length(geo.bins)) {
  
  #Create the contingency table
  geo.sample.bins[[i]] <- as.data.frame.matrix(table(geo.data[[i]]$Context,  geo.data[[i]]$Type))
  
  #Row.names as the first column
  geo.sample.bins[[i]] <- tibble::rownames_to_column(geo.sample.bins[[i]], "Context") 
}

## In geo.sample.bins ncolumns = ntypes + 1 (+1 because the first column is the row names)

# 3. Save its out R
# write.table(geo.sample.bins[[1]], 'SampleSummary8684.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")
# write.table(geo.sample.bins[[2]], 'SampleSummary8482.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")
# write.table(geo.sample.bins[[3]], 'SampleSummary8280.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")
# write.table(geo.sample.bins[[4]], 'SampleSummary8078.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")
# write.table(geo.sample.bins[[5]], 'SampleSummary7876.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")
# write.table(geo.sample.bins[[6]], 'SampleSummary7674.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")
# write.table(geo.sample.bins[[7]], 'SampleSummary7472.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")
# write.table(geo.sample.bins[[8]], 'SampleSummary7270.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")
# write.table(geo.sample.bins[[9]], 'SampleSummary7068.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")


## Now calculate absence/presence matrix
for (i in 1:length(geo.bins)) {  #No problem with replace geo.bins because at this point info is available in geo.data
  
  # Create a contingency table
  geo.bins[[i]] <- as.data.frame.matrix(table(geo.bins[[i]]$Context,  geo.bins[[i]]$Type))
  geo.bins[[i]] <-  geo.bins[[i]] %>%
    rownames_to_column(var="Context")  #Convert row.names into column "Context"
  
  # Convert to binary format
  binary_geo <-  geo.bins[[i]][,-1] #remove the ID column
  binary_geo <- as.data.frame.matrix(+(binary_geo > 0))  # Convert to binary using threshold (numeric variables)
  binary_geo <- model.matrix(~ . - 1, data = binary_geo)  # Convert to binary indicators (categorical variables)
  
  # Add the ID column back to the matrix
  geo.bins[[i]] <- as.data.frame.matrix(cbind(ID =  geo.bins[[i]]$ID, binary_geo))
  
  #Convert into numeric type
  geo.bins[[i]][,-1] <- sapply( geo.bins[[i]][,-1],as.numeric)
  print(sapply( geo.bins[[i]], class))
  
}


##Calculate distance matrix (here, Jaccard measure)
#####################################################
library(vegan)

for (i in 1:length(geo.bins)) {
  
  #Calculate Jaccard DISSIMILARITY with "vegdist"
  geo.bins[[i]] <- vegdist( geo.bins[[i]], method = "jaccard") ## This is NOT SIMILARITY
  
  #Now convert to similarity Jaccard by investment
  geo.bins[[i]] <- 1+(geo.bins[[i]]*-1) ## This is SIMILARITY
  
  #Convert object type 'list' into matrix for export it
  geo.bins[[i]] <- as.matrix( geo.bins[[i]])
  geo.bins[[i]][lower.tri( geo.bins[[i]])] <- NA   #delete duplicates
  
}


#Save result out R 

# write.table(geo.bins[[1]], 'Jaccard8684.csv',sep=";",quote=F,col.names=NA,na="")
# write.table(geo.bins[[2]], 'Jaccard8482.csv',sep=";",quote=F,col.names=NA,na="")
# write.table(geo.bins[[3]], 'Jaccard8280.csv',sep=";",quote=F,col.names=NA,na="")
# write.table(geo.bins[[4]], 'Jaccard8078.csv',sep=";",quote=F,col.names=NA,na="")
# write.table(geo.bins[[5]], 'Jaccard7876.csv',sep=";",quote=F,col.names=NA,na="")
# write.table(geo.bins[[6]], 'Jaccard7674.csv',sep=";",quote=F,col.names=NA,na="")
# write.table(geo.bins[[7]], 'Jaccard7472.csv',sep=";",quote=F,col.names=NA,na="")
# write.table(geo.bins[[8]], 'Jaccard7270.csv',sep=";",quote=F,col.names=NA,na="")
# write.table(geo.bins[[9]], 'Jaccard7068.csv',sep=";",quote=F,col.names=NA,na="")



###########################################################
####  Normalization as  j* = j * nSubtypes/nTypes    ######
###########################################################

# Where...
# nTypes = Total in the time bin with the higher number of types
# nSubtypes = Total IN the time-bin

# Relation n types ~ Jaccard 
############################

type.data <- geo.data   ## Save original info before transforming it

## Step 1: Get the data necessary to calculate the formula (j* = j * nSubtypes/nTypes)
##########

###### The j value (jaccard) ###### 
geo.bins[1]  # In this list, we have the previously calculated jaccard measure (j)


###### nSubtypes ###### (Total types in every time-bin)

# 1. Remember, we saved the summary tables with the types present in every time-bin in the list 'geo.sample.bins'.
# So, we need to count the columns
# (But remember to rest the first one because it is the 'Context' info)

# Create the empty vector
nSubtypes <- vector(mode='list', length=length(type.data)) 

# 2. Calculate the nSubtypes (n types in every time-bin)
for (i in 1:length(geo.sample.bins)) {
  
  # Count the number of columns and save it into a vector
  types.sum <- ncol(geo.sample.bins[[i]]) - 1
  
  # Save it in the vector previously created
  nSubtypes[[i]] <- types.sum
}


###### nTypes ###### (Total in the time bin with the higher number of types)

nTypes <- max(as.data.frame(nSubtypes))

## Step 3: Apply the formula (j* = j * nSubtypes/nTypes)
##########

# 1. Create the empty list to save results
geo.data.normalized <- vector(mode='list', length=length(geo.bins))

# 2. Apply the formula j* = j * (nSubtypes/nTypes) and save result
for (i in 1:length(geo.data.normalized)) {
  geo.data.normalized[[i]] <- geo.bins[[i]] * (nSubtypes[[i]]/nTypes)
}

#Select the bin where we want to write the normalized matrices and save it
# write.table(geo.data.normalized[[1]], 'Jaccard8684transform.csv',sep=";",quote=F,col.names=NA,na="")
# write.table(geo.data.normalized[[2]], 'Jaccard8482transform.csv',sep=";",quote=F,col.names=NA,na="")
# write.table(geo.data.normalized[[3]], 'Jaccard8280transform.csv',sep=";",quote=F,col.names=NA,na="")
# write.table(geo.data.normalized[[4]], 'Jaccard8078transform.csv',sep=";",quote=F,col.names=NA,na="")
# write.table(geo.data.normalized[[5]], 'Jaccard7876transform.csv',sep=";",quote=F,col.names=NA,na="")
# write.table(geo.data.normalized[[6]], 'Jaccard7674transform.csv',sep=";",quote=F,col.names=NA,na="")
# write.table(geo.data.normalized[[7]], 'Jaccard7472transform.csv',sep=";",quote=F,col.names=NA,na="")
# write.table(geo.data.normalized[[8]], 'Jaccard7270transform.csv',sep=";",quote=F,col.names=NA,na="")
# write.table(geo.data.normalized[[9]], 'Jaccard7068transform.csv',sep=";",quote=F,col.names=NA,na="")
# 

#######################################################
#                   METADATA
#######################################################

geo.coord <- read.csv("Coord_Yac.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
geo.coord <- na.omit(geo.coord)

geo.coord$ID_Yac <- str_replace_all(geo.coord$ID_Yac,
                                    c("Abric del Xicotó" = "Xicotó",
                                      "Cingle del Mas Cremat" = "Cremat",
                                      "Coves del Fem" = "Fem",
                                      "Cueva de Can Sadurní" = "Sadurní",
                                      "Cueva de Chaves" = "Chaves",
                                      "Cueva de Nerja" = "Nerja",
                                      "Guixeres de Vilobí" = "Guixeres",
                                      "La Draga" = "Draga", 
                                      "El Barranquet" = "Barranquet",
                                      "Cova de les Cendres" = "Cendres",
                                      "Mas d'Is" = "Mas_dIs",
                                      "Cueva del Toro" = "Toro",
                                      "Alonso Norte" = "Alonso",
                                      "Cabezo de la Cruz" = "Cabezo",
                                      "Botiquería dels Moros" = "Botiquería",
                                      "Forcas II" = "Forcas",
                                      "Abric de la Falguera" = "Fal",
                                      "Caserna de Sant Pau" = "Sant_Pau",
                                      "Ángel/Arenal de Fonseca" = "Ángel2",
                                      "Cova de l'Or" = "Or",
                                      "Cueva de la Cocina" = "Cocina",
                                      "El Collao" = "Collao",
                                      "Castillejos de Montefrío" = "Castillejos",
                                      "Casa Corona" = "Corona",
                                      "Cova dels Trocs" = "Trocs",
                                      "Mas Nou" = "Mas_Nou"))

####
## Incorporate it in the df with context information
####

# Step 1: Create the data.frames and save it into a list
Metadata.v8600.8400.Id <- as.data.frame.matrix(table(geo.data[[1]]$Context,  geo.data[[1]]$Type))
Metadata.v8400.8200.Id <- as.data.frame.matrix(table(geo.data[[2]]$Context,  geo.data[[2]]$Type))
Metadata.v8200.8000.Id <- as.data.frame.matrix(table(geo.data[[3]]$Context,  geo.data[[3]]$Type))
Metadata.v8000.7800.Id <- as.data.frame.matrix(table(geo.data[[4]]$Context,  geo.data[[4]]$Type))
Metadata.v7800.7600.Id <- as.data.frame.matrix(table(geo.data[[5]]$Context,  geo.data[[5]]$Type))
Metadata.v7600.7400.Id <- as.data.frame.matrix(table(geo.data[[6]]$Context,  geo.data[[6]]$Type))
Metadata.v7400.7200.Id <- as.data.frame.matrix(table(geo.data[[7]]$Context,  geo.data[[7]]$Type))
Metadata.v7200.7000.Id <- as.data.frame.matrix(table(geo.data[[8]]$Context,  geo.data[[8]]$Type))
Metadata.v7000.6800.Id <- as.data.frame.matrix(table(geo.data[[9]]$Context,  geo.data[[9]]$Type))

Metadata.list <- list("Metadata.v8600.8400.Id" = Metadata.v8600.8400.Id,
                      "Metadata.v8400.8200.Id" = Metadata.v8400.8200.Id,
                      "Metadata.v8200.8000.Id" = Metadata.v8200.8000.Id,
                      "Metadata.v8000.7800.Id" = Metadata.v8000.7800.Id,
                      "Metadata.v7800.7600.Id" = Metadata.v7800.7600.Id,
                      "Metadata.v7600.7400.Id" = Metadata.v7600.7400.Id,
                      "Metadata.v7400.7200.Id" = Metadata.v7400.7200.Id,
                      "Metadata.v7200.7000.Id" = Metadata.v7200.7000.Id,
                      "Metadata.v7000.6800.Id" = Metadata.v7000.6800.Id)

rm(Metadata.v8600.8400.Id, Metadata.v8400.8200.Id, Metadata.v8200.8000.Id, Metadata.v8000.7800.Id, Metadata.v7800.7600.Id,
   Metadata.v7600.7400.Id, Metadata.v7400.7200.Id, Metadata.v7200.7000.Id, Metadata.v7000.6800.Id)


# Step 2: Define a function to perform the desired operations on every data frame
transform_metadata <- function(df) {
  # Select the first 5 columns
  df <- df[, 1:5]
  # Assign names to the columns
  colnames(df) <- c('Id', 'Context', 'ID_Yac', 'UTMX', 'UTMY')
  # Assign 'Context' to row labels
  df$Context <- row.names(df)
  # Create a numbered 'Id' column
  df$Id <- 1:nrow(df)
  # Extract the part before the space in 'Context' and assign it to 'ID_Yac'
  df$ID_Yac <- word(df$Context, 1)
  return(df)
}

# Step 3: Apply the function to each data frame in the list
transformed_Metadata.list <- lapply(Metadata.list, transform_metadata)

# Step 4: Import coord info to the df

# Define a function that takes a data frame and applies the provided code
incorporate_coord_info <- function(df) {
  for (i in 1:nrow(df)) {
    # Find context coincidence
    c <- str_extract(df$ID_Yac[i], c(geo.coord$ID_Yac))
    c <- c[!is.na(c)]
    
    # Import X and Y coordinates when the fields 'context' in "geo" and "coord." datasets match by row
    df[i, "UTMX"] <- dplyr::filter(.data = geo.coord, ID_Yac == c)$UTM_X
    df[i, "UTMY"] <- dplyr::filter(.data = geo.coord, ID_Yac == c)$UTM_Y
    
  }
  return(df)
}

# Apply the function to each data frame in the list
transformed_Metadata.list <- lapply(transformed_Metadata.list, incorporate_coord_info)

# Delete the third column
for (i in 1:length(transformed_Metadata.list)) {
  transformed_Metadata.list[[i]] <- transformed_Metadata.list[[i]][,-3]
}

# ... and save in the pc

# write.table(transformed_Metadata.list[[1]], 'Metadata8684.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")
# write.table(transformed_Metadata.list[[2]], 'Metadata8482.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")
# write.table(transformed_Metadata.list[[3]], 'Metadata8280.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")
# write.table(transformed_Metadata.list[[4]], 'Metadata8078.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")
# write.table(transformed_Metadata.list[[5]], 'Metadata7876.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")
# write.table(transformed_Metadata.list[[6]], 'Metadata7674.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")
# write.table(transformed_Metadata.list[[7]], 'Metadata7472.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")
# write.table(transformed_Metadata.list[[8]], 'Metadata7270.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")
# write.table(transformed_Metadata.list[[9]], 'Metadata7068.csv',sep=",",quote=F,col.names=T,na="",row.names=F,dec=".")


#############################################################
#                     NETWORK METRICS                       #
#############################################################


################################################
### NODE TURNOVER RATIO : NTR = (Nap−Ndis)/Ntot
################################################


###
## Ntot
###

Ntot <- vector(mode='list', length=length(geo.sample.bins))

for (i in 1:length(Ntot)) {
  Ntot[i] <- nrow(geo.sample.bins[[i]])
}


###
## Nap - is the amount of nodes appearing in the network under study
###

Nap <- vector(mode='list', length=length(geo.sample.bins))


for (i in 2:length(geo.sample.bins)) {  #Start in 2 because this measure compare the actual network with the previos one, and the first networks do not have previous one.
  
  # j will be the previous network  
  j = i - 1 
  
  # compare actual network (i) with the previous one (j), and sum the match 
  Nap[[i]] <- sum(sapply(geo.sample.bins[[i]]$Context, function(pattern) {
    grepl(paste0("\\b", pattern, "\\b"), geo.sample.bins[[j]]$Context)
  }))
  
  # ... and get the new nodes by resting the matches with the total
  Nap[[i]] <- Ntot[[i]] - Nap[[i]]
  
} 


###
## Ndis - corresponds to the count of “disappearing nodes”
###

Ndis <- vector(mode='list', length=length(geo.sample.bins))


for (i in 2:length(geo.sample.bins)) {  #Start in 2 because this measure compare the actual network with the previos one, and the first networks do not have previous one.
  
  # j will be the previous network  
  j = i - 1 
  
  # compare actual network (i) with the previous one (j), sum the match 
  Ndis[[i]] <- sum(sapply(geo.sample.bins[[i]]$Context, function(pattern) {
    grepl(paste0("\\b", pattern, "\\b"), geo.sample.bins[[j]]$Context)
  }))
  
  # ... and get the disappeared nodes by resting the matches with the previous total
  Ndis[[i]] <- Ntot[[j]] - Ndis[[i]]
  
} 

###
# Calculate NTR as NTR = (Nap−Ndis)/Ntot
###

NTR <- vector(mode='list', length=length(geo.sample.bins))
names(NTR) = c("v8600.8400","v8400.8200","v8200.8000","v8000.7800", "v7800.7600",
               "v7600.7400", "v7400.7200", "v7200.7000", "v7000.6800")


for (i in 2:length(NTR)) {
  NTR[[i]] <- (Nap[[i]] - Ndis[[i]]) / Ntot[[i]]
}


########################################
##Trying Erdos-Renyi to test small-world 
########################################

#Import jaccard info
my_data.v8482 <- geo.data.normalized[[2]] #Change the time-bin with [[]] 
my_data.v8280 <- geo.data.normalized[[3]]
my_data.v8078 <- geo.data.normalized[[4]]
my_data.v7876 <- geo.data.normalized[[5]]
my_data.v7674 <- geo.data.normalized[[6]]
my_data.v7472 <- geo.data.normalized[[7]]
my_data.v7270 <- geo.data.normalized[[8]]
my_data.v7068 <- geo.data.normalized[[9]]

my_data.list <- list("My.data.v8400.8200" = my_data.v8482,
                     "My.data.v8200.8000" = my_data.v8280,
                     "My.data.v8000.7800" = my_data.v8078,
                     "My.data.v7800.7600" = my_data.v7876,
                     "My.data.v7600.7400" = my_data.v7674,
                     "My.data.v7400.7200" = my_data.v7472,
                     "My.data.v7200.7000" = my_data.v7270,
                     "My.data.v7000.6800" = my_data.v7068)

rm(my_data.v8482, my_data.v8280, my_data.v8078, my_data.v7876, my_data.v7674,
   my_data.v7472, my_data.v7270, my_data.v7068)

#Create empty list to save graphs
mylist.names <- c("my_graph.v8482", "my_graph.v8280", "my_graph.v8078", "my_graph.v7876", "my_graph.v7674", "my_graph.v7472", "my_graph.v7270", "my_graph.v7068")
g.list <- vector("list", length(mylist.names))
names(g.list) <- mylist.names

#Create empty list where saving original network metrics

# 1. Density
Density3 <- vector("list", length(mylist.names))
names(Density3) <- mylist.names

# 3. Clustering
Clustering.original <- vector("list", length(mylist.names))
names(Clustering.original) <- mylist.names

# 3. APL
APL.original <- vector("list", length(mylist.names))
names(APL.original) <- mylist.names


# Loop
#######

for (i in 1:length(g.list)) {
  
  # create an 'igraph object'
  g.list[[i]] <- graph_from_adjacency_matrix (my_data.list[[i]], mode = "undirected", weighted=TRUE, diag=FALSE) 
  
  # Original Density 
  Density3[[i]] <- ecount(g.list[[i]])/(vcount(g.list[[i]])*(vcount(g.list[[i]])-1)/2)
  Density3[[i]]
  
  # Original Clustering
  Clustering.original[[i]] <- transitivity(g.list[[i]], type = "undirected", weights = NA)
  APL.original[[i]] <- mean_distance(g.list[[i]], weights = NA)
  
} 



#############################################
#####           Erdos-Renyi            ######
#############################################

#Create empty list where save original network metrics

# 1. n nodes or vertices in the graph
n <- vector("list", length(mylist.names))
names(n) <- mylist.names

# 2. Erdos measures

#Clustering
clustering.erdos <-  data.frame(Undirected = rep(NA, 1000)) #clustering.erdos[i,1]
n <- length(mylist.names)
clustering.erdos.list <- replicate(n, clustering.erdos, simplify = FALSE)

#APL
APL.erdos <-  data.frame(APL = rep(NA, 1000)) #clustering.erdos[i,1]
APL.erdos.list <- replicate(n, APL.erdos, simplify = FALSE)

#gs
gs <- vector("list", length(mylist.names))


# Loop
######
set.seed(1)

for (i in 1:length(mylist.names)) {
  for (j in 1:1000) {
    #Calculate n nodes in the graphs
    n[[i]] = vcount (g.list[[i]])
    
    gs[[j]] <- erdos.renyi.game(n[[i]] , Density3[[i]], 
                                type = "gnp", #Because we use density and no number of edges to build the network
                                directed = FALSE) 
    
    clustering.erdos.list[[i]][j,1] <- transitivity(gs[[j]], type = "undirected", weights = NA) #new

    APL.erdos.list[[i]][j,1] <- mean_distance(gs[[j]], weights = NA) #new
    
  }}


# Plot
#######

# cLUSTERING COEFFICIENT

for (i in 1:length(mylist.names)) {
  colnames(clustering.erdos.list[[i]]) <- c("value")
  head(clustering.erdos.list[[i]])
  
}

# AVERAGE PATH LENGTHS (APL)

for (i in 1:length(mylist.names)) {
  colnames(APL.erdos.list[[i]]) <- c("value")
  head(APL.erdos.list[[i]])
}



# CALCULATE SMALL-WORLD METRICS
################################
C <- vector("list", length(mylist.names))
Crand <- vector("list", length(mylist.names))
L <- vector("list", length(mylist.names))
Lrand <- vector("list", length(mylist.names))
small.world.coeff.o <- vector("list", length(mylist.names))

for (i in 1:length(mylist.names)) {
  C[[i]] = Clustering.original[[i]]
  Crand[[i]] = mean(clustering.erdos.list[[i]]$value)
  L[[i]] = APL.original[[i]]
  Lrand[[i]] = mean(APL.erdos.list[[i]]$value)
  
  ## Small-world coefficient of Watts and Strogatz (1998). Values > 1 have small-world properties.
  small.world.coeff.o[[i]] <- (C[[i]]/Crand[[i]])/(L[[i]]/Lrand[[i]])
  
}




## BLOXPLOTS

# cLUSTERING COEFFICIENT
clust8482 <- ggplot(clustering.erdos.list[[1]], aes(y = value)) +
  geom_boxplot(fill = "#a8b2b6") +
  theme_minimal()+ 
  theme(panel.grid = element_blank())  +
  geom_hline(aes(yintercept=Clustering.original[[1]]), colour="#900000", linetype="dashed", lwd =1)+ #change 'yintercept' to indicate clust.coeff. for every chronological bin 
  theme(axis.title.x=element_blank(),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("8400 - 8201 cal. BP")


# AVERAGE PATH LENGTHS (APL)
APL8482 <- ggplot(APL.erdos.list[[1]], aes(y = value)) +
  geom_boxplot(fill = "#a8b2b6") +
  theme_minimal()+ 
  theme(panel.grid = element_blank())  +
  geom_hline(aes(yintercept=APL.original[[1]]), colour="#900000", linetype="dashed", lwd =1)+ #change 'yintercept' to indicate clust.coeff. for every chronological bin 
  theme(axis.title.x=element_blank(),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("8400 - 8201 cal. BP")


##8280
######
clust8280 <- ggplot(clustering.erdos.list[[2]], aes(y = value)) +
  geom_boxplot(fill = "#a8b2b6") +
  theme_minimal()+ 
  theme(panel.grid = element_blank())  +
  geom_hline(aes(yintercept=Clustering.original[[2]]), colour="#900000", linetype="dashed", lwd =1)+ #change 'yintercept' to indicate clust.coeff. for every chronological bin 
  theme(axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("8200 - 8001 cal. BP")


# AVERAGE PATH LENGTHS (APL)
APL8280 <- ggplot(APL.erdos.list[[2]], aes(y = value)) +
  geom_boxplot(fill = "#a8b2b6") +
  theme_minimal()+ 
  theme(panel.grid = element_blank())  +
  geom_hline(aes(yintercept=APL.original[[2]]), colour="#900000", linetype="dashed", lwd =1)+ #change 'yintercept' to indicate clust.coeff. for every chronological bin 
  theme(axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("8200 - 8001 cal. BP")


##8078
######
clust8078 <- ggplot(clustering.erdos.list[[3]], aes(y = value)) +
  geom_boxplot(fill = "#a8b2b6") +
  theme_minimal()+ 
  theme(panel.grid = element_blank())  +
  geom_hline(aes(yintercept=Clustering.original[[3]]), colour="#900000", linetype="dashed", lwd =1)+ #change 'yintercept' to indicate clust.coeff. for every chronological bin 
  theme(axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("8000 - 7801 cal. BP")


# AVERAGE PATH LENGTHS (APL)
APL8078 <- ggplot(APL.erdos.list[[3]], aes(y = value)) +
  geom_boxplot(fill = "#a8b2b6") +
  theme_minimal()+ 
  theme(panel.grid = element_blank())  +
  geom_hline(aes(yintercept=APL.original[[3]]), colour="#900000", linetype="dashed", lwd =1)+ #change 'yintercept' to indicate clust.coeff. for every chronological bin 
  theme(axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("8000 - 7801 cal. BP")


##7876
######
clust7876 <- ggplot(clustering.erdos.list[[4]], aes(y = value)) +
  geom_boxplot(fill = "#c8cea6") +
  theme_minimal()+ 
  theme(panel.grid = element_blank())  +
  geom_hline(aes(yintercept=Clustering.original[[4]]), colour="#900000", linetype="dashed", lwd =1)+ #change 'yintercept' to indicate clust.coeff. for every chronological bin 
  theme(axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("7800 - 7601 cal. BP")


# AVERAGE PATH LENGTHS (APL)
APL7876 <- ggplot(APL.erdos.list[[4]], aes(y = value)) +
  geom_boxplot(fill = "#a8b2b6") +
  theme_minimal()+ 
  theme(panel.grid = element_blank())  +
  geom_hline(aes(yintercept=APL.original[[4]]), colour="#900000", linetype="dashed", lwd =1)+ #change 'yintercept' to indicate clust.coeff. for every chronological bin 
  theme(axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("7800 - 7601 cal. BP")

##7674
######
clust7674 <- ggplot(clustering.erdos.list[[5]], aes(y = value)) +
  geom_boxplot(fill = "#a8b2b6") +
  theme_minimal()+ 
  theme(panel.grid = element_blank())  +
  geom_hline(aes(yintercept=Clustering.original[[5]]), colour="#900000", linetype="dashed", lwd =1)+ #change 'yintercept' to indicate clust.coeff. for every chronological bin 
  theme(axis.title.x=element_blank(),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("7600 - 7401 cal. BP")


# AVERAGE PATH LENGTHS (APL)
APL7674 <- ggplot(APL.erdos.list[[5]], aes(y = value)) +
  geom_boxplot(fill = "#a8b2b6") +
  theme_minimal()+ 
  theme(panel.grid = element_blank())  +
  geom_hline(aes(yintercept=APL.original[[5]]), colour="#900000", linetype="dashed", lwd =1)+ #change 'yintercept' to indicate clust.coeff. for every chronological bin 
  theme(axis.title.x=element_blank(),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("7600 - 7401 cal. BP")



##7472
######
clust7472 <- ggplot(clustering.erdos.list[[6]], aes(y = value)) +
  geom_boxplot(fill = "#c8cea6") +
  theme_minimal()+ 
  theme(panel.grid = element_blank())  +
  geom_hline(aes(yintercept=Clustering.original[[6]]), colour="#900000", linetype="dashed", lwd =1)+ #change 'yintercept' to indicate clust.coeff. for every chronological bin 
  theme(axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("7400 - 7201 cal. BP")


# AVERAGE PATH LENGTHS (APL)
APL7472 <- ggplot(APL.erdos.list[[6]], aes(y = value)) +
  geom_boxplot(fill = "#a8b2b6") +
  theme_minimal()+ 
  theme(panel.grid = element_blank())  +
  geom_hline(aes(yintercept=APL.original[[6]]), colour="#900000", linetype="dashed", lwd =1)+ #change 'yintercept' to indicate clust.coeff. for every chronological bin 
  theme(axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("7400 - 7201 cal. BP")

##7270
######
clust7270 <- ggplot(clustering.erdos.list[[7]], aes(y = value)) +
  geom_boxplot(fill = "#c8cea6") +
  theme_minimal()+ 
  theme(panel.grid = element_blank())  +
  geom_hline(aes(yintercept=Clustering.original[[7]]), colour="#900000", linetype="dashed", lwd =1)+ #change 'yintercept' to indicate clust.coeff. for every chronological bin 
  theme(axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("7200 - 7001 cal. BP")


# AVERAGE PATH LENGTHS (APL)
APL7270 <- ggplot(APL.erdos.list[[7]], aes(y = value)) +
  geom_boxplot(fill = "#8ad7e4") +
  theme_minimal()+ 
  theme(panel.grid = element_blank())  +
  geom_hline(aes(yintercept=APL.original[[7]]), colour="#900000", linetype="dashed", lwd =1)+ #change 'yintercept' to indicate clust.coeff. for every chronological bin 
  theme(axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("7200 - 7001 cal. BP")



########################################################################
clustTOTAL <- ggpubr::ggarrange(clust8482, clust8280, clust8078, clust7876, 
                                clust7674, clust7472, clust7270,
                                ncol = 4, nrow = 2, common.legend = TRUE)

APLTOTAL <- ggpubr::ggarrange(APL8482, APL8280, APL8078, APL7876, 
                              APL7674, APL7472, APL7270,
                              ncol = 4, nrow = 2, common.legend = TRUE)

# setwd("D:/Universidad/otros proyectos/Artículos y publicaciones/Redes_GEO/FIGURAS/Small-world")
ggsave("SW_Clust_Coef.jpg", plot = clustTOTAL, width = 22, height = 12, units = "cm", dpi = 600)
ggsave("SW_APL.jpg", plot = APLTOTAL, width = 22, height = 12, units = "cm", dpi = 600)
# setwd("D:/Universidad/otros proyectos/Artículos y publicaciones/Redes_GEO/Script")
