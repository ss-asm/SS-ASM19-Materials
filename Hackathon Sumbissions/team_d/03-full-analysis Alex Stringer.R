### Fully sourceable script to reproduce analysis ###

# Load libraries
library(tidyverse)
library(diseasemapping)
library(geostatsp)
library(mapmisc)

library(fingertipsR)

library(parallel)
options(mc.cores = 8)

# Spatial data
library(raster)
library(knitr)
library(geojsonio)
library(sp)
library(tmap)
library(spdep)
library(reshape2)
library(rsq)

### PART 1: SOURCE DATA ###

## Demographic indicators

select_indicators()
setwd("C://Users/Lewis/OneDrive/Desktop/SSAM/")
green <- read.csv("./facilities-onlygreen.csv", stringsAsFactors = FALSE)
sports <- read.csv("./facilities-nogreen.csv", stringsAsFactors = FALSE)

#93439	Percentage of adults walking for travel at least three days per week	
#93440	Percentage of adults cycling for travel at least three days per week
#90416	Overcrowded households
#92772	Number of premises licensed to sell alcohol per square kilometre
#92937	Density of fast food outlets
#91126	Unemployment
#93495	Estimated prevalence of common mental disorders: % of population aged 16 & over
#93351	Average weekly earnings
#90407	Adults with low education: % of population aged 16+
#93351	Average weekly earnings	



obesity <- fingertips_data(IndicatorID = 93088, AreaCode = NULL, DomainID = NULL,
                           ProfileID = NULL, AreaTypeID = 101, ParentAreaTypeID = NULL,
                           categorytype = FALSE, rank = FALSE)

obesity <- subset(obesity, AreaType == "District & UA")
obesity <- subset(obesity, Sex == "Persons")
obesity <- subset(obesity, Timeperiod == "2015/16")
obesity <- cbind.data.frame(obesity$AreaCode, obesity$Value)
colnames(obesity) <- c("AreaCode", "Obesity")

population <- fingertips_data(IndicatorID = 92708, AreaCode = NULL, DomainID = NULL,
                              ProfileID = NULL, AreaTypeID = 101, ParentAreaTypeID = NULL,
                              categorytype = FALSE, rank = FALSE)

population <- subset(population, AreaType == "District & UA")
population <- subset(population, Age == "All ages")
population <- subset(population, Sex == "Persons")
population <- subset(population, Timeperiod == "2016")
population <- cbind.data.frame(population$AreaCode, population$Value)
colnames(population) <- c("AreaCode", "population")

walking <- fingertips_data(IndicatorID = 93439, AreaCode = NULL, DomainID = NULL,
                           ProfileID = NULL, AreaTypeID = 101, ParentAreaTypeID = NULL,
                           categorytype = FALSE, rank = FALSE)
walking <- subset(walking, Timeperiod =="2015/16")
walking <- subset(walking, AreaType == "District & UA")
walking <- cbind.data.frame(walking$AreaCode, walking$Value)
colnames(walking) <- c("AreaCode", "walking")

cycling <- fingertips_data(IndicatorID = 93440, AreaCode = NULL, DomainID = NULL,
                           ProfileID = NULL, AreaTypeID = 101, ParentAreaTypeID = NULL,
                           categorytype = FALSE, rank = FALSE)

cycling <- subset(cycling, Timeperiod =="2015/16")
cycling <- subset(cycling, AreaType == "District & UA")
cycling <- cbind.data.frame(cycling$AreaCode, cycling$Value)
colnames(cycling) <- c("AreaCode", "cycling")


overcrowded.households <- fingertips_data(IndicatorID = 90416, AreaCode = NULL, DomainID = NULL,
                                          ProfileID = NULL, AreaTypeID = 101, ParentAreaTypeID = NULL,
                                          categorytype = FALSE, rank = FALSE)

overcrowded.households <- subset(overcrowded.households, AreaType == "District & UA")
overcrowded.households <- cbind.data.frame(overcrowded.households$AreaCode, overcrowded.households$Value)
colnames(overcrowded.households) <- c("AreaCode", "overcrowded.households")

alcohol <- fingertips_data(IndicatorID = 92772, AreaCode = NULL, DomainID = NULL,
                           ProfileID = NULL, AreaTypeID = 101, ParentAreaTypeID = NULL,
                           categorytype = FALSE, rank = FALSE)

alcohol <- subset(alcohol, AreaType == "District & UA")
alcohol <- subset(alcohol, Timeperiod =="2015/16")
alcohol <- cbind.data.frame(alcohol$AreaCode, alcohol$Value)
colnames(alcohol) <- c("AreaCode", "alcohol")

fast.foods <- fingertips_data(IndicatorID = 92937, AreaCode = NULL, DomainID = NULL,
                              ProfileID = NULL, AreaTypeID = 101, ParentAreaTypeID = NULL,
                              categorytype = FALSE, rank = FALSE)

fast.foods <- subset(fast.foods, AreaType == "District & UA")
fast.foods <- cbind.data.frame(fast.foods$AreaCode, fast.foods$Value)
colnames(fast.foods) <- c("AreaCode", "fast.foods")

unemployment <- fingertips_data(IndicatorID = 91126, AreaCode = NULL, DomainID = NULL,
                                ProfileID = NULL, AreaTypeID = 101, ParentAreaTypeID = NULL,
                                categorytype = FALSE, rank = FALSE)

unemployment <- subset(unemployment, AreaType == "District & UA")
unemployment <- subset(unemployment, Timeperiod =="2016")
unemployment <- cbind.data.frame(unemployment$AreaCode, unemployment$Value)
colnames(unemployment) <- c("AreaCode", "unemployment")

mental.health <- fingertips_data(IndicatorID = 93495, AreaCode = NULL, DomainID = NULL,
                                 ProfileID = NULL, AreaTypeID = 101, ParentAreaTypeID = NULL,
                                 categorytype = FALSE, rank = FALSE)
mental.health <- subset(mental.health, AreaType == "District & UA")
mental.health <- cbind.data.frame(mental.health$AreaCode, mental.health$Value)
colnames(mental.health) <- c("AreaCode", "mental.health")

earnings <- fingertips_data(IndicatorID = 93351, AreaCode = NULL, DomainID = NULL,
                            ProfileID = NULL, AreaTypeID = 101, ParentAreaTypeID = NULL,
                            categorytype = FALSE, rank = FALSE)
earnings <- subset(earnings, AreaType == "District & UA")
earnings <- subset(earnings, Timeperiod =="2016")
earnings <- subset(earnings, Sex == "Persons")
earnings <- cbind.data.frame(earnings$AreaCode, earnings$Value)
colnames(earnings) <- c("AreaCode", "earnings")

low.education <- fingertips_data(IndicatorID = 90407, AreaCode = NULL, DomainID = NULL,
                                 ProfileID = NULL, AreaTypeID = 101, ParentAreaTypeID = NULL,
                                 categorytype = FALSE, rank = FALSE)
low.education <- subset(low.education, AreaType == "District & UA")
low.education <- cbind.data.frame(low.education$AreaCode, low.education$Value)
colnames(low.education) <- c("AreaCode", "low.education")

data <- merge(obesity, population, by.x = "AreaCode", by.y = "AreaCode")
data <- merge(data, green, by.x = "AreaCode", by.y = "local_authority_code")
data <- merge(data, sports, by.x = "AreaCode", by.y = "local_authority_code")
data <- merge(data, alcohol, by.x = "AreaCode", by.y = "AreaCode")
data <- merge(data, cycling, by.x = "AreaCode", by.y = "AreaCode")
data <- merge(data, fast.foods, by.x = "AreaCode", by.y = "AreaCode")
data <- merge(data, low.education, by.x = "AreaCode", by.y = "AreaCode")
data <- merge(data, mental.health, by.x = "AreaCode", by.y = "AreaCode")
data <- merge(data, overcrowded.households, by.x = "AreaCode", by.y = "AreaCode")
data <- merge(data, unemployment, by.x = "AreaCode", by.y = "AreaCode")
data <- merge(data, walking, by.x = "AreaCode", by.y = "AreaCode")

indicators <- data # Final dataframe of indicators

## Region boundaries data (shapefiles)
regions_json <- geojson_read(paste0(DATAPATH,
                                    "local-authority-districts/Local_Authority_Districts_April_2019_Boundaries_UK_BFC.shp"),
                             what = "sp")

# Convert the code from a factor and rename for consistency
regions_json@data$local_authority_code <- as.character(regions_json@data$lad19cd)

# Merge them
obesity_with_regions <- sp::merge(
  x = regions_json,
  y = indicators,
  by = "local_authority_code"
)

### PART 2: MODEL ###
MODELFORMULA <-
  obesity_cnt ~ offset(log(population)) +
  greenspaces +
  sports_facilities +
  alcohol +
  mental_health +
  unemployment

DATAPATH <- "/home/alex1/ssasm-2019-teamD/data/"
MODELPATH <- "/home/alex1/ssasm-2019-teamD/models/"

DATANAME <- "obesity_regions_02"

MODELNAME <- "03-small-indicators-log"

# Fit model
tm <- Sys.time()
cat("Fitting model. Time: ",format(Sys.time()),"\n")
cat("Model formula: ",as.character(MODELFORMULA),"\n")

fittedmodel <- bym(
  MODELFORMULA,
  data = obesity_with_regions,
  priorCI = list(sdSpatial = c(0.1, 5), sdIndep = c(0.1, 5))
)

cat("Finished fitting model. Fitting took",format(Sys.time() - tm),"\n")


## Analyze fitted model

# The model can predict random effects for places with no data... but here we don't want this.
fittedmodel$data@data[is.na(fittedmodel$data@data$fitted.exp),"random.exp"] <- NA

# Merge the data
fittedmodel$data@data[ ,"lad19cd"] <- modeldata@data[ ,"lad19cd"]
merged_data <- merge(modeldata,fittedmodel$data,by = "lad19cd")

merged_data@data <- merged_data@data %>%
  mutate(random.exp = 100 * (random.exp - 1)) %>%
  rename(`Obesity (% of pop)` = obesity_pct,
         `Excess % obese` = random.exp)

# Model summary
summary(fittedmodel$inla)
cbind(fittedmodel$inla$summary.fixed,exp(fittedmodel$inla$summary.fixed$mean))

# Plot the map of observed and excess spatial variation
tmap_mode("plot")

map_all <- tm_shape(merged_data) + 
  tm_polygons(c("Obesity (% of pop)","Excess % obese"), 
              id = "lad19nm",
              palette = "-RdYlGn") + 
  tm_style("albatross") +
  tm_facets(ncol = 2) 
tmap_save(map_all,filename = paste0(MODELPATH,MODELNAME,'-all.png'))

# Grab the five top hotspots
sorted_data <- fittedmodel$data@data %>% arrange(desc(random.exp))
sorted_data[1:5, ] %>%
  inner_join(modeldata@data,by = c("lad19cd" = "local_authority_code")) %>%
  dplyr::select(lad19nm,random.exp) %>%
  mutate(random.exp = round(100*(random.exp-1),2)) %>%
  rename(pct_excess_obesity = random.exp)





