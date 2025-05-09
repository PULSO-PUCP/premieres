# clean memory
rm(list = ls())
file_Name1="hdidemiso_plus.RDS"
mergedData=readRDS(file_Name1)

# we have
str(mergedData)

## display frequency table of Regime type
table(mergedData$Regimetype)

barplot(table(mergedData$Regimetype))


## table of summary statistics for numerical columns
AllNumericNames=names(Filter(is.numeric, mergedData))
originalNumeric=AllNumericNames[!grepl('mM',AllNumericNames)]

summary(mergedData[,originalNumeric])

# boxplot for them
scaledNumeric=AllNumericNames[grepl('mM',AllNumericNames)]
DataForBox=reshape2::melt(mergedData[,scaledNumeric])
boxplot(mergedData[,scaledNumeric])


## prepare regressions
h1=formula(HumanDevelopmentIndex~Functioningofgovernment)
regre1=glm(h1,data=mergedData,family = "gaussian")
summary(regre1)

h2=formula(HumanDevelopmentIndex~Functioningofgovernment + Politicalparticipation)
regre2=glm(h2,data=mergedData,family = "gaussian")
summary(regre2)


# maps
file_Name2="MapSeattle.geojson"
file_Name3="calls911_geo.geojson"

#opening data
library(sf)
cityMap=read_sf(file_Name2)
eventsLocations=read_sf(file_Name3)

plot(cityMap['ACRES_TOTAL'],col='grey90',reset = FALSE)
plot(eventsLocations['nightTime'],add = TRUE,size=4)
legend("topright", legend=c("day",'night'), col=c("black",'yellow'), pch=16, cex=1.2)


