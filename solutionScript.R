## Data
rm(list=ls())
setwd('C:/Users/will/Programming/QVCAnalytics/ryansFiles')

zips <- read.csv('zip.csv')
merck <- read.csv('merck.csv')
data <- merge(merck, zips, by='zip')

loc1 <- zips[zips$zip==08889,] # Whitehouse Station, NJ
loc2 <- zips[zips$zip==07033,] # Kenilworth NJ
loc3 <- zips[zips$zip==19486,] # West Point, PA

## Functions

rad <- function(deg) {
  return(deg * pi / 180)
}

distance <- function(lat1, lon1, lat2, lon2) {
  ans = cos(rad(90-lat1))*cos(rad(90-lat2)) + sin(rad(90-lat1)) * sin(rad(90-lat2)) * cos(rad(lon1-lon2))
  ans[ans >= 1] <- 1
  ans[ans <= -1] <- -1
  return (acos(ans) * 3958.756)
}

## Analysis

# Remove extreme outliers (why are some values NA?)
#outliers defined as commute distance greater than 100 miles
currcommute <- distance(loc1$lat, loc1$lon, data$lat, data$lon)
data <- data[! is.na(currcommute) & currcommute < 100,]

# Commute distance for every employee to corresponsing location
commute1 <- distance(loc1$lat, loc1$lon, data$lat, data$lon)
commute2 <- distance(loc2$lat, loc2$lon, data$lat, data$lon)
commute3 <- distance(loc3$lat, loc3$lon, data$lat, data$lon)

hist(commute1-commute2)
hist(commute1-commute3)

# Total distance (sum) for all employees for each location in PA, NY, or NJ
locations <- zips[zips$state %in% c('NJ', 'NY', 'PA'),]
commutes <- c()
for(i in 1:length(locations$zip)) {
  comm <- distance(locations$lat[i], locations$lon[i], data$lat, data$lon)
  if(any(is.na(comm))==0 && 2*sum((commute1 - comm) > 0) > length(commute1)) {
    hist(commute1 - comm, main=paste(locations$zip[i]))
  }
  commutes <- c(commutes, sum(comm, na.rm=TRUE))
}

#location where the sum of the commute distance for all employess is the least
goodLocData <- data.frame(locations, commutes)
goodLocData <- subset(goodLocData, commutes > 0)
sumsolution <- subset(goodLocData, commutes == min(goodLocData$commutes))

#location where the count of employees that improve their commute is greatest

#given a zipcode find the number of employees whose commute distance to the given location
#is less than the commute to the the current location, 08889
numOfImproved <- function(thisZip) {
        thisLocation <- zips[zips$zip==thisZip,]
        thisCommute <- distance(thisLocation$lat, thisLocation$lon, data$lat, data$lon)
        isImproved <- (commute1 - thisCommute) > 0
        return(length(subset(isImproved, isImproved == TRUE)))
}

#finding the locations with the largest number of improved commutes
improvedCommutes <- sapply(locations$zip, numOfImproved)
improvedWLocations <- data.frame(locations, improvedCommutes)
countsolution <- subset(improvedWLocations, improvedCommutes == max(improvedWLocations$improvedCommutes))
