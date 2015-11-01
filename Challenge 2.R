#Code to do basic analysis of NYC data

require(dplyr)
require(data.table)


#Preprocessing in perl:
# Macintosh:Data Training John$ perl ProcCSV.pl nyc311calls.csv > nyc311reduced2.csv
# 13292  #Number that didn't parse
# Macintosh:Data Training John$ wc nyc311*.csv
# 10012261 381310399 6269199573 nyc311calls.csv
# 10012261 60063567 1123333865 nyc311reduced.csv #doesn't go into R well
# 9998969 40038576 1094462569 nyc311reduced2.csv
#Slight more than 1 in 1000 lines didn't parse
#Remember to use the input file name

filename <- "nyc311reduced2.csv"


#Load

#nyc311 <- fread(filename)

#Process as needed

#Analyze

#1. Determine 2nd most popular agency, then what fraction
#of complaints it has (i.e. second highest amount)

#2. Most surprising borough complaint

#3. Get 90% and 10%-iles of latitude and subtract.
quantiles <- with(nyc311, 
                  quantile(Latitude, c(0.1, 0.9), na.rm=TRUE)
                  )
ans.3 <- diff(quantiles) #second quantile (0.9) - first quantile (0.1)    

#4. Estimated 311 area.
#The covariance matrix of latitude and longitude in degrees is
V.degree <- with(nyc311, cov ( cbind(Latitude, Longitude), use='complete'))

#The unit conversions are from http://www.usgs.gov/faq/categories/9794/3022
#They are given for 38 degrees north of the equator,
#which is close enough to New York's 40.7.
#These can be treated as ordinary scaling factors.

feet.per.degree <- c(Latitude=364000, Longitude=288200)
km.per.foot <- 1 / 3280.84
km.per.degree <- km.per.foot * feet.per.degree

#The actual values of km.lat can't be
#interpreted, only the differences between points.
#Likewise for km.long.
km.lat <- with(nyc311, km.per.degree['Latitude'] * Latitude)
km.long <- with(nyc311, km.per.degree['Longitude'] * Longitude)
#Covariance matrix, scaled for kilometers
V.km <- cov( cbind(km.lat, km.long) , use='pair')
ans.4 <- pi * sqrt(det(V.km))


#