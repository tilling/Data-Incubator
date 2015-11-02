#Code to do basic analysis of NYC data

require(dplyr)
require(data.table)
require(lubridate)

#Preprocessing in perl:
# Macintosh:Data Training John$ perl ProcCSV.pl nyc311calls.csv > nyc311reduced2.csv
# 13292  #Number that didn't parse
# Macintosh:Data Training John$ wc nyc311*.csv
# 10012261 381310399 6269199573 nyc311calls.csv
# 10012261 60063567 1123333865 nyc311reduced.csv #doesn't go into R well
# 9998969 40038576 1094462569 nyc311reduced2.csv
#Slight more than 1 in 1000 lines didn't parse
#Remember to use the input file name

filename <- "nyc311calls.csv"


#Load

nyc311 <- fread(filename)

#Process as needed

setnames(nyc311, make.names(colnames(nyc311))) 
         #Removes spaces so we can access

#Analyze

Answers <- rep(NA, 6)

#1. Determine 2nd most popular agency, then what fraction
#of complaints it has (i.e. second highest amount)

agency.counts <- table(nyc311[ , Agency])
count2 <- sort(agency.counts, decreasing=TRUE)[2]
Answers[1] <- count2 / sum(agency.counts)
names(Answers)[1] <- 'Complaints.2nd'
#2. Most surprising borough complaint

M <- nyc311[ , table(Complaint.Type, Borough)]

#Marginal probs
Type.marginal <- rowSums(M) / sum(M)

#Conditional probabilities of types: # in cell divided by # in borough
Borough.counts <- colSums(M)
Type.conditional <- M %*% diag (1 / Borough.counts)

#Ratio of the two
Surprise.ratio <- (1/Type.marginal) * Type.conditional

Answers[2] <- max(Surprise.ratio)
names(Answers)[2] <- 'Surprise.ratio'


#3. Get 90% and 10%-iles of latitude and subtract.
quantiles <- with(nyc311, 
                  quantile(Latitude, c(0.1, 0.9), na.rm=TRUE)
                  )
Answers[3] <- diff(quantiles) #second quantile (0.9) - first quantile (0.1)    
names(Answers)[3] <- 'Quantile.diff'


#4. Estimated 311 area.
#First, convert degree differences into kilometer-scaled differences.
#The unit conversions are from http://www.usgs.gov/faq/categories/9794/3022
#They are given for 38 degrees north of the equator,
#which is close enough to New York's 40.7.

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

Answers[4] <- pi * sqrt(det(V.km))
names(Answers)[4] <- 'Area.311'

#The next two both depend on times of calls.
#Supposedly we should remove calls that "don't seem to
#corresponds to that time" or whatever, but
#there may not be time for that. So we just 
#convert the date-and-time string to R units.

#5. Difference in average number of calls per hour.
instants <- nyc311[ , mdy_hms(Created.Date)]
hr <- hour(instants)
mn <- minute(instants)
#Remove the artifactual midnights
not.midnight <- (hr + mn > 0)
hr <- hr[not.midnight]
hr.count <- table(hr)
#OK. That gets you the hour counts. How about
#the number of days involved?
days <- nyc311[not.midnight, mdy(substr(Created.Date, 1, 10))]
distinct.days <- length(unique(days))
daily.hr.count <- hr.count / distinct.days
Answers[5] <- max(daily.hr.count) - min(daily.hr.count)
names(Answers)[5] <- 'Avg.calls.diff'



#6.
instants <- sort(instants[not.midnight])
dt <- as.numeric(diff(instants))
Answers[6] <- sd(dt)
names(Answers)[6] <- 'Interval.SD'

options(digits=10)
print(Answers)
