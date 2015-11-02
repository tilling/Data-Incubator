#Challenge 3

#This is taken from ACS 2013 microdata on
#sex and marital status by geography.

require(dplyr)
require(data.table)

usa.dat <- fread('usa_00002.dat', colClasses='string')

grab.cols <- function(start, stop) {
  x <- usa.dat[ , substr(V1, start, stop)]
  return( as.integer(x) )
}

city <- grab.cols(31, 34)
sex <- grab.cols(50, 50)
age <- grab.cols(51, 53)
status <- grab.cols(54, 54)
just.married <- grab.cols(55, 55)

agernd <- 5 * round(as.numeric(age) / 5)

proper.table <- data.table(city=city, sex=sex, age=age, agernd=agernd,
                    status=status, just.married=just.married)

#Summarize everything by city -- then it gets easy
strata <- group_by(proper.table[ age > 22 & age <= 65], city, agernd)

summary <- summarize(strata, all.men = sum(sex==1), all.women = sum(sex==2),
                single.men = sum(sex==1 & status > 2), 
                married.men = sum(sex==1 & status <= 2), 
                single.women = sum(sex==2 & status > 2), 
                married.women = sum(sex==2 & status <= 2),
                grooms = sum(just.married==2 & sex==1),
                brides = sum(just.married==2 & sex==2)
              )

summary$sr.all = summary[ , all.men / all.women]
summary$sr.single = summary[ , single.men / single.women]
summary$mr.men = summary[ , married.men / single.men]
summary$mr.women = summary[ , married.women / single.women]
summary$wedrate.men = summary [ , grooms / single.men]
summary$wedrate.women = summary [ , brides / single.women]

#Graph objects for men
lowess.men <- with(summary, lowess( wedrate.men ~ log(sr.single))); 
lowess.women <- with(summary, lowess( wedrate.women ~ -log(sr.single))); 

plot(lowess.men, type='l', col='blue',
     xlab="Log ratio (own sex to opposite)", 
     ylab='Marriage rate')
lines(lowess.women, col='red')
legend( 'topleft',  legend=c('Men', 'Women'), 
        lty=c(1,1), col=c('blue', 'red'),
        bty='n')
title(main="Marriage rate vs. log sex ratio")

