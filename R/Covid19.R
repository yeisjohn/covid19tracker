
# Preamble ----------------------------------------------------------------
rm(list=ls())
# R script to show the growth of COVID-19 testing and infection in the US, state by state.
# Data from https://covidtracking.com


# Load data ---------------------------------------------------------------

covid_daily <- read.csv('data/states-daily.csv')
require(tidyverse)
# require(extrafont)
# loadfonts()
# showfonts()
# Formatting --------------------------------------------------------------

covid_daily$date <- as.Date(as.character(covid_daily$date), format='%Y%m%d')

pcols <- c('#a6979c', '#a6b1e1',  '#d4d2a5', '#ac3931')
names(pcols) <- c('Total tests','Positive tests','Hospitalized', 'Deaths')

# covid_daily$otalTestResults = total tests
# covid_daily$positive = positives
# covid_daily$hospitalized = hospitalized
# covid_daily$death = deaths

s<-which(levels(covid_daily$state) == 'CA')
tmpdat <- covid_daily[which(covid_daily$state == levels(covid_daily$state)[s]),]
dates <- unique(tmpdat$date)
params <- c(which(colnames(tmpdat) == 'totalTestResults'),
            which(colnames(tmpdat) == 'positive'),
            which(colnames(tmpdat) == 'hospitalized'),
            which(colnames(tmpdat) == 'death')
            )


plot(0, type='n', main = paste0('COVID-19 in ', levels(covid_daily$state)[s]),
     xlim=c(min(covid_daily$date), max(covid_daily$date)+10), ylim=c(0, max(tmpdat$totalTestResults)), xaxt='n', ylab='', xlab='')
rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col=adjustcolor('#f1e4e8', 0.15), border='gray15')
for(v in 1:length(params)){
  keep <- which(!is.na(tmpdat[,params[v]]))
  polygon(c(dates[keep], rev(dates[keep])), c(tmpdat[keep,params[v]], rep(0, length(keep))), border=pcols[v], col=pcols[v], lwd=2)
  if(v %in% c(2,3)){
    arrows(max(covid_daily$date), max(tmpdat[,params[v]], na.rm=T), max(covid_daily$date)+2, max(tmpdat[,params[v]], na.rm=T)+1000, lwd=2, lty=2, col='gray25', length=0)
    arrows(max(covid_daily$date)+2, max(tmpdat[,params[v]], na.rm=T)+1000, max(covid_daily$date)+5, max(tmpdat[,params[v]], na.rm=T)+1000, lwd=2, lty=2, col='gray25', length=0)
    text(font=3, max(covid_daily$date)+8, max(tmpdat[,params[v]], na.rm=T)+1000, paste0(names(pcols)[v], ' = ', prettyNum(max(tmpdat[,params[v]], na.rm=T), big.mark = ',')), cex=0.8)
    
  } else {
    arrows(max(covid_daily$date), max(tmpdat[,params[v]], na.rm=T), max(covid_daily$date)+5, max(tmpdat[,params[v]], na.rm=T), lwd=2, lty=2, col='gray25', length=0)
    text(font=3, max(covid_daily$date)+8, max(tmpdat[,params[v]], na.rm=T), paste0(names(pcols)[v], ' = ', prettyNum(max(tmpdat[,params[v]], na.rm=T), big.mark = ',')), cex=0.8)
    
  }
}
axis(side=1, at=seq(min(covid_daily$date), max(covid_daily$date), by=7), paste0(format(seq(min(covid_daily$date), max(covid_daily$date), by=7), '%m/%d')))

