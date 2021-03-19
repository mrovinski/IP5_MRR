#Individual Project 5
#Author: Molly Rovinski
#Version: 1.0
#Semester: Spring 2021
#Goal: Recreate the analysis done in Python for IP4_MRR in R.
#Note: The redownloaded datafile includes data up to 2011. The original file used in IP4 was updated. 



library(datasets)
library(tidyverse)
library(psych)
library (ggplot2)
library (dplyr)

getwd()
setwd(dir = "/Users/mollyrovinski/Documents/2020-2021 Semester/Data Science")
getwd()
GTD<-read.csv("gtd.csv", header = TRUE, sep =',')

#get column names
colnames(GTD)

#get first 5 rows
head(GTD)

#get last 5 rows
tail(GTD)

#get data types
sapply(GTD, class)

#get summary on each variable
summary(GTD)

#get percentage of missing data for each column
colMeans(is.na(GTD))

#count number of kills by year
my_summary<-GTD %>%
  count(nkill, iyear, sort = TRUE)

#count for country 
country_text <- GTD %>%
  count(country_txt, sort=TRUE)

#get frequency tables AND distribution graphs 
library(epiDisplay)
tab1(GTD$extended, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$crit1, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$crit2, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$crit3, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$doubtterr, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$multiple, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$alternative_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$country_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$region_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$vicinity, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$specificity, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$attacktype1_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$attacktype2_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$attacktype3_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$success, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$suicide, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$weaptype1_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$weaptype2_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$weaptype3_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$weaptype4_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$weapsubtype1_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$weapsubtype2_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$weapsubtype3_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$weapsubtype4_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$targtype1_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$targsubtype1_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$natlty1_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$targtype2_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$targsubtype2_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$natlty2_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$targtype3_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$targsubtype3_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$natlty3_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$guncertain1, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$guncertain2, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$guncertain3, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$individual, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$claimed, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$claimmode_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$compclaim, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$claim2, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$claimmode2_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$claim3, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$claimmode3_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$property, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$propextent_txt, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$ishostkid, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$ransom, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$hostkidoutcome, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$INT_LOG, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$INT_IDEO, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$INT_MISC, sort.group = "decreasing", cum.percent = TRUE)
tab1(GTD$INT_ANY, sort.group = "decreasing", cum.percent = TRUE)

#create correlation heat map
GTD_num.cor <- cor(GTD[, c("eventid", "iyear", "imonth", "iday")])
library(reshape2)
GTDmelt.cor <- melt(GTD_num.cor)
head(GTDmelt.cor)
ggplot(data=GTDmelt.cor, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

#create scatterplots 
ggplot(data=GTD, mapping=aes(x=iyear, y=nkill)) + geom_point(color = "blue") 
ggplot(data=GTD, mapping=aes(x=nkill, y=nperps)) + geom_point(color = "red") 

#create barplots for categorical variables
counts <- table(GTD$iyear)
barplot(counts,main = "Attacks in a Year", xlab="Year", col="blue")

counts2 <-table(GTD$country_txt)
barplot(counts2, main="Attacks per Country", xlab="Countries", col="purple")

counts3 <-table(GTD$weaptype1_txt)
barplot(counts3, main="Primary Weapon Type in Attack", xlab="Weapon Type", col="purple")

counts4 <-table(GTD$targtype1_txt)
barplot(counts4, main=" Target of Attacks", xlab="Target", col="red")

counts5 <-table(GTD$natlty1_txt)
barplot(counts5, main="Nationality of Target", xlab="Countries", col="purple")

counts6 <-table(GTD$attacktype1_txt)
barplot(counts6, main="Type of Attacks", xlab="Attack Types", col="blue")

counts7 <-table(GTD$success)
barplot(counts7, main="Was the Attack a Success?", xlab="Yes/No", col="purple")

#create a boxplot 
boxplot(GTD$iyear, main = "Trend of Terrorist Attack", ylab="Years")
