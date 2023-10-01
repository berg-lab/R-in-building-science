# Developed by Mohammad Heidarinejad, PhD, PE 
# Updated on: 09/04/2023
# Contact: muh182@iit.edu

# // Copyright (c) 2022-2023 The Built Environment Research Group (BERG)
# // Distributed under the MIT software license, see the accompanying
# // file LICENSE or http://www.opensource.org/licenses/mit-license.php.

# import libraries
library(tidyverse) # removing NA values
library(vtable) # showing the summary stats
library(dplyr) # selecting building types

# get the current path
getwd()

# set the csv file path - change this based on your file path
setwd("/Users/mohammad/Desktop")

# read the benchmarking file
data <- read.csv("Chicago_Energy_Benchmarking_-_2019_Data_Reported_in_2020.csv")

# method 1: using column #s
# select only the columns that are needed
chicagoData<-data[,c(10,11,12,21,22)]

# change the column headers
names(chicagoData)<-c('PropertyType','Area','YearBuilt','SiteEUI','SourceEUI')

# method 2: using the column names (might not work since the column names are long)
# column names can be selected too. For example, c("PropertyType",Year.Built",...)] # uncomment if this method is used

# remove NA readings
chicagoDataClean<-chicagoData%>%
  drop_na(c('PropertyType','Area','YearBuilt','SiteEUI','SourceEUI'))

# print number of NA values removed
n_removed = 0
n_removed = nrow(chicagoData)-nrow(chicagoDataClean)
print(paste("total number of rows removed = ",n_removed))

# show the current histogram (notice the histogram is skewed to left due to outliers)
hist(chicagoDataClean$SiteEUI,main='Histogram of Site EUI (with outliers)',xlab='Site EUI (kBtu/sqft)')
summary(chicagoDataClean$SiteEUI) 

# remove outliers
# method 1: using the IQR method

#Q1 = quantile(chicagoDataClean$SiteEUI, probs = 0.25)  
#Q3 = quantile(chicagoDataClean$SiteEUI, probs = 0.75)  
#IQR = Q3-Q1

# upper range
#upperRange = Q3 + IQR

# lower range
#lowerRange = Q1 - IQR

# method 2: using engineerging assumptions 
lowerRange = 10 # removing buildings with an EUI of 10 kBtu/sqft or less
upperRange = 2000 # removing buildings with an EUI of 2000 kBtu/sqft or greater


# show boxplot and histogram of EUIs when the dataset is cleaned
SiteEUInoOutliers<-subset(chicagoDataClean$SiteEUI,chicagoDataClean$SiteEUI<upperRange&
                            chicagoDataClean$SiteEUI>lowerRange)
boxplot(SiteEUInoOutliers)
hist(SiteEUInoOutliers,main='Histogram of Site EUI (no outlier)',xlab='Site EUI (kBtu/sqft)')

# select small buildings less than 250,000 sqft
chicagoDataSmall<-chicagoDataClean[chicagoDataClean$Area<250000&chicagoDataClean$SiteEUI<upperRange,]

# find summary statistics for each column using the sumtable command
sumtable(chicagoDataSmall,c('SiteEUI','SourceEUI','YearBuilt','Area'),
         summ=c('mean(x)','median(x)','max(x)','min(x)','sd(x)'),
         summ.names=c('Mean','Median','Maximum','Minimum','standard deviation'))

# select large buildings more than 250,000 sqft
chicagoDataBig<-chicagoDataClean[chicagoDataClean$Area>=250000&chicagoDataClean$SiteEUI<upperRange,,]

# find summary statistics for each column using the sumtable command
sumtable(chicagoDataBig,c('SiteEUI','SourceEUI','YearBuilt','Area'),
         summ=c('mean(x)','median(x)','max(x)','min(x)','sd(x)'),
         summ.names=c('Mean','Median','Maximum','Minimum','standard deviation'))

# show how to do some comparisons

# compare Year Built between small and large buildings 
boxplot(chicagoDataBig$SiteEUI,chicagoDataSmall$SiteEUI,
        names=c('Chicago large buildings','Chciago small buildings'),main='Chicago Year Built',
        xlab='Building Type',ylab='Year Built',outline=FALSE)

# select building types and do some comparisons
chicagoOffice<-filter(chicagoDataClean,PropertyType=='Office')
chicagoUniversity<-filter(chicagoDataClean,PropertyType=='College/University')
chicagoMultiFamily<-filter(chicagoDataClean,PropertyType=='Multifamily Housing')
chicagoSchool<-filter(chicagoDataClean,PropertyType=='K-12 School')
chicagoHospital<-filter(chicagoDataClean,PropertyType==
                      'Hospital (General Medical & Surgical)')

# plot data on charts to see patterns for different building types
boxplot(chicagoOffice$SiteEUI,chicagoMultiFamily$SiteEUI,chicagoSchool$SiteEUI,chicagoUniversity$SiteEUI,chicagoHospital$SiteEUI,
        names=c('Chicago Office','Chicao Multi Family','Chicago K-12','Chicago College','Chicago Hospital'),
        main='Site EUI Property Type',
        xlab='Property Type',ylab='Site EUI (kBtu/sqft)',outline=FALSE)
