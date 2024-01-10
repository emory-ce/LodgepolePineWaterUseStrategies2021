### DBH Formatting, analysis, and visualization 

## Install Libraries
library (ggplot2)
library (tidyverse)
library(dplyr)
library(lubridate)

B1Ccol <-'#011627'
B1T1Col <- '#82D173'
B1T2Col <- '#157A6E'
MCol <- '#721817'
SiteColors <- c(B1Ccol,B1T1Col,B1T2Col,MCol)

setwd("C:/Users/12259/Dropbox (Old)/My PC (LAPTOP-FGN59URH)/Desktop/Thesis Research/Field Data/Lodgepole_WaterUseStrategies_2021")

##################
###Read in Data###
##################

### Read in and Format DBH Data
dbh_M <- read.csv(file = "DBH measurement_since 2021_M.csv")
dbh_M$Site <- 'M'
colnames(dbh_M)=c('TreeNumb', 'Oct9_19', 'July18_20','change1','Oct15_20','change2','June4_21', 'change3','Oct8_21','change4','Site')
dbh_M$Block <- NA
dbh_M$Treatment <- 'M'

dbh_B1C <- read.csv(file = "DBH measurement_since 2021_B1C.csv")
dbh_B1C <- dbh_B1C [-c(11:12)]
dbh_B1C$Site <- 'B1C'
colnames(dbh_B1C)=c('TreeNumb', 'Oct9_19', 'July18_20','change1','Oct15_20','change2','June4_21', 'change3','Oct8_21','change4','Site')
dbh_B1C$Block <- 1
dbh_B1C$Treatment <- 'C'


dbh_B1T1 <- read.csv(file = "DBH measurement_since 2021_B1T1.csv")
dbh_B1T1 <- dbh_B1T1 [-c(11:12)]
dbh_B1T1$Site <- 'B1T1'
colnames(dbh_B1T1)=c('TreeNumb', 'Oct9_19', 'July18_20','change1','Oct15_20','change2','June4_21', 'change3','Oct8_21','change4','Site')
dbh_B1T1$Block <- 1
dbh_B1T1$Treatment <- 1


dbh_B1T2 <- read.csv (file = "DBH measurement_since 2021_B1T2.csv")
dbh_B1T2 <- dbh_B1T2 [-c(11:12)]
dbh_B1T2$Site <- 'B1T2'
colnames(dbh_B1T2)=c('TreeNumb', 'Oct9_19', 'July18_20','change1','Oct15_20','change2','June4_21', 'change3','Oct8_21','change4','Site')
dbh_B1T2$Block <- 1
dbh_B1T2$Treatment <- 2


dbh_B2C <- read.csv(file = "DBH measurement_since 2021_B2C.csv")
dbh_B2C <- dbh_B2C [-c(11:12)]
dbh_B2C$Site <- 'B2C'
colnames(dbh_B2C)=c('TreeNumb', 'Oct9_19', 'July18_20','change1','Oct15_20','change2','June4_21', 'change3','Oct8_21','change4','Site')
dbh_B2C$Block <- 2
dbh_B1C$Treatment <- 'C'

dbh_B2T1 <- read.csv(file = "DBH measurement_since 2021_B2T1.csv")
dbh_B2T1 <- dbh_B2T1 [-c(11:12)]
dbh_B2T1$Site <- 'B2T1'
colnames(dbh_B2T1)=c('TreeNumb', 'Oct9_19', 'July18_20','change1','Oct15_20','change2','June4_21', 'change3','Oct8_21','change4','Site')
dbh_B2T1$Block <- 2
dbh_B2T1$Treatment <- 1

dbh_B2T2 <- read.csv (file = "DBH measurement_since 2021_B2T2.csv")
dbh_B2T2 <- dbh_B2T2 [-c(11:12)]
dbh_B2T2$Site <- 'B2T1'
colnames(dbh_B2T2)=c('TreeNumb', 'Oct9_19', 'July18_20','change1','Oct15_20','change2','June4_21', 'change3','Oct8_21','change4','Site')
dbh_B2T2$Block <- 2
dbh_B2T2$Treatment <- 2

dbh_B3C <- read.csv(file = "DBH measurement_since 2021_B3C.csv")
dbh_B3C <- dbh_B3C [-c(11:12)]
dbh_B3C$Site <- 'B3C'
colnames(dbh_B3C)=c('TreeNumb', 'Oct9_19', 'July18_20','change1','Oct15_20','change2','June4_21', 'change3','Oct8_21','change4','Site')
dbh_B3C$Block <- 3
dbh_B3C$Treatment <- 'C'

dbh_B3T1 <- read.csv(file = "DBH measurement_since 2021_B3T1.csv")
dbh_B3T1 <- dbh_B3T1 [-c(11:12)]
dbh_B3T1$Site <- 'B3T1'
colnames(dbh_B3T1)=c('TreeNumb', 'Oct9_19', 'July18_20','change1','Oct15_20','change2','June4_21', 'change3','Oct8_21','change4','Site')
dbh_B3T1$Block <- 3
dbh_B3T1$Treatment <- 1
dbh_B3T2 <- read.csv (file = "DBH measurement_since 2021_B3T2.csv")
dbh_B3T2 <- dbh_B3T2 [-c(11:12)]
dbh_B3T2$Site <- 'B3T1'
colnames(dbh_B3T2)=c('TreeNumb', 'Oct9_19', 'July18_20','change1','Oct15_20','change2','June4_21', 'change3','Oct8_21','change4','Site')
dbh_B3T2$Block <- 3
dbh_B3T2$Treatment <- 2

dbh <- rbind(dbh_B1C, dbh_B1T1, dbh_B1T2, dbh_M)

dbh$change3 [49] <- NA


dbh$change_2019to20 <- as.numeric(dbh$Oct15_20) - dbh$Oct9_19
dbh$change_2020to21 <- dbh$Oct8_21 - as.numeric (dbh$Oct15_20)


Total_dbh <- read.csv(file = 'Total_DBH.csv')
colnames(Total_dbh)=c('TreeNumb', 'Oct9_19', 'July18_20','July31_2020','Jul20_Oct19',
                      'Oct15_20','Oct20_Jul20','June4_21', 'Jun21_Oct20','Oct8_21',
                      'Oct_21_Jun21','dif21_2020','Block', 'Treatment')
Total_dbh [Total_dbh == 'NA'] <- NA

Total_dbh$dif21_2020 <- Total_dbh$Oct8_21 - as.numeric (Total_dbh$Oct15_20)
Total_dbh$dif21_2020 [Total_dbh$dif21_2020 < 0 ] <- NA
Total_dbh$dif20_2019 <- as.numeric(Total_dbh$Oct15_20) - as.numeric (Total_dbh$Oct9_19)
Total_dbh$dif20_2019 [Total_dbh$dif20_2019 < 0 ] <- NA
Total_dbh$dif20_2019 [Total_dbh$dif20_2019 > 30] <- NA

Total_dbh$July18_20 <- as.numeric(Total_dbh$July18_20)

### Calculating Mean DBH and Mean delta DBH
mean (Total_dbh$July18_20 [Total_dbh$Treatment == '2'])

mean_B1C_July20 <- (mean(Total_dbh$July18_20) + mean (Total_dbh$July31_2020))/2


mean_B1T2_Oct19 <- 64.93
mean_B2T2_Oct19 <- 66.59
mean_B3T2_Oct19 <- 60.33
(mean_B1T2 + mean_B2T2 + mean_B3T2) /3


aggregate(Total_dbh$July18_20, list (Total_dbh$Treatment), FUN = mean)

#######################
###### DBH Boxplots####
#######################

# Outdated 2020 plot
par(mfrow=c(1,3),mar=c(4.5,4.5,1.5,1.5))
boxplot(dbh$Oct9_19~dbh$Site,
        col = c(B1Ccol,B1T1Col,B1T2Col,MCol),
        xlab = 'Site',
        ylab = 'DBH (cm)',
        ylim = c(0,140),
        main = '9 Oct 2019')
boxplot(dbh$July18_20~dbh$Site,
        col = c(B1Ccol,B1T1Col,B1T2Col,MCol),
        xlab = 'Site',
        yaxt = 'n',
        ylab = '',
        ylim = c(0,140),
        main = '18 July 2020')

boxplot(dbh$Oct15_20~dbh$Site,
        col = c(B1Ccol,B1T1Col,B1T2Col,MCol),
        xlab = 'Site',
        yaxt = 'n',
        ylab = '',
        ylim = c(0,140),
        main = '15 Oct 2020')

# Plot of mean and range DBH for the spring and fall of 2021

par(mfrow=c(1,2),mar=c(4.5,4.5,1.5,1.5))
boxplot (dbh$June4_21~dbh$Site,
         col =c(B1Ccol, B1T1Col, B1T2Col, MCol),
         xlab = '',
         ylab = 'DBH (cm)',
         main = 'DBH Spring 2021'
         )
boxplot (dbh$Oct8_21~dbh$Site,
         col =c(B1Ccol, B1T1Col, B1T2Col, MCol),
         xlab = '',
         ylab = 'DBH (cm)',
         main = 'DBH Fall 2021')

#Boxplot of delta DBH from 2021 growing season
par (mfrow = c(1,1), mar = c(4,4,1,1))
boxplot (dbh$change4~dbh$Site,
         col = c(B1Ccol, B1T1Col, B1T2Col, MCol),
         xlab = 'Site',
         ylab = 'Change in DBH',
         ylim = c(0,5.5),
         main = 'Change in DBH over 2021')

# Boxplot of delta DBH 2020-2021 and 2021 growing season
par(mfrow=c(1,2),mar=c(4.5,4.5,1.5,1.5))
boxplot (dbh$change3~dbh$Site,
         col = c(B1Ccol, B1T1Col, B1T2Col, MCol),
         xlab = 'Site',
         ylab = 'Change in DBH',
         main = 'Change in DBH 2020-2021')
boxplot (dbh$change4~dbh$Site,
         col = c(B1Ccol, B1T1Col, B1T2Col, MCol),
         xlab = 'Site',
         ylab = 'Change in DBH',
         ylim = c(0,5.5),
         main = 'Change in DBH over 2021')


deltaDBH_19_20 <- ggplot (data = Total_dbh, aes (x= Treatment, y = dif20_2019,
                                          fill = Treatment)) +
        geom_boxplot() + scale_fill_manual(values = c(B1T1Col,B1T2Col, B1Ccol, MCol )) +
        xlab('Treatment') + ylab ('Change in DBH 2019-2020') + theme_bw() +
        theme(legend.position = 'bottom')

deltaDBH_20_21 <- ggplot (data = Total_dbh, aes (x= Treatment, y = dif21_2020,
                                          fill = Treatment)) +
        geom_boxplot() + scale_fill_manual(values = c(B1T1Col,B1T2Col, B1Ccol, MCol )) +
        xlab('Treatment') + ylab ('Change in DBH 2020-2021') + theme_bw() +
        theme ( legend.position = 'bottom')
deltaDBH_19_20
deltaDBH_20_21

