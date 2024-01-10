# Meteorological Data and Soil Moisture Data
setwd("C:/Users/12259/Dropbox (Old)/My PC (LAPTOP-FGN59URH)/Desktop/Thesis Research/Field Data/Lodgepole_WaterUseStrategies_2021")

  #Organization
library(dplyr)
library (ggplot2)
library (tidyr)

  #Calculating Palmer Drought Severity Index
library(precintcon)
#library (scPDSI)
library (SPEI)

B1Ccol <-'#011627'
B1T1Col <- '#82D173'
B1T2Col <- '#157A6E'
MCol <- '#721817'
SiteColors <- c(B1Ccol,B1T1Col,B1T2Col,MCol)


# Read in Upper Penticton Creek Watershed Experimental Data Repository Files
  #UPC Data Access Link:
  #https://zenodo.org/record/5520109#.YljCksjMK3A
il_all_sites <- read.csv (file = "il_all_sites.csv")
il_site_coords <- read.csv(file = "il_site_coords.csv")

daily_p1_met <- read.csv (file = 'met_p1_raw.csv')
monthly_p1_met <- read.csv (file= 'met_p1_monthly.csv')


#Read in Meteorological Data from Block 1 Stations
Met_B1C <- read.csv(file = "B1C_met_2021.csv")
Met_B1T1 <- read.csv(file = "B1T1_met_2021.csv")
Met_B1T2 <- read.csv(file = "B1T2_met_2021.csv")


#Read in Soil Moisture Data
Soil_Moisture_2021 <- read.csv(file = "Soil_Moisture_2021.csv")


#Read in extracted soil water content
SWC_2021 <- read.csv (file = "soil_extraction_water_cont.csv")



#### FORMAT METEOROLOGICAL DATA ####

# Calculate Mean Daily Precipitation for each day from historic data
il_all_sites$POSIXdate_start <- as.POSIXct(il_all_sites$start_dt,
                                           format = "%Y-%m-%d %H:%M:%S")
il_all_sites$POSIXdate_end <- as.POSIXct(il_all_sites$end_dt,
                                           format = "%Y-%m-%d %H:%M:%S")

# Subset of Data from site P7
  # Forest Type Lodgepole pine forest in 240
  #Beginning in 1997 and ending in 2008
il_P7 <- subset (il_all_sites, il_all_sites$site == 'p7')


daily_sum_precip <- il_P7 %>%
  mutate(day = as.Date(POSIXdate_start, format = "%Y-%m-%d"))

# use dplyr
daily_sum_precip <- il_P7 %>%
  mutate(day = as.Date(POSIXdate_start, format="%Y-%m-%d")) %>%
  group_by(day) %>% # group by the day column
  summarise(total_precip=sum (ppt)) %>%  # calculate the SUM of all precipitation that occurred on each day
  na.omit()

nrow(daily_sum_precip)
# 437 events between 1997 & 2008

# Use Tidyr to complete days with no precip
daily_sum_precip <- daily_sum_precip %>%
  mutate(day = as.Date(day)) %>%
  complete(day = seq.Date(min(day), max(day), by="day"))
daily_sum_precip [is.na(daily_sum_precip)] = 0

# how large is the resulting data frame?
nrow(daily_sum_precip)
# 4118


# Calculate Potential Evaporation for PDSI

monthly_p1_met$PE <- 





#Format POSIXdate & Rename Columns
Met_B1C$Site <- 'B1C'
Met_B1T1$Site <- 'B1T1'
Met_B1T2$Site <- 'B1T2'

Met_B1C$POSIXdate <- as.POSIXlt (Met_B1C$Date.Time..GMT.07.00, format = '%m/%d/%Y %H:%M')
Met_B1T1$POSIXdate <- as.POSIXlt (Met_B1T1$Date.Time..GMT.07.00, format = '%m/%d/%Y %H:%M')
Met_B1T2$POSIXdate <- as.POSIXlt (Met_B1T2$Date.Time..GMT.07.00, format = '%m/%d/%Y %H:%M')

colnames(Met_B1C)=c('ID_Num', 'Date_Time', 'Wind_Direction',
                     'Voltage_V','Current_mA','Temp','rH', 
                     'Site','POSIX_Date')
colnames(Met_B1T1)=c('ID_Num', 'Date_Time','Voltage_V', 'Current_mA',
                     'Wind_Direction','Wind_Speed_mph', 'Gust_Speed_mph',
                     'Solar_Radiation', 'Temp', 'rH', 'Site', 'POSIX_Date')
colnames(Met_B1T2)=c('ID_Num', 'Date_Time', 'Voltage_V','Current_mA',
                     'Precip', 'Wind_Speed_mph', 'Gust_Speed_mph',
                     'Wind_Direction', 'Temp', 'rH',
                     'Solar_Radiation_W', 'Site', 'POSIX_Date')

Met_B1C$Date <- as.Date (Met_B1C$Date, format = "%m/%d/%Y %H:%M")
Met_B1C$Month <- format (Met_B1C$Date, '%m')

Met_B1T1$Date <- as.Date (Met_B1T1$Date, format = "%m/%d/%Y %H:%M")
Met_B1T1$Month <- format (Met_B1T1$Date, '%m')

Met_B1T2$Date <- as.Date (Met_B1T2$Date, format = "%m/%d/%Y %H:%M")
Met_B1T2$Month <- format (Met_B1T2$Date, '%m')



Met_B1C$Temp_C <- (Met_B1C$Temp - 32) * 5/9
Met_B1T1$Temp_C <- (Met_B1T1$Temp - 32) * 5/9
Met_B1T2$Temp_C <- (Met_B1T2$Temp - 32) * 5/9
 
Met_B1C$Temp [Met_B1C$Temp == -888.88] <- NA
Met_B1C$rH [Met_B1C$rH == -888.88] <- NA

colnames(SWC_2021)=c('Sample_ID', 'Date', 'Block', 'Treatment','Depth',
                     'Date_Extracted','Distillation_Unit', 'Freeze_Time', 'Start',
                     'Extraction_Time (hrs)','Flast_Weight','Flask_w_Samp',
                     'Sample_Weight','Dry_Sample_Flask','Dry_Weight','Water_Loss',
                     'WC', 'Notes')

Met_B1T2$Precip_cm <- Met_B1T2$Precip * 2.54
Met_B1T2$Precip_mm <- Met_B1T2$Precip_cm * 1000

#### FORMAT SOIL MOISTURE DATA ####

Soil_Moisture_2021$POSIXdate <- as.POSIXlt (Soil_Moisture_2021$ï..Timestamp, format = "%m/%d/%Y %H:%M")
Soil_Moisture_2021$Date <- as.Date (Soil_Moisture_2021$ï..Timestamp, format = "%m/%d/%Y %H:%M")
Soil_Moisture_2021$Month <- format (Soil_Moisture_2021$Date, '%m')



SWC_2021$POSIXdate <- as.POSIXlt (SWC_2021$Date, format = '%m/%d/%Y')
SWC_2021 [SWC_2021 == 'NA'] <-- NA 
SWC_2021$Date <- as.Date (SWC_2021$Date, format = "%m/%d/%Y")
SWC_2021$Month <- format (SWC_2021$Date, '%m')


SWC_2021$WC <- SWC_2021$Water_Loss / SWC_2021$Dry_Weight * 100

SWC_2021_B1C = filter(SWC_2021,Block == 1 & Treatment == 'C')
SWC_2021_B1T1 = filter(SWC_2021,Block == 1 & Treatment == '1')
SWC_2021_B1T2 = filter(SWC_2021,Block == 1 & Treatment == '2')

SWC_2021_Block1 = filter (SWC_2021, Block == 1)



#### METEOROLGICAL ANALTSIS ####
monthly_precip_sum <- aggregate(x = Met_B1T2$Precip_cm,                # Specify data column
                          by = list(Met_B1T2$Month),              # Specify group indicator
                          FUN = sum)
monthly_precip_mean <- aggregate(x = Met_B1T2$Precip_cm,                # Specify data column
                            by = list(Met_B1T2$Month),              # Specify group indicator
                            FUN = mean)
monthly_precip_events <- aggregate(x = Met_B1T2$Precip_cm,                # Specify data column
                            by = list(Met_B1T2$Month),              # Specify group indicator
                            FUN = length)
monthly_precip <- cbind (monthly_precip_sum, monthly_precip_mean$x, monthly_precip_events$x)
colnames (monthly_precip) = c('Month','Total_Monthly_Precip', 'Mean_Monthly_Precip', "Total_Events")
                                            



### SOIL MOISTURE ANALYSIS ###

sum_SWC_2021 <- aggregate(x = SWC_2021$WC,                # Specify data column
          by = list(SWC_2021$Month, SWC_2021$Treatment, SWC_2021$Depth),              # Specify group indicator
          FUN = mean)
colnames (sum_SWC_2021) = c('Month','Block','Treatment','WC')

min.Date = min(Soil_Moisture_2021$POSIXdate)
min.SWC = min(Soil_Moisture_2021$B1T2_S35_Avg)
max.Date = max(Soil_Moisture_2021$POSIXdate)
max.SWC = max(Soil_Moisture_2021$B1C_S35_Avg)
xl <- ymd("2021-07-22","2021-10-08")

Soil_Moisture_2021$Date <- as.Date(Soil_Moisture_2021$POSIXdate)

mean (as.numeric(Soil_Moisture_2021$B1T2_Dif))


###############
#####PLOTS#####
###############

# ###Plotting soil moisture by sensor
# par(mfrow=c(1,1),mar=c(5,5,1,1))
# plot (Soil_Moisture_2021$POSIXdate,
#       Soil_Moisture_2021$Sens_1, type = 'l',
#       ylim = c(0,0.75))
# lines(Soil_Moisture_2021$POSIXdate,
#       Soil_Moisture_2021$Sens_2)
# lines(Soil_Moisture_2021$POSIXdate,
#       Soil_Moisture_2021$Sens_3)
# lines(Soil_Moisture_2021$POSIXdate,
#       Soil_Moisture_2021$Sens_4)
# lines(Soil_Moisture_2021$POSIXdate,
#       Soil_Moisture_2021$Sens_5)
# lines(Soil_Moisture_2021$POSIXdate,
#       Soil_Moisture_2021$Sens_6)
# lines(Soil_Moisture_2021$POSIXdate,
#       Soil_Moisture_2021$Sens_7)
# lines(Soil_Moisture_2021$POSIXdate,
#       Soil_Moisture_2021$Sens_8)
# 
# par(mfrow=c(1,1),mar=c(5,5,1,1))
# plot (Soil_Moisture_2021$POSIXdate, 
#       Soil_Moisture_2021$B1C_S5_Avg,
#       ylim = c(0,0.4),
#       xlab = 'Date',
#       ylab = 'Soil Water Content (mm3/mm)',
#       type = 'l', col = B1Ccol)
# lines (Soil_Moisture_2021$POSIXdate, Soil_Moisture_2021$B1C_S35_Avg,
#        lty = 2,
#        col = B1Ccol)
# lines (Soil_Moisture_2021$POSIXdate, Soil_Moisture_2021$B1T1_S5_Avg,
#        col = B1T1Col)
# lines (Soil_Moisture_2021$POSIXdate, Soil_Moisture_2021$B1T1_S35_Avg,
#        col = B1T1Col,
#        lty = 2)
# lines (Soil_Moisture_2021$POSIXdate, Soil_Moisture_2021$B1T2_S5_Avg,
#        col = B1T2Col)
# lines (Soil_Moisture_2021$POSIXdate, Soil_Moisture_2021$B1T2_S35_Avg,
#        col = B1T2Col,
#        lty = 2)
# legend ("topleft", legend = c("B1C @ 5 cm", "B1C @ 35 cm", 
#                                "B1T1 @ 5 cm", "B1T1 @ 35 cm",
#                                "B1T2 @ 5 cm", "B1T2 @ 35 cm"),
#         col = c(B1Ccol, B1Ccol, B1T1Col, B1T1Col, B1T2Col, B1T2Col),
#         lty = 1:2, cex = 0.65)
# 
# 
# # Plotting each treatment individually
# par(mfrow=c(3,1),mar=c(2,5,1,5))
# plot (Soil_Moisture_2021$POSIXdate, 
#       Soil_Moisture_2021$B1C_S5_Avg,
#       ylim = c(0,0.5),
#       xlab = '',
#       ylab = '',
#       type = 'l', col = B1Ccol)
# lines (Soil_Moisture_2021$POSIXdate, Soil_Moisture_2021$B1C_S35_Avg,
#        lty = 2,
#        col = B1Ccol)
# plot (Soil_Moisture_2021$POSIXdate, 
#       Soil_Moisture_2021$B1T1_S5_Avg,
#       ylim = c(0,0.5),
#       xlab = '',
#       ylab = 'Soil Water Content (mm3/mm)',
#       type = 'l', col = B1T1Col)
# lines (Soil_Moisture_2021$POSIXdate, Soil_Moisture_2021$B1T1_S35_Avg,
#        lty = 2,
#        col = B1T1Col)
# plot (Soil_Moisture_2021$POSIXdate, 
#       Soil_Moisture_2021$B1T2_S5_Avg,
#       ylim = c(0,0.5),
#       xlab = 'Date',
#       ylab = '',
#       type = 'l', col = B1T2Col)
# lines (Soil_Moisture_2021$POSIXdate, Soil_Moisture_2021$B1T2_S35_Avg,
#        lty = 2,
#        col = B1T2Col)
# plot ( x = NA, y = NA)
# legend ("right", inset = c( -0.2, 0), legend = c("B1C @ 5 cm", "B1C @ 35 cm", 
#                                                  "B1T1 @ 5 cm", "B1T1 @ 35 cm",
#                                                  "B1T2 @ 5 cm", "B1T2 @ 35 cm"),
#         col = c(B1Ccol, B1Ccol, B1T1Col, B1T1Col, B1T2Col, B1T2Col),
#         lty = 1:2, cex = 0.65, box.lty = 0)

### Plotting Site Meteorological Data

par (mfrow = c(1,1), mar = c(4,5,1,2),family="serif")
plot (Met_B1T1$POSIX_Date, 
      Met_B1T1$Temp,
      xlab = 'Date',
      ylab = 'Temperature (F)',
      type = 'l', lty = 2, col = B1T1Col)



#Temperature (F)
par(mfrow=c(3,1),mar=c(4,5,1,2))
plot (Met_B1C$POSIX_Date, 
      Met_B1C$Temp,
      xlab = '',
      ylab = '',
      type = 'l', col = B1Ccol)
plot (Met_B1T1$POSIX_Date, 
      Met_B1T1$Temp,
      xlab = '',
      ylab = 'Temperature (F)',
      type = 'l', col = B1T1Col)
plot (Met_B1T2$POSIX_Date, 
      Met_B1T2$Temp,
      xlab = 'Date',
      ylab = '',
      type = 'l', col = B1T2Col)


#Temp C
par(mfrow=c(3,1),mar=c(4,5,1,2))
plot (Met_B1C$POSIX_Date, 
      Met_B1C$Temp_C,
      xlab = '',
      ylab = '',
      type = 'l', col = B1Ccol)
plot (Met_B1T1$POSIX_Date, 
      Met_B1T1$Temp_C,
      xlab = '',
      ylab = 'Temperature (C)',
      type = 'l', col = B1T1Col)
plot (Met_B1T2$POSIX_Date, 
      Met_B1T2$Temp_C,
      xlab = 'Date',
      ylab = '',
      type = 'l', col = B1T2Col)


# Relative Humidity
par(mfrow=c(3,1),mar=c(4,5,1,2), family = 'serif')
plot (Met_B1C$POSIX_Date, 
      Met_B1C$rH,
      xlab = '',
      ylab = '',
      type = 'l', col = B1Ccol)
plot (Met_B1T1$POSIX_Date, 
      Met_B1T1$rH,
      xlab = '',
      ylab = 'Relative Humidity',
      type = 'l', col = B1T1Col)
plot (Met_B1T2$POSIX_Date, 
      Met_B1T2$rH,
      xlab = 'Date',
      ylab = '',
      type = 'l', col = B1T2Col)


# Precipitation 


par (mfrow = c(1,1), mar = c(4,5,1,2),family="serif")
plot (Met_B1T2$POSIX_Date, 
      Met_B1T2$Precip_cm,
      xlab = 'Date',
      ylab = 'Precipitation (cm)',
      type = 'l', lwd = 2, col = B1Ccol)



#################
par(mfrow=c(2,2), tcl=-0.5, family="serif")
  #Plotted in quadrants
# Top left panel - B1C
plot(x = Soil_Moisture_2021$POSIXdate, y = Soil_Moisture_2021$B1C_S5_Avg, 
     xlab="", ylab="Soil Moisture Content (m3/m3)", 
     ylim=c(0,max.SWC),type = 'l', col = B1Ccol)
lines (Soil_Moisture_2021$POSIXdate, Soil_Moisture_2021$B1C_S35_Avg,
       lty = 2,
       col = B1Ccol)

# Top right panel - Legend
plot.new()
legend ("right", inset = c( 0, 0), legend = c("B1C @ 5 cm", "B1C @ 35 cm", 
                                                 "B1T1 @ 5 cm", "B1T1 @ 35 cm",
                                                 "B1T2 @ 5 cm", "B1T2 @ 35 cm"),
        col = c(B1Ccol, B1Ccol, B1T1Col, B1T1Col, B1T2Col, B1T2Col),
        lty = 1:2, cex = 1, box.lty = 0)
# Bottom left panel - B1T1
plot(x = Soil_Moisture_2021$POSIXdate, y = Soil_Moisture_2021$B1T1_S5_Avg, 
     xlab="", ylab="Soil Moisture Content (m3/m3)", 
     ylim=c(0,max.SWC),type = 'l', col = B1T1Col)
lines (Soil_Moisture_2021$POSIXdate, Soil_Moisture_2021$B1T1_S35_Avg,
       lty = 2,
       col = B1T1Col)


# Bottom right panel
plot(x = Soil_Moisture_2021$POSIXdate, y = Soil_Moisture_2021$B1T2_S5_Avg, 
     xlab="", ylab="Soil Moisture Content (m3/m3)", 
     ylim=c(0,max.SWC),type = 'l', col = B1T2Col)
lines (Soil_Moisture_2021$POSIXdate, Soil_Moisture_2021$B1T2_S35_Avg,
       lty = 2,
       col = B1T2Col)

  # Plotted in rows
par(mfrow=c(3,1),mar=c(4,5,1,2), family = 'serif')

plot (Soil_Moisture_2021$Date, 
      Soil_Moisture_2021$B1C_S5_Avg,
      xlab="", ylab=" ", 
      ylim=c(0,max.SWC),type = 'l', col = B1Ccol)
lines (Soil_Moisture_2021$Date,
       Soil_Moisture_2021$B1C_S35_Avg,
       col = B1Ccol,
       lty = 2)

plot (Soil_Moisture_2021$Date, 
      Soil_Moisture_2021$B1T1_S5_Avg,
      xlab="", ylab="Soil Moisture Content (m3/m3)", 
      ylim=c(0,max.SWC),type = 'l', col = B1T1Col)
lines (Soil_Moisture_2021$Date,
       Soil_Moisture_2021$B1T1_S35_Avg,
       col = B1T1Col,
       lty = 2)

plot (Soil_Moisture_2021$Date, 
      Soil_Moisture_2021$B1T2_S5_Avg,
      xlab="Date", ylab=" ", 
      ylim=c(0,max.SWC),type = 'l', col = B1T2Col)
lines (Soil_Moisture_2021$Date,
       Soil_Moisture_2021$B1C_S35_Avg,
       col = B1T2Col,
       lty = 2)




# Moisture of 5 cm v. 35 cm
par(mfrow=c(2,2), tcl=-0.5, family="serif")

# Top left panel - B1C
plot(x = Soil_Moisture_2021$B1C_S5_Avg, y = Soil_Moisture_2021$B1C_S35_Avg, 
     xlab="5 cm Depth", ylab="35 cm Depth (m3/m3)", 
     ylim=c(0,max.SWC), col = B1Ccol)

# Top right panel - Legend
plot.new()
legend ("right", inset = c( 0, 0), legend = c("B1C", 
                                              "B1T1",
                                              "B1T2"),
        col = c(B1Ccol, B1Ccol, B1T1Col, B1T1Col, B1T2Col, B1T2Col),
        lty = 1, cex = 1, box.lty = 0)
# Bottom left panel - B1T1
plot(x = Soil_Moisture_2021$B1T1_S5_Avg, y = Soil_Moisture_2021$B1T1_S35_Avg, 
     xlab="5 cm Depth", ylab="35 cm Depth (m3/m3)", 
     ylim=c(0,max.SWC), col = B1T1Col)

# Bottom right panel
plot(x = Soil_Moisture_2021$B1T2_S5_Avg, y = Soil_Moisture_2021$B1T2_S35_Avg, 
     xlab="", ylab="Soil Moisture Content (m3/m3)", 
     ylim=c(0,max.SWC), col = B1T2Col)


B1C_depth_ratio <- lm (Soil_Moisture_2021$B1C_S35_Avg~Soil_Moisture_2021$B1C_S5_Avg)
# slope 0.59; int 0.048
B1T1_depth_ratio <- lm (Soil_Moisture_2021$B1T1_S35_Avg~Soil_Moisture_2021$B1T1_S5_Avg)
# slope 0.84; int 0.027
B1T2_depth_ratio <- lm (Soil_Moisture_2021$B1T2_S35_Avg~Soil_Moisture_2021$B1T2_S5_Avg)
# slope 0.53; int 0.015


#Difference in moisture between depths (35-5)

Soil_Moisture_2021$B1C_Dif <- Soil_Moisture_2021$B1C_S35_Avg - Soil_Moisture_2021$B1C_S5_Avg
Soil_Moisture_2021$B1T1_Dif <- Soil_Moisture_2021$B1T1_S35_Avg - Soil_Moisture_2021$B1T1_S5_Avg
Soil_Moisture_2021$B1T2_Dif <- Soil_Moisture_2021$B1T2_S35_Avg - Soil_Moisture_2021$B1T2_S5_Avg

par(mfrow=c(1,1), mar=c(4,4,1,0.5), family="serif")
plot (x = Soil_Moisture_2021$POSIXdate, y = Soil_Moisture_2021$B1C_Dif,
      xlab="Date", ylab="Moisture Difference 35cm - 5cm (m3/m3)", 
      ylim=c(-0.15,0.30), col = B1Ccol, type = 'l')
lines (x = Soil_Moisture_2021$POSIXdate, y = Soil_Moisture_2021$B1T1_Dif,
       col = B1T1Col)
lines (x = Soil_Moisture_2021$POSIXdate, y = Soil_Moisture_2021$B1T2_Dif,
       col = B1T2Col)
legend("bottomleft",                    # Add legend to plot
       legend = c("B1C", "B1T1", "B1T2"),
       col = c(B1Ccol, B1T1Col, B1T2Col),
       lty = 1,
       cex = 0.662,
       horiz = TRUE)



legend ("bottom", legend = c("B1C", "B1T1", "B1T2"),
        col = c(B1Ccol, B1T1Col, B1T2Col),
        lty = 1, cex = 1, box.lty = 1)




# Grams Water Loss from drying
library(ggplot2)
Soil_Moisture_Boxplot <- ggplot(SWC_2021_Block1, aes(x = Date, y = WC)) + 
  geom_point(aes(color = Treatment, fill = Treatment), 
               binaxis='y', stackdir='center') +
  scale_color_manual(values = c(B1Ccol, B1T1Col, B1T2Col)) + 
  scale_fill_manual(values = c(B1Ccol, B1T1Col, B1T2Col))
Soil_Moisture_Boxplot <- Soil_Moisture_Boxplot + facet_grid(rows = vars(Depth)) + theme_bw() +
  theme ( legend.position = 'bottom')+ ylab('Water Content (%)')
Soil_Moisture_Boxplot 



Soil_Moisture_Boxplot <- ggplot( ) + 
  geom_point(data = SWC_2021, aes(x = Month, y = WC, color = Treatment, fill = Treatment), 
             binaxis='y', stackdir='center') +
  scale_color_manual(values = c(B1Ccol, B1T1Col, B1T2Col)) + 
  scale_fill_manual(values = c(B1Ccol, B1T1Col, B1T2Col))
Soil_Moisture_Boxplot <- Soil_Moisture_Boxplot + facet_grid(rows = vars(Depth)) + theme_bw() +
  theme ( legend.position = 'bottom')+ ylab('Water Content (%)')
Soil_Moisture_Boxplot 
