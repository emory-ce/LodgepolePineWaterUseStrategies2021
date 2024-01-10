
# Lodgepole pine water isotope figures 

#Meteorological data including historical precipitation data (1996-2005), local station data from each of the treatments in block 1, VPD,
  #4 calculations of PET & calculated dryness index
source('Met_2021.R')
#Diameter at breast height of 45 trees per plot in each block plus 10 matures from 2019-2021
  #Includes anovas of variance in delta DBH from 2020-2021
source ('DBH_2021.R')
#Soil moisture data collected from continuous moisture probes deployed across block 1 at depths 5cm and 40 cm
  #Also includes gravimetric water content from soil samples collected for isotope analysis
  # Summary statistics; Tests of normality and outliers;  analysis of varience soil water content by treatment, block, depth, and season
source ('SoilMoisture_2021.R')
# Branch and soil water isotope analysis 
  # Summary statistics; Tests of normality and outliers of soil moisture, soil d2H, soil d18O, and branch d2H and d18O
  #analysis of variance in d2H and d18O in branch water by block, treatment, and season
  #analysis of varience in soil water d2H and d18O by block, treatment, depth, and season
source ('WaterIso_2021.R')
#Using the MixSIAR Bayesian mixing model using the monte carlo method to partition different potential water sources from the soil profile
  #Using different depth levels defined in 'WaterIso_2021 as potential sources for treatments at different points of the growing season
  #Also includes the use of spring snow melt as a potential water source
source ('Iso_MixingModel.R')


library(readxl)
library(writexl)
library (openxlsx)



#Defining Colors
TreatmentC_Col <- '#8EA604'    #Citron - greens
TreatmentC_S5_Col <- 'green'
TreatmentC_S35_Col <- '#1F2F16'

Treatment1_Col <- '#571F4E'
Treatment1_S5_Col <- '#C6878F'
Treatment1_S35_Col <- '#5E0035'       #Purples
  
Treatment2_Col <- '#218380'    #Teal
Treatment2_S5_Col <- '#C2E7D9'
Treatment2_S35_Col <- '#2F3061'

Mature_Col <- '#BF3100'         #Rust -red
Precip_Col <- '#1B2CC1'
Temp_Col <- '#C1B2AB'
GW_Col <- '#523F38'
Stream_Col <- '#23F0C7'
Snow_Col <- '#7692FF'


Col_5 <- '#EDB458'

Col_35 <- '#37F7AD'

Col_Deep <- '#2F3061'




##############################
### Meteorological Figures ###
##############################

### Plotting Site Meteorological Data

#Misc, old, and unedited
##########################
#Temp C
par(mfrow=c(1,1),mar=c(4,5,1,2))
plot (Met_B1T1$POSIX_Date, 
      Met_B1T1$Temp_C,
      xlab = '',
      ylab = 'Temperature (C)',
      type = 'l', col = B1T1Col)
lines (Met_B1T2$POSIX_Date, 
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
plot (Met_B1T1$POSIX_Date [Met_B1T1$POSIX_Date >= '2021-06-03 09:21:00 PDT'], 
      Met_B1T1$rH [Met_B1T1$POSIX_Date >= '2021-06-03 09:21:00 PDT'],
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
      type = 'l', lwd = 2, col = TreatmentC_Col)
##########################

#Precipitation and Soil Moisture


### Plot historical precipition data mean + 2021 precipitaition 
# #monthly_mean_precip$dataset <- 'Historical'
# names (monthly_mean_precip) = c('month','precip','dataset')
# B1T2_monthly_precip$dataset <- '2021'
# names (B1T2_monthly_precip) = c('month', 'precip','dataset')
# d <- rbind(monthly_mean_precip, B1T2_monthly_precip)
# Historical_precip <- ggplot(d, aes( month, precip, fill = dataset)) + geom_bar(stat = 'identity', position = "dodge", na.rm=TRUE)+
#   theme_bw()+
#   scale_fill_manual(values = c(GW_Col, Precip_Col))
# Historical_precip


# Daily precipitation 2021
PrecipDailyBarA <- ggplot(data = Met_B1T2, aes(Date, Precip_mm)) +
  geom_bar(stat="identity", na.rm = TRUE, color = Precip_Col, fill = Precip_Col) +
  xlab("Date") + ylab("Precipitation (mm)") +
  theme_bw() +
  theme(
    axis.title.y = element_text(color = 'black', size=13),
    axis.title.y.right = element_text(color = 'blue', size=13))

PrecipDailyBarA

Precip_andhist  <- ggplot ()+ 
  geom_bar (data = hist_mean_dailybyyr, aes (x = Date, y = ppt), stat = 'identity', color = GW_Col, linewidth = 0.5) +
  geom_bar(data = Met_B1T2, aes(Date, Precip_mm),stat="identity", na.rm = TRUE, color = Precip_Col, fill = Precip_Col, linewidth = 0.75) +
  theme_bw() +
  ylab('Precipitation (mm)')+
  theme(
    axis.title.y = element_text(color = 'black', size=13),
    axis.title.y.right = element_text(color = 'blue', size=13))
Precip_andhist


hist_mean_dailybyyr$Date <- as.Date (as.numeric(hist_mean_dailybyyr$JD),
                                     origin = as.Date('2021-01-01'))
hist_mean_dailybyyr$Week <- strftime(hist_mean_dailybyyr$Date, format = "%V") 
Met_B1T2$Week <- strftime(Met_B1T2$Date, format = "%V") 






Precip_andhist_weekly  <- ggplot ()+ 
  geom_bar (data = hist_weekly_precip, aes (x = Week, y = ppt), stat = 'identity', color = GW_Col, size = 0.5,
            position = position_dodge2(preserve = "single")) +
  geom_bar(data = B1T2_Precip_Weekly, aes(Week, mean_Weekly_Precip_mm),stat="identity", na.rm = TRUE, color = Precip_Col, fill = Precip_Col, size = 0.75, 
           position = position_dodge2(preserve = "single"), alpha = 0.75) +
  theme_bw() +
  ylab('Precipitation (mm)')+
  theme(
    axis.title.y = element_text(color = 'black', size=13),
    axis.title.y.right = element_text(color = 'blue', size=13))
Precip_andhist_weekly




Temp_Precip <- ggplot ()+ 
  geom_line (data = Met_B1T2, aes (x = Date, y = Temp_C), color = Temp_Col, size = 0.75) +
  geom_bar(data = B1T2_Tmean, aes(Date, Precip_mm),stat="identity", na.rm = TRUE, color = Precip_Col) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Temperature (Celsius °)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.-15, name="Precipitation (mm/d)"),
  ) + 
  theme_bw() +
  theme(
    axis.title.y = element_text(color = 'black', size=13),
    axis.title.y.right = element_text(color = 'blue', size=13))
Temp_Precip

Temp <- ggplot ()+ 
  geom_line (data = Met_B1T2, aes (x = Date, y = Temp_C), color = Temp_Col, size = 0.75) +
  theme_bw() +
  ylab ('Temperature (°C)')+
  theme(
    axis.title.y = element_text(color = 'black', size=13),
    axis.title.y.right = element_text(color = 'blue', size=13))
Temp



rH <- ggplot (data = Met_B1T2)+
  geom_line (data = Met_B1T2, aes (x = Date, y = rH), color = Mature_Col, size = 0.6) +
  theme_bw() +
  ylab ('Relative Humidity (%)')+
  theme(
    axis.title.y = element_text(color = 'black', size=13),
    axis.title.y.right = element_text(color = 'blue', size=13))
rH
VPD <- ggplot (data = Met_B1T2)+
  geom_line (data = Met_B1T2, aes (x = Date, y = VPD), color = Treatment1_Col, size = 0.6)+
  theme_bw()+
  ylab ('VPD (kPa)')
  
MetFig <- Temp/ (rH | VPD) 
MetFig


### Summary Meteorological Figure including:
#Temperature (C), Precipitation (mm/d), PET (mm/d), and continuous soil moisture at 

p1 <- ggplot ()+ 
  geom_line (data = Met_B1T2, aes (x = Date, y = Temp_C), color = Temp_Col, size = 0.75) +
  geom_bar(data = B1T2_Tmean, aes(Date, Precip_mm),stat="identity", na.rm = TRUE, color = Precip_Col) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Temperature (Celsius °)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1, name="Precipitation (mm/d)"),
  ) + 
  scale_x_date(limits = as.Date(c("2021-07-22","2021-10-08")))+
  theme_bw() +
  theme(
    axis.title.y = element_text(color = 'black', size=13),
    axis.title.y.right = element_text(color = 'blue', size=13))
p1
#P1 does not have the correct secondary y axis scales to show precipitation events

p3 <- ggplot (Soil_Moisture_2021)+
  geom_line(aes(x=Date,y=B1T1_S5_Avg), color = Treatment1_S5_Col, lty = 1)+
  geom_line(aes(x=Date,y=B1T1_S35_Avg), color = Treatment1_S35_Col, lty =2) +
  theme_bw()+
  ylab ('Soil Moisture Content (mm^3 / mm') + labs (title = 'B1T1 Average')

p3

p4 <- ggplot (Soil_Moisture_2021)+
  geom_line(aes(x=Date,y=B1T2_S5_Avg), color = Treatment2_S5_Col, lty = 1)+
  geom_line(aes(x=Date,y=B1T2_S35_Avg), color = Treatment2_S35_Col, lty =2) +
  theme_bw() +
  ylab ('') + labs (title = 'B1T2 Average')

p4

p2 <- ggplot (Soil_Moisture_2021)+
  geom_line(aes(x=Date,y=B1C_S5_Avg), color = TreatmentC_S5_Col, lty = 1)+
  geom_line(aes(x=Date,y=B1C_S35_Avg), color = TreatmentC_S35_Col, lty =2) +
  theme_bw()+
  ylab ('') + labs (title = 'B1C Average')
p2

####
Soil_Moisture_2021$JD <- format (Soil_Moisture_2021$Date, '%j')

Met_B1T2$JD <- format (Met_B1T2$Date, '%j')
Met_subset <- subset (Met_B1T2, Met_B1T2$JD >= 203 & Met_B1T2$JD <=281 )


PrecipDailyBarB <- ggplot(data = Met_subset, aes(Date, Precip_mm)) +
  geom_bar(stat="identity", na.rm = TRUE, color = Precip_Col, fill = Precip_Col) +
  xlab("Date") + ylab("Precipitation (mm/d)") +
  theme_bw() +
  theme(
    axis.title.y = element_text(color = 'black', size=13),
    axis.title.y.right = element_text(color = 'blue', size=13))
###
p2 + p3 + p4+ PrecipDailyBarB + plot_layout(ncol = 1)



# Potential Evapotranspiration Figure including 4 models
PET_plot <- ggplot (data = PET) +
  geom_line (aes(x=Date,y=PET_HS, color = 'Hargreaves'), linewidth  = 0.75)+
  geom_line (aes(x=Date,y=PET_HA, color = 'Hamon'), size  = 0.75)+
#  geom_line (aes(x=Date,y=PET_TH, color = 'Thornthwaite'), size  = 0.75)+
  geom_line (aes(x=Date,y=PET_PT, color = 'Priestley-Taylor'), size  = 0.75)+
  scale_color_manual(name = 'Method', values = c('Hargreaves'= Treatment1_Col,
                                                 'Hamon'= Treatment2_Col,
                                                 'Thornthwaite'= TreatmentC_Col,
                                                 'Priestley-Taylor'= Mature_Col))+
  theme_bw()+
  theme (legend.position = 'top')+ labs (x= 'Julian Day', y = 'Potential Evapotranspiration (mm/d)')
PET_plot

#Plot historical dryness v 2021 dryness
#Historical
#SPI
mean_spi3 <- aggregate(spi3~month, data = Drynesshist,
                                     FUN = mean)
SD_spi3 <- aggregate(spi3~month, data = Drynesshist,
                       FUN = sd)
mean_spi3_hist <- cbind (mean_spi3, SD_spi3$spi3)
colnames (mean_spi3_hist) =c ('month','spi3','spi3_SD')
mean_spi3_hist$year <- 'hist'
#SPEI
mean_spei3 <- aggregate(spei3_TH~month, data = Drynesshist,
                       FUN = mean)
SD_spei3 <- aggregate(spei3_TH~month, data = Drynesshist,
                       FUN = sd)
mean_spei3_TH_hist <- cbind (mean_spei3, SD_spei3$spei3_TH)
colnames (mean_spei3_TH_hist) =c ('month','spei3','spei3_SD')

#2021
#SPI
mean_spi3 <- aggregate(spi3~month, data = Dryness2021,
                       FUN = mean)
SD_spi3 <- aggregate(spi3~month, data = Dryness2021,
                     FUN = sd)
mean_spi3_2021 <- cbind (mean_spi3, SD_spi3$spi3)
colnames (mean_spi3_2021) =c ('month','spi3','spi3_SD')
mean_spi3_2021$year <- '2021'
#SPEI
mean_spei3 <- aggregate(spei3_TH~month, data = Dryness2021,
                        FUN = mean)
SD_spei3 <- aggregate(spei3_TH~month, data = Dryness2021,
                      FUN = sd)
mean_spei3_TH_2021 <- cbind (mean_spei3, SD_spei3$spei3_TH)
colnames (mean_spei3_TH_2021) =c ('month','spei3','spei3_SD')

mean_spei3_TH_hist$year <- 'hist'
mean_spei3_TH_2021$year <- '2021'
spei_means <- rbind (mean_spei3_TH_hist, mean_spei3_TH_2021)
spi_mean <- rbind (mean_spi3_hist, mean_spi3_2021)

pd<-position_dodge(.9)

SPI_plot <- ggplot(data = spi_mean, aes(x = month, y = spi3, group = year,
                                              colour = year, shape = year)) +
  geom_point(size = 4, position = pd) +
  geom_line(position = pd) +
  geom_errorbar(aes(ymin = spi3 - spi3_SD, ymax = spi3 + spi3_SD), colour = "black",
                position = pd, width = 0.1) +
  scale_colour_manual(values = c(Precip_Col, Treatment2_Col)) +
  scale_shape_manual(values = c(19, 17))+
  theme_bw() + labs (x = 'Month', y = 'SPI')+
  theme(legend.position = 'none')



SPEI_plot <- ggplot(data = spei_means, aes(x = month, y = spei3, group = year,
                          colour = year, shape = year)) +
  geom_point(size = 4, position = pd) +
  geom_line(position = pd) +
  geom_errorbar(aes(ymin = spei3 - spei3_SD, ymax = spei3 + spei3_SD), colour = "black",
                position = pd, width = 0.1) +
  scale_colour_manual(values = c(Precip_Col, Treatment2_Col),
                        name  ="Year",
                        breaks=c("2021", "hist"),
                        labels=c("2021", "1997-2008")) +
  scale_shape_discrete(name  ="Year",
                       breaks=c("2021", "hist"),
                       labels=c("2021", "1997-2008"))+
theme_bw() + labs (x = 'Month', y = 'SPEI')+
  theme(legend.position = c(1,1), legend.background = element_rect(fill='transparent'),legend.justification = c(1.2,1.1))

#Plot Dryness
  #Historical
  mean_di <- aggregate(PET_TH.P~month, data = Drynesshist,
                         FUN = mean)
  SD_di <- aggregate(PET_TH.P~month, data = Drynesshist,
                       FUN = sd)
  mean_di_hist <- cbind (mean_di, SD_di$PET_TH.P)
  colnames (mean_spi3_hist) =c ('month','di','di_SD')
  mean_di_hist$year <- 'hist'
  
  
  #2021
  mean_di <- aggregate(PET_TH.P~month, data = Dryness2021,
                       FUN = mean)
  SD_di <- aggregate(PET_TH.P~month, data = Dryness2021,
                     FUN = sd)
  mean_di_2021 <- cbind (mean_di, SD_di$PET_TH.P)
  colnames (mean_spi3_hist) =c ('month','di','di_SD')
  mean_di_2021$year <- '2021'
  
  di_mean <- rbind (mean_di_hist, mean_di_2021)
  colnames (di_mean) = c('month','di','di_SD','year')
  pd<-position_dodge(.9)
  
  di_plot <- ggplot(data = di_mean, aes(x = month, y = di, group = year,
                                          colour = year, shape = year)) +
    geom_point(size = 4, position = pd) +
    geom_line(position = pd) +
    geom_errorbar(aes(ymin = di - di_SD, ymax = di + di_SD), colour = "black",
                  position = pd, width = 0.1) +
    scale_colour_manual(values = c(Precip_Col, Treatment2_Col)) +
    scale_shape_manual(values = c(19, 17))+
    theme_bw() + labs (x = 'Month', y = 'Dryness Index (PET/P)')+
    theme(legend.position = 'none')
  

di_plot + SPI_plot + SPEI_plot+plot_layout(ncol = 3)
  
  
  

# Stich  Dryness (PET/P), SPI, and SPEI together
DrynessData$Har_PET
Dryness2021
Drynesshist
plot (Dryness2021$month, Dryness2021$PET_TH.P, type = 'l', ylim = c(-3,10))
points (Dryness2021$month, Dryness2021$spi3)
points (Dryness2021$month, Dryness2021$spei3_TH, col = 'red')


####################################
### DBH & Annual growth Tables ###
###################################

#One Way Anovas
June_2019_dbh_ANOVA
Oct_2019_dbh_ANOVA
Jul_2020_dbh_ANOVA
Oct_2020_dbh_ANOVA
June_2021_dbh_ANOVA
Oct_2021_dbh_ANOVA
June_2022_dbh_ANOVA
Oct_2022_dbh_ANOVA

#Growing season ANOVA
summary (GS_19_dbh_ANOVA)
GS_20_dbh_ANOVA
GS_21_dbh_ANOVA
summary (GS_22_dbh_ANOVA)

#Pairwise 
GS19.pwc.dbh.dif
GS20.pwc.dbh.dif
GS21.pwc.dbh.dif
GS22.pwc.dbh.dif

library (table1)

table1::label(Total_DBH_plus$June_2019) <- "Spring 2019"
table1::label(Total_DBH_plus$Oct_2019) <- "Fall 2019"
table1::label(Total_DBH_plus$GS_19) <- "2019 Change over Growing Season"
table1::label(Total_DBH_plus$Jul_2020) <- "Spring 2020*"
table1::label(Total_DBH_plus$Oct_2020) <- 'Fall 2020'
table1::label(Total_DBH_plus$GS_20) <- "2020 Change over Growing Season"
table1::label(Total_DBH_plus$June_2021) <- "Spring 2021"
table1::label(Total_DBH_plus$Oct_2021) <- 'Fall 2021'
table1::label(Total_DBH_plus$GS_21) <- "2021 Change over Growing Season"

DBH_summary_table <- table1::table1(~June_2019 + Oct_2019 + GS_19 +
                                   Jul_2020 + Oct_2020 + GS_20 +
                                   June_2021 + Oct_2021 + GS_21 + June_6_2022 + Oct_13_2022 + GS_22| Treatment, data = Total_DBH_plus)
dataset_names <- list('Sheet1' = DBH_summary_table)
write.xlsx(dataset_names, file = 'DBH summary table 2022.xlsx')


####################################
### DBH & Annual growth Figures ###
###################################

#Plot boxplot of 2019 Growing Season with ANOVA
GS_19_dbh_bxpl <- ggboxplot(Total_DBH_plus_noMature, x = "Treatment", y = 'GS_19', color = 'black',
                            fill = 'Treatment', palette = c(Treatment1_Col, Treatment2_Col, TreatmentC_Col),
                            add = 'jitter')+
  ylab ('DBH (mm)') + scale_x_discrete(limits=c('1','2','C')) + theme(legend.position = 'none')

GS_19_dbh_bxpl

GS19.pwc.dbh.dif.nomatures <- Total_DBH_plus_noMature %>%
  pairwise_t_test(
    GS_19 ~ Treatment,
    p.adjust.method = "bonferroni"
  )
GS19.pwc.dbh.dif.nomatures

# Visualization: box plots with p-values - d18O by date collected
pwc.dbh.dif <- GS19.pwc.dbh.dif.nomatures %>% add_xy_position(x = "Treatment")
GS_19_bxpl_aov <- GS_19_dbh_bxpl + 
  stat_pvalue_manual(pwc.dbh.dif) +
  labs(
    subtitle = get_test_label(GS19.res.aov.dbh, detailed = TRUE),
    caption = get_pwc_label(pwc.dbh.dif)
  )

GS_19_bxpl_aov
###

#Plot boxplot of 2020 Growing Season with ANOVA
GS_20_dbh_bxpl <- ggboxplot(Total_DBH_plus_noMature, x = "Treatment", y = 'GS_20', color = 'black',
                            fill = 'Treatment', palette = c(Treatment1_Col, Treatment2_Col, TreatmentC_Col),
                            add = 'jitter')+
  ylab ('DBH (mm)') + scale_x_discrete(limits=c('1','2','C')) + theme(legend.position = 'none')

GS_20_dbh_bxpl

GS20.pwc.dbh.dif.nomatures <- Total_DBH_plus_noMature %>%
  pairwise_t_test(
    GS_20 ~ Treatment,
    p.adjust.method = "bonferroni"
  )
GS20.pwc.dbh.dif.nomatures

# Visualization: box plots with p-values - d18O by date collected
pwc.dbh.dif <- GS20.pwc.dbh.dif.nomatures %>% add_xy_position(x = "Treatment")
GS_20_dbh_bxpl_aov <- GS_20_dbh_bxpl + 
  stat_pvalue_manual(pwc.dbh.dif) +
  labs(
    subtitle = get_test_label(GS20.res.aov.dbh, detailed = TRUE),
    caption = get_pwc_label(pwc.dbh.dif)
  )

GS_20_dbh_bxpl_aov
###

#Plot boxplot of 2021 Growing Season with ANOVA
GS_21_dbh_bxpl <- ggboxplot(Total_DBH_plus_noMature, x = "Treatment", y = 'GS_21', color = 'black',
                            fill = 'Treatment', palette = c(Treatment1_Col, Treatment2_Col, TreatmentC_Col),
                            add = 'jitter')+
  ylab ('DBH (mm)') + scale_x_discrete(limits=c('1','2','C')) + theme(legend.position = 'none')

GS_21_dbh_bxpl

GS21.pwc.dbh.dif.nomatures <- Total_DBH_plus_noMature %>%
  pairwise_t_test(
    GS_21 ~ Treatment,
    p.adjust.method = "bonferroni"
  )
GS21.pwc.dbh.dif.nomatures

#Plot boxplot of 2021 Growing Season with ANOVA
GS_22_dbh_bxpl <- ggboxplot(Total_DBH_plus, x = "Treatment", y = 'Oct_13_2022', color = 'black',
                            fill = 'Treatment', palette = c(Treatment1_Col, Treatment2_Col, TreatmentC_Col, Mature_Col),
                            add = 'jitter')+
  ylab ('DBH (mm)') + scale_x_discrete(limits=c('1','2','C','M')) + theme(legend.position = 'none')

GS_22_dbh_bxpl





# Visualization: box plots with p-values - d18O by date collected
pwc.dbh.dif <- GS22.pwc.dbh.dif.nomatures %>% add_xy_position(x = "Treatment")
GS_22_dbh_bxpl_aov <- GS_22_dbh_bxpl + 
  stat_pvalue_manual(pwc.dbh.dif) +
  labs(
    subtitle = get_test_label(GS21.res.aov.dbh, detailed = TRUE),
    caption = get_pwc_label(pwc.dbh.dif)
  )

GS_21_dbh_bxpl_aov 
###

#Patchwork the three together
GS_19_bxpl_aov + GS_20_dbh_bxpl_aov + GS_21_dbh_bxpl_aov+plot_layout(ncol = 3)






###############################
### Soil Moisture Tables #####
###############################

SWC_summary_table
  #Summary table is similar if not the same as SWC_tdm_summary
SWC_mean_table
#write.csv(SWC_summary_table, "R_Exports/SWC Summary Table.csv")
#write.csv(SWC_mean_table, "R_Exports/SWC Mean Table.csv")

SM_cont_tdm_summary
#OR 
Soilmoisture.monthly.means
#% WC
SWC_tdm_summary

B1C_depth_ratio <- lm (Soil_Moisture_2021$B1C_S35_Avg~Soil_Moisture_2021$B1C_S5_Avg)
# slope 0.59; int 0.048
B1T1_depth_ratio <- lm (Soil_Moisture_2021$B1T1_S35_Avg~Soil_Moisture_2021$B1T1_S5_Avg)
# slope 0.84; int 0.027
B1T2_depth_ratio <- lm (Soil_Moisture_2021$B1T2_S35_Avg~Soil_Moisture_2021$B1T2_S5_Avg)
# slope 0.53; int 0.015

# ANOVAS
  #3 Way anova of % water content
WC_AOV_depthtreatmentmonth<- summary (AOV_depth_treatment_soilmoisture)
  #3 Way anova of continious SM
SM_dtm_aov

soil.535.pwc.OvSMOW
soil.535.pwc.HvSMOW

#Soil depth summary
sum_soil_iso_depth_profile
depthmeans
range_means
get_anova_table(res.aov.depth.OvSMOW)
get_anova_table(res.aov.depth.HvSMOW)

pwc.depth.OvSMOW
pwc.depth.HvSMOW



###############################
### Soil Moisture Figures #####
###############################

##### CONTINIOUS SOIL MOISTURE PLOT BY Treatment
B1C_SMC <- ggplot (Soil_Moisture_2021)+
  geom_line(aes(x=Date,y=B1C_S5_Avg), color = TreatmentC_S5_Col, lty = 1)+
  geom_line(aes(x=Date,y=B1C_S35_Avg), color = TreatmentC_S35_Col, lty =2) +
  theme_bw() + ylim(c(0,0.32)) + ylab ('') + xlab ('') + ggtitle('B1C')

B1C_SMC

B1T1_SMC <- ggplot (Soil_Moisture_2021)+
  geom_line(aes(x=Date,y=B1T1_S5_Avg), color = Treatment1_S5_Col, lty = 1)+
  geom_line(aes(x=Date,y=B1T1_S35_Avg), color = Treatment1_S35_Col, lty =2) +
  theme_bw()+ ylim(c(0,0.32)) +
  ylab (expression(paste("Soil Moisture Content ( ", m^{3},m^{-3},")"))) +
  xlab ('') + ggtitle ('B1T1')

B1T1_SMC

B1T2_SMC <- ggplot (Soil_Moisture_2021)+
  geom_line(aes(x=Date,y=B1T2_S5_Avg), color = Treatment2_S5_Col, lty = 1)+
  geom_line(aes(x=Date,y=B1T2_S35_Avg), color = Treatment2_S35_Col, lty =2) +
  theme(legend.position = 'bottom') +
  theme_bw()+ ylim(c(0,0.32)) + ylab ('') + ggtitle ('B1T2')
B1T2_SMC



B1C_SMC + B1T1_SMC + B1T2_SMC + PrecipDailyBarA + plot_layout(ncol = 1)
###

##### CONTINIOUS SOIL MOISTURE PLOT BY Depth
Depth5_SMC <- ggplot (Soil_Moisture_2021)+
  geom_line(aes(x=Date,y=B1C_S5_Avg), color = TreatmentC_Col, lty = 1, size = 1)+
  geom_line(aes(x=Date,y=B1T1_S5_Avg), color = Treatment1_Col, lty =1, size = 1) +
  geom_line(aes(x=Date,y=B1T2_S5_Avg), color = Treatment2_Col, lty =1, size = 1) +
  theme_bw() + ylim(c(0,0.32)) + labs (y = expression(paste("(",m^{3},m^{-3},")")), x = '', title = 'Depth 5cm')

Depth5_SMC

Depth35_SMC <- ggplot (Soil_Moisture_2021)+
  geom_line(aes(x=Date,y=B1C_S35_Avg), color = TreatmentC_Col, lty = 1, size =1)+
  geom_line(aes(x=Date,y=B1T1_S35_Avg), color = Treatment1_Col, lty =1, size= 1) +
  geom_line(aes(x=Date,y=B1T2_S35_Avg), color = Treatment2_Col, lty = 1, size =1)+
  theme (legend.position = 'top')+
  theme_bw()+ ylim(c(0,0.32)) + labs (title = 'Depth 35 cm', y = 'Soil Moisture Content', x = 'Date')

Depth35_SMC

difdepth <- ggplot (Soil_Moisture_2021, aes (x = POSIXdate, y = ))+
  geom_line(aes(x=Date,y=B1C_S35_Avg - B1C_S5_Avg), color = TreatmentC_Col, lty = 1, size =1)+
  geom_line(aes(x=Date,y=B1T1_S35_Avg - B1T1_S5_Avg), color = Treatment1_Col, lty =1, size= 1) +
  geom_line(aes(x=Date,y=B1T2_S35_Avg - B1T2_S5_Avg), color = Treatment2_Col, lty = 1, size =1)+
  theme_bw()+ ylim(c(-0.2,0.2)) + labs (title = 'Change in Moisture (35cm-5cm)', y = expression(paste(Delta,'Soil Moisture Content')), x = 'Date')
difdepth

#Note this plot is missing a legend
Depth5_SMC + Depth35_SMC +difdepth+plot_layout(ncol = 1)
###



#Misc old and uneditted figures
###############################
## Plotting Soil Moisture Data
# ###Plotting soil moisture by sensor
par(mfrow=c(1,1),mar=c(5,5,1,1))
plot (Soil_Moisture_2021$POSIXdate, 
      Soil_Moisture_2021$B1C_S5_Avg,
      ylim = c(0,0.4),
      xlab = 'Date',
      ylab = 'Soil Water Content (mm3/mm)',
      type = 'l', col = B1Ccol)
lines (Soil_Moisture_2021$POSIXdate, Soil_Moisture_2021$B1C_S35_Avg,
       lty = 2,
       col = B1Ccol)
lines (Soil_Moisture_2021$POSIXdate, Soil_Moisture_2021$B1T1_S5_Avg,
       col = B1T1Col)
lines (Soil_Moisture_2021$POSIXdate, Soil_Moisture_2021$B1T1_S35_Avg,
       col = B1T1Col,
       lty = 2)
lines (Soil_Moisture_2021$POSIXdate, Soil_Moisture_2021$B1T2_S5_Avg,
       col = B1T2Col)
lines (Soil_Moisture_2021$POSIXdate, Soil_Moisture_2021$B1T2_S35_Avg,
       col = B1T2Col,
       lty = 2)
legend ("topleft", legend = c("B1C @ 5 cm", "B1C @ 35 cm", 
                              "B1T1 @ 5 cm", "B1T1 @ 35 cm",
                              "B1T2 @ 5 cm", "B1T2 @ 35 cm"),
        col = c(B1Ccol, B1Ccol, B1T1Col, B1T1Col, B1T2Col, B1T2Col),
        lty = 1:2, cex = 0.65)

###############################
#This does not work but may have helpful bits of code for formatting figures with error bars
  #% SWC with depth by treatment

  # The errorbars overlapped, so use position_dodge to move them horizontally
  pd <- position_dodge(0.1) # move them .05 to the left and right

p1 <- ggplot(mean_SWC_profile_, aes(x=Depth, y=WC, colour=Treatment)) + 
  geom_errorbar(aes(ymin=WC-sd, ymax=WC+sd), width=.1,) +
  geom_line() +
  geom_point()+
  scale_color_manual(values = c(Treatment1_Col, Treatment2_Col, TreatmentC_Col)) + scale_x_reverse()+
coord_flip()
p1
  #d18O with depth by treatment
p2 <- ggplot(mean_iso_profile_OvSMOW, aes(x=Depth, y=OvSMOW, colour=Treatment)) + 
  geom_errorbar(aes(ymin=OvSMOW-sd, ymax=OvSMOW+sd), width=.1,) +
  geom_line() +
  geom_point()+
  scale_color_manual(values = c(Treatment1_Col, Treatment2_Col, TreatmentC_Col))+ scale_x_reverse()+
  coord_flip()
p2



########################
### Isotope Tables ####
########################

#Branch Tables
#Treatment, Block, Month d18O and d2H mean sd and se
branch.data.sum

#Summaries of used datasets before outliers were removed
summary (soil_iso)
summary (met_iso)
summary (branch_iso)
summary (soil_depth_profile_gw)


  #Meteoroligcal means and soil moisture from 9-10 day collected September precipitation
mean_Met_910
mean_SM_910


#Branch summary dataset after outliers were removed
branch.data.sum_NO
#d2H and d18O from branch iso with modified matures by block,treatment, and month
branch_iso_MM_summary

get_anova_table(res.aov.branch.OvSMOW)
pwc.branch.OvSMOW
get_anova_table(res.aov.branch.HvSMOW)
pwc.branch.HvSMOW

#Two way ANOVA
get_anova_table(res.aov.branch.OvSMOW.MM2)
#Under MM, the two way analysis of variance shows more significant influence
get_anova_table(res.aov.branch.HvSMOW.MM2)


#Three way ANOVA
get_anova_table(res.aov.branch.OvSMOW.noMature3)
#Monthly influence is even higher
get_anova_table(res.aov.branch.HvSMOW.noMature3)

get_anova_table(res.aov.branch.HvSMOW.MM2)
get_anova_table(res.aov.branch.HvSMOW.MM2_block)
get_anova_table(res.aov.branch.HvSMOW.noMature3)

#Grouped by treatment and block tested for variance by month
time.effect.HvSMOW
time.effect.OvSMOW

branch_MM_HvSMOW_pwc
branch_MM_OvSMOW_pwc
#write.csv(branch_MM_HvSMOW_pwc, 'Branch HvSMOW PWC.csv')
#write.csv(branch_MM_OvSMOW_pwc, 'Branch OvSMOW PWC.csv')

#Soil
#Depth, Treatment, Month d18O and d2H mean, sd, and se
soil.data.sum
#write.csv(soil.data.sum, 'soil.data.sum.csv')
#Anova of soil d18O by depth and treatment 
summary (AOV_depth_treatment_OvSMOW)
summary (AOV_depth_treatment_HvSMOW)
#both depth and month are significant

#Three way repeated measured anova
get_anova_table(res.aov.soil.OvSMOW3)


#Two way by time
two.way.soil.td
two.way.soil.BT

time.effect.HvSMOW.soil

#Pairwise test 
month.pwc.soil_iso.HvSMOW
month.pwc.soil_iso.OvSMOW

#Two way anova by block and month
get_anova_table(res.aov.soil.HvSMOW.2)
get_anova_table(res.aov.soil.OvSMOW.2)


#Three way anova of soil
AOV.soil.3var.HvSMOW <- get_anova_table(res.aov.soil.HvSMOW.top3)
AOV.soil.3var.OvSMOW <- get_anova_table(res.aov.soil.OvSMOW.top3)


#pair wise test by month
pwc.soil.HvSMOW
pwc.soil.OvSMOW

pwc_HvSMOW_Soil


#Three way repeated measures anova and Post-HOC tests
#ANOVAS
get_anova_table(res.aov.soil.OvSMOW3)
get_anova_table(res.aov.soil.HvSMOW3)
#OR
soil_iso_535%>%anova_test(OvSMOW~Depth*Month*Treatment)
soil_iso_535%>%anova_test(HvSMOW~Depth*Month*Treatment)



#Two way interaction 
three.way.soil.OvSMOW
three.way.soil.HvSMOW

#Two-way interactions grouped by treatment and tested by month and
soil.aov.OvSMOW.twoway.dm
soil.aov.HvSMOW.twoway.dm

TukeyHSD (soil.aov.HvSMOW.twoway.dm)

lm.soil.OvSMOW
lm.soil.HvSMOW

#Simple simple main effect
treatment.effect.OvSMOW
treatment.effect.HvSMOW
#No significance of treatment

#Simple Simple comparison
#PWC of Depth by Treatment and Month
sscomp.pwc.depth.OvSMOW.soil
sscomp.pwc.depth.HvSMOW.soil

sscomp.pwc.OvSMOW.soil%>% filter (Treatment == '1', Month == '06')

#PWC of Month by Treatment and Depth
sscomp.pwc.month.OvSMOW.soil
sscomp.pwc.month.HvSMOW.soil


#AOV model
model.aov.OvSMOW
model.aov.HvSMOW

#Standard PWC by month
soil.535.pwc.OvSMOW
soil.535.pwc.HvSMOW

#Fit Tukey two.way to Month and Depth

# Visualization: box plots with p-values
#d18O
sscomp.pwc.month.OvSMOW.soil <- sscomp.pwc.month.OvSMOW.soil %>% add_xy_position(x = "Treatment")
pwc.filtered <- sscomp.pwc.depth.OvSMOW.soil %>% filter(Treatment == "2", Depth == "5")
bxp.3way.soils.OvSMOW +
  stat_pvalue_manual(
    pwc.filtered, color = "Treatment", linetype = "Depth", hide.ns = TRUE,
    tip.length = 0, step.increase = 0.1, step.group.by = "Month"
  ) +
  labs(
    subtitle = get_test_label(three.way.soil.OvSMOW, detailed = TRUE),
    caption = get_pwc_label(sscomp.pwc.month.OvSMOW.soil)
  )


########
soil_sig_dataset <- list('Sheet1' = as.data.frame(soil.535.pwc.OvSMOW), 'Sheet2' = as.data.frame(soil.535.pwc.HvSMOW), 
                         'Sheet3' = as.data.frame(depthmeans),
                         'Sheet4' = as.data.frame(pwc.depth.HvSMOW), 'Sheet5' = as.data.frame(pwc.depth.OvSMOW),
                         'Sheet6' = as.data.frame(AOV.soil.3var.HvSMOW), 'Sheet7' = as.data.frame(AOV.soil.3var.OvSMOW))
#write.xlsx(soil_sig_dataset, file = 'soil_iso_SM_significance_tables.xlsx')


#Branch Isotope Summary Table from dataaset without outliers
table1::label(Treatment1$OvSMOW) <- 'Treatment 1 d18O'
table1::label(Treatment1$HvSMOW) <- 'Treatment 1 d2H'
T1_branch_iso_table <- table1::table1(~OvSMOW + HvSMOW | Month, data = Treatment1)

table1::label(Treatment2$OvSMOW) <- 'Treatment 2 d18O'
table1::label(Treatment2$HvSMOW) <- 'Treatment 2 d2H'
T2_branch_iso_table <- table1::table1(~OvSMOW + HvSMOW | Month, data = Treatment2)

table1::label(TreatmentC$OvSMOW) <- 'Control d18O'
table1::label(TreatmentC$HvSMOW) <- 'Control 1 d2H'
C_branch_iso_table <- table1::table1(~OvSMOW + HvSMOW | Month, data = TreatmentC)

dataset_names <- list('Sheet1' = T1_branch_iso_table, 'Sheet2' = T2_branch_iso_table, 'Sheet3' = C_branch_iso_table)
#write.xlsx(dataset_names, file = 'Branch Isotope Summary.xlsx')

#Soil water summary table by treatment and depths 5 and 35 over months
soil_iso_535


#Soil depth profile and GW by treatments over depths ( no seasonal componant)
soil_depth_profile_gw


### Unsorted
#SWC (%), d18O, and d2H with depth from 3 treatments on one collection day in July
#Mean and SD of d2H table
mean_iso_profile_HvSMOW <- aggregate (HvSMOW ~ Treatment + Depth,
                                      data = soil_depth_profile_gw, mean)
sd_iso_profile_HvSMOW <- aggregate (HvSMOW ~ Treatment + Depth,
                                    data = soil_depth_profile_gw, FUN = 'sd')
mean_iso_profile_HvSMOW$sd <- sd_iso_profile_HvSMOW$HvSMOW

#Mean and SD of d18O table
mean_iso_profile_OvSMOW <- aggregate (OvSMOW ~ Treatment + Depth,
                                      data = soil_depth_profile_gw, mean)
sd_iso_profile_OvSMOW <- aggregate (OvSMOW ~ Treatment + Depth,
                                    data = soil_depth_profile_gw, FUN = 'sd')
mean_iso_profile_OvSMOW$sd <- sd_iso_profile_OvSMOW$OvSMOW

#Mean and SD of Water content table
mean_SWC_profile_ <- aggregate (WC ~ Treatment + Depth,
                                data = SWC_depth_profile, mean)
sd_SWC_profile_ <- aggregate (WC ~ Treatment + Depth,
                              data = SWC_depth_profile, FUN = 'sd')
mean_SWC_profile_$sd <- sd_SWC_profile_$WC
###






#############################
### Soil Isotope Figures ####
#############################

bxp_OvSMOW_soil
bxp_HvSMOW_soil

#Only samples from depths 5 cm and 35 cm
bxp_OvSMOW_soil535
bxp_HvSMOW_soil535

soil.sum.biplot

## Unsorted
# Visualization: box plots with p-values
pwc_HvSMOW_Soil <- pwc_HvSMOW_Soil %>% add_xy_position(x = "Treatment")
pwc.Soil_HvSMOW.filtered <- pwc_HvSMOW_Soil %>% 
  filter(Depth == 5 | Depth == 35)
bxp_HvSMOW_soil535 + 
  stat_pvalue_manual(pwc_HvSMOW_Soil, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov.soil.HvSMOW3, detailed = TRUE),
    caption = get_pwc_label(pwc_HvSMOW_Soil)
  )

bxp_OvSMOW_soil535 <- ggboxplot (soil_iso_535, x = 'Month', y = 'OvSMOW',
                                 color = 'Treatment', palette = c(Treatment1_Col, Treatment2_Col, TreatmentC_Col),
                                 facet.by = 'Depth', short.panel.labs = FALSE
)+ labs (y = (expression(paste(delta^{18},"O"))), x = '') + theme(legend.position = 'none')
bxp_OvSMOW_soil535

bxp_HvSMOW_soil535 <- ggboxplot (soil_iso_535, x = 'Month', y = 'HvSMOW',
                                 color = 'Treatment', palette = c(Treatment1_Col, Treatment2_Col, TreatmentC_Col),
                                 facet.by = 'Depth', short.panel.labs = FALSE
) + labs (y = (expression(paste(delta^{2},"H")))) + theme(legend.position = 'bottom')
bxp_HvSMOW_soil535
bxp_OvSMOW_soil535 + bxp_HvSMOW_soil535 + plot_layout(ncol = 1)
  
bxp_OvSMOW_DMT <- ggboxplot(
    soil_iso_535, x = "Month", y = "OvSMOW",
    color = "Treatment", palette = "jco"
  ) + facet_grid(.~Depth) +theme_bw()
bxp_OvSMOW_DMT


bxp_OvSMOW_soil535 <- ggboxplot (soil_iso_535, x = 'Month', y = 'OvSMOW',
                                 color = 'Treatment', palette = c(Treatment1_Col, Treatment2_Col, TreatmentC_Col),
                                 facet.by = 'Depth', short.panel.labs = FALSE
)+ labs (y = (expression(paste(delta^{18},"O"))), x = '') + theme(legend.position = 'none')
bxp_OvSMOW_soil535





dried_soils_535$Month <- as.character(dried_soils_535$Month)
dried_soils_535$Month <- factor(dried_soils_535$Month , levels=c(6, 7, 8, 9, 10))


bxp_moisture_soil535 <- ggboxplot (dried_soils_535, x = 'Month', y = 'WC',
                                 color = 'Treatment', palette = c(Treatment1_Col, Treatment2_Col, TreatmentC_Col),
                                 short.panel.labs = TRUE)+ labs (y = 'Soil Water Content (%)', x = '') + theme(legend.position = 'bottom') + 
  facet_grid (col=vars(Depth))
bxp_moisture_soil535

###
dried_summary_table_$Depth <- as.character(dried_summary_table_$Depth)
dried_summary_table_ <- as.data.frame(dried_summary_table_)
dried_summary_table_$TreatmentDepth <- paste (dried_summary_table_$Treatment, dried_summary_table_$Depth)
p<-ggplot(dried_summary_table_, aes(x=Month, y=mean, color=Treatment, shape = Depth)) + 
  geom_point(position = position_dodge(0.3))+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0.3)) + theme_bw()+ labs (y = 'Soil Water Content (%)', x = 'Month')+
  scale_color_manual(values = c(Treatment1_Col, Treatment2_Col, TreatmentC_Col))

p
###


palette = c(Treatment1_S35_Col, Treatment1_S5_Col,
            Treatment2_S35_Col, Treatment2_S5_Col,
            TreatmentC_S35_Col, TreatmentC_S5_Col)

#### Depth profile figures
bxp_depth_OvSMOW
bxp_depth_HvSMOW

df_OvSMOW
df_HvSMOW

soil_depth_profile$WC_num <- as.numeric(sub("%", "",soil_depth_profile$WC,fixed=TRUE))/100
df_SM <-soil_depth_profile %>%
  group_by(Treatment, Depth) %>%
  get_summary_stats(c(WC_num), type = "mean_sd")
df_SM <- as.data.frame(df_SM)
profile_means<-soil_depth_profile%>%
  group_by(Treatment, Depth)%>%
  get_summary_stats(c(d18O,d2H), type = "mean_sd")
df_O18 <- subset (profile_means, profile_means$variable == 'd18O')
df_18O <- as.data.frame(df_O18)
df_2H <- subset (profile_means, profile_means$variable == 'd2H')
df_2H <- as.data.frame (df_2H)


### Soil depth profile of d18O
df_18O$Treatment [1] <- 2
depth_profile_OvSMOW <- ggplot (df_18O, aes (x = Depth, y = mean, color = Treatment))+
  geom_point()+
  geom_line(lwd = 1)+
  geom_errorbar(aes(ymin = mean-sd, ymax=mean+sd), width = 0.2,
                position=position_dodge(0.05))+
  scale_x_reverse()+
  #reverse depth so it starts at zero
  coord_flip() + theme_bw() + theme (legend.position = 'none') + 
  labs (x='Depth(cm)', y = (expression(paste(delta^{18},"O"))))+
  scale_color_manual(values = c(Treatment1_Col, Treatment2_Col, TreatmentC_Col))

depth_profile_OvSMOW


### Soil depth profile of d2H
df_2H$Treatment [1] <- 2
depth_profile_HvSMOW <- ggplot (df_2H, aes (x = Depth, y = mean, color = Treatment))+
  geom_point()+
  geom_line(lwd = 1)+
  geom_errorbar(aes(ymin = mean-sd, ymax=mean+sd), width = 0.2,
                position=position_dodge(0.05))+
  scale_x_reverse()+ 
  #reverse depth so it starts at zero
  coord_flip() + theme_bw() + theme (legend.position = 'none') + 
  labs (x=element_blank(), y = (expression(paste(delta^{2},"H"))))+
  scale_color_manual(values = c(Treatment1_Col, Treatment2_Col, TreatmentC_Col))

depth_profile_HvSMOW


### Soil depth profile of moisture
df_SM$Treatment [1] <- 2
soil_moisture_depthprof <-  ggplot (df_SM, aes (x = Depth, y = mean, color = Treatment))+
  geom_point()+
  geom_line(lwd = 1)+
  geom_errorbar(aes(ymin = mean-sd, ymax=mean+sd), width = 0.2,
                position=position_dodge(0.05))+
  scale_x_reverse()+
  #reverse depth so it starts at zero
  coord_flip() + theme_bw() + labs(y = 'Water content (%)', x = '')+
  scale_color_manual(values = c(Treatment1_Col, Treatment2_Col, TreatmentC_Col))

soil_moisture_depthprof


depth_profile_OvSMOW + depth_profile_HvSMOW + soil_moisture_depthprof +plot_layout(ncol = 3)








###############################
### Branch Isotope Figures ####
###############################

branch.sum.biplot

#d18O for 3 way ANOVA
bxp_OvSMOW3 <- ggboxplot(
  branch_iso_MM, x = "Month", y = "OvSMOW",
  color = "Treatment", palette = "jco"
) + facet_grid(.~Block) +theme_bw()
bxp_OvSMOW3

#Boxplot with anova of monthly variance and boxplots broken up by treatment
bxp_aov_pwc_OvSMOW
bxp_aov_pwc_HvSMOW

bxp_aov_pwc_HvSMOW + bxp_aov_pwc_OvSMOW + plot_layout(ncol = 2)

bxp_OvSMOW3
bxp_HvSMOW3
#OR
bxp_HvSMOW_BTM



bxp_OvSMOW_branch <- ggboxplot (branch_iso_NO, x = 'Month', y = 'OvSMOW',
                                color = 'Treatment', palette = c(Treatment1_Col, Treatment2_Col, TreatmentC_Col, Mature_Col),
                                short.panel.labs = FALSE
)+ labs (y = (expression(paste(delta^{18},"O"))), x = '') + theme(legend.position = 'none')
bxp_OvSMOW_branch

bxp_HvSMOW_branch <- ggboxplot (branch_iso_NO, x = 'Month', y = 'HvSMOW',
                                color = 'Treatment', palette = c(Treatment1_Col, Treatment2_Col, TreatmentC_Col, Mature_Col),
                                short.panel.labs = FALSE
) + labs (y = (expression(paste(delta^{2},"H")))) + theme(legend.position = 'right')
bxp_HvSMOW_branch

bxp_OvSMOW_branch +bxp_HvSMOW_branch + plot_layout(ncol = 2)



##########################################
### Biplots and misc. Isotope Figures ####
##########################################
library (viridis)
biplot_byDate
soil.sum.biplot
tree.sum.biplot

#Soil biplot by depth - build on soil.sum.biplot
ggplot(soil_iso, aes(OvSMOW, HvSMOW))+
  geom_point(aes(color = Depth)) +
  scale_color_viridis(option = "D")

soil_iso$Depth_chr <- as.character(soil_iso$Depth)

ggplot(soil_iso, aes(OvSMOW, HvSMOW))+
  geom_point(aes(color = Depth_chr)) +
  geom_smooth(aes(color = Depth_chr, fill = Treatment), method = "lm") + 
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) 


##############################
### MixSIAR Model Figures ####
##############################
setwd("C:/Users/12259/Desktop/MixSIAR Models/Scenario 6")

#Scenario 6
proportions.6 <- read.csv(file = 'scenario6proportions.csv')
proportions.6$Mean_100 <- proportions.6$Mean * 100
#Plotting proportions
#convert 'position' to factor and specify level order
proportions.6$Depth <- factor(proportions.6$Depth, levels=c('5', '35', 'Deep','GW'))


#convert 'position' to factor and specify level order
proportions.6$Depth <- factor(proportions.6$Depth, levels=c('5', '35', 'Deep','GW'))


prop_bar_noSD<-ggplot(proportions.6, aes(x = as.factor (Month), y = Mean_100, fill =as.factor(Depth))) + 
  geom_bar(stat = 'identity') + scale_fill_manual(values = c(Col_5,Col_Deep, Col_35, GW_Col))+
  labs (fill = 'Uptake Source', x = 'Month', y = 'Plant water uptake source (%)') + 
  theme_bw()+theme(legend.position = 'bottom', axis.text.x = element_text(angle = 90))+
  facet_grid (~Treatment)

prop_bar_noSD


##########################
### Print Data Tables ####
##########################





##GRoundwater Depth
ggplot (data = gs241)+
  geom_line (aes (x = POSIXdate, y = Weir_1_14015), color = Precip_Col, lwd = 1)+
  geom_line (aes(x = POSIXdate, y = Weir_2_14023), color = Treatment1_S5_Col, lwd = 1)+
  geom_line(aes(x = POSIXdate, y = Weir_3_14024), color = Treatment1_S35_Col, lwd = 1)+
  theme_bw()+labs (x = 'Date', y = 'Depth to groundwater (mm)')





  