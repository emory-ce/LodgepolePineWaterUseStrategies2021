getwd()

#Sapflow read in, formatting, calculations, analysis, and visualization
library('dplyr')
library('ggplot2')
library('tidyr')
library('lattice')

###Specifiy Colors###
B1Ccol <-'#011627'
B1T1Col <- '#82D173'
B1T2Col <- '#157A6E'
MCol <- '#721817'
SiteColors <- c(B1Ccol,B1T1Col,B1T2Col,MCol)

#Read in Data
Sap_2021 <- read.csv(file = "Sapflow_2021_Summary.csv")

head(Sap_2021)
TDP1 <- Sap_2021

