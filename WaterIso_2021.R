# Branch and Soil water isotopic read in, analysis, and visualtization 
  # Also including soil moisture analysis + plots
### Read in and format CSV
iso_extract <- read.csv(file = "twig_extraction_water_cont.csv")
head(iso_extract)
colnames(iso_extract)=c('Sample_ID','Date','Extraction_Date','Distillation_Unit','Start_Time','Extraction_Time',
                        'tube_film','sample_tube_film','sample_weight(g)','driedsample_tube_film(g)','dried_weight(g)','water_loss(g)','water_cont')

iso_extract <- iso_extract [-c(14:18)]
head (iso_extract)
