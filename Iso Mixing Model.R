
getwd()
source ('WaterIso_2021.R')


library (MixSIAR)
library (R2WinBUGS)
library (splancs)


#Load mixture consumer data
cons <- data.frame(branch_iso$HvSMOW, branch_iso$OvSMOW, branch_iso$Treatment, branch_iso$Month)
colnames (cons) = c('d2H', 'd18O', "Treatment",'Month')

# Load in source data 
n_length <- aggregate (HvSMOW~Range+Month+Treatment, data = soil_iso_gw,
                       function (x) length = length(x))
soil_mean_HvSMOW <- aggregate (HvSMOW~Range+Month+Treatment,
                                         data = soil_iso_gw,
                               function(x) c(mean = mean(x), sd = sd(x)))

soil_mean_OvSMOW <- aggregate (OvSMOW~Range+Month+Treatment,
                              data = soil_iso_gw,
                              function(x) c(mean=mean(x),sd=sd(x)))

source_df <- data.frame(cbind (soil_mean_HvSMOW$Range, soil_mean_HvSMOW$Treatment, soil_mean_HvSMOW$HvSMOW, soil_mean_OvSMOW$OvSMOW))
names (source_df) <- c("Depth",'Treatment','Meand2H','SDd2H','Meand18O','SDd18O')
source_df$n <- as.numeric(n_length$HvSMOW)
#Load in discrimination 

#tropical enrichment factors
n_length <- aggregate (HvSMOW~Range, data = soil_iso_gw,
                       function (x) length = length(x))
soil_mean_HvSMOW <- aggregate (HvSMOW~Range,
                               data = soil_iso_gw,
                               function(x) c(mean = mean(x), sd = sd(x)))

soil_mean_OvSMOW <- aggregate (OvSMOW~Range,
                               data = soil_iso_gw,
                               function(x) c(mean=mean(x),sd=sd(x)))

tef_df <- data.frame(cbind (soil_mean_HvSMOW$Range, soil_mean_HvSMOW$HvSMOW, soil_mean_OvSMOW$OvSMOW))
names (tef_df) <- c("Depth",'Meand2H','SDd2H','Meand18O','SDd18O')

tef_df

#write.csv(cons, "cons.csv")
write.csv(source_df, "sources.csv", row.names = FALSE)
#write.csv(tef_df, 'tef.csv')
# Plot_data accounts for fractionation in source samples (according to youtube vid)

#CSV formatted correctly - write.csv commented out
mix <- load_mix_data (filename = 'cons.csv',
                      iso_names = c('d2H','d18O'),
                      factors = c('Treatment','Month'),
                      fac_random = c(FALSE, FALSE),
                      fac_nested = c(TRUE, FALSE),
                      cont_effects = NULL)

source <- load_source_data(filename = 'source_means.csv',
                           source_factors = c('Treatment'),
                           conc_dep = FALSE,
                           data_type = 'means',
                           mix)

# Load the source data
source <- load_source_data(filename='source_means.csv',
                           source_factors="Treatment", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)



#CSV formatted correctly - write.csv commented out
discr <- load_discr_data(filename = 'tef.csv', mix)


#Default "Uninformed" alpha = 1 - Generalist Prior
##plot_prior(alpha.prior=c(1),source)

#informative prior
##plot_prior(alpha.prior=c(0.5,0.5,1),source)



#Write JAGS model
##write_JAGS_model ()


#Longer you run = more likely for convergance 
#MAY TAKE A LONG TIME
#Run a test model first

##jag.1 <- run_model(run = 'test', mix, source, discr, model_filename, 
##                   alpha.prior = __, resid_err, process_err)



#pairs_plot shows correlation to different sources




#Apply model to inputs and populate .txt diagnostics, summary stats, and plot series back into folder
##output_JAGS(jags.1,mix,souce,output_options)
