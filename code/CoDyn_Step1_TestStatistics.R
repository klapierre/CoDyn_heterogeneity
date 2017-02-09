#### NOTE TO USERS:
## The CoDyn workflow follows the following steps
# I) Generation of test statistics for subsequent analysis and visualization
    ## Data Requirements:
        # 1) The raw species-level data: relative cover_nceas and converge_12012015_cleaned.csv
        # 2) The raw site-level data: siteinfo_key.csv
    ## Files:
        # 1) CoDyn_Step0_BrayCurtisFuntions.R, which includes functions iterative Bray Curtis analyses. 
        # 2)  CoDyn_Step1_TestStatistics.R (this file), which requires CoDyn_Step0_BrayCurtisFuntions.R and generates two dataframes of test statics: 
              #a) SpatioTemp_testStatistics_20170208.csv: 
                  # single-timestep temporal heterogeneity in relation to a suite of explanatory variables
              #b) SpaceTemp_RateChange_20170208.csv
                  # temporal heterogeneity over successive timesteps and associated explanatory variables

# II) Analysis and visualization 
    ## Data Requirements: dataout file from CoDyn_Step1_TestStatistics.R
    ## Files:
        # 1) CoDyn_Step2_AnalysisVisuals.R, which:
              # a) Performs mixed-effect models on SpatioTemp_testStatistics_20170208.csv
              # b) Creates figure 1 SpatioTemp_testStatistics_20170208.csv
              # c) Creates figure 2 from output of CoDyn_Step2_Analysis.R
              # d) Creates figure 3 from SpaceTemp_RateChange_20170208.csv

###############################################################################
# I) Generation of test statistics for subsequent analysis and visualization###
###############################################################################

## Load packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(vegan)

######################
#### DATA PREPPING ###
######################

## Read in the complete dataframe
dat <- read.csv('relative cover_nceas and converge_12012015_cleaned.csv',
                stringsAsFactors = F) %>%
  tbl_df() %>%
  gather(species, abundance, sp1:sp99) %>%
  mutate(siteyr = paste(site_project_comm, experiment_year, sep = "_"))


##############################
### EXPLANATORY VARIABLES ####
##############################

## Read in the key with site-level information
key <- read.csv("siteinfo_key.csv") %>%
  tbl_df()

#### DIVERSITY METRICS ###########################################
### Calculate diversity metrics within a site/project and year ###
spatial_diversity <- dat %>%
  # summarize data at the site/project level
  group_by(site_code, site_project_comm, experiment_year, species) %>%
  summarize(abundance = mean(abundance)) %>%
  # create a presence/absence column
  mutate(rich = ifelse(abundance >0 , 1, 0)) %>%
  tbl_df() %>%
  # group by site/project and calculate diversity variables
  group_by(site_code, site_project_comm, experiment_year) %>%
  summarize(spatial_rich = sum(rich), spatial_Shannon=diversity(abundance), 
            spatial_J=spatial_Shannon/log(spatial_rich), spatial_dominance = max(abundance), 
            spatial_evenness = diversity(abundance, "inv")/spatial_rich) %>%
  tbl_df()

#### BRAY CURTIS SPATIAL DISPERSION ###############
## Function to calculate BC over plots within a year
space_BC <- function(df){
  subber <- df %>%
    select(sitesubplot, species, abundance) %>%
    spread(species, abundance, fill=0) %>%
    select(-sitesubplot)
  DM <-vegdist(subber, method='bray',diag=F, upper=TRUE)
  dispersion = mean(DM)
  return(dispersion)
}

# Select data for analyses
dat2 <- dat %>%
  arrange(siteyr) %>%
  mutate(species=as.character(species)) %>%
  select(siteyr, sitesubplot, species, abundance)

# split data by site and year, and run the spatial_BC function
X <- split(dat2, dat2$siteyr)
myout <- do.call("rbind", lapply(X, FUN=space_BC))

# create a dataframe of spatial BC results
spatial_dispersion <- as.data.frame(cbind(siteyr=row.names(myout), dispersion=myout[,1])) %>%
  tbl_df() %>%
  mutate(dispersion=as.numeric(as.character(dispersion))) %>%
  arrange(siteyr) 

########## PUT THE EXPLANATORY VARIABLES TOGETHER ############
# merge the site key info with diversity metrics
exp1 <- merge(key, spatial_diversity) %>%
  tbl_df() %>%
  mutate(siteyr = paste(site_project_comm, experiment_year, sep = "_"))

# merge in the spatial BC results for a full dataset of explanatory variables
explanatory_variables <- merge(exp1, spatial_dispersion) %>%
  tbl_df() %>%
  mutate(spatialdispersion = dispersion) %>%
  select(-dispersion)

################################
#### RESPONSE VARIABLES ########
################################

## Calculate mean values for species across all plots
spatial_means <- dat %>%
  group_by(site_code, site_project_comm, experiment_year, species) %>%
  summarize(abundance = mean(abundance)) 

## Calculate Bray Curtis between plots over successive time intervals
source("CoDyn_Step0_BrayCurtisFuntions.R")

rate_interval_out_BC <- rate_change_interval_BC(spatial_means, "experiment_year", "species", "abundance", "site_project_comm") %>%
  tbl_df() %>%
  mutate(interval = as.numeric(as.character(interval)),
         experiment_year = as.numeric(as.character(experiment_year)),
         distance = as.numeric(as.character(distance)))
rate_interval_out_BC$siteyr <- paste(rate_interval_out_BC$site_project_comm, 
                                     rate_interval_out_BC$experiment_year, sep = "_")


######################################################################
##### PUT DATASETS TO LINK RESPONSE AND EXPLANATORY VARIABLES ########
######################################################################

## Select first time intervals for analyses 
temporal_response <- rate_interval_out_BC %>%
  filter(interval == 1) %>%
  mutate(temporaldistance = distance) %>%
  select(-distance, -interval)

## Merge to create the full dataset 
dataout <- merge(explanatory_variables, temporal_response) %>%
  tbl_df() %>%
  mutate(system = "aquatic",
         system = ifelse(broad_ecosystem_type == "herbaceous" | broad_ecosystem_type == "woody", 
                         "terrestrial", system))
dataout$lifespan <- factor(dataout$lifespan, levels = c("subannual", "annual", "longer"))


#write.csv(dataout, "SpatioTemp_testStatistics_20170208.csv", row.names = F)

###################################################################################
##### PUT TOGETHER DATASET OF MULTIPLE TIMESTEPS AND EXPLANATORY VARIABLES ########
###################################################################################

#  merge spatial dispersion with temporal intervals
rateout0 <- merge(spatial_dispersion, rate_interval_out_BC, by="siteyr", all.y=T) %>%
  tbl_df()

# merge in key and format lifespan names for publication
rateout <- merge(rateout0, key ) %>%
  tbl_df() %>%
  mutate(lifespan2 = "Subannual",
         lifespan2 = ifelse(lifespan == "annual", "Annual", lifespan2),
         lifespan2 = ifelse(lifespan == "longer", "Long-lived", lifespan2)) 

rateout$lifespan <- factor(rateout$lifespan, levels = c("subannual", "annual", "longer"))
rateout$lifespan2 <- factor(rateout$lifespan2, levels = c("Subannual", "Annual", "Long-lived"))

#write.csv(rateout, "SpaceTemp_RateChange_20170208.csv", row.names = F)