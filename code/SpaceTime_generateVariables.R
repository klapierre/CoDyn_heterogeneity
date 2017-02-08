library(tidyr)
library(dplyr)
library(ggplot2)
library(vegan)
library(grid)
library(gridExtra)

######################
#### DATA PREPPING ###
######################

## Read in the complete dataframe

dat <- read.csv('~/Dropbox/CoDyn/R files/11_06_2015_v7/relative cover_nceas and converge_12012015_cleaned.csv',
                   stringsAsFactors = F) %>%
  tbl_df() %>%
  gather(species, abundance, sp1:sp99) %>%
  mutate(siteyr = paste(site_project_comm, experiment_year, sep = "_"))
  

##############################
### EXPLANATORY VARIABLES ####
##############################

## Read in the key with site-level information
key <- read.csv("~/Dropbox/CoDyn/R files/11_06_2015_v7/siteinfo_key.csv") %>%
  tbl_df()

#### DIVERSITY METRICS ################
### Calculate diversity metrics within a site and year ###
spatial_diversity <- dat %>%
  group_by(site_code, site_project_comm, experiment_year, species) %>%
  summarize(abundance = mean(abundance)) %>%
  #filter(abundance > 0) %>%
  mutate(rich = ifelse(abundance >0 , 1, 0)) %>%
  tbl_df() %>%
  group_by(site_code, site_project_comm, experiment_year) %>%
  summarize(spatial_rich = sum(rich), spatial_Shannon=diversity(abundance), 
            spatial_J=spatial_Shannon/log(spatial_rich), spatial_dominance = max(abundance), 
            spatial_evenness = diversity(abundance, "inv")/spatial_rich) %>%
  tbl_df()

#### BRAY CURTIS SPATIAL DISPERSION ####
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

# Select and split data
dat2 <- dat %>%
  arrange(siteyr) %>%
  mutate(species=as.character(species)) %>%
  select(siteyr, sitesubplot, species, abundance)

X <- split(dat2, dat2$siteyr)
myout <- do.call("rbind", lapply(X, FUN=space_BC))

spatial_dispersion <- as.data.frame(cbind(siteyr=row.names(myout), dispersion=myout[,1])) %>%
  tbl_df() %>%
  mutate(dispersion=as.numeric(as.character(dispersion))) %>%
  arrange(siteyr) 

exp1 <- merge(key, spatial_diversity) %>%
  tbl_df() %>%
  mutate(siteyr = paste(site_project_comm, experiment_year, sep = "_"))

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
source("FN_rate_change_BC.R")
# Or open and run "FN_rate_change_BC.R

rate_interval_out_BC <- rate_change_interval_BC(spatial_means, "experiment_year", "species", "abundance", "site_project_comm") %>%
  tbl_df() %>%
  mutate(interval = as.numeric(as.character(interval)),
         experiment_year = as.numeric(as.character(experiment_year)),
         distance = as.numeric(as.character(distance)))
rate_interval_out_BC$siteyr <- paste(rate_interval_out_BC$site_project_comm, 
                                      rate_interval_out_BC$experiment_year, sep = "_")


##### PUT TOGETHER RESPONSE AND EXPLANATORY VARIABLES FOR RANDOM EFFECTS MODEL ########
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

# 
# ggplot(dataout, aes(x=spatialdispersion, y=temporaldistance)) +
#   geom_point(aes(color=site_project_comm)) + geom_smooth(method = "lm", se = F) +
#  theme(legend.position="none")  + facet_grid(lifespan~succession) 

#write.csv(dataout, "SpatioTemp_testStatistics_20170208.csv", row.names = F)

##### PUT TOGETHER DATASETS TO LOOK AT DIFFERENCES BETWEEN TIMESTEPS ########

rateout0 <- merge(spatial_dispersion, rate_interval_out_BC, by="siteyr", all.y=T) %>%
  tbl_df()


rateout <- merge(rateout0, key ) %>%
  tbl_df() %>%
  mutate(lifespan2 = "Subannual",
         lifespan2 = ifelse(lifespan == "annual", "Annual", lifespan2),
         lifespan2 = ifelse(lifespan == "longer", "Long-lived", lifespan2)) %>%
  mutate(lifespan3 = "A",
         lifespan3 = ifelse(lifespan == "annual", "B", lifespan3),
         lifespan3 = ifelse(lifespan == "longer", "C", lifespan3))

rateout$lifespan <- factor(rateout$lifespan, levels = c("subannual", "annual", "longer"))
rateout$lifespan2 <- factor(rateout$lifespan2, levels = c("Subannual", "Annual", "Long-lived"))

#write.csv(rateout, "SpaceTemp_RateChange.csv", row.names = F)

#### PLOT THE SPATIAL TEMPORAL TRAJECTORIES ##

## SET GGPLOT THEME ##
theme_set(theme_bw())
theme_update( panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
            strip.background = element_blank(),
             text = element_text(size = 24),
            strip.text= element_text(size = 24), 
            axis.text = element_text(size = 14))



dataout %>%
  group_by(lifespan) %>%
  summarize(min_length = min(dataset_length))

rateout2 <- rateout %>%
  filter(interval <= 6)

#######################
### MAKE FIGURE 3 #####
#######################

a <- ggplot(subset(rateout2, interval ==1), aes(x=dispersion, y=distance)) + 
  geom_point(color = "black", size = .5) +
  geom_smooth(aes(color = as.factor(interval)), method="lm", se = F) + 
  labs(x="Spatial heterogeneity", y="", color="Interval") +
  facet_wrap(~lifespan2) + ylim(0,1) + xlim(0,1) +
  scale_color_manual(values = c("turquoise")) 

b <- ggplot(rateout2, aes(x=dispersion, y=distance, group=interval)) + 
  geom_smooth(aes(color=interval), method="lm", se = F) + 
  labs(x="Spatial heterogeneity", y="", color="Interval") +
  facet_wrap(~lifespan2) + ylim(0,1) + xlim(0,1) +
  scale_color_continuous(high = "coral", low = "turquoise", limits = c(1,6)) #+ 
 # theme(legend.position = "bottom")

pdf("Figure3_Rout.pdf", width = 12, height = 8)
grid.arrange(a + xlab("") + theme(axis.text.x=element_blank())  ,
             b + theme(strip.text = element_blank()), 
             left = textGrob("Temporal heterogeneity", rot=90, gp=gpar(fontsize = 24), vjust =1.5))
dev.off()
