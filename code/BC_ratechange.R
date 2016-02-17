library(tidyr)
library(dplyr)
library(ggplot2)
library(vegan)


#### DATA CLEANING ###
## Read in Meghan's wide data
rawdat <- read.csv('~/Dropbox/CoDyn/R files/11_06_2015_v7/relative cover_nceas and converge_12012015_cleaned.csv',
                stringsAsFactors = F) %>%
  tbl_df() %>%
  arrange(sitesubplot, experiment_year) 

## Convert to long form
dat0 <- rawdat %>%
  gather(species, abundance, sp1:sp99) %>%
  filter(abundance>0) %>%
  filter(!is.na(sitesubplot)) 

#make a dataframe of all the plots ever sampled, even if nothing was in them at the time
sampled_plots <-rawdat %>%
  select(site_code, project_name, plot_id, community_type, experiment_year) %>%
  unique() %>%
  mutate(sitesubplot=paste(site_code, project_name, plot_id, community_type, sep="_")) %>%
  tbl_df()

#merge that with dat0 so that years aren't dropped for complete species absences
dat00<-merge(dat0, sampled_plots, id=c("sitesubplot", "experiment_year"), all.y=T)

# Run a function to fill in 0s for species present in the plot but not that year
fill_zeros <- function (df, year="year", species="species", abundance="abundance") {
  nosp <- length(unique(df[,species]))
  df2 <- df[c(year, species, abundance)] %>%
    spread(species, abundance, fill=0) %>%
    gather(species, abundance, 2:(nosp+1))
  return(df2)
}

# apply the fill_zeros function across dat00
X <- split(dat00, dat00$sitesubplot)
out <- lapply(X, FUN=fill_zeros, "experiment_year")
ID <- unique(names(out))
out <- mapply(function(x, y) "[<-"(x, "sitesubplot", value = y) ,
              out, ID, SIMPLIFY = FALSE)
dat000 <- do.call("rbind", out) %>%
  tbl_df() %>%
  filter(!is.na(species), !is.na(abundance), !is.na(sitesubplot), !is.na(experiment_year))

# create a key of sites to merge with dat000
subkey <- rawdat %>%
  select(site_code, sitesubplot, site_project_comm) %>%
  unique()

# merge in the subkey
dat <- merge(subkey, dat000)

# remove previous data iterations
rm(dat0, dat00, dat000, rawdat)

# read in Meghan's diversity output
div_out <- read.csv('~/Dropbox/CoDyn/R files/11_06_2015_v7/spatial_temporal_heterogeneity_diversity.csv') %>%
  tbl_df()

# create a key of site attributes
key <- div_out %>%
  select(site_code, site_project_comm, location, Country, Continent, Lat, Long,
         MAP_mm, plot_size, spatial_extent, succession, lifespan,
         trophic_level, taxa, ANPP, broad_ecosystem_type) %>%
  unique()


### CALCULATE BRAY CURTIS WITHIN SITESUBPLOTS OVER SUCCESSIVE TIME INTERVALS###

# Set working directory to the GitHub CoDyn_heterogeneity -> code folder
source("FN_rate_change_BC.R")
# Or open and run "FN_rate_change_BC.R

rate_interval_out_BC0 <- rate_change_interval_BC(dat, "experiment_year", "species", "abundance", "sitesubplot") %>%
  tbl_df() %>%
  mutate(interval = as.numeric(as.character(interval)),
         experiment_year = as.numeric(as.character(experiment_year)),
         distance = as.numeric(as.character(distance)))
  
rate_interval_out_BC1 <- merge(subkey, rate_interval_out_BC0)
rate_interval_out_BC <- merge(key, rate_interval_out_BC1)
rm(rate_interval_out_BC0, rate_interval_out_BC1)

# Visualize by time interval
# ggplot(rat2bc, aes(x=interval, y=distance, group=sitesubplot, color=site_code)) + 
#   geom_smooth(method="lm", se=F) + facet_wrap(~lifespan) + xlim(0,40)

## CALCULATE BRAY CURTIS ACROSS SUBPLOTS WITHIN A SITE AND YEAR ##
## ie, re-run the plot-level Bray Curtis (because the div_out doesn't have the original experiment_year)

dat$siteyr <- paste(dat$site_project_comm, dat$experiment_year, sep="_")
mysites <- unique(dat$siteyr) 

subber <- subset(dat, siteyr == mysites[i])

## Function to transpose data
transpose_community_spatial <- function(df, replicate.var, species.var, abundance.var) {
  df<-as.data.frame(df)
  df[species.var]<-if(is.factor(df[[species.var]])==TRUE){factor(df[[species.var]])} else {df[species.var]}  
  comdat<-tapply(df[[abundance.var]], list(df[[replicate.var]], as.vector(df[[species.var]])), sum)
  comdat[is.na(comdat)]<-0
  comdat<-as.data.frame(comdat)
  return(comdat)
}

## Function to calculate BC over plots within a year
spaceBC <- function(df){
subber2 <- df %>%
  select(sitesubplot, species, abundance) %>%
  spread(species, abundance, fill=0) %>%
  select(-sitesubplot)
DM <-vegdist(subber2, method='bray',diag=F, upper=TRUE)
dispersion = mean(DM)
return(dispersion)
}

# Select and split data
dat2 <- dat %>%
  arrange(siteyr) %>%
  mutate(species=as.character(species)) %>%
  select(siteyr, sitesubplot, species, abundance)

X <- split(dat2, dat$siteyr)
myout<-do.call("rbind", lapply(X, FUN=spaceBC2))

spatial_dispersion <- as.data.frame(cbind(siteyr=row.names(myout), dispersion=myout[,1])) %>%
  tbl_df() %>%
  mutate(dispersion=as.numeric(as.character(dispersion)))
  
# Create a siteyr column to merge
rate_interval_out_BC$siteyr <- paste(rate_interval_out_BC$site_project_comm, rate_interval_out_BC$experiment_year, sep="_")

#ratetog <- merge(spatial_dispersion, rate_interval_out, by="siteyr", all.y=T)