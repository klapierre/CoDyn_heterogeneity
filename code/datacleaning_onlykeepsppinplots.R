library(tidyr)
library(dplyr)
library(codyn)
library(ggplot2)


datpath = "~/Dropbox/CoDyn/R files/10_08_2015_v6/CoDyn_heterogeneity" # this likely will be different for different folks

#read in the data

rawdat <- read.csv(file.path(datpath, 'relative cover_nceas and converge_10092015.csv'), row.names = 1) 

prevdat <-rawdat %>%
  tbl_df() %>%
  gather(species, abundance, sp1:sp322) %>%
  mutate(sitesubplot=paste(site_code, project_name, plot_id, community_type, sep="_")) %>%
    filter(!is.na(sitesubplot)) 


dat <-rawdat %>%

    tbl_df() %>%
  
  # reduce the species columns to a species and abundance column
  gather(species, abundance, sp1:sp322) %>%
  
  #remove any zeros
  filter(abundance>0) %>%
  
  #create a unique "sitesubplot" - is this the same as unid?
  mutate(sitesubplot=paste(site_code, project_name, plot_id, community_type, sep="_")) %>%
  
  #remove any non-existent sites (ie, NAs at the end of the excel spreadsheet)
  filter(!is.na(sitesubplot)) 


sampled_plots <-rawdat %>%
  select(site_code, project_name, plot_id, community_type, experiment_year) %>%
  unique() %>%
  mutate(sitesubplot=paste(site_code, project_name, plot_id, community_type, sep="_"))


dat2<-merge(dat, sampled_plots, id=c("sitesubplot", "experiment_year"), all.y=T)

# a function to fill in 0s for species present in the plot but not that year
fill_zeros <- function (df, year="year", species="species", abundance="abundance") {
  nosp <- length(unique(df[,species]))
  df2 <- df[c(year, species, abundance)] %>%
    spread(species, abundance, fill=0) %>%
    gather(species, abundance, 2:(nosp+1))
  return(df2)
}

# apply the fill_zeros function across dat2
X <- split(dat2, dat2$sitesubplot)
out <- lapply(X, FUN=fill_zeros, "experiment_year")
ID <- unique(names(out))
out <- mapply(function(x, y) "[<-"(x, "sitesubplot", value = y) ,
              out, ID, SIMPLIFY = FALSE)
dat3 <- do.call("rbind", out) %>%
  tbl_df()

#Check that there are no sitesubplots that had absolutely nothing ever
dat3 %>%
  filter(is.na(species)) %>%
  select(sitesubplot) %>%
  unique() %>%
  nrow()

#make a key of sitesubplots by site_code and project_name
dat.key01 <-dat %>%
  select(site_code, project_name, sitesubplot) %>%
  unique()

#import experimental data
expt.info<-read.csv(file.path(datpath, "exptInfo_ecosystem type.csv"))

#merge expt.info with the key
dat.key <- merge (dat.key01, expt.info, all.x=T)

#test that the sitesubplot are all at least 2 yrs long 
dat.test<-dat3 %>%
  select(sitesubplot, experiment_year) %>%
  unique() %>%
  mutate(count=1) %>%
  group_by(sitesubplot) %>%
  summarize(timelength=sum(count))


#create dat2, which only has sitesubplots with 2 yrs or more
dat4<-merge(dat3, dat.test) %>%
  filter(timelength>1) %>%
  tbl_df()

##Prevdat2 should have only species within a sitesubplot that existed at one point in time
##And only includes sitesubplots for which data were collected for at least two years
prevdat2 <-merge(prevdat, dat.test) %>%
  filter(timelength>1) %>%
  tbl_df()


###RUN CODYN METRICS
turn.dat<-turnover(prevdat2, time.var="experiment_year", replicate.var="sitesubplot", metric="total")
