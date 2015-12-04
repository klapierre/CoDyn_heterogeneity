library(tidyr)
library(dplyr)
library(codyn)
library(ggplot2)


datpath = "~/Dropbox/CoDyn/R files/11_06_2015_v7/CoDyn_heterogeneity" # this likely will be different for different folks

#read in the data

#for some reason datpath is no longer working for me (LH)
#bring it latest here:
#rawdat <- read.csv(file.path(datpath, "relative cover_nceas and converge_12012015.csv"), row.names = 1) 
rawdat <- read.csv( "relative cover_nceas and converge_12012015.csv", row.names = 1) 

# prevdat <-rawdat %>%
#   tbl_df() %>%
#   gather(species, abundance, sp1:sp392) %>%
#   mutate(sitesubplot=paste(site_code, project_name, plot_id, community_type, sep="_")) %>%
#     filter(!is.na(sitesubplot)) 


dat <-rawdat %>%

    tbl_df() %>%
  
  # reduce the species columns to a species and abundance column
  gather(species, abundance, sp1:sp392) %>%
  
  #remove any zeros
  filter(abundance>0) %>%
  
  #create a unique "sitesubplot" - is this the same as unid?
  mutate(sitesubplot=paste(site_code, project_name, plot_id, community_type, sep="_")) %>%
  
  #remove any non-existent sites (ie, NAs at the end of the excel spreadsheet)
  filter(!is.na(sitesubplot)) 

#make a dataframe of all the plots ever sampled, even if nothing was in them at the time
sampled_plots <-rawdat %>%
  select(site_code, project_name, plot_id, community_type, experiment_year) %>%
  unique() %>%
  mutate(sitesubplot=paste(site_code, project_name, plot_id, community_type, sep="_")) %>%
  tbl_df()

#merge that into dat so that years aren't dropped for complete species absences
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
  tbl_df() %>%
  filter(!is.na(species), !is.na(abundance), !is.na(sitesubplot), !is.na(experiment_year))


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

#create dat4, which only has sitesubplots with 2 yrs or more
#and which 
dat4<-merge(dat3, dat.test) %>%
  filter(timelength>1) %>%
  tbl_df()

#identify sitesubplots that only ever have one species
dat.test1<-dat4%>%
  select(sitesubplot, species) %>%
  unique() %>%
  group_by(sitesubplot) %>%
  mutate(totrich=n())

#remove sitesubplots that only ever have one species, creating dat6
dat5<-merge(dat4, dat.test1) %>%
  filter(totrich>1) %>%
  mutate(species=as.character(species))%>%
  tbl_df()

#identify sitesubplots in which all of the species never vary
dat.test2 <- dat5 %>%
  group_by(sitesubplot, species) %>%
  summarize(sppvar=var(abundance)) %>%
  tbl_df() %>%
  mutate(var0=ifelse(sppvar==0, 1, 0)) %>%
  group_by(sitesubplot) %>%
  summarize(var0=sum(var0), totcount=n()) %>%
  mutate(problem=ifelse(var0==totcount, 1, 0))

#remove sitesubplots with species that never vary (important for VR)
dat6 <- merge (dat5, dat.test2) %>%
  filter(problem==0) %>%
  select(-problem)

###RUN CODYN METRICS
turn.dat<-turnover(dat5, time.var="experiment_year", replicate.var="sitesubplot")
synch.dat<-synchrony(dat5, time.var="experiment_year", replicate.var="sitesubplot")
synch.dat2<-synchrony(dat5, time.var="experiment_year", replicate.var="sitesubplot", metric="Gross")
vr.dat <- variance_ratio(dat6, time.var="experiment_year", species.var="species", abundance.var="abundance", replicate.var="sitesubplot", bootnumber=1, average.replicates=F)

