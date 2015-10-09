###Using compiled datasets to assess changes in RACs
library(tidyr)
library(codyn)
library(dplyr)
library(ggplot2)
library(reshape2)
library(vegan)

#set path
datpath = "~/Dropbox/CoDyn/R files/10_08_2015_v6/CoDyn_heterogeneity"

#read in data
dat <- read.csv(file.path(datpath, 'relative cover_nceas and converge_10092015.csv'), row.names = 1)%>%
  tbl_df() %>%
  # reduce the species columns to a species and abundance column
  gather(species, abundance, sp1:sp322) %>% #old col name is 1, and old values become 2
  #create a unique "sitesubplot" - is this the same as unid?
  mutate(sitesubplot=paste(site_code, project_name, plot_id, community_type, sep="_")) %>%
  
  #remove any non-existent sites (ie, NAs at the end of the excel spreadsheet)
  filter(!is.na(sitesubplot)) 


##make a dat key
dat.key <- dat %>%
  select(site_code, experiment_year, project_name, plot_id, community_type) %>%
  unique()


#write a function to get J from longform data
#requires both vegan and codyn
J_longform<-function(df, time.var="year", species.var="species", abundance.var="abundance"){
  com.use<-codyn:::transpose_community(df, time.var, species.var, abundance.var)
  div.out<-diversity(com.use)
  rich.out<-specnumber(com.use)
  J<-div.out/log(rich.out)
  return(J)
}

Jout <-as.data.frame(cbind(J=as.numeric(), sitesubplot=as.character()))
mysitesubplots<-unique(dat$sitesubplot)
for (i in 1:length(mysitesubplots)) {
  subber<-dat %>%
    filter(sitesubplot == mysitesubplots[i])
  subout <- data.frame(J_longform(subber, time.var="experiment_year"))
  names(subout)[1]="J"
  subout$sitesubplot <-unique(subber$sitesubplot)
  Jout<-rbind(Jout, subout)
}

#check this, was slow, might sort then cbind
Jout2 <-merge(Jout, dat.key)

#then use this to aggregate by year, or site, whatever!
  
