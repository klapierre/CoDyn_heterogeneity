###Using compiled datasets to assess changes in RACs
library(tidyr)
library(codyn)
library(dplyr)
library(ggplot2)
library(reshape2)
library(vegan)


theme_set(theme_bw(20))

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
  select(site_code, experiment_year, project_name, plot_id, community_type, sitesubplot) %>%
  unique()


##EVENNESS

#write a function to get J from longform data
#requires both vegan and codyn
J_longform<-function(df, time.var="year", species.var="species", abundance.var="abundance"){
  com.use<-codyn:::transpose_community(df, time.var, species.var, abundance.var)
  div.out<-diversity(com.use)
  rich.out<-specnumber(com.use)
  J<-div.out/log(rich.out)
  return(J)
}

#DOING EVENNESS 2 WAYS. 
# 1 - CALCULATE EVENNES FOR EACH PLOT AND THEN AVERAGE EVENNESS IN A YEAR. 
# 2 - AVERAGE COVER ACORSS PLOTS AND TAKE SINGLE EVENNESS VALUE FOR A YEAR.

# METHOD 1 do evenness for each plot in each year
Jout <-as.data.frame(cbind(J=as.numeric(), sitesubplot=as.character()))
mysitesubplots<-unique(dat$sitesubplot)
for (i in 1:length(mysitesubplots)) {
  subber<-dat %>%
    filter(sitesubplot == mysitesubplots[i])
  subout <- data.frame(J_longform(subber, time.var="experiment_year"))
  names(subout)[1]="J"
  subout$sitesubplot <-unique(subber$sitesubplot)
  subout$experiment_year<-row.names(subout)
  Jout<-rbind(Jout, subout)
}

#need to merge by sitesubplot and year
Jout2 <-merge(Jout, dat.key, by=c("sitesubplot", "experiment_year"), all=T)%>%
  #drop NAs
  filter(!is.na(J)) 

#calculate mean evenness for each year
Jmeans<-aggregate(J~site_code+project_name+community_type+experiment_year, mean, data=Jout2)  

# METHOD 2 take overall species cover across all plots and calculate a single evenness number for each year

#get average cover of each species in a year
ave<-aggregate(abundance~site_code+project_name+community_type+species+experiment_year, mean, data=dat)
ave$siteprojcom<-as.character(with(ave,paste(site_code, project_name, community_type, sep='_')))
                                   
Javeout<-as.data.frame(cbind(Jave=as.numeric(), siteprojcom=as.character()))
mysite<-unique(as.character(ave$siteprojcom))
for (i in 1:length(mysite)){
  subber<-ave%>%
    filter(siteprojcom==mysite[i])
  subout<-data.frame(J_longform(subber, time.var="experiment_year"))
  names(subout)[1]="Jave"
  subout$siteprojcom<-unique(subber$siteprojcom)
  subout$experiment_year<-row.names(subout)
  Javeout<-rbind(Javeout, subout)
}

#merge all to a singe dataframe
Jmeans$siteprojcom<-as.character(with(Jmeans, paste(site_code, project_name, community_type, sep="_")))
Evenness<-merge(Jmeans, Javeout, by=c("siteprojcom","experiment_year"))

##there is a lot of variation in evenness for a year depending on how evenness is calculated
#not quite sure what the biological relevance is here. I think when the experimental scale (method 2) is more even than the plot scale (method 1) (above line) it means the site is patchy, with different plots being dominated by different species. When, the plot scale is more even than the experiment scale (below the line) it suggests while each plot is relativley even, there is a strong dominant found in each plot.

ggplot(Evenness, aes(x=J, y=Jave, color=site_code))+
  geom_point(size=4)+
  geom_abline()+
  scale_x_continuous(limits=c(0,1))+
  scale_y_continuous(limits=c(0,1))+
  xlab("J (plot scale)")+
  ylab("J (experiment scale)")

         
###TURNOVER
# 1 - CALCULATE TURNOVER FOR EACH PLOT AND THEN AVERAGE TURNOVER IN A YEAR. 
# 2 - AVERAGE COVER ACORSS PLOTS AND TAKE SINGLE TURNOVER VALUE FOR A YEAR.

#METHOD 1.
#not working for LUQ Snails or NTL ZOO. I will just drop these for now and then have it added back in once I figure out the problem. They both get the same error.
luq<-subset(dat, site_project_comm=="LUQ_snails_0")
luq2<-aggregate(abundance~experiment_year+plot_id, length, data=luq)
luq3<-aggregate(plot_id~experiment_year, length, data=luq2)
#years 1995-1998 have many more plots than 40.
luqtest<-subset(dat, site_project_comm=="LUQ_snails_0"&experiment_year!=1995&experiment_year!=1996&experiment_year!=1997&experiment_year!=1998)
test<-turnover(luqtest, time.var = "experiment_year", replicate.var = "plot_id")

#is this the same problem for NTL?
ntl<-subset(dat, site_project_comm=="NTL_ZOO_0")
ntl2<-aggregate(abundance~experiment_year+plot_id, length, data=ntl)
ntl3<-aggregate(plot_id~experiment_year, length, data=ntl2)
#yes 2013.50 has an extra plot.
ntltest<-subset(dat, site_project_comm=="NTL_ZOO_0"&experiment_year!=2013.50)
test2<-turnover(ntltest, time.var = "experiment_year", replicate.var = "plot_id")

#the error is:  
#Error in mapply(FUN = f, ..., SIMPLIFY = FALSE) : 
#zero-length inputs cannot be mixed with those of non-zero length 

#proceeding with subset of data
dat2<-subset(dat,site_project_comm!="LUQ_snails_0"&site_project_comm!="NTL_ZOO_0")
totturn<-as.data.frame(cbind(totalt=as.numeric(), siteprojcom=as.character(), experiment_year=as.numeric()))
mysites<-unique(dat2$site_project_comm)

for (i in 1:length(mysites)){
  subber<-dat2%>%
    filter(site_project_comm==mysites[i])
  subout<-turnover(df=subber, time.var="experiment_year", replicate.var = "plot_id")
  names(subout)[1]="totalt"
  subout$siteprojcom<-unique(subber$site_project_comm)
  totturn<-rbind(totturn, subout)
}
#average up to a single turnover number for a year
totturn_ave<-aggregate(totalt~siteprojcom+experiment_year, mean, data=totturn)

###method 2. to make this compatible with other analysis, using the average abundance of a species for within a year across all plots.

totalturnave <-as.data.frame(cbind(totaltave=as.numeric(), siteprojcom=as.character(), experiment_year=as.numeric()))
mysites<-unique(ave$siteprojcom)
for (i in 1:length(mysites)) {
  subber<-ave %>%
    filter(siteprojcom == mysites[i])
  subout <- turnover(df=subber, time.var="experiment_year", species.var="species", abundance.var="abundance")
  names(subout)[1]="totaltave"
  subout$siteprojcom <-unique(subber$siteprojcom)
  totalturnave<-rbind(totalturnave, subout)
}
#merge with other datset
turnover_all<-merge(totturn_ave, totalturnave, by=c("siteprojcom","experiment_year"))%>%
  separate(siteprojcom, into=c("site_code","project_name","community_type"), sep="_", remove=F)

#plot
ggplot(turnover_all, aes(x=totalt, y=totaltave, color=site_code))+
  geom_point(size=4)+
  geom_abline()+
  scale_x_continuous(limits=c(0,1))+
  scale_y_continuous(limits=c(0,1))+
  xlab("Turnover (plot scale)")+
  ylab("Turnover (experiment scale)")

###Looking at this overtime TURNOVER
ggplot(turnover_all, aes(x=experiment_year, y=totaltave))+
  geom_point(size=4)+
  scale_y_continuous(limits=c(0,1))+
  xlab("Year")+
  ylab("Turnover (experiment scale)")+
  facet_wrap(~siteprojcom, ncol=7, scales="free_x")
#Evenness
ggplot(Evenness, aes(x=experiment_year, y=Jave))+
  geom_point(size=4)+
  scale_y_continuous(limits=c(0,1))+
  xlab("Year")+
  ylab("Evenness (experiment scale)")+
  facet_wrap(~siteprojcom, ncol=7, scales="free_x")

####going to hold off on this until we have the final dataset
###read in other output file to expore relationships
# output <- read.csv(file.path(datpath, 'spatial_temporal_heterogeneity_diversity.csv'), row.names = 1)%>%
#   filter(!is.na(temporal_distance))%>%
#   mutate(siteprojcom=paste(site_code, project_name.x, community_type, sep="_"))
# 
# #there is something wrong with this file, many rows are repeated, averaging up for one row for each experiment per year
# output2<-aggregate(cbind(dispersion, temporal_distance, H, log_S, S, J)~label+site_code+experiment_year+project_name.x+community_type+siteprojcom, mean, data=output)
# 
# #this merge will only work with the data from Kim and I where experiment year is already a string of numbers
# dispersion<-merge(output2, totalturn, by=c("siteprojcom", "experiment_year"))
# 
# #for this limited nubmer of datasets, there appears to be no relationship with turnover and spatial heterogeniety.
# with(dispersion, plot(totalt, dispersion))
# with(dispersion, cor.test(totalt, dispersion))
# with(dispersion, plot(temporal_distance, dispersion))
# with(dispersion, cor.test(temporal_distance, dispersion))
