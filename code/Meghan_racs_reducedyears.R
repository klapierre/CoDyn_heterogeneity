###Using compiled datasets to assess changes in RACs
library(tidyr)
library(codyn)
library(dplyr)
library(ggplot2)
library(reshape2)
library(vegan)
library(swirl)

theme_set(theme_bw(20))

#set path
datpath = "~/Dropbox/CoDyn/R files/11_06_2015_v7"

#read in data
dat <- read.csv(file.path(datpath, 'relative cover_nceas and converge_12012015_cleaned.csv'), row.names = 1)
  
  
dat3<-dat%>%
  tbl_df() %>%
  # reduce the species columns to a species and abundance column
  gather(species, abundance, sp1:sp99) %>% #old col name is 1, and old values become 2
  #remove any non-existent sites (ie, NAs at the end of the excel spreadsheet)
  filter(!is.na(sitesubplot))

dat2<-dat3%>%
  mutate(year=experiment_year)
#Get 1 time step per year for all the aquatic data.
## April 2016 - NO longer doing this becuase it made some of the phyto communities have very high species richness.
#   mutate(year=trunc(experiment_year))%>%
#   tbl_df()%>%
#   group_by(site_code,project_name,community_type, site_project_comm, sitesubplot, year,plot_id,species)%>%
#   summarize(abundance=max(abundance))

##Calculate three basic diversity measures, richness (S), Shannon's Diversity (H), and Evenness(J)

#write a function to get J from longform data
#requires both vegan and codyn
# J_longform<-function(df, time.var="year", species.var="species", abundance.var="abundance"){
#   com.use<-codyn:::transpose_community(df, time.var, species.var, abundance.var)
#   H<-diversity(com.use)
#   S<-specnumber(com.use)
#   J<-H/log(S)
#   out1<-cbind(H, S)
#   output<-cbind(out1, J)
#   return(output)
# }

#DOING EVENNESS 2 WAYS. 
# 1 - CALCULATE EVENNES FOR EACH PLOT AND THEN AVERAGE EVENNESS IN A YEAR. 
# 2 - AVERAGE COVER ACORSS PLOTS AND TAKE SINGLE EVENNESS VALUE FOR A YEAR.
# dat.key<-dat%>%
#   select(sitesubplot, experiment_year, site_code, project_name, community_type, site_project_comm, plot_id)
# 
# H<-diversity(dat[,3:393])
# S<-specnumber(dat[,3:393])
# InvD<-diversity(dat[,3:393],"inv")
# SimpEven<-InvD/S
# out1<-cbind(H, S)
# output<-cbind(out1, SimpEven)
# DiversityMeasures<-cbind(dat.key, output)

#calculate mean evenness for each year
# Jmeans<-aggregate(cbind(H,S,J)~site_code+project_name+community_type+year, mean, data=Jout2)  

# METHOD 2 take overall species cover across all plots and calculate a single evenness number for each year

#get average cover of each species in a year
ave <- dat2 %>%
  tbl_df() %>%
  group_by(site_code, project_name, community_type, species, year) %>%
  summarize(abundance=mean(abundance)) %>%
  #get ride of the grouping 
  tbl_df() %>%
  filter(abundance >0) %>%
  mutate(siteprojcom=paste(site_code, project_name, community_type, sep='_'))

ave_wide<-ave%>%
  spread(species, abundance, fill=0)

avedat.key<-ave_wide%>%
  select(year, site_code, project_name, community_type, siteprojcom)

H<-diversity(ave_wide[,6:396])
S<-specnumber(ave_wide[,6:396])
InvD<-diversity(ave_wide[,6:396],"inv")
SimpEven<-InvD/S
out1<-cbind(H, S)
output<-cbind(out1, SimpEven)
DiversityMeasures_ave<-cbind(avedat.key, output)

plot(S, SimpEven)

#merge all to a singe dataframe
# Jmeans$siteprojcom<-as.character(with(Jmeans, paste(site_code, project_name, community_type, sep="_")))
# Div_measures<-merge(Jmeans, Javeout, by=c("siteprojcom","year"))

##there is a lot of variation in evenness for a year depending on how evenness is calculated
#not quite sure what the biological relevance is here. I think when the experimental scale (method 2) is more even than the plot scale (method 1) (above line) it means the site is patchy, with different plots being dominated by different species. When, the plot scale is more even than the experiment scale (below the line) it suggests while each plot is relativley even, there is a strong dominant found in each plot.
# # 
# ggplot(Div_measures, aes(x=J, y=Jave, color=site_code))+
#   geom_point(size=4)+
#   geom_abline()+
#   scale_x_continuous(limits=c(0,1))+
#   scale_y_continuous(limits=c(0,1))+
#   xlab("J (plot scale)")+
#   ylab("J (experiment scale)")
# ggplot(Div_measures, aes(x=H, y=Have))+
#   geom_point(size=4)+
#   geom_abline()+
#   xlab("H (plot scale)")+
#   ylab("H (experiment scale)")
# ggplot(Div_measures, aes(x=S, y=Save, color=site_code))+
#   geom_point(size=4)+
#   geom_abline()+
#   xlab("S (plot scale)")+
#   ylab("S (experiment scale)")

#Evenness
ggplot(DiversityMeasures_ave, aes(x=year, y=SimpEven))+
  geom_point(size=4)+
  scale_y_continuous(limits=c(0,1))+
  xlab("Year")+
  ylab("Evenness (experiment scale)")+
  facet_wrap(~siteprojcom, ncol=7, scales="free")
ggplot(DiversityMeasures_ave, aes(x=year, y=S))+
  geom_point(size=4)+
  xlab("Year")+
  ylab("Richness")+
  facet_wrap(~siteprojcom, ncol=7, scales="free")

###TURNOVER
# 1 - CALCULATE TURNOVER FOR EACH PLOT AND THEN AVERAGE TURNOVER IN A YEAR. 
# 2 - AVERAGE COVER ACORSS PLOTS AND TAKE SINGLE TURNOVER VALUE FOR A YEAR.

#METHOD 1.
# 
# dat2.key<-dat2%>%
#   select(site_code, project_name, community_type, site_project_comm, sitesubplot)%>%
#   unique()
# 
# turnover<-turnover(dat2, replicate.var="sitesubplot")
# gain<-turnover(dat2, replicate.var="sitesubplot", metric="appearance")
# loss<-turnover(dat2, replicate.var="sitesubplot", metric="disappearance")
# 
# allturn1<-merge(turnover, gain, by=c("sitesubplot", "year"))
# allturn2<-merge(allturn1, loss, by=c("sitesubplot", "year"))
# allturn<-merge(allturn2, dat2.key, by="sitesubplot")
# 
# #average up to a single turnover number for a year
# totturn_ave<-aggregate(cbind(total, appearance, disappearance)~+year, mean, data=allturn)

###method 2. to make this compatible with other analysis, using the average abundance of a species for within a year across all plots.

turnover2<-turnover(ave, replicate.var="siteprojcom")
gainave<-turnover(ave, replicate.var="siteprojcom", metric="appearance")
lossave<-turnover(ave, replicate.var="siteprojcom", metric="disappearance")

turn1<-merge(turnover2, gainave, by=c("siteprojcom", "year"))
turn<-merge(turn1, lossave, by=c("siteprojcom","year"))

ggplot(turnover2, aes(x=year, y=total))+
  geom_point(size=4)+
  scale_y_continuous(limits=c(0,1))+
  xlab("Year")+
  ylab("Turnover (experiment scale)")+
  facet_wrap(~siteprojcom, ncol=7, scales="free")

ggplot(gainave, aes(x=year, y=appearance))+
  geom_point(size=4)+
  scale_y_continuous(limits=c(0,1))+
  xlab("Year")+
  ylab("Proportion Species Gained")+
  facet_wrap(~siteprojcom, ncol=7, scales="free")


ggplot(lossave, aes(x=year, y=disappearance))+
  geom_point(size=4)+
  scale_y_continuous(limits=c(0,1))+
  xlab("Year")+
  ylab("Proportion species Lost")+
  facet_wrap(~siteprojcom, ncol=7, scales="free")


####mean rank shift
rank<-mean_rank_shift(ave, replicate.var = "siteprojcom")

rank2<-rank%>%
  separate(year_pair, c("year1","year"), sep="-", remove=F)

turnrank<-merge(rank2, turn, by=c("siteprojcom","year"))

ggplot(rank2, aes(x=year1, y=MRS))+
  geom_point(size=4)+
  xlab("Year")+
  ylab("Mean Rank Shift")+
  facet_wrap(~siteprojcom, ncol=7, scales="free")

##rate of change
rc<-rate_change(ave, replicate.var = "siteprojcom")

##dominance
dom<-ave%>%
  tbl_df()%>%
  group_by(siteprojcom, year)%>%
  summarize(dom=max(abundance))

domturnrank<-merge(turnrank, dom, by=c("siteprojcom","year"), all=T)

ggplot(dom, aes(x=year, y=dom))+
  geom_point(size=4)+
  scale_y_continuous(limits=c(0,1))+
  xlab("Year")+
  ylab("Dominance")+
  facet_wrap(~siteprojcom, ncol=7, scales="free")

####relationship between community dynamic measures

allmetrics<-merge(DiversityMeasures_ave, domturnrank, by=c("siteprojcom","year"))%>%
  select(-year1, -year_pair)

pairs(allmetrics[,7:13])

phytos<-subset(ave, siteprojcom=="GLK_PHYTOS_ERIE_0"&year==2012)

###for SESYNC March 2016 Talk

#compare KNZ 00id and grasshoppers
explist<-ave%>%
  select(site_code, project_name, community_type)%>%
  unique()

subset<-ave%>%
  filter(site_code=="FCE")%>%
  mutate(Community=ifelse(project_name=="algae","Algae",ifelse(project_name=="FCE_inverts_driftfence_wet_season", "Invertebrates","Small Fish")))

subset_wide<-subset%>%
  spread(species, abundance, fill=0)

subset.dat.key<-subset_wide%>%
  select(year, site_code, project_name, community_type, Community)

##Evenness Richness
S<-specnumber(subset_wide[,6:94])
InvD<-diversity(subset_wide[,6:94],"inv")
SimpEven<-InvD/S
outputsubset<-cbind(S, SimpEven)

DiversityMeasures_subset<-cbind(subset.dat.key, outputsubset)

###mean rank
subsetrank<-mean_rank_shift(subset, replicate.var = "Community")
subsetrank2<-subsetrank%>%
  separate(year_pair, c("year1","year2"), sep="-", remove=F)

##gain loss
subsetgainave<-turnover(subset, replicate.var="Community", metric="appearance")
subsetlossave<-turnover(subset, replicate.var="Community", metric="disappearance")%>%
  mutate(appearance=-1*disappearance)%>%
  select(-disappearance)
subsetlosgain<-rbind(subsetlossave, subsetgainave)

ggplot(DiversityMeasures_subset, aes(x=year, y=S))+
  geom_point(size=4)+
  xlab("Year")+
  scale_y_continuous(limits=c(0,80))+
  ylab("Richness")+
  facet_wrap(~Community, ncol=3)
ggplot(DiversityMeasures_subset, aes(x=year, y=SimpEven))+
  geom_point(size=4)+
  scale_y_continuous(limits=c(0,1))+
  xlab("Year")+
  ylab("Evenness")+
  facet_wrap(~Community, ncol=3)
ggplot(subsetrank2, aes(x=year1, y=MRS))+
  geom_point(size=4)+
  scale_y_continuous(limits=c(0,8))+
  xlab("Year")+
  ylab("Mean Rank Shift")+
  facet_wrap(~Community, ncol=3)
ggplot(subsetlosgain, aes(x=year, y=appearance))+
  geom_point(size=4)+
  scale_y_continuous(limits=c(-1,1))+
  xlab("Year")+
  ylab("Proportion Species Gained and Lost")+
  facet_wrap(~Community, ncol=7)+
  geom_line(aes(y=0))


rc<-rate_change(subset, replicate.var = "Community")

#merge with other datset
# turnover_all<-merge(totturn_ave, totalturnave, by=c("siteprojcom","year"))%>%
#   separate(siteprojcom, into=c("site_code","project_name","community_type"), sep="_", remove=F)

#plot
# ggplot(turnover_all, aes(x=totalt, y=totaltave, color=site_code))+
#   geom_point(size=4)+
#   geom_abline()+
#   scale_x_continuous(limits=c(0,1))+
#   scale_y_continuous(limits=c(0,1))+
#   xlab("Turnover (plot scale)")+
#   ylab("Turnover (experiment scale)")


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
