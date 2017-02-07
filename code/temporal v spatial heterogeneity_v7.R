library(vegan)
library(reshape2)
library(ggplot2)
library(gtools)
library(plyr)
library(grid)
library(lme4)
library(codyn)
library(tidyr)
library(dplyr)

# If don't have codyn, try this:
# library(drat)
# addRepo("NCEAS")
# install.packages("codyn")
 

# setwd("C:\\Users\\Kim\\Dropbox\\working groups\\community dynamics working group\\CoDyn\\R files\\10_08_2015_v6")

setwd("~/Dropbox/CoDyn/R files/11_06_2015_v7")

theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank())

####################################################################################
####################################################################################
####################################################################################



##READ ME: ALL OF THIS CAN BE SKIPPED. THIS IS TO GET RELATIVE ABUNDANCE OF ALL SPECIES. SCROLL DOWN TO THE START HERE





#read in the species composition dataset
nceas <- read.csv('NewData_NCEAS_Nov11.csv')%>%
  select(-X)
names(nceas)[names(nceas)=='plot'] <- 'plot_id'
nceas$plot_id <- as.character(nceas$plot_id)# for merging

###fixing problems with LUQ snails and #fix luq snails and NTL zoo.

fix<-subset(nceas, project_name!="snails"&project_name!="ZOO")

luq<-subset(nceas, project_name=="snails")
luq2<-luq%>%
  filter(experiment_year==1992)%>%
  tbl_df()%>%
  select(plot_id)%>%
  unique()
luq3<-merge(luq, luq2, by="plot_id")
#check for only 40 plots/year
#luq4<-aggregate(abundance~experiment_year+plot_id, length, data=luq3)
#luq5<-aggregate(plot_id~experiment_year, length, data=luq4)
#yes, there are now only 40 plots for all years.

# fix NTL Zoo, it is typo in 2013 TR was entered as also Tr.
ntl<-subset(nceas, project_name=="ZOO")
ntl$plot_id<-toupper(ntl$plot_id)
# #check for 7 plots each year
#ntl3<-aggregate(abundance~experiment_year+plot_id, length, data=ntl)
#ntl4<-aggregate(plot_id~experiment_year, length, data=ntl3)
# #yes there are 7 plots each year

#get these back with full dataset
nceas_all1<-rbind(fix, luq3)
nceas_all<-rbind(nceas_all1, ntl)

#converge datasets - remove these datasets because they are too short or we are not allowed to use them
converge <- read.csv("corre_relcov.csv")
finalYear <- aggregate(converge["treatment_year"], by=converge[c("site_code", "project_name", "community_type")], FUN=max)
eightYear <- subset(finalYear, subset=(treatment_year>=8))
drop <- c('treatment_year')
eightYear <- eightYear[,!colnames(eightYear) %in% drop]
eightYearData <- merge(eightYear, converge)
eightYearControl <- subset(eightYearData, subset=(plot_mani==0))
eightYearControl$site_project <- paste(eightYearControl$site_code, eightYearControl$project_name, sep='_')
eightYearControl$site_project_comm <- with(eightYearControl, paste(site_code, project_name, community_type, sep='_'))
converge2a <- subset(eightYearControl, subset=(site_project_comm!='CDR_e001_D'))
converge2b <- subset(converge2a, subset=(site_project!='ANG_watering' & site_project!='CAU_RMAPC' & site_project!='dcgs_gap' & site_project!='Finse_WarmNut' & site_project!='ORNL_FACE' & site_project!='ARC_MNT' & site_project!='ARC_MAT2_1996' & site_project!='SGS_ESA' & site_project!='KNZ_BGP' & site_project!='NWT_246Nfert' & site_project!='KNZ_RHPs' & site_project!='CDR_BioCON' & site_project!='KUFS_E6' & site_project!='IMGERS_Yu' & site_project!='ASGA_Exp1' & site_project!='ARC_MAT2' & site_project!='ASGA_clonal' & site_project!='DL_NSFC'))
converge2b$drop<-ifelse(converge2b$site_project=="CDR_e002"&converge2b$treatment_year>10, 1, 0)
converge2c<-subset(converge2b, drop!=1)
names(converge2c)[names(converge2c)=='calendar_year'] <- 'experiment_year'
names(converge2c)[names(converge2c)=='relcov'] <- 'abundance'
converge2<-converge2c%>%
  select(experiment_year, plot_id, site_code, project_name, community_type, species, abundance)


#merge nceas data and converge data
alldata <- rbind(nceas_all, converge2)%>%
  mutate(site_project=paste(site_code, project_name, sep='_'))%>%
  mutate(site_project_comm =paste(site_code, project_name, community_type, sep='_'))%>%
  mutate(unid=paste(plot_id, community_type, project_name, site_code, experiment_year, sep="_"))
alldata[is.na(alldata)] <- 0 

#get relative cover

#make a dataframe with only the species data and with other information
information<-alldata%>%
  select(experiment_year, plot_id, site_code, project_name, community_type, site_project, site_project_comm, unid)%>%
  unique()

#get species list
species<-alldata%>%
  select(unid, species, abundance)


#get sum of cover for each plot
sumcover<-alldata%>%
  select(unid, species, abundance)%>%
  tbl_df()%>%
  group_by(unid)%>%
  summarise(totcov = sum(abundance))

#merge species data and sum of cover by plot
relcov<-merge(sumcover, species, by="unid") # ! SLOW ! 

#get only plots with some cover > 0 (a few plots have cover=0 and the ones that do make sense biologically)
relcov_a<-subset(relcov, subset=(totcov!=0))%>%
  mutate(relcov=abundance/totcov)%>%#calculate relative cover for each species in each plot
  select(unid, species, relcov)%>%
  filter(relcov!=0)

#merge the species data back together with the informational data
relcov_long<-merge(information, relcov_a, by="unid")


write.csv(relcov_long, '~/Dropbox/CoDyn/R files/11_06_2015_v7/relative cover_nceas and converge_02072017.csv', row.names=F)


#then run data thorugh datacleaning step.

###################################
##################################
######
#######start from here!
#######
##################################
###################################


alldata3 <- read.csv('~/Dropbox/CoDyn/R files/11_06_2015_v7/relative cover_nceas and converge_12012015_cleaned.csv')

#make year as factor
expt.list=data.frame(expt=levels(droplevels(alldata3$site_project_comm)))

newYear <- data.frame(row.names=1)

for(i in 1:length(expt.list$expt)) {
  
  dataset=alldata3[alldata3$site_project_comm==as.character(expt.list$expt[i]),]
  names(dataset)[names(dataset)=='experiment_year'] <- 'year'
  dataset$experiment_year<- as.numeric(as.factor(dataset$year))
  newYear=rbind(dataset, newYear)
  
}

alldata2 <- newYear

# makes a label for each unique year in each community, project, site
alldata2$label=as.factor(paste(alldata2$site_code, alldata2$project_name, alldata2$community_type, alldata2$experiment_year, sep="::"))

#makes a dataframe with just the experiment descriptor variables (e.g., plot_mani, factors manipulated, etc)

# DF: what is species_num from? not an R object. Should this be the sum from alldata2? 

expInfo <- alldata2 %>%
  select(site_code, project_name, community_type) %>%
  unique()%>%
  mutate(label=as.factor(paste(site_code, project_name, community_type, sep="::")))

# #import experiment ANPP and MAP data
# expSiteInfo<-read.csv(file.path(datpath, "Experiment_Info.csv"))%>%
#   mutate(site_project_comm=paste(site_code, project_name, community_type, sep='_'))
# 
# 
# 
# expSiteInfoYear <- ddply(alldata2, c('site_code', 'project_name', 'community_type', 'experiment_year', 'calendar_year', 'label'), summarise,
#                          species_num = mean(species_num))


#########################
#calculate spatial heterogeneity for each site, project, community, and year

#makes a new dataframe with just the label; here, expt.year includes all site, project, community, exp yr, trt yr designations
expt.year.list=data.frame(expt.year=levels(droplevels(alldata2$label))) 

#makes an empty dataframe
spatialHetero=data.frame(row.names=1) 

#be sure to change which columns are species columns!!
colnames(alldata2) 

#get bray curtis dissimilarity values for each site, project, community, and year
for(i in 1:length(expt.year.list$expt.year)) {
  
  #create a dataset for each unique year, experiment combination
  dataset=alldata2[alldata2$label==as.character(expt.year.list$expt.year[i]),]
  
  #subset only columns with species data in them
  species=dataset[,4:394]
  
  # species <- species[,colSums(species) > 0]
  
  #calculate bray-curtis dissimilarity
  bc=vegdist(species, method='bray',diag=F, upper=TRUE)
  
#   #calculate distances to centroid (i.e., dispersion)
#   disp=betadisper(bc, dataset$treatment, type="centroid")
  
  #dataframe of bray curtis dissimilarities among plots in each year, exp
  bc.d=as.data.frame(as.matrix(bc)) 
  
  #melt dataframe
  bc.d$names <- row.names(bc.d)

  #get matrix in long form
  #this has all combinantions 2x, which will not affect average but could affect other calcualtions like standard error b/c of different n
  bcSpatialMelt <- melt(bc.d, id='names', variable.name='plot', value.name='spatial_distance')%>%
    filter(names!=plot)

  #collecting and labeling distances
  dispersion=data.frame(data.frame(expt.year=expt.year.list$expt.year[i], 
                                 names=bcSpatialMelt$names,
                                 spatial_distance=bcSpatialMelt$spatial_distance))
    
  #pasting dispersions into the dataframe made for this analysis
  spatialHetero=rbind(dispersion, spatialHetero)
  
}

###mean spatial heterogeneity for a site in a year across all reps (i.e., mean across all bray-curtis of each rep to all other reps)
spatialHeteroInfo<- ddply(spatialHetero, c('expt.year'), summarise,
                           dispersion=mean(spatial_distance))%>%
  separate(expt.year, into=c("site_code", "project_name", "community_type", "experiment_year"), sep="::")%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))


####################################
#calculate temporal heterogeneity for each site, project, and community

#melt and reshape data to get means of all species

allMelt <- alldata2%>%
  gather(species, abundance, sp1:sp99)%>%
  tbl_df()%>%
  group_by(experiment_year, site_project_comm, species)%>%
  summarize(abundance=mean(abundance))

meanAbundance <- dcast(allMelt, experiment_year+site_project_comm ~ species)

###using codyn package
# meanAbundanceLong <- melt(meanAbundance, id=c('site_code', 'project_name', 'community_type', 'calendar_year', 'experiment_year', 'data_type', 'site_project', 'label'), variable.name='species', value.name='abundance')
# meanAbundanceLong$site_project_comm <- with(meanAbundanceLong, paste(site_code, project_name, community_type, sep='_'))

# rateChange <- rate_change(meanAbundanceLong, time.var='experiment_year', species.var='species', abundance.var='abundance', replicate.var='site_project_comm')

# turnover <- turnover(meanAbundanceLong, time.var='experiment_year', species.var='species', abundance.var='abundance', replicate.var='site_project_comm', metric='total')
# names(turnover)[names(turnover)=='total'] <- 'total_turnover'


#using converge/diverge methods
#makes a new dataframe with just the label; here, expt.year includes all site, project, community, exp yr, trt yr designations
expt.list=data.frame(expt=levels(droplevels(meanAbundance$site_project_comm))) 

#makes an empty dataframe
temporalHetero=data.frame(row.names=1) 

colnames(meanAbundance)

#get bray curtis dissimilarity values for each site, project, community, and year
for(i in 1:length(expt.list$expt)) {
  
  #create a dataset for each unique year, experiment combination
  dataset=meanAbundance[meanAbundance$site_project_comm==as.character(expt.list$expt[i]),]
    
  #need this to keep track of year
  labels=unique(dataset[,'experiment_year'])
  
  #subset only columns with species data in them
  species=dataset[,c(1, 3:393)]
  
  #get list of each experiment year
  rownames <- meanAbundance[,1]
  
  row.names(species)<-species$experiment_year
  
  #calculate bray-curtis dissimilarity
  bc=vegdist(species[,2:392], method='bray', diag=F, upper.tri=T)
    
  #dataframe of bray curtis dissimilarities among years in each exp
  bc.d=as.data.frame(as.matrix(bc))
  
  #give experiment year names to rows
  bc.d$year1 <- row.names(bc.d)
  
  #get matrix in long form
  bcMelt <- melt(bc.d, id='year1', variable.name='year2', value.name='temporal_distance')
  
  #collecting and labeling distances
  dispersion=data.frame(data.frame(expt=expt.list$expt[i], 
                                   year1=bcMelt$year1,
                                   year2=bcMelt$year2,
                                   temporal_distance=bcMelt$temporal_distance))
  
  #pasting dispersions into the dataframe made for this analysis
  temporalHetero=rbind(temporalHetero, dispersion)
  
}

temporalHetero$year1_num <- as.numeric(as.character(temporalHetero$year1))
temporalHetero$year2_num <- as.numeric(as.character(temporalHetero$year2))

temporalHetero$next_year <- temporalHetero$year2_num - temporalHetero$year1_num

temporalHeteroRealYears <- subset(temporalHetero, subset=(next_year==1))%>%
  select(expt, temporal_distance, year1_num)
names(temporalHeteroRealYears)[names(temporalHeteroRealYears)=="expt"] <- "site_project_comm"
names(temporalHeteroRealYears)[names(temporalHeteroRealYears)=="year1_num"] <- "experiment_year"

  
#####################################
#merge spatial and temporal

# #turnover
# spaceTurnover <- merge(spatialHeteroInfo, turnover, by=c('site_project_comm', 'experiment_year'))
# 
# #rate change (mean spatial through time)
# spaceRateChange <- merge(spatialHeteroInfo, rateChange, by=c('site_project_comm'))
# spaceRateChangeMean <- ddply(spaceRateChange, c('site_project_comm', 'site_code', 'project_name', 'community_type'), summarise,
#                              rate_change=mean(rate_change),
#                              dispersion_mean=mean(dispersion))

#converge/diverge method
spaceTime <- merge(spatialHeteroInfo, temporalHeteroRealYears, by=c('site_project_comm', 'experiment_year'), all=T)

#########################
#make some figures!

# #turnover
# ggplot(data=spaceTurnover, aes(x=dispersion, y=total_turnover, colour=site_project_comm)) +
#   geom_point(pch=1) +
#   geom_smooth(method=lm, se=F)
# 
# #rate change
# ggplot(data=spaceRateChangeMean, aes(x=dispersion_mean, y=rate_change)) +
#   geom_point(pch=1)

#import experiment info

#getting rid of hay's NA's (adds extra lines in temporal step)
spaceTimeSubset <- na.omit(spaceTime)

expInfo <- read.csv('~/Dropbox/CoDyn/R files/11_06_2015_v7/exptInfo_ecosystem type.csv')

spaceTimeInfo <- merge(spaceTimeSubset, expInfo, by=c('site_code', "project_name"), all=T)


#converge/diverge method
ggplot(data=spaceTimeInfo, aes(x=dispersion, y=temporal_distance, colour=system)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T))

ggplot(data=spaceTimeInfo, aes(x=dispersion, y=temporal_distance, colour=broad_ecosystem_type)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F, size=1) +
  geom_smooth(aes(group=1, se=T), method=lm, colour='black', size=3)

ggplot(data=spaceTimeInfo, aes(x=dispersion, y=temporal_distance, colour=site_project_comm)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~broad_ecosystem_type)

ggplot(data=spaceTimeInfo, aes(x=dispersion, y=temporal_distance, colour=trophic_level)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~broad_ecosystem_type)

ggplot(data=spaceTimeInfo, aes(x=dispersion, y=temporal_distance, colour=succession)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~broad_ecosystem_type)

ggplot(data=spaceTimeInfo, aes(x=dispersion, y=temporal_distance, colour=system)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~succession)

ggplot(data=spaceTimeInfo, aes(x=dispersion, y=temporal_distance, colour=lifespan)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~broad_ecosystem_type)

ggplot(data=spaceTimeInfo, aes(x=dispersion, y=temporal_distance, colour=system)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~trophic_level)

ggplot(data=spaceTimeInfo, aes(x=dispersion, y=temporal_distance, colour=site_project_comm)) +
  geom_point(size=3) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~site_code)

sev <- subset(spaceTimeInfo, subset=(site_code=='SEV'))
ggplot(data=sev, aes(x=dispersion, y=temporal_distance, colour=site_project_comm)) +
  geom_point(size=3) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T))

knz <- subset(spaceTimeInfo2, subset=(site_code=='KNZ'))
ggplot(data=knz, aes(x=dispersion, y=temporal_distance, colour=site_project_comm)) +
  geom_point(size=3) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T))


################################
#evenness
colnames(meanAbundance)

evenness=data.frame(row.names=1) 

meanAbundance$site_project_comm_year<-with(meanAbundance, paste(site_project_comm, experiment_year, sep="_"))

expt.list.year=data.frame(expt=unique(meanAbundance$site_project_comm_year)) 

for(i in 1:length(expt.list.year$expt)) {
  
  dataset=meanAbundance[meanAbundance$site_project_comm_year==as.character(expt.list.year$expt[i]),]
  species <- dataset[,3:393]
  H <- as.data.frame(diversity(species))
  S <- as.data.frame(specnumber(species))
  J <- as.data.frame(H/log(S))
  dom<-max(species)
  
  #collecting and labeling distances
  even=data.frame(data.frame(expt.year=expt.list.year$expt[i],
                             J=J$'diversity(species)',
                             S=S$'specnumber(species)',
                             H=H$'diversity(species)',
                            dom=dom
                             ))
  
  evenness=rbind(evenness, even)

}

spaceTimeInfo$label<-with(spaceTimeInfo, paste(site_code, project_name, community_type, experiment_year, sep="_"))
names(evenness)[names(evenness)=='expt.year'] <- 'label'

spaceTimeEven <- merge(spaceTimeInfo, evenness, by='label')
spaceTimeEven$log_S <- log(spaceTimeEven$S +1)

##graphs of relationships bewteen dispersion and measures of evenness
with(spaceTimeEven, plot(dispersion, J))
with(spaceTimeEven, cor.test(dispersion, J))
with(spaceTimeEven, plot(dispersion, dom))
with(spaceTimeEven, cor.test(dispersion, dom))
with(spaceTimeEven, plot(J, dom))
with(spaceTimeEven, cor.test(J, dom))
with(spaceTimeEven, plot(J, S))
with(spaceTimeEven, cor.test(J, S))
with(spaceTimeEven, plot(S, dom))
with(spaceTimeEven, cor.test(S, dom))

write.csv(spaceTimeEven, '~/Dropbox/CoDyn/R files/11_06_2015_v7/spatial_temporal_heterogeneity_diversity.csv')

ggplot(data=spaceTimeEven, aes(x=dispersion, y=temporal_distance, colour=J)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  scale_color_gradient(low='blue', high='red') +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~broad_ecosystem_type)

ggplot(data=spaceTimeEven, aes(x=dispersion, y=temporal_distance, colour=log_S)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  scale_color_gradient(low='blue', high='red') +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~broad_ecosystem_type)

# ggplot(data=spaceTimeEven, aes(x=dispersion, y=temporal_distance, colour=H)) +
#   geom_point(size=4) +
#   geom_smooth(method=lm, se=F) +
#   scale_color_gradient(low='blue', high='red') +
#   geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
#   facet_wrap(~broad_ecosystem_type)


ggplot(data=spaceTimeEven, aes(x=dispersion, y=temporal_distance, colour=ANPP..g.m2.)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  scale_color_gradient(low='blue', high='red') +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~broad_ecosystem_type)

ggplot(data=spaceTimeEven, aes(x=dispersion, y=temporal_distance, colour=MAP_mm)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  scale_color_gradient(low='blue', high='red') +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~system)

ggplot(data=spaceTimeEven, aes(x=dispersion, y=temporal_distance, colour=temp_C)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  scale_color_gradient(low='blue', high='red') +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~system)

#######
#######
####### WITH PRESENT ABSENT DATA
#######
#######
#######

alldata4 <- read.csv('~/Dropbox/CoDyn/R files/11_06_2015_v7/relative cover_nceas and converge_12012015_cleaned.csv')

presabs<-alldata4%>%
  gather(species, abund, sp1:sp99)%>%
  filter(abund>0)%>%
  mutate(present=1)
  
#make year as factor
expt.list=data.frame(expt=levels(droplevels(presabs$site_project_comm)))

newYear <- data.frame(row.names=1)

for(i in 1:length(expt.list$expt)) {
  
  dataset=presabs[presabs$site_project_comm==as.character(expt.list$expt[i]),]
  names(dataset)[names(dataset)=='experiment_year'] <- 'year'
  dataset$experiment_year<- as.numeric(as.factor(dataset$year))
  newYear=rbind(dataset, newYear)
  
}

alldata2 <- newYear

# makes a label for each unique year in each community, project, site
alldata2$label=as.factor(paste(alldata2$site_code, alldata2$project_name, alldata2$community_type, alldata2$experiment_year, sep="::"))

#makes a dataframe with just the experiment descriptor variables (e.g., plot_mani, factors manipulated, etc)

# DF: what is species_num from? not an R object. Should this be the sum from alldata2? 

expInfo <- alldata2 %>%
  select(site_code, project_name, community_type) %>%
  unique()%>%
  mutate(label=as.factor(paste(site_code, project_name, community_type, sep="::")))


presabs_wide<-alldata2%>%
  select(-X, -abund)%>%
  spread(species, present, fill=0)

#########################
#calculate spatial heterogeneity for each site, project, community, and year

#makes a new dataframe with just the label; here, expt.year includes all site, project, community, exp yr, trt yr designations
expt.year.list=data.frame(expt.year=levels(droplevels(presabs_wide$label))) 

#makes an empty dataframe
spatialHetero=data.frame(row.names=1) 

#be sure to change which columns are species columns!!
colnames(presabs_wide) 

#get bray curtis dissimilarity values for each site, project, community, and year
for(i in 1:length(expt.year.list$expt.year)) {
  
  #create a dataset for each unique year, experiment combination
  dataset=presabs_wide[presabs_wide$label==as.character(expt.year.list$expt.year[i]),]
  
  #subset only columns with species data in them
  species=dataset[,10:400]
  
  # species <- species[,colSums(species) > 0]
  
  #calculate bray-curtis dissimilarity
  jac=vegdist(species, method='jaccard',diag=F, upper=TRUE)
  
  #   #calculate distances to centroid (i.e., dispersion)
  #   disp=betadisper(bc, dataset$treatment, type="centroid")
  
  #dataframe of bray curtis dissimilarities among plots in each year, exp
  jac.d=as.data.frame(as.matrix(jac)) 
  
  #melt dataframe
  jac.d$names <- row.names(jac.d)
  
  #get matrix in long form
  #this has all combinantions 2x, which will not affect average but could affect other calcualtions like standard error b/c of different n
  jacSpatialMelt <- melt(jac.d, id='names', variable.name='plot', value.name='spatial_distance')%>%
    filter(names!=plot)
  
  #collecting and labeling distances
  dispersion=data.frame(data.frame(expt.year=expt.year.list$expt.year[i], 
                                   names=jacSpatialMelt$names,
                                   spatial_distance=jacSpatialMelt$spatial_distance))
  
  #pasting dispersions into the dataframe made for this analysis
  spatialHetero=rbind(dispersion, spatialHetero)
  
}

###mean spatial heterogeneity for a site in a year across all reps (i.e., mean across all bray-curtis of each rep to all other reps)
spatialHeteroInfo<- ddply(spatialHetero, c('expt.year'), summarise,
                          dispersion=mean(spatial_distance))%>%
  separate(expt.year, into=c("site_code", "project_name", "community_type", "experiment_year"), sep="::")%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))


####################################
#calculate temporal heterogeneity for each site, project, and community

#melt and reshape data to get means of all species

allMelt <- presabs_wide%>%
  gather(species, present, sp1:sp99)

allMelt_mean<-aggregate(present~experiment_year+site_project_comm+species, sum, data=allMelt)

allMelt2<-allMelt_mean%>%
  filter(present>0)%>%
  mutate(pres=1)%>%
  select(-present)%>%
  spread(species, pres, fill=0)
  

###using codyn package
# meanAbundanceLong <- melt(meanAbundance, id=c('site_code', 'project_name', 'community_type', 'calendar_year', 'experiment_year', 'data_type', 'site_project', 'label'), variable.name='species', value.name='abundance')
# meanAbundanceLong$site_project_comm <- with(meanAbundanceLong, paste(site_code, project_name, community_type, sep='_'))

# rateChange <- rate_change(meanAbundanceLong, time.var='experiment_year', species.var='species', abundance.var='abundance', replicate.var='site_project_comm')

# turnover <- turnover(meanAbundanceLong, time.var='experiment_year', species.var='species', abundance.var='abundance', replicate.var='site_project_comm', metric='total')
# names(turnover)[names(turnover)=='total'] <- 'total_turnover'


#using converge/diverge methods
#makes a new dataframe with just the label; here, expt.year includes all site, project, community, exp yr, trt yr designations
expt.list=data.frame(expt=levels(droplevels(allMelt2$site_project_comm))) 

#makes an empty dataframe
temporalHetero=data.frame(row.names=1) 

colnames(allMelt2)

#get bray curtis dissimilarity values for each site, project, community, and year
for(i in 1:length(expt.list$expt)) {
  
  #create a dataset for each unique year, experiment combination
  dataset=allMelt2[allMelt2$site_project_comm==as.character(expt.list$expt[i]),]
  
  #need this to keep track of year
  labels=unique(dataset[,'experiment_year'])
  
  #subset only columns with species data in them
  species=dataset[,c(1,3:393)]
  
  #get list of each experiment year
  rownames <- allMelt2[,1]
  
  row.names(species)<-species$experiment_year
  
  #calculate bray-curtis dissimilarity
  jac=vegdist(species[,2:392], method='jaccard', diag=F, upper.tri=T)
  
  #dataframe of bray curtis dissimilarities among years in each exp
  jac.d=as.data.frame(as.matrix(jac))
  
  #give experiment year names to rows
  jac.d$year1 <- row.names(jac.d)
  
  #get matrix in long form
  jacMelt <- melt(jac.d, id='year1', variable.name='year2', value.name='temporal_distance')
  
  #collecting and labeling distances
  dispersion=data.frame(data.frame(expt=expt.list$expt[i], 
                                   year1=jacMelt$year1,
                                   year2=jacMelt$year2,
                                   temporal_distance=jacMelt$temporal_distance))
  
  #pasting dispersions into the dataframe made for this analysis
  temporalHetero=rbind(temporalHetero, dispersion)
  
}

temporalHetero$year1_num <- as.numeric(as.character(temporalHetero$year1))
temporalHetero$year2_num <- as.numeric(as.character(temporalHetero$year2))

temporalHetero$next_year <- temporalHetero$year2_num - temporalHetero$year1_num

temporalHeteroRealYears <- subset(temporalHetero, subset=(next_year==1))%>%
  select(expt, temporal_distance, year1_num)
names(temporalHeteroRealYears)[names(temporalHeteroRealYears)=="expt"] <- "site_project_comm"
names(temporalHeteroRealYears)[names(temporalHeteroRealYears)=="year1_num"] <- "experiment_year"


#####################################
#merge spatial and temporal

# #turnover
# spaceTurnover <- merge(spatialHeteroInfo, turnover, by=c('site_project_comm', 'experiment_year'))
# 
# #rate change (mean spatial through time)
# spaceRateChange <- merge(spatialHeteroInfo, rateChange, by=c('site_project_comm'))
# spaceRateChangeMean <- ddply(spaceRateChange, c('site_project_comm', 'site_code', 'project_name', 'community_type'), summarise,
#                              rate_change=mean(rate_change),
#                              dispersion_mean=mean(dispersion))

#converge/diverge method
spaceTime <- merge(spatialHeteroInfo, temporalHeteroRealYears, by=c('site_project_comm', 'experiment_year'), all=T)

spaceTime2<-spaceTime%>%
  mutate(jac_disp=dispersion,
         jac_tempdist=temporal_distance)%>%
  select(-dispersion, -temporal_distance)

old<-read.csv('~/Dropbox/CoDyn/R files/11_06_2015_v7/spatial_temporal_heterogeneity_diversity.csv')

presabsResults<-merge(spaceTime2, old, by=c("site_code","project_name","community_type","site_project_comm","experiment_year"))


write.csv(presabsResults,'~/Dropbox/CoDyn/R files/11_06_2015_v7/PresentAbsent_spatial_temporal_heterogeneity_diversity.csv')
