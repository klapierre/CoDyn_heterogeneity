library(vegan)
library(reshape2)
library(ggplot2)
library(gtools)
library(plyr)
library(grid)
library(lme4)
library(codyn)

# If don't have codyn, try this:
# library(drat)
# addRepo("NCEAS")
# install.packages("codyn")
 

# setwd("C:\\Users\\Kim\\Dropbox\\working groups\\community dynamics working group\\CoDyn\\R files\\10_08_2015_v6")

datpath = "~/Dropbox/CoDyn/R files/10_08_2015_v6/CoDyn_heterogeneity" 

theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank())

####################################################################################
####################################################################################
####################################################################################

#read in the species composition dataset
nceas <- read.csv(file.path(datpath, 'NewData_NCEAS.csv'))
names(nceas)[names(nceas)=='plot'] <- 'plot_id'
nceas2 <- subset(nceas, subset=(project_name!='NTL_PHYTOS_CORE'))
drop <- c('broad_ecosystem_type', 'system')
nceas3 <- nceas2[,!colnames(nceas2) %in% drop]

#converge datasets - remove these datasets because they are too short or we are not allowed to use them
converge <- read.csv(file.path(datpath, "all_relcov2_08062015.csv"))
finalYear <- aggregate(converge["treatment_year"], by=converge[c("site_code", "project_name", "community_type")], FUN=max)
eightYear <- subset(finalYear, subset=(treatment_year>=8))
drop <- c('treatment_year')
eightYear <- eightYear[,!colnames(eightYear) %in% drop]
eightYearData <- merge(eightYear, converge)
eightYearControl <- subset(eightYearData, subset=(plot_mani==0))
eightYearControl$site_project <- paste(eightYearControl$site_code, eightYearControl$project_name, sep='_')
eightYearControl$site_project_comm <- with(eightYearControl, paste(site_code, project_name, community_type, sep='_'))
converge2a <- subset(eightYearControl, subset=(site_project_comm!='CDR_e001_D'))
converge2 <- subset(converge2a, subset=(site_project!='ANG_watering' & site_project!='CAU_RMAPC' & site_project!='dcgs_gap' & site_project!='Finse_WarmNut' & site_project!='ORNL_FACE' & site_project!='ARC_MNT' & site_project!='ARC_MAT2_1996' & site_project!='SGS_ESA' & site_project!='KNZ_BGP' & site_project!='NWT_246Nfert' & site_project!='KNZ_RHPs' & site_project!='CDR_BioCON' & site_project!='KUFS_E6' & site_project!='China_Qiang' & site_project!='ASGA_Exp1' & site_project!='ARC_MAT2' & site_project!='ASGA_clonal' & site_project!='DL_NSFC' & site_project!='CDR_e002'))
drop <- c('site_project', 'site_project_comm')
converge3 <- converge2[,!colnames(converge2) %in% drop]


#merge nceas data and converge data
alldata <- smartbind(nceas3, converge3)
alldata[is.na(alldata)] <- 0
alldata$site_project <- with(alldata, paste(site_code, project_name, sep='_'))
alldata$site_project_comm <- with(alldata, paste(site_code, project_name, community_type, sep='_'))


#get relative cover

#make a unique ID for each plot in the final dataset by combining id, project name, site name
alldata$unid2<-paste(alldata$plot_id, alldata$community_type, alldata$project_name, alldata$site_code, alldata$experiment_year, sep="_")

#make a dataframe with only the informational data
information<-alldata[,c(1:7,395:404)]

#make a dataframe with only the species data
species<-alldata[,c(8:394,404)]

#transpose to get species in rows
melt<-melt(species,id="unid2")
head(melt)

#get sum of cover for each plot
sumcover<-aggregate(value~unid2, FUN="sum", data=melt)

#merge species data and sum of cover by plot
relcov<-merge(sumcover, melt, by="unid2")

#get only plots with some cover > 0 (for some reason lots of plots have cover=0)
relcov_a<-subset(relcov, subset=(value.x!=0))
relcov_b<-subset(relcov, subset=(value.x==0))

#calculate relative cover for each species in each plot
relcov_a$relcov<-relcov_a$value.y/relcov_a$value.x

#delete the old cover data and sum of cover data
relcov2a<-relcov_a[,-c(2,4)]

# relcov2 <- aggregate(relcov ~ unid2 + variable, mean, data=relcov2a)

#re-transpose to get species as columns
relcov3<-dcast(relcov2a, unid2 ~ variable)

#merge the species data back together with the informational data
relcov4<-merge(information, relcov3, by="unid2")

write.csv(relcov4, 'relative cover_nceas and converge_10082015.csv')
alldata3 <- read.csv('relative cover_nceas and converge_10082015.csv')

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
expInfo <- ddply(alldata2, c("site_code", "project_name", "community_type", "data_type"), summarise,
                 species_num=mean(species_num))
names(expInfo) <- sub("^dist$", "disturbance", names(expInfo))
expInfo$label=as.factor(paste(expInfo$site_code, expInfo$project_name, expInfo$community_type, sep="::"))

#import experiment ANPP and MAP data
expSiteInfo<-read.csv("Experiment_Info.csv")
expSiteInfo$site_project_comm <- with(expSiteInfo, paste(site_code, project_name, community_type, sep='_'))

expSiteInfoYear <- ddply(alldata2, c('site_code', 'project_name', 'community_type', 'experiment_year', 'calendar_year', 'label'), summarise,
                         species_num = mean(species_num))


#########################
#calculate spatial heterogeneity for each site, project, community, and year

#these datasets are redundant
dataSubseta <- subset(alldata2, subset=(site_project_comm!='NTL_NTL_PHYTOS_JULY_0' & site_project_comm!='NTL_core_annual_0' & site_project_comm!='NTL_core_spring_0' & site_project_comm!='NTL_core_summer_0' & site_project_comm!='NTL_core_fall_0' & site_project_comm!='NTL_core_winter_0'))

#this datasets are broken
dataSubset <- subset(dataSubseta, subset=(site_project_comm!='OND_OND_INVERTS_0'))

#makes a new dataframe with just the label; here, expt.year includes all site, project, community, exp yr, trt yr designations
expt.year.list=data.frame(expt.year=levels(droplevels(dataSubset$label))) 

#makes an empty dataframe
spatialHetero=data.frame(row.names=1) 

#be sure to change which columns are species columns!!
colnames(dataSubset) 

#get bray curtis dissimilarity values for each site, project, community, and year
for(i in 1:length(expt.year.list$expt.year)) {
  
  #create a dataset for each unique year, experiment combination
  dataset=dataSubset[dataSubset$label==as.character(expt.year.list$expt.year[i]),]
  
  #subset only columns with species data in them
  species=dataset[,19:405]
  
  #calculate bray-curtis dissimilarity
  bc=vegdist(species, method='bray')
  
#   #calculate distances to centroid (i.e., dispersion)
#   disp=betadisper(bc, dataset$treatment, type="centroid")
  
  #dataframe of bray curtis dissimilarities among plots in each year, exp
  bc.d=as.data.frame(as.matrix(bc)) 
  
  #melt dataframe
  bc.d$names <- row.names(bc.d)

  #get matrix in long form
  bcSpatialMelt <- melt(bc.d, id='names', variable.name='plot', value.name='spatial_distance')

  bcSpatialMelt2 <- subset(bcSpatialMelt, subset=(spatial_distance!=0))

  #collecting and labeling distances
  dispersion=data.frame(data.frame(expt.year=expt.year.list$expt.year[i], 
                                 names=bcSpatialMelt2$names,
                                 spatial_distance=bcSpatialMelt2$spatial_distance))
    
  #pasting dispersions into the dataframe made for this analysis
  spatialHetero=rbind(dispersion, spatialHetero)
  
}

###mean spatial heterogeneity for a site in a year across all reps (i.e., mean across all bray-curtis of each rep to all other reps)
spatialHeteroMean <- ddply(spatialHetero, c('expt.year'), summarise,
                           dispersion=mean(spatial_distance))
names(spatialHeteroMean)[names(spatialHeteroMean)=="expt.year"] <- "label"
spatialHeteroInfo <- merge(spatialHeteroMean, expSiteInfoYear, by=c('label'))
spatialHeteroInfo$site_project_comm <- with(spatialHeteroInfo, paste(site_code, project_name, community_type, sep='_'))


####################################
#calculate temporal heterogeneity for each site, project, and community

#melt and reshape data to get means of all species
drop <- c('X', 'unid', 'id', 'plot_mani', 'treatment_year', 'treatment', 'species_num', 'site_project_comm', 'calendar_year', 'unid2', 'X.1')
alldata3 <- dataSubset[,!colnames(dataSubset) %in% drop]
allMelt <- melt(alldata3, id=c('site_code', 'project_name', 'community_type', 'experiment_year', 'plot_id', 'data_type', 'site_project', 'label'))
meanAbundance <- dcast(allMelt, site_code + project_name + community_type + experiment_year + data_type + site_project + label ~ variable, mean)

###using codyn package
# meanAbundanceLong <- melt(meanAbundance, id=c('site_code', 'project_name', 'community_type', 'calendar_year', 'experiment_year', 'data_type', 'site_project', 'label'), variable.name='species', value.name='abundance')
# meanAbundanceLong$site_project_comm <- with(meanAbundanceLong, paste(site_code, project_name, community_type, sep='_'))

# rateChange <- rate_change(meanAbundanceLong, time.var='experiment_year', species.var='species', abundance.var='abundance', replicate.var='site_project_comm')

# turnover <- turnover(meanAbundanceLong, time.var='experiment_year', species.var='species', abundance.var='abundance', replicate.var='site_project_comm', metric='total')
# names(turnover)[names(turnover)=='total'] <- 'total_turnover'


#using converge/diverge methods
#make a label for each unique site, project, community
meanAbundance$expt=as.factor(paste(meanAbundance$site_code, meanAbundance$project_name, meanAbundance$community_type, sep="::"))

#makes a new dataframe with just the label; here, expt.year includes all site, project, community, exp yr, trt yr designations
meanAbundance$site_project_comm <- as.factor(with(meanAbundance, paste(site_code, project_name, community_type, sep='_')))
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
  species=dataset[,c(7, 9:395)]
  
  #get list of each experiment year
  rownames <- meanAbundance[,7]
  
  row.names(species)<-species$experiment_year
  
  #calculate bray-curtis dissimilarity
  bc=vegdist(species[,2:388], method='bray', diag=F, upper.tri=T)
    
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

temporalHeteroRealYears <- subset(temporalHetero, subset=(next_year==1))

names(temporalHeteroRealYears)[names(temporalHeteroRealYears)=="expt"] <- "site_project_comm"
names(temporalHeteroRealYears)[names(temporalHeteroRealYears)=='year1'] <- 'time_step'
temporalHeteroRealYears$experiment_year <- temporalHeteroRealYears$year1_num

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
spaceTimeSubset <- subset(spaceTime, subset=(site_code!='NA'))

expInfo <- read.csv('exptInfo_ecosystem type.csv')

spaceTimeInfo <- merge(spaceTimeSubset, expInfo, by=c('site_code'), all=T)

spaceTimeInfo2 <- spaceTimeInfo[complete.cases(spaceTimeInfo$site_project_comm),]


#converge/diverge method
ggplot(data=spaceTimeInfo2, aes(x=dispersion, y=temporal_distance, colour=system)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T))

ggplot(data=spaceTimeInfo2, aes(x=dispersion, y=temporal_distance, colour=broad_ecosystem_type)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F, size=1) +
  geom_smooth(aes(group=1, se=T), method=lm, colour='black', size=3)

ggplot(data=spaceTimeInfo2, aes(x=dispersion, y=temporal_distance, colour=site_project_comm)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~broad_ecosystem_type)

ggplot(data=spaceTimeInfo2, aes(x=dispersion, y=temporal_distance, colour=trophic_level..consumer..primary.)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~broad_ecosystem_type)

ggplot(data=spaceTimeInfo2, aes(x=dispersion, y=temporal_distance, colour=succession)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~broad_ecosystem_type)

ggplot(data=spaceTimeInfo2, aes(x=dispersion, y=temporal_distance, colour=system)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~succession)

ggplot(data=spaceTimeInfo2, aes(x=dispersion, y=temporal_distance, colour=lifespan..subannual..annual.or.longer.)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~broad_ecosystem_type)

ggplot(data=spaceTimeInfo2, aes(x=dispersion, y=temporal_distance, colour=system)) +
  geom_point(size=4) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~trophic_level..consumer..primary.)

ggplot(data=spaceTimeInfo2, aes(x=dispersion, y=temporal_distance, colour=site_project_comm)) +
  geom_point(size=3) +
  geom_smooth(method=lm, se=F) +
  geom_smooth(method=lm, colour='black', size=2, aes(group=1, se=T)) +
  facet_wrap(~site_code)

sev <- subset(spaceTimeInfo2, subset=(site_code=='SEV'))
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

for(i in 1:length(expt.year.list$expt.year)) {
  
  dataset=meanAbundance[meanAbundance$label==as.character(expt.year.list$expt.year[i]),]
  species <- dataset[,9:395]
  H <- as.data.frame(diversity(species))
  S <- as.data.frame(specnumber(species))
  J <- as.data.frame(H/log(S))
  
  #collecting and labeling distances
  even=data.frame(data.frame(expt.year=expt.year.list$expt.year[i],
                             J=J$'diversity(species)',
                             S=S$'specnumber(species)',
                             H=H$'diversity(species)'
                             ))
  
  evenness=rbind(evenness, even)

}

names(evenness)[names(evenness)=='expt.year'] <- 'label'

spaceTimeEven <- merge(spaceTimeInfo2, evenness, by='label')
spaceTimeEven$log_S <- log(spaceTimeEven$S +1)

write.csv(spaceTimeEven, 'spatial_temporal_heterogeneity_diversity.csv')

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


