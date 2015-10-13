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

         
