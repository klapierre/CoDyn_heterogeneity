library(tidyverse)
library(codyn)
library(vegan)
library(Kendall)
library(gridExtra)
library(reldist)
library(grid)
library(gtable)

codyndat1<-read.csv('C:\\Users\\megha\\Dropbox\\CoDyn\\R Files\\11_06_2015_v7\\relative cover_nceas and converge_12012015_cleaned.csv')%>%
  gather(species, abundance, sp1:sp99)%>%
  filter(site_code!="MISS")%>%
  select(-X)%>%
 
miss<-read.csv('C:\\Users\\megha\\Dropbox\\CoDyn\\R Files\\11_06_2015_v7\\miss_fish_oct2017.csv')%>%
  select(-broad_ecosystem_type, -system, -data_type)%>%
  mutate(sitesubplot=paste(site_code, project_name, plot_id, community_type, sep="_"), 
         site_project_comm=paste(site_code, project_name, community_type, sep='_'),
         community_type=as.factor(community_type))

codyndat<-rbind(codyndat1, miss)

codyndat_info<-read.csv("C:\\Users\\megha\\Dropbox\\CoDyn\\R Files\\11_06_2015_v7\\siteinfo_key.csv")%>%
  filter(site_project_comm!="")

###CLEANING CODYN DATASET
#restrict to species that are present in an experiment
splist<-codyndat%>%
  group_by(site_code, project_name, community_type, species)%>%
  summarize(present=sum(abundance))%>%
  filter(present!=0)%>%
  select(-present)

#merge back and will drop species that do not exist in a dataset
codyndat_clean<-merge(codyndat, splist, by=c("site_code","project_name","community_type","species"))%>%
  select(-sitesubplot, -site_code, -project_name, -community_type)%>%
  mutate(id=paste(site_project_comm, plot_id, sep="::"))

#function to calculate richness
S<-function(x){
  x1<-x[x!=0]
  length(x1)
}

#function to calculate EQ evenness from Smith and Wilson 1996
#' @x the vector of abundances of each species
#' if all abundances are equal it returns a 1
E_q<-function(x){
  x1<-x[x!=0]
  if (length(x1)==1) {
    return(NA)
  }
  if (abs(max(x1) - min(x1)) < .Machine$double.eps^0.5) {##bad idea to test for zero, so this is basically doing the same thing testing for a very small number
    return(1)
  }
  r<-rank(x1, ties.method = "average")
  r_scale<-r/max(r)
  x_log<-log(x1)
  fit<-lm(r_scale~x_log)
  b<-fit$coefficients[[2]]
  2/pi*atan(b)
}


#function to calculate E1/D (inverse of Simpson's) from Smith and Wilson 1996
#' @S the number of species in the sample
#' @x the vector of abundances of each species
#' @N the total abundance
#' @p the vector of relative abundances of each species
E_simp<-function(x, S=length(x[x!=0]), N=sum(x[x!=0]), ps=x[x!=0]/N, p2=ps*ps ){
  D<-sum(p2)
  (1/D)/S
}

#calculating gini coefficeint using the gini function in the reldist package
#' @x the vector of abundances of each species
#' this tive the inverse of other measures of evenness??
Gini<-function(x){
  x1<-x[x!=0]
  1-reldist::gini(x1)
}

##need to get this working with NAs for mean calculations
codyndat_diversity <- group_by(codyndat_clean, site_project_comm, experiment_year, plot_id) %>% 
  summarize(S=S(abundance),
            E_q=E_q(abundance),
            Gini=Gini(abundance),
            E_simp=E_simp(abundance))%>%
  tbl_df()%>%
  group_by(site_project_comm, experiment_year)%>%
  summarize(S=mean(S),
            E_Q=mean(E_q, na.rm=T),
            Gini=mean(Gini),
            E_simp=mean(E_simp))

#drop MISS
codyndat_diversity_nomiss<-codyndat_diversity%>%
  filter(site_project_comm!="MISS_FISH_0")

#plot this
plot(codyndat_diversity$S, codyndat_diversity$E_Q)
plot(codyndat_diversity_nomiss$S, codyndat_diversity_nomiss$E_Q)

#look into whether this is still a problem at the plot level
codyndat_diversity_plot <- group_by(codyndat_clean, site_project_comm, experiment_year, plot_id) %>% 
  summarize(S=S(abundance),
            E_q=E_q(abundance),
            Gini=Gini(abundance),
            E_simp=E_simp(abundance))

codyndat_diversity_nomiss_plot<-codyndat_diversity_plot%>%
  filter(site_project_comm!="MISS_FISH_0")

#plot this
plot(codyndat_diversity_plot$S, codyndat_diversity_plot$E_q)
plot(codyndat_diversity_nomiss_plot$S, codyndat_diversity_nomiss_plot$E_q)

#look at fish only
alldat<-merge(codyndat_diversity, codyndat_info, by="site_project_comm")%>%
  filter(taxa=="fish")

ggplot(data=alldat, aes(x=S, y=E_Q, color=site_project_comm))+
  geom_point()
