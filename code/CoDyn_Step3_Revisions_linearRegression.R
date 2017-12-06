### Run regressions within each site ##
## Extract and visualize slopes colored by p vals ##
library(tidyverse)
#dataout <- read.csv(file.path(datpath, 'SpatioTemp_testStatistics_20170208.csv'))

dat <- dataout %>%
  mutate(sitesystem0 = paste(site_code, project_name,  community_type,  sep = "_"),
         sitesystem = paste(sitesystem0, System, sep = "-"))

out <- data.frame(cbind(sitesystem = as.character(), slope = as.numeric(),
                        pval = as.numeric(), Rsq =as.numeric(), RsqAdj = as.numeric()))
sitesystems <- unique(dat$sitesystem)

for (i in 1:length(sitesystems)){
  subber <- subset(dat, dat$sitesystem == sitesystems[i])
  l <- lm(Temporal_heterogeneity ~ Spatial_heterogeneity, data = subber)
  sitesystem <- sitesystems[i]
  slope <- summary(l)$coefficients[2,1]
  pval <- summary(l)$coefficients[2,4]
  Rsq <- summary(l)$r.squared
  RsqAdj <- summary(l)$adj.r.squared
  subout <- data.frame(sitesystem, slope, pval, Rsq, RsqAdj)
  out <- rbind(out, subout)
}
  
output <- out %>% 
  tbl_df() %>%
  separate(sitesystem, c("site", "system"), sep = "-") %>%
  mutate(sig = ifelse(pval <= 0.05, "yes", "no"))
  
pdf("Slope_bysite.pdf", width = 12, height = 8)
ggplot(output, aes(x=slope, fill= sig)) + geom_histogram() + facet_wrap(~system)
dev.off()
numsig <- output %>%
  group_by(sig) %>%
  summarise(totcount = n())

ggplot(randomout_forhist, aes(x=Spatial_heterogeneity)) + geom_histogram() +
  facet_wrap(~System2) + labs(x="Slope", y="Count")


### Make a histogram of the slopes for each site for model 1
siteskey <- dataout %>%
  select(site_code, project_name, community_type, System) %>%
  unique() %>%
  mutate(site_project = paste(community_type, project_name, site_code,  sep = ":"))
randomout <-as.data.frame(ranef(m1)$community_type)
randomout <- randomout %>%
  mutate(site_project = row.names(randomout))
randomout_forhist <- left_join(siteskey, randomout) %>%
  mutate(System2 = "Aquatic",
         System2 = ifelse(System == "terrestrial", "Terrestrial", System2))
#pdf("CoDyn_randomeffect_slopes_aquaticvterrestrial_m1.pdf", width = 10, height = 6)
ggplot(randomout_forhist, aes(x=Spatial_heterogeneity)) + geom_histogram() +
  facet_wrap(~System2) + labs(x="Slope", y="Count")
#dev.off()
