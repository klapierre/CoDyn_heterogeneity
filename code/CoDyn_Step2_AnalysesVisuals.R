#########################################################

##Load packages
library(dplyr)
library(nlme)
library(lme4) # for lmer
library(sjPlot) # for plotting random effects of mixed effects models. install.packages('sjPlot',dep=T)
library(ggplot2)
library(xtable) # for making LaTeX formatted tables
library(gridExtra)
library(grid)
library(Hmisc)
library(MuMIn)

## Set ggplot2 theme
theme_set(theme_bw())
theme_update( panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              strip.background = element_blank(),
              text = element_text(size = 24),
              strip.text= element_text(size = 24), 
              axis.text = element_text(size = 14))


#######################################################
## DATAFRAMES FROM CoDyn_Step1_TestStatistics.R #######
## Format dataframes for publication exports #########

#dataout <- read.csv(file.path(datpath, 'SpatioTemp_testStatistics_20170208.csv'))

## Format names in dataout to make them publication-ready
dataout$lifespan2<-ifelse(dataout$lifespan=="longer","long-lived",as.character(dataout$lifespan))
names(dataout)<- c("siteyr", "site_project_comm","experiment_year","site_code","project_name","community_type","location","Country","Continent","Lat","Long","MAP","Plot_size","Spatial_extent","Successional","Lifespan1","Trophic_level","Taxa","ANPP","Broad_ecosystem_type","Number_plots","MAT","Dataset_legnth","Time_step","Spatial_richness","Spatial_ShannonD","Spatial_ShannonJ","Spatial_dominance","Spatial_evenness","Spatial_heterogeneity","Temporal_heterogeneity","System","Lifespan")

################################################################
# MODELS TO LINK TEMPORAL VARIABILITY WITH EXPLANATORY VARIABLES
################################################################

#############################
##### Model 1 # spatial only

# create the null model
m.null <- lmer(Temporal_heterogeneity ~ 1 +
                 (Spatial_heterogeneity | site_code / project_name / community_type),
               data = dataout)

# create the actual model
m1 <- lmer(Temporal_heterogeneity ~ 1 +
             Spatial_heterogeneity +
             (Spatial_heterogeneity | site_code / project_name / community_type),
           data = dataout)


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
#pdf("CoDyn_randomeffect_slopes_aquaticvterrestrial_m1_v2_color.pdf", width = 10, height = 6)
ggplot(randomout_forhist, aes(x=Spatial_heterogeneity)) + geom_histogram(aes(fill=System2)) +
  facet_wrap(~System2) + labs(x="Slope", y="Count") + scale_fill_manual(values=c("blue", "darkgreen")) +
  theme(legend.position = "none")
#dev.off()

# compare null and actual
anova(m.null, m1)

d.anova <- anova(m.null, m1)
d.anova[1,'R2m'] <- round(MuMIn::r.squaredGLMM(m.null)['R2m'], 3)
d.anova[1,'R2c'] <- round(MuMIn::r.squaredGLMM(m.null)['R2c'], 3)
d.anova[2,'R2m'] <- round(MuMIn::r.squaredGLMM(m1)['R2m'], 3)
d.anova[2,'R2c'] <- round(MuMIn::r.squaredGLMM(m1)['R2c'], 3)

# Output from model 1
#write.csv(d.anova, 'TABLE_model_fit.csv')
# 
# summary(m1)
# ranef(m1) # Estimates for the random effects 
# fixef(m1) # Estimate (slopes) 

# Fixed effects table 
t_m1 <- xtable(summary(m1)$coefficients)
#print.xtable(t_m1, type="html",file="tablem1.html")
################################################


#### Plot overall, not aggregated
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

d.lines <- dataout %>% select(site_project_comm,
                          site_code, project_name, 
                          community_type,
                          Trophic_level, Taxa,
                          Broad_ecosystem_type, System)
d.lines$dispersion <- dataout$Spatial_heterogeneity
d.lines$y.pred <- predict(m1)
d.x.min <- d.lines%>%group_by(site_project_comm)%>%filter(dispersion == min(dispersion))
d.x.max <- d.lines%>%group_by(site_project_comm)%>%filter(dispersion == max(dispersion))
d.lines2 <- rbind(d.x.min, d.x.max)
d.lines2$System <- Hmisc::capitalize(as.character(d.lines2$System))

d.lines.diff<-merge(d.x.min, d.x.max, by=c("site_project_comm","site_code","project_name","community_type", "Trophic_level","Taxa","Broad_ecosystem_type","System"))
d.lines.diff$slope_dir<-d.lines.diff$y.pred.y-d.lines.diff$y.pred.x


###########################
###FIGURES 1 of model 1 ###
###########################

dat.plot <- dataout
# capitalize aquatic/terrestrial labels 
dat.plot$System <- Hmisc::capitalize(as.character(dat.plot$System))

## Create the plot
#pdf("Fig1_aquaticvsterrestrial.pdf", width=7, height=6)
ggplot(dat.plot,
       aes(x = Spatial_heterogeneity, 
           y = Temporal_heterogeneity,
           color = System)) + 
  xlab("Spatial heterogeneity") + ylab("Temporal heterogeneity") +
  geom_point(alpha = .25, shape = 1, alpha=0.5) +
  scale_color_manual(values=c("blue","darkgreen"))+
   geom_smooth(method = "lm", se =F) + 
  geom_line(data = d.lines2,
            aes(y = y.pred, 
                x = dispersion,
                group = site_project_comm,
                color = System),
            size = .25,
            alpha = 0.5)  +
  # facet_grid(. ~ System)  +
  geom_abline(intercept = fixef(m1)['(Intercept)'], slope = fixef(m1)['Spatial_heterogeneity'],size = 2, color = 1) +
  theme_bw(base_size = 12) +
  coord_equal()+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())
#dev.off()

###########################
########Models 2 and 3 ####
###########################

## Model 2 using the experimental parameters

m2 <- lmer(Temporal_heterogeneity ~ Spatial_heterogeneity +
             Plot_size +
             Number_plots +
             Spatial_extent +
             Dataset_legnth +
             Time_step +
             (Spatial_heterogeneity | site_code / project_name / community_type),
           data = dataout)

# ## ONLY RANDOM INTERCEPT
# m2 <- lmer(Temporal_heterogeneity ~ Spatial_heterogeneity +
#              Plot_size + 
#              Number_plots + 
#              Spatial_extent +
#              Dataset_legnth + 
#              Time_step + 
#              (Spatial_heterogeneity | site_code / project_name / community_type),
#            data = dataout)
summary(m2)
ranef(m2) # Estimates for the random effects 
fixef(m2) # Estimate (slopes) 

### Make a histogram of the slopes for each site for model 2
siteskey <- dataout %>%
  select(site_code, project_name, System) %>%
  unique() %>%
  mutate(site_project = paste(project_name, site_code, sep = ":"))
randomout2 <-as.data.frame(ranef(m2)$project_name)
randomout2 <- randomout2 %>%
  mutate(site_project = row.names(randomout2))
randomout_forhist2 <- left_join(siteskey, randomout2) %>%
  mutate(System2 = "Aquatic",
         System2 = ifelse(System == "terrestrial", "Terrestrial", System2))
#pdf("CoDyn_randomeffect_slopes_aquaticvterrestrial_m2_color.pdf", width = 10, height = 6)
ggplot(randomout_forhist2, aes(x=Spatial_heterogeneity)) + geom_histogram(aes(fill=System2)) +
  facet_wrap(~System2) + labs(x="Slope", y="Count") + scale_fill_manual(values=c("blue", "darkgreen")) +
  theme(legend.position = "none")
#dev.off()

# Fixed effects table
t1<-xtable(summary(m2)$coefficients)
#print.xtable(t1, type="html",file="table1.html")

##figure
sjp.setTheme(base=theme_bw())
p2 <- sjp.lmer(m2, type = 'fe.std',
             fade.ns = T,
             sort.est="sort.all",
             title="Experimental predictors",
             y.offset=.15)

# Model 3 using biological parameters about the site
m3 <- lmer(Temporal_heterogeneity ~ Spatial_heterogeneity +
             #taxa  + #confounded with lifespan and system
             Lifespan +
             MAP+
             #ANPP+ #dropping this b/c have missing data
             MAT+
             Successional +
             Trophic_level +
             System +
             #system:dispersion +
             (Spatial_heterogeneity | site_code / project_name / community_type),
           data = dataout)
# 
# ## ONLY RANDOM INTERCEPT
# m3 <- lmer(Temporal_heterogeneity ~ Spatial_heterogeneity + 
#              #taxa  + #confounded with lifespan and system
#              Lifespan + 
#              MAP+
#              #ANPP+ #dropping this b/c have missing data
#              MAT+
#              Successional +
#              Trophic_level +
#              System +
#              #system:dispersion +
#              (1 | site_code / project_name / community_type),
#            data = dataout)
summary(m3)
ranef(m3) # Estimates for the random effects 
fixef(m3) # Estimate (slopes) 

#print table
t2<-xtable(summary(m2)$coefficients)
#print.xtable(t2, type="html",file="table2.html")

#figure
p3<-sjp.lmer(m3, type = 'fe.std',
             fade.ns = T,
             sort.est="sort.all",
             title="Biological predictors",
             y.offset=.25)

### Make a histogram of the slopes for each site for model 3
siteskey <- dataout %>%
  select(site_code, project_name, community_type, System) %>%
  unique() %>%
  mutate(site_project = paste(community_type,project_name, site_code, sep = ":"))
randomout3 <-as.data.frame(ranef(m3)$community_type)
randomout3 <- randomout3 %>%
  mutate(site_project = row.names(randomout3))
randomout_forhist3 <- left_join(siteskey, randomout3) %>%
  mutate(System2 = "Aquatic",
         System2 = ifelse(System == "terrestrial", "Terrestrial", System2))
#pdf("CoDyn_randomeffect_slopes_aquaticvterrestrial_m3_v2.pdf", width = 10, height = 6)
ggplot(randomout_forhist3, aes(x=Spatial_heterogeneity)) + geom_histogram() +
  facet_wrap(~System2) + labs(x="Slope", y="Count")
#dev.off()

#################################################
#### Models 2 and 3 graphics to make Figure 2 ###
################################################

#pdf("Fig2.pdf", width=12, height=6)
grid.arrange(p2$plot, p3$plot, ncol=2, left=textGrob("Predictors of temporal heterogeneity", rot=90, gp=gpar(fontsize=14)), bottom=textGrob("Standardized effect size", gp=gpar(fontsize=14)))
#dev.off()


#####################
### FIGURE 3 ########
#####################


#######################
### MAKE FIGURE 3 #####
#######################


# Double check the cut-off point (ie, minimum length of dataset)
dataout %>%
  group_by(Lifespan) %>%
  dplyr::summarize(min_length = min(Dataset_legnth))

# Subset rateout to only minimum length of a dataset
rateout2 <- rateout %>%
  filter(interval <= 6) 


a <- ggplot(subset(rateout2, interval ==1), aes(x=dispersion, y=distance)) + 
  geom_point(color = "black", size = .5) +
  geom_smooth(aes(color = as.factor(interval)), method="lm", se = F) + 
  labs(x="Spatial heterogeneity", y="", color="Interval") +
  facet_wrap(~lifespan2) + ylim(0,1) + xlim(0,1) +
  scale_color_manual(values = c("turquoise")) 

b <- ggplot(rateout2, aes(x=dispersion, y=distance, group=interval)) + 
  geom_smooth(aes(color=interval), method="lm", se = F) + 
  labs(x="Spatial heterogeneity", y="", color="Interval") +
  facet_wrap(~lifespan2) + ylim(0,1) + xlim(0,1) +
  scale_color_continuous(high = "coral", low = "turquoise", limits = c(1,6)) #+ 

#pdf("Figure3_Rout.pdf", width = 12, height = 8)
pdf("Figure3_Rout_v2.pdf", width = 8, height = 6)
grid.arrange(a + xlab("") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_blank(), text = element_text(size = 10), 
                                  strip.text = element_text(size = 16, color = "black"), strip.background = element_blank())  ,
             b + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text = element_blank(), text = element_text(size = 10), axis.title.x = element_text(size = 18,color="black")), 
             left = textGrob("Temporal heterogeneity", rot=90, gp=gpar(fontsize = 18), vjust =1.5))
dev.off()

### Notes: Labeled panels and cleaned up legend offline


##################################################################
########### ANALYZE DIFFERENCES IN THE SLOPES BY INTERVAL ########

# make a column where interval is a factor
rateout2$facinterval <- as.factor(rateout2$interval)


# mixed effect model to look at the interaction within subannual
m_subannual <- lme(distance ~  dispersion*facinterval, 
         random =  ~1|site_code / project_name / community_type, 
          data = subset(rateout2, lifespan2 == "Subannual"))
summary(m_subannual)



# mixed effect model to look at the interaction within annual
m_annual <- lme(distance ~  dispersion*facinterval, 
         random =  ~1|site_code / project_name / community_type, 
         data = subset(rateout2, lifespan2 == "Annual"))
summary(m_annual)



# mixed effect model to look at the interaction within long-lived
m_long <- lme(distance ~  dispersion*facinterval, 
         random =  ~1|site_code / project_name / community_type, 
         data = subset(rateout2, lifespan2 == "Long-lived"))
summary(m_long)
