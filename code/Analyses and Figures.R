#########################################################
##packages
#######

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

#######################################################
## load and clean up data names
datpath = file.path("~/Dropbox/CoDyn/R files/11_06_2015_v7") # or we can do URL method per Matt
dat<-read.csv(file.path(datpath, 'SpatioTemp_testStatistics_20170208.csv'), row.names = 1)
dat$lifespan2<-ifelse(dat$lifespan=="longer","long-lived",as.character(dat$lifespan))

names(dat)<- c("site_project_comm","experiment_year","site_code","project_name","community_type","location","Country","Continent","Lat","Long","MAP","Plot_size","Spatial_extent","Successional","Lifespan1","Trophic_level","Taxa","ANPP","Broad_ecosystem_type","Number_plots","MAT","Dataset_legnth","Time_step","Spatial_richness","Spatial_ShannonD","Spatial_ShannonJ","Spatial_dominance","Spatial_evenness","Spatial_heterogeneity","Temporal_heterogeneity","System","Lifespan")

###############################################
# model fits


#####model 1 # spatial only

m.null <- lmer(Temporal_heterogeneity ~ 1 +
                 (Spatial_heterogeneity | site_code / project_name / community_type),
               data = dat)
m1 <- lmer(Temporal_heterogeneity ~ 1 +
             Spatial_heterogeneity +
             (Spatial_heterogeneity | site_code / project_name / community_type),
           data = dat)
anova(m.null, m1)

d.anova <- anova(m.null, m1)
d.anova[1,'R2m'] <- round(MuMIn::r.squaredGLMM(m.null)['R2m'], 3)
d.anova[1,'R2c'] <- round(MuMIn::r.squaredGLMM(m.null)['R2c'], 3)
d.anova[2,'R2m'] <- round(MuMIn::r.squaredGLMM(m1)['R2m'], 3)
d.anova[2,'R2c'] <- round(MuMIn::r.squaredGLMM(m1)['R2c'], 3)
write.csv(d.anova, 'TABLE_model_fit.csv')
# 
# summary(m1)
# ranef(m1) # Estimates for the random effects 
# fixef(m1) # Estimate (slopes) 
# 
######################

# Fixed effects table
t_m1<-xtable(summary(m1)$coefficients)
print.xtable(t_m1, type="html",file="~/Dropbox/CoDyn/R Files/11_06_2015_v7/tablem1.html")
################################################


#### Plot overall, not aggregated
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

d.lines <- dat %>% select(site_project_comm,
                          site_code, project_name, 
                          community_type,
                          Trophic_level, Taxa,
                          Broad_ecosystem_type, System)
d.lines$dispersion <- dat$Spatial_heterogeneity
d.lines$y.pred <- predict(m1)
d.x.min <- d.lines%>%group_by(site_project_comm)%>%filter(dispersion == min(dispersion))
d.x.max <- d.lines%>%group_by(site_project_comm)%>%filter(dispersion == max(dispersion))
d.lines2 <- rbind(d.x.min, d.x.max)
d.lines2$System <- Hmisc::capitalize(as.character(d.lines2$System))

d.lines.diff<-merge(d.x.min, d.x.max, by=c("site_project_comm","site_code","project_name","community_type", "Trophic_level","Taxa","Broad_ecosystem_type","System"))
d.lines.diff$slope_dir<-d.lines.diff$y.pred.y-d.lines.diff$y.pred.x

#write.csv(d.lines.diff, "~/Dropbox/CoDyn/R Files/11_06_2015_v7/notrealslopes.csv")

###FIGURES 1 of model 1

dat.plot <- dat
dat.plot$System <- Hmisc::capitalize(as.character(dat.plot$System))

###################

########################
# -- one panel

pdf("~/Dropbox/CoDyn/R Files/11_06_2015_v7/Fig1.pdf", width=7, height=6)

ggplot(dat.plot,
       aes(x = Spatial_heterogeneity, 
           y = Temporal_heterogeneity,
           color = System)) + 
  xlab("Spatial heterogeneity") + ylab("Temporal heterogeneity") +
  geom_point(alpha = .25, shape = 1, alpha=0.5) +
  scale_color_manual(values=c("blue","darkgreen"))+
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

dev.off()

########Models 2 and 3

###model 2 using the experimental parameters

m2 <- lmer(Temporal_heterogeneity ~ Spatial_heterogeneity +
             Plot_size + 
             Number_plots + 
             Spatial_extent +
             Dataset_legnth + 
             Time_step + 
             (Spatial_heterogeneity | site_code / project_name / community_type),
           data = dat)
summary(m2)
ranef(m2) # Estimates for the random effects 
fixef(m2) # Estimate (slopes) 


# Fixed effects table
t1<-xtable(summary(m2)$coefficients)
print.xtable(t1, type="html",file="~/Dropbox/CoDyn/R Files/11_06_2015_v7/table1.html")

##figure
sjp.setTheme(base=theme_bw())
p2<-sjp.lmer(m2, type = 'fe.std',
             fade.ns = T,
             sort.est="sort.all",
             title="Experimental predictors",
             y.offset=.15)

###Model 3
###using biological parameters about the site
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
           data = dat)
summary(m3)
ranef(m3) # Estimates for the random effects 
fixef(m3) # Estimate (slopes) 
#print table
t2<-xtable(summary(m2)$coefficients)
print.xtable(t2, type="html",file="~/Dropbox/CoDyn/R Files/11_06_2015_v7/table2.html")

#figure
p3<-sjp.lmer(m3, type = 'fe.std',
             fade.ns = T,
             sort.est="sort.all",
             title="Biological predictors",
             y.offset=.25)

####combine to make figure 2
pdf("~/Dropbox/CoDyn/R Files/11_06_2015_v7/Fig2.pdf", width=12, height=6)

grid.arrange(p2$plot, p3$plot, ncol=2, left=textGrob("Predictors of temporal heterogeneity", rot=90, gp=gpar(fontsize=14)), bottom=textGrob("Standardized effect size", gp=gpar(fontsize=14)))

dev.off()
