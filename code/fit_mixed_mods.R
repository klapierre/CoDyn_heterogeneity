# Fitting models for spatial and temporal evenness data
# NCEAS working group in Community Dynamics Oct 2015
# Script started by Eric Sokol, edited by Dan Flynn and now others

library(nlme)
library(lme4) # for lmer
library(sjPlot) # for plotting random effects of mixed effects models. install.packages('sjPlot',dep=T)
library(ggplot2)
library(xtable) # for making LaTeX formatted tables
library(gridExtra)
library(grid)
# Find latest

datpath = file.path("~/Dropbox/CoDyn/R files/11_06_2015_v7") # or we can do URL method per Matt

figpath = "~/Documents/git/CoDyn_heterogeneity/writing/images"

# -- read in data

#dat <- read.csv(file.path(datpath, 'spatial_temporal_heterogeneity_diversity.csv'), row.names = 1) OLD DATASET

dat<-read.csv(file.path(datpath, 'SpatioTemp_testStatistics_20170208.csv'), row.names = 1)
dat$lifespan2<-ifelse(dat$lifespan=="longer","long-lived",as.character(dat$lifespan))

names(dat)<- c("site_project_comm","experiment_year","site_code","project_name","community_type","location","Country","Continent","Lat","Long","MAP","Plot_size","Spatial_extent","Successional","Lifespan1","Trophic_level","Taxa","ANPP","Broad_ecosystem_type","Number_plots","MAT","Dataset_legnth","Time_step","Spatial_richness","Spatial_ShannonD","Spatial_ShannonJ","Spatial_dominance","Spatial_evenness","Spatial_heterogeneity","Temporal_heterogeneity","System","Lifespan")

pdat <- read.csv(file.path(datpath, "PresentAbsent_spatial_temporal_heterogeneity_diversity.csv"))


# Adding in plot-level evenness and richness, need to recalculate timesteps  (not all of them actually are years). Code from v6 script


###we do not need this step anymore - Meghan Feb 8 2017
# plotlev <- read.csv(file.path(datpath, "RichEvenness_PlotLevel.csv"))
# 
# newYear <- data.frame(row.names=1)
# 
# for(i in unique(plotlev$site_project_comm)) { 
#   
#   xx = plotlev[plotlev$site_project_comm == i,]
#   xx$experiment_year1 <- as.numeric(as.factor(xx$experiment_year))
#   newYear=rbind(newYear, xx)
#   
# }
# 
# plotlev <- newYear
# 
# names(plotlev)[which(names(plotlev) == "J")] = "plotJ"
# 
# plotlev$uniqueID = paste(plotlev$site_project_comm, plotlev$experiment_year1)
# dat$uniqueID =  paste(dat$site_project_comm, dat$experiment_year)
# 
# d2 <- merge(dat, plotlev[c("uniqueID","rich","shannon","plotJ")], by = "uniqueID", all.x = T, all.y = F)

# Goals:

# 1. Temporal distance by dispersion. Temporal distance = bray-curtis across adjacent years within an experiment. Dispersion: bray-curtis across spatial replicates within an experiment. Take into account evenness J and other covariates
# - by biome (aquatic/terrestrial)
# - by ecosystem type (herbaceous/woody/lake/river)

# Questions: 1. Spatial vs temporal variability as predictor or response?
#               - Spatial as driver.       
#            2. Include other metrics from codyn package? variance_ratio, synchrony...
#               - Maybe later. First things first. synchrony could be tried


# Lmer models with hierarchical structure for the random effects

# Simple model, not using
# 
# m1 <- lmer(temporal_distance ~ dispersion + J + 
#              + dispersion:J +
#            (dispersion | site_code / project_name / community_type),
#            data = dat)
# 
# summary(m1)
# ranef(m1) # Estimates for the random effects 
# fixef(m1) # Estimate (slopes) for the fixed effects of dispersion and evenness J. Both are positive
# sjp.lmer(m1, type = "fe")


# Random slopes for spatial heterogeneity, first by study design variables

m1 <- lmer(Temporal_heterogeneity ~ Spatial_heterogeneity +
             Plot_size + 
             Number_plots + 
             Spatial_extent +
             Dataset_legnth + 
             Time_step + 
             (Spatial_heterogeneity | site_code / project_name / community_type),
           data = dat)
summary(m1)
ranef(m1) # Estimates for the random effects 
fixef(m1) # Estimate (slopes) 


# Fixed effects table
t1<-xtable(summary(m1)$coefficients)
print.xtable(t1, type="html",file="~/Dropbox/CoDyn/R Files/11_06_2015_v7/table1.html")

#pdf(file.path(figpath, "designmodel.pdf"), width = 5, height = 5)
sjp.setTheme(base=theme_bw())
p1<-sjp.lmer(m1, type = 'fe.std',
         fade.ns = T,
         sort.est="sort.all",
         title="Experimental predictors",
         y.offset=.15)
#dev.off();system(paste("open", file.path(figpath, "designmodel.pdf"), "-a /Applications/Preview.app"))

# now again, but with organsim and system features as predictors
#feb 2017 this model looks at the biology of the site overall
m2 <- lmer(Temporal_heterogeneity ~ Spatial_heterogeneity + 
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
summary(m2)
ranef(m2) # Estimates for the random effects 
fixef(m2) # Estimate (slopes) 

t2<-xtable(summary(m2)$coefficients)
print.xtable(t2, type="html",file="~/Dropbox/CoDyn/R Files/11_06_2015_v7/table2.html")

p2<-sjp.lmer(m2, type = 'fe.std',
             fade.ns = T,
             sort.est="sort.all",
             title="Biological predictors",
             y.offset=.25)

pdf("~/Dropbox/CoDyn/R Files/11_06_2015_v7/Fig2.pdf", width=12, height=6)
grid.arrange(p1$plot, p2$plot, ncol=2, left=textGrob("Predictors of temporal heterogeneity", rot=90, gp=gpar(fontsize=14)), bottom=textGrob("Standardized effect size", gp=gpar(fontsize=14)))
dev.off()


###exploring the data
ggplot(data=dat, aes(x=spatialdispersion, y=temporaldistance))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline()+
  facet_wrap(~lifespan)

ggplot(data=subset(dat, lifespan=="longer"), aes(x=spatialdispersion, y=temporaldistance))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline()+
  facet_wrap(~succession)

###we decided that since bray-curtis is related to richness and evenness we are not going to look at these aspects of the community.

# m3 <- lmer(temporaldistance ~ spatialdispersion + 
#              spatial_evenness + 
#              #spatial_J+
#              #spatial_dominance + 
#              #taxa  +
#              #lifespan + 
#              log(spatial_rich) + #we are dropping this b/c of varying spatial extent of datasets
#              #MAP_mm+
#              #ANPP+
#              #temp_C+
#              #succession +
#              #trophic_level +
#              #system +
#              #system:dispersion +
#              (spatialdispersion | site_code / project_name / community_type),
#            data = dat)
# #pdf(file.path(figpath, "systemmodel.pdf"), width = 5, height = 5)
# p3<-sjp.lmer(m3, type = 'fe.std',
#          fade.ns = T,
#          sort.est="sort.all",
#          title="Biological Predictors",
#          y.offset=.25,
#          axisTitle.x = "Predictors of Temporal Heterogeneity",
#          axisTitle.y = "Standardized Effect Size")
#dev.off();system(paste("open", file.path(figpath, "systemmodel.pdf"), "-a /Applications/Preview.app"))



##### interaction of evenness and spatial heterogeneity (dispersion)
m3 <- lmer(temporal_distance ~ dispersion * J + 
             dispersion * plotJ + 
             (dispersion | site_code / project_name / community_type),
           data = d2)

xtable(summary(m3)$coefficients)

###### Dominance and evenness. Show dominance w/o evenness, evenness w/o dominance, and both interaction

mdom <- lmer(temporal_distance ~ dispersion +
               dom +
               plot_size + 
               num_plots + 
               spatial_extent +
               dataset_length + 
               time_step + 
               (dispersion | site_code / project_name / community_type),
             data = d2)

meve <- lmer(temporal_distance ~ dispersion +
              J +
               plot_size + 
               num_plots + 
               spatial_extent +
               dataset_length + 
               time_step + 
              (dispersion | site_code / project_name / community_type),
            data = d2)

mboth <- lmer(temporal_distance ~ dispersion +
               dom * J +
                plot_size + 
                num_plots + 
                spatial_extent +
                dataset_length + 
                time_step + 
               (dispersion | site_code / project_name / community_type),
             data = d2)

anova(mdom, meve, mboth) # meve is the worst

pdf(file.path(figpath, "Dom_vs_Eve.pdf"), width = 5, height = 5)
dontshow = c("plot_size","num_plots","spatial_extent","dataset_length","time_step")
sjp.lmer(mdom, type = 'fe.std', remove.estimates = dontshow)
sjp.lmer(meve, type = 'fe.std', remove.estimates = dontshow)
sjp.lmer(mboth, type = 'fe.std', remove.estimates = dontshow)

dev.off();system(paste("open", file.path(figpath, "Dom_vs_Eve.pdf"), "-a /Applications/Preview.app"))

############ For interaction of lifespan and interval

m4 <- lmer(
  temporal_distance ~ dispersion + 
        ( dispersion | lifespan ),
  data = dat)

summary(m4)


###### Now running with the presence-absence

m5 <- lmer(jac_tempdist ~ jac_disp +
             S + 
             J + 
             (jac_disp | site_code / project_name / community_type),
           data = pdat)

summary(m5)
ranef(m5) # Estimates for the random effects 
fixef(m5) # Estimate (slopes) 

xtable(summary(m5)$coefficients)

pdf(file.path(figpath, "pa-evenness.pdf"), width = 5, height = 5)

sjp.lmer(m5, type = 'fe.std', 
         axisTitle.x = "Predictors of temporal heterogeneity",
         axisTitle.y = "Effect size",
         fade.ns = F)
dev.off();system(paste("open", file.path(figpath, "pa-evenness.pdf"), "-a /Applications/Preview.app"))


######### Plot-level richness and evenness

m6 <- lmer(temporal_distance ~ dispersion +
             rich + 
             plotJ + 
             (dispersion | site_code / project_name / community_type),
           data = d2)

summary(m6)
ranef(m6) # Estimates for the random effects 
fixef(m6) # Estimate (slopes) 

sjp.lmer(m6, type = 'fe.std', 
         axisTitle.x = "Predictors of temporal heterogeneity",
         axisTitle.y = "Effect size",
         fade.ns = F)



ggplot(d2, aes(x = dispersion, y = plotJ)) + 
  xlab("Spatial Heterogeneity") + ylab("Plot-levelEvenness") +
  geom_point(aes(color = taxa), alpha = 0.7, size = 0.8) + 
  scale_color_hue(l=40) +
  theme_bw()



# For just longer-lifespan organisms, is there still going to be a relationship?
mlong <- lmer(temporal_distance ~  dispersion + 
              (dispersion | site_code / project_name / community_type),
            data = dat[dat$lifespan == "longer",])
summary(mlong) # yes, still significant effect, more so for longer than not actually
sjp.lmer(mlong, type = "fe")


### 




# Additional analyses:

# Rarefactions in space and time to show that our data do show real relationship, not underestimating time or space component

# Why is J more important than S?
# J and S both calculated across all plots within that time step, within that study

# BC sensitive to abundance. So instead of BC, look at turnover (as a metric not as sensitive to abundance)

# Could add into model: ratio of spatial extent by dataset length
# two different studies with either giant plots or giant spatial extent.



#### Plot overall, not aggregated
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



pdf(file.path(figpath, "overall.pdf"), width = 7, height = 5)

ggplot(dat, aes(x = dispersion, y = temporal_distance)) + 
  xlab("Spatial Heterogeneity") + ylab("Temporal heterogeneity") +
  geom_point(aes(color = taxa), alpha = 0.7, size = 0.8) + 
  scale_color_hue(l=40) +
  theme_bw()

dev.off();system(paste("open", file.path(figpath, "overall.pdf"), "-a /Applications/Preview.app"))


# And aquatic vs terrestrial

pdf(file.path(figpath, "aqterr.pdf"), width = 7, height = 5)

ggplot(dat, aes(x = dispersion, y = temporal_distance, group = system)) + 
  xlab("Spatial Heterogeneity") + ylab("Temporal heterogeneity") +
  geom_point(aes(color = system), alpha = 0.7, size = 0.8) + 
  geom_smooth(method = "lm") +
  scale_color_hue(l=40) +
  theme_bw()

dev.off();system(paste("open", file.path(figpath, "aqterr.pdf"), "-a /Applications/Preview.app"))





# TODO bubbleplots for Scott. Size of bubbles by ... 
#   number of plots - X..plots
#   plot size       - plot_size..m
#   total area      - spatial_extent..m2.
#   total time span - dataset_length

dat.agg <- aggregate(dat[c("temporal_distance","dispersion","spatial_extent","time_step","dataset_length","J","S","ANPP","MAP_mm","temp_C")], 
                     dat[c("site_code","project_name","community_type")],
                     mean, na.rm=T)

leng <- aggregate(dat[,"temporal_distance"], 
                     dat[c("site_code","project_name","community_type")],
                     length)

dat.agg <- data.frame(dat.agg, n = leng[,ncol(leng)])

pdf(file.path(figpath, "overallagg.pdf"), width = 10, height = 6)

ggplot(dat.agg, aes(x = dispersion, y = temporal_distance)) + 
    xlab("Spatial Heterogeneity") + ylab("Temporal heterogeneity") +
    geom_point(aes(size = log2(spatial_extent), col = dataset_length), alpha = 0.8) + 
  scale_size_area(max_size = 15) +
  #scale_color_hue(l=40) +
  theme_bw()

dev.off();system(paste("open", file.path(figpath, "overallagg.pdf"), "-a /Applications/Preview.app"))




colz = as.numeric(dat.agg$site_code)
ccolz = rainbow(n = length(unique(colz)), v = 0.5, alpha = 0.7)[colz]

plot(temporal_distance ~ dispersion, data = dat,
     pch = 16, col = alpha("black", alpha = 0.01)) 

with(dat[dat$system=="aquatic",], 
     points(dispersion, temporal_distance, col = alpha('red', 0.5), pch = 16)
     )
with(dat[dat$system=="aquatic",], 
  abline(lm(temporal_distance ~ dispersion), col = 'red') )


with(dat[dat$system=="terrestrial",], points(dispersion, temporal_distance, col = alpha('blue', 0.5), pch = 16, ))
with(dat[dat$system=="terrestrial",], 
     abline(lm(temporal_distance ~ dispersion), col = 'blue') )


plot(temporal_distance ~ dispersion, data = dat,
     pch = 16, col = alpha("black", alpha = 0.01)) 

abline(a=fixef(m22)[1], b = fixef(m22)[2])

with(dat.agg,
points(dispersion, temporal_distance,
        pch = 16, 
        col = ccolz,
        cex = J*3))


with(dat.agg,
     points(dispersion, temporal_distance,
            pch = 16, 
            col = ccolz,
            cex = dataset_length))


######### Looking at relationships between our predictors
plot(J ~ dispersion, data = dat) # yup they are related

plot(S ~ dispersion, data = dat)


plot(S ~ dispersion, data = dat, type = "n")
with(dat[dat$system=="aquatic",], 
     points(dispersion, S, col = alpha('red', 0.5), pch = 16)
)
with(dat[dat$system=="aquatic",], 
     abline(lm(S ~ dispersion), col = 'red') )

with(dat[dat$system=="terrestrial",], points(dispersion, S, col = alpha('blue', 0.5), pch = 16, ))
with(dat[dat$system=="terrestrial",], 
     abline(lm(S ~ dispersion), col = 'blue') )

library(ggplot2)
ggplot(dat, aes(dispersion, S, col = taxa)) + geom_point()


ggplot(dat, aes(dispersion, S, col = taxa)) + geom_point() #+ geom_smooth()


ggplot(dat, aes(dispersion, temporal_distance, col = taxa)) + geom_point() + geom_smooth(method="lm")


ggplot(dat, aes(dispersion, temporal_distance, col = lifespan)) + geom_point() + geom_smooth(method="lm")


# Ok, how about at different lifespans, does the effect of dataset length matter?

ggplot(dat, aes(dispersion, temporal_distance, col = dataset_length)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~lifespan)



#levels(cut(log(dat.agg$spatial_extent..m2.)/5, 5))

#legend(cex = levels(cut(log(dat.agg$spatial_extent..m2.)/5, 5))

########## Eric code below

# x is dispersion
# y is temporal_distance
# random factor 

# -- Transform Bray dists
dat$dispersion_asinsqrt <- asin(sqrt(dat$dispersion))
dat$temporal_distance_asinsqrt <- asin(sqrt(dat$temporal_distance))

# -- make simple vars
y <- dat$temporal_distance
x <- dat$dispersion
x.cov <- dat$J
f_rand <- dat$site_project_comm
f_rand_levels <- as.character(levels(f_rand))


mod.lm<-gls(y ~ x,
            data = dat,
            method='ML')

# 
mod.rand.slope<-lme(y ~ x,
                    random = ~ x | f_rand,
                    data = dat,
                    method='ML')

mod.rand.int<-lme(y ~ x,
                  random = ~ 1 | f_rand,
                  data = dat,
                  method='ML')

mod.no.fixed.rand.slope<-lme(y ~ 1,
                             random = ~ x | f_rand,
                             data = dat,
                             method='ML')

mod.rand.slope.cov<-lme(y ~ x + J,
                        random = ~ x | f_rand,
                        data = dat,
                        method='ML')

anova.compare<-anova(mod.lm,
                     mod.rand.slope,
                     mod.rand.int,
                     mod.no.fixed.rand.slope)

anova.compare.cov<-anova(mod.rand.slope, mod.rand.slope.cov)

# -- write anova results to .csv file
write.csv(anova.compare, file='TABLE_anova_random_effects.csv', row.names = F)
write.csv(anova.compare.cov, file='TABLE_anova_covariate.csv', row.names = F)



# ----------------------------------------------------
# -- which mod to plot

# mod.temp <- mod.rand.int  
mod.temp <- mod.rand.slope
# mod.temp <- mod.rand.slope.cov
# ----------------------------------------------------
F0<-fitted(mod.temp,level=0)
F1<-fitted(mod.temp,level=1)

I<-order(x)

x_s<-sort(x)
plot(x_s,F0[I],lwd=4,type="l",ylim=c(0,max(y)),
     ylab="Temporal Bray-Curtis",xlab="Spatial Bray-Curtis")

d.plotting<-data.frame(
  x=x,
  y=y,
  f_rand=f_rand,
  col=as.numeric(f_rand),
  pch=as.numeric(f_rand),
  lty=as.numeric(f_rand)
)

d.legend<-d.plotting[
  !duplicated(d.plotting$f_rand),
  ]

row.names(d.legend)<-d.legend$f_rand

for (i in as.character(levels(f_rand))){
  x1<-x[f_rand==i]
  y1<-F1[f_rand==i]
  K<-order(x1)
  lines(sort(x1),y1[K],
        col=d.legend[i,'col'],
        lty=d.legend[i,'lty'])
}

points(x, y, pch=d.plotting$pch,
       col=d.plotting$col)

plot.new()
plot.window(c(-1,1),c(-1,1))
legend(
  x='topleft',
  legend=d.legend$f_rand,
  pch=d.legend$pch,
  col=d.legend$col,
  lty=d.legend$lty,
  bty='o',
  cex=.75
)

savePlot('Fig_best_mix_mod_all','pdf')

dat.coeffs<-data.frame(
  data.set.id=c('Global_model_fixed',row.names(mod.temp$coefficients[[2]][[1]])),
  rbind(
    mod.temp$coefficients[[1]],
    mod.temp$coefficients[[2]][[1]]),
  row.names=NULL
)

# -- write coefficients to a .csv file
write.csv(dat.coeffs, file='TABLE_mixed_mod_rand_effects.csv')
