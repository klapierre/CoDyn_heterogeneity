# Fitting models for spatial and temporal evenness data
# NCEAS working group in Community Dynamics Oct 2015
# Script started by Eric Sokol, edited by Dan Flynn and now others

library(nlme)
library(lme4) # for lmer
library(sjPlot) # for plotting random effects of mixed effects models. install.packages('sjPlot',dep=T)

datpath = "~/Dropbox/CoDyn/R files/11_06_2015_v7/" # or we can do URL method per Matt

# -- read in data
dat <- read.csv(file.path(datpath, 'spatial_temporal_heterogeneity_diversity.csv'), row.names = 1)

dat <- dat[!is.na(dat$temporal_distance),] # 40 rows of NA

# Goals:

# 1. Temporal distance by dispersion. Temporal distance = bray-curtis across adjacent years within an experiment. Dispersion: bray-curtis across spatial replicates within an experiment. Take into account evenness J and other covariates
# - by biome (aquatic/terrestrial)
# - by ecosystem type (herbaceous/woody/lake/river)

# Questions: 1. Spatial vs temporal variability as predictor or response?
#               - Spatial as driver.       
#            2. Include other metrics from codyn package? variance_ratio, synchrony...
#               - Maybe later. First things first. synchrony could be tried


# Lmer models with hierarchical structure for the random effects


# Variables to include

#   

m1 <- lmer(temporal_distance ~ dispersion + J + 
             + dispersion:J +
           (dispersion | site_code / project_name / community_type),
           data = dat)

summary(m1)
ranef(m1) # Estimates for the random effects 
fixef(m1) # Estimate (slopes) for the fixed effects of dispersion and evenness J. Both are positive
sjp.lmer(m1, type = "fe")


# Plotting the random effects. Since this model is intercept only, we can interpret these easily as the differences betwen community types or project names or sites in mean temporal distance.
pdf("Random Effects plots m1.pdf", width = 10, height = 10)
sjp.lmer(m1)
dev.off(); system("open 'Random Effects plots m1.pdf' -a /Applications/Preview.app")

## Now adding in a million covariates
m2 <- lmer(temporal_distance ~ dispersion + J + 
             plot_size + 
            
             spatial_extent +
             dataset_length + 
             time_step +
             (1 | site_code / project_name / community_type),
           data = dat)
summary(m2)
ranef(m2) # Estimates for the random effects 
fixef(m2) # Estimate (slopes) for the fixed effects of dispersion and evenness J. Both are positive

pdf("Random Effects plots m2.pdf", width = 10, height = 10)
sjp.lmer(m2)
dev.off(); system("open 'Random Effects plots m2.pdf' -a /Applications/Preview.app")

m3 <- lmer(temporal_distance ~ dispersion + 
             J + 
             plot_size + 
             num_plots + 
             spatial_extent +
             dataset_length + 
             time_step +
             temp_C + 
             MAP_mm +
             S + 
             lifespan + 
             succession + 
             trophic_level + 
            # system + 
             broad_ecosystem_type + 
            # ANPP..g.m2. +
             (1 | site_code / project_name / community_type),
           data = dat)
summary(m3)
ranef(m3) # Estimates for the random effects 
fixef(m3) # Estimate (slopes) for the fixed effects of dispersion and evenness J. Both are positive

pdf("Random Effects plots m3.pdf", width = 10, height = 10)
sjp.lmer(m3)
sjp.lmer(m3, type = "fe") # fixed effects
dev.off(); system("open 'Random and Fixed Effects plots m3.pdf' -a /Applications/Preview.app")

# Random slopes as per Eric's models below



m22 <- lmer(temporal_distance ~ dispersion + #J + 
              S +
              ANPP + 
             plot_size + 
             num_plots + 
             spatial_extent +
             dataset_length + 
             time_step + 
             (dispersion | site_code / project_name / community_type),
           data = dat)
summary(m22)
ranef(m22) # Estimates for the random effects 
fixef(m22) # Estimate (slopes) for
sjp.lmer(m22, type = "fe")




m23 <- lmer(temporal_distance ~ dispersion + #J +  #dispersion:J + 
            #  broad_ecosystem_type * taxa  +
              succession +
              trophic_level +
              system +
              #system:dispersion +
              (dispersion | site_code / project_name / community_type),
            data = dat)
summary(m23)
ranef(m23) # Estimates for the random effects 
fixef(m23) # Estimate (slopes) 
sjp.lmer(m23, type = "fe")


##### Organism-level predictors


m24 <- lmer(temporal_distance ~  dispersion + 
              taxa  +
              lifespan +
              #trophic_level +
              #system +
              #system:dispersion +
              (dispersion | site_code / project_name / community_type),
            data = dat)
summary(m24)
ranef(m23) # Estimates for the random effects 
fixef(m23) # Estimate (slopes) 
sjp.lmer(m24, type = "fe")


# For just longer-lifespan organisms, is there still going to be a relationship?
m25 <- lmer(temporal_distance ~  dispersion + 
              
              (dispersion | site_code / project_name / community_type),
            data = dat[dat$lifespan=="longer",])
summary(m25)
sjp.lmer(m25, type = "fe")



### 


d <- tapply(dat$dispersion,
               dat$site_project_comm,
               mean, na.rm=T)
dt <- tapply(dat$temporal_distance,
             dat$site_project_comm,
             mean, na.rm=T)

summary(lm(dt ~ d))
plot(d, dt)
abline(lm(dt ~ d))
# Additional analyses:

# Rarefactions in space and time to show that our data do show real relationship, not underestimating time or space component

# Why is J more important than S?
# J and S both calculated across all plots within that time step, within that study

# BC sensitive to abundance. So instead of BC, look at turnover (as a metric not as sensitive to abundance)

# Could add into model: ratio of spatial extent by dataset length


# two different studies with either giant plots or giant spatial extent.



# TODO bubbleplots for Scott. Size of bubbles by ... 
#   number of plots - X..plots
#   plot size       - plot_size..m
#   total area      - spatial_extent..m2.
#   total time span - dataset_length
dat.agg <- aggregate(dat[c("temporal_distance","dispersion","spatial_extent","dataset_length","J")], 
                     dat[c("site_code","project_name","community_type")],
                     mean, na.rm=T)

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
