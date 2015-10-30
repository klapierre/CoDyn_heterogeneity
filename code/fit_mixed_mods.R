# Fitting models for spatial and temporal evenness data
# NCEAS working group in Community Dynamics Oct 2015
# Script started by Eric Sokol, edited by Dan Flynn and now others

library(nlme)
library(lme4) # for lmer
library(sjPlot) # for plotting random effects of mixed effects models. install.packages('sjPlot',dep=T)

datpath = "~/Dropbox/CoDyn/R files/10_08_2015_v6/CoDyn_heterogeneity" # or we can do URL method per Matt

# -- read in data
dat <- read.csv(file.path(datpath, 'spatial_temporal_heterogeneity_diversity.csv'), row.names = 1)

dat <- dat[!is.na(dat$temporal_distance),] # 40 rows of NA

# Lmer models with hierarchical structure for the random effects
m1 <- lmer(temporal_distance ~ dispersion + J + 
           (1 | site_code / project_name.x / community_type),
           data = dat)
summary(m1)
ranef(m1) # Estimates for the random effects 
fixef(m1) # Estimate (slopes) for the fixed effects of dispersion and evenness J. Both are positive

# Plotting the random effects. Since this model is intercept only, we can interpret these easily as the differences betwen community types or project names or sites in mean temporal distance.
pdf("Random Effects plots m1.pdf", width = 10, height = 10)
sjp.lmer(m1)
dev.off(); system("open 'Random Effects plots m1.pdf' -a /Applications/Preview.app")

## Now adding in a million covariates
m2 <- lmer(temporal_distance ~ dispersion + J + 
             plot_size..m2. + 
            # X..plots + 
             spatial_extent..m2. +
             dataset_length + 
             time_step.y +
             (1 | site_code / project_name.x / community_type),
           data = dat)
summary(m2)
ranef(m2) # Estimates for the random effects 
fixef(m2) # Estimate (slopes) for the fixed effects of dispersion and evenness J. Both are positive

pdf("Random Effects plots m2.pdf", width = 10, height = 10)
sjp.lmer(m2)
dev.off(); system("open 'Random Effects plots m2.pdf' -a /Applications/Preview.app")

m3 <- lmer(temporal_distance ~ dispersion + 
             J + 
             plot_size..m2. + 
             # X..plots + 
             spatial_extent..m2. +
             dataset_length + 
             time_step.y +
             temp_C + 
             MAP_mm +
             S + 
             lifespan..subannual..annual.or.longer. + 
             succession + 
             trophic_level..consumer..primary. + 
            # system + 
             broad_ecosystem_type + 
            # ANPP..g.m2. +
             (1 | site_code / project_name.x / community_type),
           data = dat)
summary(m3)
ranef(m3) # Estimates for the random effects 
fixef(m3) # Estimate (slopes) for the fixed effects of dispersion and evenness J. Both are positive

pdf("Random Effects plots m3.pdf", width = 10, height = 10)
sjp.lmer(m3)
dev.off(); system("open 'Random Effects plots m3.pdf' -a /Applications/Preview.app")

# Random slopes as per Eric's models below


m22 <- lmer(temporal_distance ~ dispersion + J + 
             plot_size..m2. + 
             # X..plots + 
             spatial_extent..m2. +
             dataset_length + 
             time_step.y +
             (dispersion | site_code / project_name.x / community_type),
           data = dat)
summary(m22)
ranef(m22) # Estimates for the random effects 
fixef(m2) # Estimate (slopes) for


# TODO bubbleplots for Scott. Size of bubbles by 



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
