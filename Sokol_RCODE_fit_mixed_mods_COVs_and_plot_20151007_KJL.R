# graphics.off()
# windows(width=10,
#         height=7,
#         pointsize=10,
#         rescale='fixed')

# nf<-layout(matrix(c(1,2),1,2),
#            heights=c(1,1),
#            widths=c(3,1))
# layout.show(nf)

setwd('C:\\Users\\Kim\\Dropbox\\working groups\\community dynamics working group\\CoDyn\\R files\\10_07_2015_v5')

# -- read in data
# dat<-read.csv('spaceTimeInfo2.csv', row.names = 1)
dat<-read.csv('spaceTimeEven.csv', row.names = 1)

dat<-dat[!is.na(dat$temporal_distance),]

# x is dispersion
# y is temporal_distance
# random factor 

# -- Transform Bray dists
dat$dispersion_asinsqrt<-asin(sqrt(dat$dispersion))
dat$temporal_distance_asinsqrt<-asin(sqrt(dat$temporal_distance))

# -- make simple vars
y <- dat$temporal_distance
x <- dat$dispersion
x.cov <- dat$J
f_rand <- dat$site_project_comm
f_rand_levels <- as.character(levels(f_rand))

# -- make plotting matrix
d.plotting<-data.frame(
  x=x,
  y=y,
  f_rand=f_rand,
  col=as.numeric(f_rand),
  pch=as.numeric(f_rand)
)

# -- mixed models
require(nlme)
mod.lm<-gls(y ~ x,
            data = dat,
            method='ML')
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
write.csv(anova.compare, file='TABLE_anova_random_effects.csv')
write.csv(anova.compare.cov, file='TABLE_anova_covariate.csv')



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
