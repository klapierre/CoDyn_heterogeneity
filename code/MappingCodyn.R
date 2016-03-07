# mapping codyn data

library(maps)
library(scales)

datpath = file.path("~/Dropbox/CoDyn/R files", sort(dir("~/Dropbox/CoDyn/R files/"), T)[1]) 

dat <- read.csv(file.path(datpath, 'spatial_temporal_heterogeneity_diversity.csv'), row.names = 1)

# 68 studies
dat.agg <- aggregate(dat[c("temporal_distance","dispersion","spatial_extent","time_step","dataset_length","J","S","ANPP","MAP_mm","temp_C")], 
                     dat[c("site_code","project_name","community_type","Lat","Long")],
                     mean, na.rm=T)

leng <- aggregate(dat[,"temporal_distance"], 
                  dat[c("site_code","project_name","community_type","Lat","Long")],
                  length)

descriptors <- aggregate(dat[c("lifespan","trophic_level","taxa","system")],
          dat[c("site_code","project_name","community_type","Lat","Long")],
          unique)

dat.agg <- data.frame(dat.agg, n = leng[,ncol(leng)], descriptors)

colz = alpha(c("midnightblue", "darkgreen"), 0.7)
ccolz = rep(colz[1], nrow(dat.agg))
ccolz[dat.agg$system == "terrestrial"] = colz[2]


reg<-map(plot=FALSE)$names
omits <- grep("Antarctica", reg)
reg <- reg[is.na(match(reg, reg[omits]))]

pdf(file="../figures/BigMap.pdf", width = 15, height = 10)

map(col = "grey50", interior = F, regions = reg)
points(
  dat.agg$Long,
  dat.agg$Lat,
  lwd = 3,
  pch = 1, cex = log(dat.agg$spatial_extent)/5,
  col = ccolz
)

levs <- hist(log(dat.agg$spatial_extent)/5, breaks = 2, plot=F)

par(xpd=T)
legend(-180, 50, bty = "n",
       title = expression(paste("Spatial extent (",m^2,")")),
       pch = 1, pt.lwd = 4,
       legend = format(ceiling(exp(levs$mids*5)), scientific=T, digits = 2),
       pt.cex = levs$mids,
       y.intersp = 2,
       x.intersp = 2
) 

legend(-180, -20, bty = "n",
       pch = 1, pt.lwd = 4, pt.cex = 3,
        title = "Biome type",
       legend = c("Aquatic","Terrestrial"),
       col = colz,
       y.intersp = 2,
       x.intersp = 2)

dev.off()
system("open '../figures/BigMap.pdf' -a /Applications/Preview.app")

par(xpd=F)
# North america only

pdf(file="../figures/NAmap.pdf", width = 7, height = 10)

map(col = "grey50", interior = F, regions = c("USA","Canada","Mexico"), xlim = c(-170, -50))
points(
  dat.agg$Long,
  dat.agg$Lat,
  lwd = 3,
  pch = 1, cex = log(dat.agg$spatial_extent)/5,
  col = ccolz
)

levs <- hist(log(dat.agg$spatial_extent)/5, breaks = 2, plot=F)

par(xpd=T)
legend(-180, 50, bty = "n",
       title = expression(paste("Spatial extent (",m^2,")")),
       pch = 1, pt.lwd = 4,
       legend = format(ceiling(exp(levs$mids*5)), scientific=T, digits = 2),
       pt.cex = levs$mids,
       y.intersp = 2,
       x.intersp = 2
) 

legend(-180, 30, bty = "n",
       pch = 1, pt.lwd = 4, pt.cex = 3,
       title = "Biome type",
       legend = c("Aquatic","Terrestrial"),
       col = colz,
       y.intersp = 2,
       x.intersp = 2)
par(xpd=F)

system("open '../figures/NAmap.pdf' -a /Applications/Preview.app")
