# mapping codyn data

library(maps)
library(scales)

datpath = file.path("~/Dropbox/CoDyn/R files", sort(dir("~/Dropbox/CoDyn/R files/"), T)[1]) 

dat <- read.csv(file.path(datpath, 'spatial_temporal_heterogeneity_diversity.csv'), row.names = 1)


dat.agg <- aggregate(dat[c("temporal_distance","dispersion","spatial_extent","time_step","dataset_length","J","S","ANPP","MAP_mm","temp_C")], 
                     dat[c("site_code","project_name","community_type","Lat","Long")],
                     mean, na.rm=T)

leng <- aggregate(dat[,"temporal_distance"], 
                  dat[c("site_code","project_name","community_type")],
                  length)

descriptors <- aggregate(dat[c("lifespan","trophic_level","taxa","system")],
          dat[c("site_code","project_name","community_type")],
          unique)

dat.agg <- data.frame(dat.agg, n = leng[,ncol(leng)], descriptors)

colz = alpha(c("midnightblue", "darkgreen"), 0.6)
ccolz = rep(colz[1], nrow(dat.agg))
ccolz[dat.agg$system == "terrestrial"] = colz[2]

map(fill = T, col = "grey60")
points(
  dat.agg$Long,
  dat.agg$Lat,
  lwd = 3,
  pch = 1, #cex = log(nd$n)/1.5,
  col = ccolz
)

dev.print(pdf, "figures/Big Map.pdf", width = 15, height = 10)
system("open 'figures/Big Map.pdf' -a /Applications/Preview.app")
