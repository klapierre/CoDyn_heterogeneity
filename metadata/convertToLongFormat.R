library(tidyr)
library(dplyr)

df <- read.csv("relative_cover_nceas_and_converge_12012015_cleaned.csv", header=TRUE, sep=",", quote="\"", as.is = TRUE, na.strings = "NA")
head(df)

longdf <- df %>% gather(species, relative_cover, sp1:sp99)
head(longdf)

nozerodf <- filter(longdf, relative_cover > 0)

write.csv(nozerodf, file = "relative_cover_nceas_and_converge_12012015_long.csv", row.names = FALSE)
