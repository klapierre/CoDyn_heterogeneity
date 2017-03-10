library(tidyr)
library(dplyr)

#set the path to the file on your system including the file name separated by forward slashes
file_path <- "relative_cover_nceas_and_converge_12012015_long.csv"

df <- read.csv(file_path, header=TRUE, sep=",", quote="\"", as.is = TRUE, na.strings = "NA")

#go from long table format back to wide matrix with species names as column headings. Fill in 0s.
wide_matrixdf <- spread(df, species, relative_cover, fill = 0, convert = FALSE, drop = TRUE, sep = NULL)

#sort back to original sequence of records
sorted_matrixdf <- arrange(wide_matrixdf, rowID)

