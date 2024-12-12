library(terra)
library(tidyverse)

terraOptions(datatype="FLT8S")

x<- read_csv("Multibeam_metadata.csv")
x<- x %>% filter(!is.na(Server_Location))

unique(str_extract(x$Filename, "\\..+$"))

x<- x %>% mutate(Server_Location = case_when(str_detect(Server_Location, "^NFWF/") ~ str_replace(Server_Location, "^NFWF/", "Z://"),
                                         str_detect(Server_Location, "^comit_wrk/") ~ str_replace(Server_Location, "^comit_wrk/", "Y://"),
                                         TRUE ~ Server_Location))

x<- x %>% mutate(NewFilename=paste0(paste(formatC(ID, width = 3, format = "d", flag = "0"), Site, Subsite,Type, Source, sep = "_"), ".tif"))

for (i in 1:nrow(x)) {
  print(i)
  if(!is.na(x$Server_Location[i])){
    writeRaster(rast(x$Server_Location[i]), paste0("D://Ilich/GIS/", x$NewFilename[i]))
    }
}
