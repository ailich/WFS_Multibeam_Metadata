library(terra)
library(tidyverse)

terraOptions(datatype="FLT8S")

x<- read_csv("Z://2_Projects/GIS/Multibeam_Metadata_And_Footprints_AI/Multibeam_metadata.csv")
x<- x %>% filter(!is.na(Server_Location))

x<- x %>% mutate(Server_Location= if_else(str_detect(Server_Location, "^D:"), Server_Location, paste0("Z:/", Server_Location)))

x<- x %>% mutate(NewFilename=paste0(paste(formatC(ID, width = 3, format = "d", flag = "0"), Site, Subsite,Type, Source, sep = "_"), ".tif"))

for (i in 1:nrow(x)) {
  print(i)
  if(!is.na(x$Server_Location[i])){
    writeRaster(rast(x$Server_Location[i]), paste0("D://Ilich/GIS/", x$NewFilename[i]))
    }
}
