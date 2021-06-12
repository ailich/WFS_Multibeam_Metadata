#Check that local filepaths match those on server
rm(list=ls())
library(tidyverse)
setwd("C:/Users/socce/Documents/Grad_School/Research/R_Projects/WFS_Multibeam_Metadata/")

df<- read_csv("Multibeam_metadata.csv")
df<- df %>% filter(!is.na(Server_Location))
df<- df %>% mutate(Server_Location= paste0("Z://", Server_Location))
df<- df %>% filter(!file.exists(Server_Location))
nrow(df)==0
