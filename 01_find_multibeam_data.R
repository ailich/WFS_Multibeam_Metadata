rm(list=ls())
library(raster)
library(tidyverse)

setwd("../Unified_WFS_Map2/")

MB_df<- tibble(Area=character(), Subarea=character(), Type=factor(levels="Bathymetry", "Backscatter"), Resolution=numeric(), Units= character(), Projection=character(), Ellipsoid=character(), Server_Location=character(), Filename=character(), Source=character(), Vessel=character(), Sonar=character(), Frequency_kHz=numeric())

extract_MBmetadata<- function(data, Area, Subarea, Type, Source=NA_character_, Vessel=NA_character_, Sonar=NA_character_, Frequency_kHz= NA_real_){
  if(!(Type=="Bathymetry" | Type =="Backscatter")){
    stop("Type must be 'Bathymetry' or 'Backscatter'")
    }
  Type<- factor(Type, levels= c("Bathymetry", "Backscatter"))
  Subarea=as.character(Subarea)
  if(class(data)=="RasterLayer"){
    Server_Location<- str_extract(filename(data), "2_Projects\\\\.*")
    Server_Location<- str_replace_all(Server_Location, "\\\\", "/")
    if(!str_detect(tolower(Server_Location), tolower(Type))){
      warning("Type not found in Filename. You may have made a mistake")
      }
    proj_string<- as.character(raster::crs(data))
    Projection<- suppressWarnings(str_extract(proj_string, regex("(?<=proj=)[[:alpha:]]+", perl=TRUE)))
    Zone<-suppressWarnings(str_extract(proj_string, regex("(?<=zone=)[[:digit:]]+", perl=TRUE)))
    Ellipsoid<-suppressWarnings(str_extract(proj_string, regex("(?<=datum=)[[:alnum:]]+", perl=TRUE)))
    if(is.na(Ellipsoid)){
    Ellipsoid<- suppressWarnings(str_extract(proj_string, regex("(?<=ellps=)[[:alnum:]]+", perl=TRUE)))
    }
    if(is.na(Ellipsoid)){
      Ellipsoid<- paste(unlist(str_extract_all(proj_string, "\\+((rf)|(f)|(a)|(b))=[^ ]+")), collapse = " ")
      }
    if(!is.na(Zone)){
    Projection<- paste(Projection, Zone)
    }
    Units<- suppressWarnings(str_extract(proj_string, regex("(?<=units=)[[:alpha:]]+", perl=TRUE)))
    output<- tibble(Area=Area, Subarea=Subarea, Type= Type, Resolution=raster::res(data)[1], Units= Units, Projection=Projection, Ellipsoid=Ellipsoid, Server_Location=Server_Location, Filename= basename(Server_Location), Source=Source, Vessel=Vessel, Sonar=Sonar, Frequency_kHz=Frequency_kHz)
  } else{
    output<- tibble(Area=Area, Subarea=Subarea, Type= Type, Resolution=NA_real_, Units= NA_character_, Projection=NA_character_, Ellipsoid=NA_character_, Server_Location=NA_character_,Filename= NA_character_, Source=Source, Vessel=Vessel, Sonar=Sonar, Frequency_kHz=Frequency_kHz)
    }
  return(output)
  }


#Elbow
EL1_bathy<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/Elbow/EL1/Bathymetry/EL1_Bathymetry_CUBE_2m.tiff")
EL1_bs<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/Elbow/EL1/Backscatter/EL1_1mTimeSeriesBS_TrimmedtoMosaic_AVG800.tiff")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = EL1_bathy, Area = "EL", Subarea = 1, Type = "Bathymetry", Source = "CSCAMP")) %>%
  bind_rows(extract_MBmetadata(data = EL1_bs, Area = "EL", Subarea = 1, Type = "Backscatter", Source = "CSCAMP"))


EL2_bathy<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/Elbow/EL2/Bathymetry/EL2_AGU.bag") #Artifacts in surface
EL2_bs<- NA #No Backscatter
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = EL2_bathy, Area = "EL", Subarea = 2, Type = "Bathymetry", Source = "CSCAMP")) %>%
  bind_rows(extract_MBmetadata(data = EL2_bs, Area = "EL", Subarea = 2, Type = "Backscatter", Source = "CSCAMP"))

EL3_bathy<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/Elbow/EL3/Bathymetry/EL3_4mSwathBathy.bag") #Artifacts in surface
EL3_bs<- NA #No Backscatter
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = EL3_bathy, Area = "EL", Subarea = 3, Type = "Bathymetry", Source = "CSCAMP")) %>%
  bind_rows(extract_MBmetadata(data = EL3_bs, Area = "EL", Subarea = 3, Type = "Backscatter", Source = "CSCAMP"))

EL4_bathy<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/Elbow/EL4/Bathymetry/201804Elbow2mSA.bag") #Artifacts in surface
EL4_bs<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/Elbow/EL4/Backscatter/EL4BS1m.tiff")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = EL4_bathy, Area = "EL", Subarea = 4, Type = "Bathymetry", Source = "CSCAMP")) %>% 
  bind_rows(extract_MBmetadata(data = EL4_bs, Area = "EL", Subarea = 4, Type = "Backscatter", Source = "CSCAMP"))

EL5_bathy<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/Elbow/EL5/Bathymetry/1809_EL5_2mBathy_PRELIMINARY.bag")
EL5_bs<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = EL5_bathy, Area = "EL", Subarea = 5, Type = "Bathymetry", Source = "CSCAMP")) %>% 
  bind_rows(extract_MBmetadata(data = EL5_bs, Area = "EL", Subarea = 5, Type = "Backscatter", Source = "CSCAMP"))

EL6A_bathy<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/Elbow/EL6/Bathymetry/EL6A_PRELIMINARY.bag")
EL6A_bs<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = EL6A_bathy, Area = "EL", Subarea = "6A", Type = "Bathymetry", Source = "CSCAMP")) %>% 
  bind_rows(extract_MBmetadata(data = EL6A_bs, Area = "EL", Subarea = "6A", Type = "Backscatter", Source = "CSCAMP"))

EL6B_bathy<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/Elbow/EL6/Bathymetry/EL6B_PRELIMINARY.bag")
EL6B_bs<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = EL6B_bathy, Area = "EL", Subarea = "6B", Type = "Bathymetry", Source = "CSCAMP")) %>% 
  bind_rows(extract_MBmetadata(data = EL6B_bs, Area = "EL", Subarea = "6B", Type = "Backscatter", Source = "CSCAMP"))

#Radius
RADIUS1_bathy<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/Radius_Ulna/RADIUS_1/Bathymetry/RadiusStepPRELIMINARY.bag")
RADIUS1_bs<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = RADIUS1_bathy, Area = "RADIUS", Subarea = 1, Type = "Bathymetry", Source = "CSCAMP")) %>% 
  bind_rows(extract_MBmetadata(data = RADIUS1_bs, Area = "RADIUS", Subarea = 1, Type = "Backscatter", Source = "CSCAMP"))

RADIUS2A_bathy<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/Radius_Ulna/RADIUS_2/Bathymetry/RadiusStep2A_PRELIMINARY_3m.bag")
RADIUS2A_bs<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = RADIUS2A_bathy, Area = "RADIUS", Subarea = "2A", Type = "Bathymetry", Source = "CSCAMP")) %>% 
  bind_rows(extract_MBmetadata(data = RADIUS2A_bs, Area = "RADIUS", Subarea = "2A", Type = "Backscatter", Source = "CSCAMP"))

#Ulna
ULNA_bathy1A<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/Radius_Ulna/ULNA_1/Bathymetry/ULbow1A_PRELIMINARY_3m.bag")
ULNA_bs1A<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = ULNA_bathy1A, Area = "ULNA", Subarea = "1A", Type = "Bathymetry", Source = "CSCAMP")) %>% 
  bind_rows(extract_MBmetadata(data = ULNA_bs1A, Area = "ULNA", Subarea = "1A", Type = "Backscatter", Source = "CSCAMP"))

ULNA_bathy1B<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/Radius_Ulna/ULNA_1/Bathymetry/ULbow1B_PRELIMINARY_3m.bag")
ULNA_bs1B<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = ULNA_bathy1B, Area = "ULNA", Subarea = "1B", Type = "Bathymetry", Source = "CSCAMP")) %>% 
  bind_rows(extract_MBmetadata(data = ULNA_bs1B, Area = "ULNA", Subarea = "1B", Type = "Backscatter", Source = "CSCAMP"))

ULNA_bathy2AB<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/Radius_Ulna/ULNA_2/Bathymetry/UlnaLedge2A2B_PRELIMINARY_3m.bag")
ULNA_bs2AB<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = ULNA_bathy2AB, Area = "ULNA", Subarea = "2AB", Type = "Bathymetry", Source = "CSCAMP")) %>% 
  bind_rows(extract_MBmetadata(data = ULNA_bs2AB, Area = "ULNA", Subarea = "2AB", Type = "Backscatter", Source = "CSCAMP"))

#SWFMG
SWFMG_bathy1<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/SWFMG/SWFMG1/Bathymetry/CUBE/SWFMGMay2016_2.5mCube.bag")
SWFMG_bs1<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/SWFMG/SWFMG1/Backscatter/SWFMGMay2016_1mBackScatter.tiff")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = SWFMG_bathy1, Area = "SWFMG", Subarea = 1, Type = "Bathymetry", Source = "CSCAMP")) %>% 
  bind_rows(extract_MBmetadata(data = SWFMG_bs1, Area = "SWFMG", Subarea = 1, Type = "Backscatter", Source = "CSCAMP"))

SWFMG_bathy2<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/SWFMG/SWFMG2/Bathymetry/Swath_Angle/SWFMG2June2016_2m.tiff")
SWFMG_bs2<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/SWFMG/SWFMG2/Backscatter/SWFMG2June2016_1mBackScatter_cropped.tif")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = SWFMG_bathy2, Area = "SWFMG", Subarea = 2, Type = "Bathymetry", Source = "CSCAMP")) %>%
  bind_rows(extract_MBmetadata(data = SWFMG_bs2, Area = "SWFMG", Subarea = 2, Type = "Backscatter", Source = "CSCAMP"))

SWFMG_bathy3<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/SWFMG/SWFMG3/Bathymetry/CUBE/SWFMG3July2016Cube_2m.bag")
SWFMG_bs3<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/SWFMG/SWFMG3/Backscatter/SWFMG3July2016_1mBS.tiff")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = SWFMG_bathy3, Area = "SWFMG", Subarea = 3, Type = "Bathymetry", Source = "CSCAMP")) %>%
  bind_rows(extract_MBmetadata(data = SWFMG_bs3, Area = "SWFMG", Subarea = 3, Type = "Backscatter", Source = "CSCAMP"))

SWFMG_bathy4<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/SWFMG/SWFMG4/Bathymetry/CUBE/SWFMG4Cube2m.bag")
SWFMG_bs4<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/SWFMG/SWFMG4/Backscatter/SWFMG4BeamAverage1m.tiff")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = SWFMG_bathy4, Area = "SWFMG", Subarea = 4, Type = "Bathymetry", Source = "CSCAMP")) %>%
  bind_rows(extract_MBmetadata(data = SWFMG_bs4, Area = "SWFMG", Subarea = 4, Type = "Backscatter", Source = "CSCAMP"))

SWFMG_bathy5<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/SWFMG/SWFMG5/Bathymetry/SWFMG5_4mCUBEBathy.bag")
SWFMG_bs5<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = SWFMG_bathy5, Area = "SWFMG", Subarea = 5, Type = "Bathymetry", Source = "CSCAMP")) %>%
  bind_rows(extract_MBmetadata(data = SWFMG_bs5, Area = "SWFMG", Subarea = 5, Type = "Backscatter", Source = "CSCAMP"))

SWFMG_bathy6<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/SWFMG/SWFMG6_WFMG1/Bathymetry/Cube/WFMG_4mCUBEBathy.bag")
SWFMG_bs6<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = SWFMG_bathy6, Area = "SWFMG", Subarea = 6, Type = "Bathymetry", Source = "CSCAMP")) %>%
  bind_rows(extract_MBmetadata(data = SWFMG_bs6, Area = "SWFMG", Subarea = 6, Type = "Backscatter", Source = "CSCAMP"))

SWFMG_bathy7AB<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/SWFMG/SWFMG7_WGMG2/Bathymetry/WFMG2_4mSwathBathy.bag")
SWFMG_bs7AB<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = SWFMG_bathy7AB, Area = "SWFMG", Subarea = "7AB", Type = "Bathymetry", Source = "CSCAMP")) %>%
  bind_rows(extract_MBmetadata(data = SWFMG_bs7AB, Area = "SWFMG", Subarea = "7AB", Type = "Backscatter", Source = "CSCAMP"))

SWFMG_bathy8A<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/SWFMG/SWFMG8_WFMG3/Bathymetry/1809_WFMG3C_2mBathy_PRELIMINARY.bag")
SWFMG_bs8A<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = SWFMG_bathy8A, Area = "SWFMG", Subarea = "8A", Type = "Bathymetry", Source = "CSCAMP")) %>%
  bind_rows(extract_MBmetadata(data = SWFMG_bs8A, Area = "SWFMG", Subarea = "8A", Type = "Backscatter", Source = "CSCAMP"))

SWFMG_bathy8B<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/SWFMG/SWFMG8_WFMG3/Bathymetry/1809_WFMG3D_2mBathy_PRELIMINARY.bag")
SWFMG_bs8B<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = SWFMG_bathy8B, Area = "SWFMG", Subarea = "8B", Type = "Bathymetry", Source = "CSCAMP")) %>%
  bind_rows(extract_MBmetadata(data = SWFMG_bs8B, Area = "SWFMG", Subarea = "8B", Type = "Backscatter", Source = "CSCAMP"))

SWFMG_bathy9<- NA
SWFMG_bs9<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = SWFMG_bathy9, Area = "SWFMG", Subarea = "9", Type = "Bathymetry", Source = "CSCAMP")) %>%
  bind_rows(extract_MBmetadata(data = SWFMG_bs9, Area = "SWFMG", Subarea = "9", Type = "Backscatter", Source = "CSCAMP"))

SWFMG_bathyCombined<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/SWFMG/Combined Bathy/SWFMG_UptoDec2018/SWFMG_UptoDec2018.tif")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = SWFMG_bathyCombined, Area = "SWFMG", Subarea = "UptoDec2018", Type = "Bathymetry", Source = "CSCAMP"))

EFMG_bathy1<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/SWFMG/EFMG/EFMG1/Bathymetry/EastFMG1mswathangleApril2017.bag")
EFMG_bs1<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = EFMG_bathy1, Area = "EFMG", Subarea = "1", Type = "Bathymetry", Source = "CSCAMP")) %>%
  bind_rows(extract_MBmetadata(data = EFMG_bs1, Area = "EFMG", Subarea = "1", Type = "Backscatter", Source = "CSCAMP"))

#MS_GAP
MSGAP_bathy<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/MS_Gap/Bathymetry/Madison_Swanson_Gap_2m_Swath_Bathy.bag")
MSGAP_bs<- raster("Data/NFWF_Server/2_Projects/GIS/C_Multibeam_C-scamp/MS_Gap/Backscatter/WBII2016_04_MadisonSwansonGap_Snippetgeotiff.tif")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = MSGAP_bathy, Area = "MS", Subarea = "Gap", Type = "Bathymetry", Source = "CSCAMP")) %>%
  bind_rows(extract_MBmetadata(data = MSGAP_bs, Area = "MS", Subarea = "Gap", Type = "Backscatter", Source = "CSCAMP"))


#Non C-SCAMP Data

#Desoto Canyon
Desoto_bathyC<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/DeSoto_Canyon/Bathymetry/Central_Bathymetry/cenbathg/w001001.adf")
Desoto_bsC<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/DeSoto_Canyon/Backscatter/Central_Backscatter/cenmosg/w001001.adf")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = Desoto_bathyC, Area = "DeSoto Canyon", Subarea = "Central", Type = "Bathymetry", Source = "USGS")) %>%
  bind_rows(extract_MBmetadata(data = Desoto_bsC, Area = "DeSoto Canyon", Subarea = "Central", Type = "Backscatter", Source = "USGS"))

Desoto_bathyN<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/DeSoto_Canyon/Bathymetry/Northern_Bathymetry/nthbathg/w001001.adf")
Desoto_bsN<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/DeSoto_Canyon/Backscatter/Northern_Backscatter/nthmosg/w001001.adf")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = Desoto_bathyN, Area = "DeSoto Canyon", Subarea = "Northern", Type = "Bathymetry", Source = "USGS")) %>%
  bind_rows(extract_MBmetadata(data = Desoto_bsN, Area = "DeSoto Canyon", Subarea = "Northern", Type = "Backscatter", Source = "USGS"))

Desoto_bathyS<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/DeSoto_Canyon/Bathymetry/Southern_Bathymetry/sthbathg/w001001.adf")
Desoto_bsS<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/DeSoto_Canyon/Backscatter/Southern_Backscatter/sthmosg/w001001.adf")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = Desoto_bathyS, Area = "DeSoto Canyon", Subarea = "Southern", Type = "Bathymetry", Source = "USGS")) %>%
  bind_rows(extract_MBmetadata(data = Desoto_bsS, Area = "DeSoto Canyon", Subarea = "Southern", Type = "Backscatter", Source = "USGS"))

#FMG
FMG_bathy<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/Florida_Middle_Grounds_HAPC/Bathymetry/2006_fmg.asc")
FMG_bs<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/Florida_Middle_Grounds_HAPC/Backscatter/2006_07_fmg_5m_NAfixed.tif")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = FMG_bathy, Area = "FMG", Subarea = "", Type = "Bathymetry", Source = "David Naar")) %>%
  bind_rows(extract_MBmetadata(data = FMG_bs, Area = "FMG", Subarea = "", Type = "Backscatter", Source = "David Naar"))

#MS NE
MS_bathyNE<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/Madison_Swanson_MPA/NEcorner/Bathymetry/2002_madison_0001.asc")
MS_bsNE<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/Madison_Swanson_MPA/NEcorner/Backscatter/2002_07_Madison_5m_NAFixed.tif")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = MS_bathyNE, Area = "MS", Subarea = "NE Corner", Type = "Bathymetry", Source = "David Naar")) %>%
  bind_rows(extract_MBmetadata(data = MS_bsNE, Area = "MS", Subarea = "NE Corner", Type = "Backscatter", Source = "David Naar"))

#Pinnacles
Pinnacles_bathy<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/Pinnacles/Bathymetry/bathyg/w001001.adf")
Pinnacles_bs<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/Pinnacles/Backscatter/mosg/w001001.adf")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = Pinnacles_bathy, Area = "Pinnacles", Subarea = "", Type = "Bathymetry", Source = "USGS")) %>%
  bind_rows(extract_MBmetadata(data = Pinnacles_bs, Area = "Pinnacles", Subarea = "", Type = "Backscatter", Source = "USGS"))

#Pulley Ridge
Pulley_bathy<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/Pulley_Ridge/Bathymetry/allpr_filcrop.asc")
Pulley_bs<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = Pulley_bathy, Area = "Pulley Ridge", Subarea = "", Type = "Bathymetry", Source = "USGS")) %>%
  bind_rows(extract_MBmetadata(data = Pulley_bs, Area = "Pulley Ridge", Subarea = "", Type = "Backscatter", Source = "USGS"))

#Steamboat Lumps
SL_bathy<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/Steamboat_Lumps_MPA/Bathymetry/sbbathyg/w001001.adf")
SL_bs<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/Steamboat_Lumps_MPA/Backscatter/sbmosg/w001001.adf")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = SL_bathy, Area = "SL", Subarea = "", Type = "Bathymetry", Source = "USGS")) %>%
  bind_rows(extract_MBmetadata(data = SL_bs, Area = "SL", Subarea = "", Type = "Backscatter", Source = "USGS"))

#The Edges
Edges_bathy<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/The_Edges_Seasonal_MPA/Bathymetry/TheEdges_2005_08_corridor_001.asc")
Edges_bs<- NA
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = Edges_bathy, Area = "Edges", Subarea = "", Type = "Bathymetry", Source = "David Naar")) %>%
  bind_rows(extract_MBmetadata(data = Edges_bs, Area = "Edges", Subarea = "", Type = "Backscatter", Source = "David Naar"))

#Twin Ridges
TwinRidges_bathy<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/Twin_Ridges/Bathymetry/2002_twin_ridges_0001.asc")
TwinRidges_bs<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/Twin_Ridges/Backscatter/2002_07_twin_ridges_5m.tif")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = TwinRidges_bathy, Area = "Twin Ridges", Subarea = "", Type = "Bathymetry", Source = "David Naar")) %>%
  bind_rows(extract_MBmetadata(data = TwinRidges_bs, Area = "Twin Ridges", Subarea = "", Type = "Backscatter", Source = "David Naar"))

#WFS
WFS_bathyC<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/West_Florida_Shelf/Central/Bathymetry/cbathyg/w001001.adf")
WFS_bsC<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/West_Florida_Shelf/Central/Backscatter/cmosg/w001001.adf")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = WFS_bathyC, Area = "WFS", Subarea = "Central", Type = "Bathymetry", Source = "USGS")) %>%
  bind_rows(extract_MBmetadata(data = WFS_bsC, Area = "WFS", Subarea = "Central", Type = "Backscatter", Source = "USGS"))

WFS_bathyN<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/West_Florida_Shelf/Northern/Bathymetry/nbathyg/w001001.adf")
WFS_bsN<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/West_Florida_Shelf/Northern/Backscatter/nmosg/w001001.adf")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = WFS_bathyN, Area = "WFS", Subarea = "Northern", Type = "Bathymetry", Source = "USGS")) %>%
  bind_rows(extract_MBmetadata(data = WFS_bsN, Area = "WFS", Subarea = "Northern", Type = "Backscatter", Source = "USGS"))

WFS_bathyS<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/West_Florida_Shelf/Southern/Bathymetry/sbathyg/w001001.adf")
WFS_bsS<- raster("Data/NFWF_Server/2_Projects/GIS/G_Multibeam_Other_Sources/West_Florida_Shelf/Southern/Backscatter/smosg/w001001.adf")
MB_df<- MB_df %>% 
  bind_rows(extract_MBmetadata(data = WFS_bathyS, Area = "WFS", Subarea = "Southern", Type = "Bathymetry", Source = "USGS")) %>%
  bind_rows(extract_MBmetadata(data = WFS_bsS, Area = "WFS", Subarea = "Southern", Type = "Backscatter", Source = "USGS"))
