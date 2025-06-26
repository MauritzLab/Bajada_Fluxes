# Check soil moisture and water potential data from Sap Flux Network 
# probes installed 11 August 2023
# script author: Marguerite Mauritz, Victoria Martinez
# 17 August 2023
 
# load libraries
library(data.table)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(zoo)
library(readxl)
library(cowplot)
library(reshape2)
library(bigleaf)

# import data 
basedir <- "C:/Users/memauritz/OneDrive - University of Texas at El Paso/Bahada/SapFlowNet/SoilProbes/Data/ASCII"
setwd(basedir)

# read column names and units
sfn_soildat_colnames1 <- fread("BajadaCR1000XSapFlowSoilMoisture_SoilData_SapFlow.dat",
                       header = TRUE, skip=1,sep=",", fill=TRUE,
                       na.strings=c(-9999,"#NAME?"))[1,]
# read data and use column names
sfn_soildat1 <- fread("BajadaCR1000XSapFlowSoilMoisture_SoilData_SapFlow.dat",
              header = FALSE, skip=4, sep=",", fill=TRUE,
              na.strings=c(-9999,"#NAME?"),
              col.names=colnames(sfn_soildat_colnames1))

# read data and use column names from single file
# sfn_soildat2 <- fread("TOA5_20231027_26185.Sapflow_Soil_2023_09_15_1330.dat",
#                       header = FALSE, skip=4, sep=",", fill=TRUE,
#                       na.strings=c(-9999,"#NAME?"),
#                       col.names=colnames(sfn_soildat_colnames1))

# list all 26185.Sapflow files
sfn_files <- list.files(path=basedir,full.names=TRUE, pattern="26185.Sapflow_Soil")

# import and combine data from sfn_files
sfn_soildat2 <- do.call("rbind", lapply(sfn_files, header = FALSE, fread, sep=",", dec=".",skip = 4,
                                       fill=TRUE, na.strings=c(-9999,"#NAME?"), col.names=colnames(sfn_soildat_colnames1)))

# read metadata for probe IDs (accurate after 10-27-2023 when probes were moved from mesquite 2 to 5cm at M1, C2, bare)
sfn_metadata <- fread("C:/Users/memauritz/OneDrive - University of Texas at El Paso/Bahada/SapFlowNet/SoilProbes/sapfluxProbeID_Metadata.csv")

# merge both data files
sfn_soildat <- rbind(sfn_soildat1, sfn_soildat2)

# format TIMESTAMP and make long format
sfn_soildat <- sfn_soildat %>%
  mutate(datetime = ymd_hms(TIMESTAMP, tz="UTC"))%>%
  pivot_longer(!c(TIMESTAMP, datetime, RECORD),names_to="measurement", values_to="value")

# specify columns to skip for soil data
rows_to_skip <- c("BattV_Avg","PTemp_C_Avg")

# separate columns from probe into metric and probe.number
sfn_soildat <- sfn_soildat %>%
  filter(!measurement %in% rows_to_skip) %>% 
  tidyr::separate(measurement, into=c("metric","probe.num"), sep = "_", remove=FALSE, fill="right", extra="drop")%>%
  bind_rows(filter(sfn_soildat, measurement %in% rows_to_skip))%>%
  mutate(metric=case_when (measurement %in% rows_to_skip ~ as.character(measurement),
                           TRUE ~ as.character(metric)))

# add metadata to data file
sfn_soildat <- right_join(sfn_soildat,sfn_metadata, by="measurement")
  
# graph to check datalogger voltage and paneltemp
sfn_soildat %>% filter(datetime>as.Date("2023-08-10")&
                         metric %in% c("BattV_Avg","PTemp_C_Avg"))%>%
  ggplot(., aes(datetime, value))+
  geom_line()+
  facet_grid(metric~location,scales="free_y")


#data after this is after mesquite_23 probes were removed and placed at 5cm depth at bare, creosote_1,creosote_2 and mesquite_1
#creosote_1 does not have 5cm probe sensor


# # graph data by metric and color by number 
# sfn_soildat %>% filter(datetime>as.Date("2023-10-27")&
#                           #probe.num %in% c(NA,1,2,3)&
#                           metric %in% c("T","Temp","VWC","WaterPot"))%>%
# ggplot(., aes(datetime, value,color=factor(depth)))+
#   geom_line()+
#   facet_grid(metric~location,scales="free_y")

#graph # graph data by metric and color by number with VWC values less than one
sfn_soildat %>% filter(datetime>as.Date("2023-10-27")&
                         #probe.num %in% c(NA,1,2,3)&
                         ((metric=="VWC"&value<=1)|metric %in% c("T","Temp","WaterPot")))%>%
  ggplot(., aes(datetime, value,color=factor(depth)))+
  geom_line()+
  facet_grid(metric~location,scales="free_y")


#graph VWC and T from CS650 without 5cm
sfn_soildat %>% filter(datetime>as.Date("2023-10-27")&
                         #probe.num %in% c(NA,1,2,3)&
                         metric %in% c("T","VWC")&
                         depth!=5)%>%
  ggplot(., aes(datetime, value,color=factor(depth)))+
  geom_line()+
  facet_grid(metric~location,scales="free_y")


# graph data only from 5cm 
sfn_soildat %>% filter(datetime>as.Date("2023-10-27")&
                         #probe.num %in% c(NA,1,2,3)&
                        metric %in% c("T","Temp","VWC","WaterPot")&
                         depth==5)%>%
  ggplot(., aes(datetime, value,color=factor(depth)))+
  geom_line()+
  facet_grid(metric~location,scales="free_y")

##Soil Sensor Data compared to Met Rain from Tower at Bahada

# import rain data from Tower Climate met from CZO data 
Rain_met.colname <- fread(paste("C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/CZO_Data/Bahada/CR3000/L1/TowerClimate_met/Bahada_CR3000_met_L1_2025.csv",sep=""),
                          header = TRUE,, skip=1, sep=",", fill=TRUE,
                          na.strings=c(-9999,"#NAME?"))[1,]

Rain_met.CZO <- fread(paste("C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/CZO_Data/Bahada/CR3000/L1/TowerClimate_met/Bahada_CR3000_met_L1_2025.csv",sep=""),               
                  header = FALSE, skip=4, sep=",", fill=TRUE,
                  na.strings=c(-9999,"#NAME?"),
                  col.names = colnames(Rain_met.colname))

# list all Tower Met files
Met_files.CZO <- list.files(path="C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/CZO_Data/Bahada/CR3000/L1/TowerClimate_met/",full.names=TRUE, pattern="Bahada_CR3000_met_L1")

# import and combine data from Bahada Tower Met files
Rain_met.CZO <- do.call("rbind", lapply(Met_files.CZO, header = FALSE, fread, sep=",", dec=".",skip = 4,
                                        fill=TRUE, na.strings=c(-9999,"#NAME?"), col.names=colnames(Rain_met.colname)))
#format TIMESTAMP and make long format
Rain_met.CZO <- Rain_met.CZO %>%
  mutate(datetime = ymd_hms(TIMESTAMP, tz="UTC"))%>%
  pivot_longer(!c(TIMESTAMP, datetime, RECORD),names_to="measurement", values_to="value")

# import rain data from Tower Climate met from E: drive
Rain_met.E <- fread(paste("Y:/Bahada/CR3000/L1/TowerClimate_met/Bahada_CR3000_met_L1_2025.csv", sep=""),               
                  header = FALSE, skip=4, sep=",", fill=TRUE,
                  na.strings=c(-9999,"#NAME?"),
                  col.names = colnames(Rain_met.colname))

#format TIMESTAMP and make long format
Rain_met.E <- Rain_met.E %>%
  mutate(datetime = ymd_hms(TIMESTAMP, tz="UTC"))%>%
  pivot_longer(!c(TIMESTAMP, datetime, RECORD),names_to="measurement", values_to="value")

Rain_met <- full_join(Rain_met.CZO, Rain_met.E)

# # specify columns to skip for soil data
# met_rows_to_skip <- c("t_hmp",
#                       "rh_hmp", 
#                       "e_hmp", 
#                       "atm_press", 
#                       "hor_wnd_spd", 
#                       "hor_wnd_dir", 
#                       "par", 
#                       "albedo", 
#                       "lws_2", 
#                       "NetRs", 
#                       "NetRl", 
#                       "UpTot",
#                       "DnTot",
#                       "CO2_raw",
#                       "H2O_raw")

# separate columns from measurement into precipitation
Rain_met <- Rain_met %>%
  filter(measurement == "precip_Tot")

#create daily summary for VWC and Water Pots to see dynamics more clearly
soil.day <- sfn_soildat%>%
  mutate(date_time = ymd_hms(TIMESTAMP, tz = "UTC"),
                     date = as.Date(datetime),
                     year = year(datetime),
                     month = month(datetime),
                     day = day(datetime))%>%
  group_by(date, metric, probe.num, depth)%>%
  summarise(dailyavg = mean(value))%>%
  filter(metric %in% c("VWC","WaterPot"))

#create daily summary for met precipitation
Rain_met.day <- Rain_met%>%
  mutate(date_time = ymd_hms(TIMESTAMP, tz = "UTC"),
         date = as.Date(datetime),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime))%>%
  group_by(date, measurement)%>%
  summarise(dailyavg = mean(value))
  
#create plot for VWC in soil probes
Soil.plot <-
soil.day %>% filter(date>as.Date("2023-10-27")& 
                         metric == "VWC")%>%
  ggplot(., aes(date, dailyavg,color=factor(depth)))+
  geom_line()+
  labs(title = "Daily VWC Avg of Soil Sensor by Depth", x = "Time", y = "VWC (m^3/m^3)")

#create plot for WaterPot in soil probes
Soil.plot2 <-
  soil.day %>% filter(date>as.Date("2023-10-27")& 
                        metric == "WaterPot")%>%
  ggplot(., aes(date, dailyavg,color=factor(depth)))+
  geom_line()+
  labs(title = "Daily Water Potential Avg by Depth", x = "Time", y = "VWC (m^3/m^3)")

# Find common date range to create aligned plots
start_date <- as.Date("2023-10-27")
end_date <- max(
  max(soil.day$date, na.rm = TRUE),
  max(Rain_met.day$date, na.rm = TRUE))

# Plot for Soil VWC
Soil.plot <- soil.day %>%
  filter(date > start_date & metric == "VWC") %>%
  ggplot(aes(date, dailyavg, color = factor(depth))) +
  geom_line() +
  scale_x_date(limits = c(start_date, end_date)) +  # sync x-axis
  labs(title = "Daily VWC Avg of Soil Sensor by Depth", x = "Time", y = "VWC (m³/m³)")

# Plot for Met precipitation
Met.plot <- Rain_met.day %>%
  filter(date > start_date & measurement == "precip_Tot") %>%
  ggplot(aes(date, dailyavg)) +
  geom_line(color = "red") +  # use fixed color
  scale_x_date(limits = c(start_date, end_date)) +  # sync x-axis
  labs(title = "Daily Met Precipitation Avg", x = "Time", y = "Rain (mm)")

# Combine VWC and met rain plots
plot_grid(Soil.plot, Met.plot, nrow = 2, align = "v")

# Combine WaterPot and met rain plots
plot_grid(Soil.plot2, Met.plot, nrow = 2, align = "v")

####

# 
# sfn_soildat <- sfn_soildat %>%
#   mutate(date_time = ymd_hms(TIMESTAMP, tz = "UTC"),
#     date = as.Date(date_time),
#     year = year(date_time),
#     month = month(date_time))
# 

# soil_VWC.day <- sfn_soildat%>%
#   group_by(date)%>%
#   summarise(VWC1.day = mean(VWC_1_Avg, na.rm = TRUE),
#             VWC2.day = mean(VWC_2_Avg, na.rm = TRUE),
#             VWC3.day = mean(VWC_3_Avg, na.rm = TRUE),
#             VWC4.day = mean(VWC_4_Avg, na.rm = TRUE),
#             VWC5.day = mean(VWC_5_Avg, na.rm = TRUE),
#             VWC6.day = mean(VWC_6_Avg, na.rm = TRUE),
#             VWC7.day = mean(VWC_7_Avg, na.rm = TRUE),
#             VWC8.day = mean(VWC_8_Avg, na.rm = TRUE),
#             VWC9.day = mean(VWC_9_Avg, na.rm = TRUE),
#             VWC10.day = mean(VWC_10_Avg, na.rm = TRUE),
#             VWC11.day = mean(VWC_11_Avg, na.rm = TRUE),
#             VWC12.day = mean(VWC_12_Avg, na.rm = TRUE),
#             VWC13.day = mean(VWC_13_Avg, na.rm = TRUE),
#             VWC14.day = mean(VWC_14_Avg, na.rm = TRUE),
#             VWC15.day = mean(VWC_15_Avg, na.rm = TRUE),
#             WaterPot1.day = mean(WaterPot_1_Avg, na.rm = TRUE),
#             WaterPot2.day = mean(WaterPot_2_Avg, na.rm = TRUE),
#             WaterPot3.day = mean(WaterPot_3_Avg, na.rm = TRUE),
#             WaterPot4.day = mean(WaterPot_4_Avg, na.rm = TRUE),
#             WaterPot5.day = mean(WaterPot_5_Avg, na.rm = TRUE),
#             WaterPot6.day = mean(WaterPot_6_Avg, na.rm = TRUE),
#             WaterPot7.day = mean(WaterPot_7_Avg, na.rm = TRUE),
#             WaterPot8.day = mean(WaterPot_8_Avg, na.rm = TRUE),
#             WaterPot9.day = mean(WaterPot_9_Avg, na.rm = TRUE),
#             WaterPot10.day = mean(WaterPot_10_Avg, na.rm = TRUE),
#             WaterPot11.day = mean(WaterPot_11_Avg, na.rm = TRUE),
#             WaterPot12.day = mean(WaterPot_12_Avg, na.rm = TRUE),
#             WaterPot13.day = mean(WaterPot_13_Avg, na.rm = TRUE),
#             WaterPot14.day = mean(WaterPot_14_Avg, na.rm = TRUE),
#             WaterPot15.day = mean(WaterPot_15_Avg, na.rm = TRUE))%>%
#   mutate(year = year(date),
#          DOY = yday(date))
# 

# 
# 
# 
# #reshape into long format
# soil_VWC_long <- soil_VWC.day %>%
#   pivot_longer(
#     cols = starts_with(c("VWC", "WaterPot")),
#     names_to = "sensor",
#     values_to = c("VWC", "WaterPot"))
# 
# #create new datetime column
# soil_VWC_long <- soil_VWC_long %>%
#   mutate(datetime = ymd_h(paste(year, month, hour, sep = "-")))
# 

#          
