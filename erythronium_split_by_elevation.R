###Erythronium
##Separating only elevations###( T_temp + Strata + (T_temp × Stratum) + (1| Site) + (1| Year)
#put MAM after phenoDOY_LE or HE or ME for when plotting graphs for MAM...
#when running the code make sure flow goes as it is..if not putting the relevant spring period
#under the PhenoDoy_.... in the model is compulsory...


# Load required packages
library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(corrplot)
library(tidyverse)


canopy <- read.csv("C:/Users/User/Documents/canopy image analysis/Canopy_DOY_final.csv", header=TRUE) %>%
  mutate(Strata = 'Canopy',
         Site = gsub('C', '', Site)) %>%
  rename(DOY = DOY_canopy.greenup) 


ery <- read.csv("C:/Users/User/Documents/erythronium/Extracted_DOYs_50/Ery_final_DOY_50.csv", header=TRUE) %>%
  mutate(Strata ='Understory',
         Site = gsub('G','',Site))

###low elevation, MA

climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_MA = rowMeans(climate_new[,7:8]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_MA')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_MA))


climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_MA - T_spatial)

# Merge Datasets 
PhenoDOY_LE <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% c(1,3))


lmm.doy_LE_MA <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_LE, REML = TRUE)
AICc(lmm.doy_LE_MA) 
summary(lmm.doy_LE_MA)

lmm.doy_LE_MA_df<-as.data.frame(summary(lmm.doy_LE_MA)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'MA', .before ='Fixed_Effects')



###low elevation, MAM

climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_MAM = rowMeans(climate_new[,7:9]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_MAM')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_MAM))


climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_MAM - T_spatial)


# Merge Datasets 
PhenoDOY_LE_MAM_ery <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% c(1,3))

lmm.doy_LE_MAM_ery <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_LE_MAM_ery, REML = TRUE)
AICc(lmm.doy_LE_MAM_ery) 
summary(lmm.doy_LE_MAM_ery)

lmm.doy_LE_MAM_ery_df<-as.data.frame(summary(lmm.doy_LE_MAM_ery)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'MAM', .before ='Fixed_Effects')


###low elevation, MAMJ

climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_MAMJ = rowMeans(climate_new[,7:10]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_MAMJ')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_MAMJ))


climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_MAMJ - T_spatial)

# Merge Datasets 
PhenoDOY_LE <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% c(1,3))

lmm.doy_LE_MAMJ <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_LE, REML = TRUE)
AICc(lmm.doy_LE_MAMJ) 
summary(lmm.doy_LE_MAMJ)

lmm.doy_LE_MAMJ_df<-as.data.frame(summary(lmm.doy_LE_MAMJ)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'MAMJ', .before ='Fixed_Effects')

###low elevation, AM

climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_AM = rowMeans(climate_new[,8:9]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_AM')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_AM))


climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_AM - T_spatial)

# Merge Datasets 
PhenoDOY_LE <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% c(1,3))

lmm.doy_LE_AM <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_LE, REML = TRUE)
AICc(lmm.doy_LE_AM)
summary(lmm.doy_LE_AM)

lmm.doy_LE_AM_df<-as.data.frame(summary(lmm.doy_LE_AM)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'AM', .before ='Fixed_Effects')


###low elevation, AMJ

climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_AMJ = rowMeans(climate_new[,8:10]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_AMJ')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_AMJ))


climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_AMJ - T_spatial)

# Merge Datasets 
PhenoDOY_LE <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% c(1,3))

lmm.doy_LE_AMJ <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_LE, REML = TRUE)
AICc(lmm.doy_LE_AMJ)
summary(lmm.doy_LE_AMJ)

lmm.doy_LE_AMJ_df<-as.data.frame(summary(lmm.doy_LE_AMJ)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'AMJ', .before ='Fixed_Effects')


###low elevation, MJ

climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_MJ = rowMeans(climate_new[,9:10]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_MJ')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_MJ))


climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_MJ - T_spatial)

# Merge Datasets 
PhenoDOY_LE <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% c(1,3))

lmm.doy_LE_MJ <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_LE, REML = TRUE)
AICc(lmm.doy_LE_MJ)
summary(lmm.doy_LE_MJ)

lmm.doy_LE_MJ_df<-as.data.frame(summary(lmm.doy_LE_MJ)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'MJ', .before ='Fixed_Effects')

#Bind dataframes together
lmm_low.ele_ery <- rbind(lmm.doy_LE_MA_df, lmm.doy_LE_MAM_df, lmm.doy_LE_MAMJ_df, lmm.doy_LE_AM_df, lmm.doy_LE_AMJ_df, lmm.doy_LE_MJ_df)

write.csv(lmm_low.ele_ery, 'C:/Users/User/Documents/Pheno_outputs/split_by_elevation/erythronium/lmm_low.ele_ery.csv')

===============================================================================
  ##mid elevations, MA
  
climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_MA = rowMeans(climate_new[,7:8]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_MA')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_MA))

climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_MA - T_spatial)

# Merge Datasets 
PhenoDOY_ME <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% 5)

lmm.doy_ME_MA <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_ME, REML = TRUE)
AICc(lmm.doy_ME_MA)
summary(lmm.doy_ME_MA)

lmm.doy_ME_MA_df<-as.data.frame(summary(lmm.doy_ME_MA)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'MA', .before ='Fixed_Effects')

###mid elevation, MAM

climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_MAM = rowMeans(climate_new[,7:9]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_MAM')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_MAM))

climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_MAM - T_spatial)

# Merge Datasets 
PhenoDOY_ME_MAM_ery <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% 5)

lmm.doy_ME_MAM_ery <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_ME_MAM_ery, REML = TRUE)
AICc(lmm.doy_ME_MAM_ery)
summary(lmm.doy_ME_MAM_ery)

lmm.doy_ME_MAM_ery_df<-as.data.frame(summary(lmm.doy_ME_MAM_ery)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'MAM', .before ='Fixed_Effects')

###mid elevation, MAMJ

climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_MAMJ = rowMeans(climate_new[,7:10]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_MAMJ')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_MAMJ))

climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_MAMJ - T_spatial)

# Merge Datasets 
PhenoDOY_ME <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% 5)

lmm.doy_ME_MAMJ <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_ME, REML = TRUE)
AICc(lmm.doy_ME_MAMJ) 
summary(lmm.doy_ME_MAMJ)

lmm.doy_ME_MAMJ_df<-as.data.frame(summary(lmm.doy_ME_MAMJ)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'MAMJ', .before ='Fixed_Effects')

###mid elevation, AM

climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_AM = rowMeans(climate_new[,8:9]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_AM')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_AM))

climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_AM - T_spatial)

# Merge Datasets 
PhenoDOY_ME <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% 5)

lmm.doy_ME_AM <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_ME, REML = TRUE)
AICc(lmm.doy_ME_AM)
summary(lmm.doy_ME_AM)

lmm.doy_ME_AM_df<-as.data.frame(summary(lmm.doy_ME_AM)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'AM', .before ='Fixed_Effects')

###mid elevation, AMJ

climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_AMJ = rowMeans(climate_new[,8:10]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_AMJ')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_AMJ))

climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_AMJ - T_spatial)

# Merge Datasets 
PhenoDOY_ME <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% 5)


lmm.doy_ME_AMJ <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_ME, REML = TRUE)
AICc(lmm.doy_ME_AMJ) 
summary(lmm.doy_ME_AMJ)

lmm.doy_ME_AMJ_df<-as.data.frame(summary(lmm.doy_ME_AMJ)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'AMJ', .before ='Fixed_Effects')


###mid elevation, MJ

climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_MJ = rowMeans(climate_new[,9:10]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_MJ')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_MJ))

climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_MJ - T_spatial)

# Merge Datasets 
PhenoDOY_ME <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% 5)

lmm.doy_ME_MJ <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_ME, REML = TRUE)
AICc(lmm.doy_ME_MJ) 
summary(lmm.doy_ME_MJ)

lmm.doy_ME_MJ_df<-as.data.frame(summary(lmm.doy_ME_MJ)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'MJ', .before ='Fixed_Effects')


#Bind dataframes together
lmm_mid.ele_ery <- rbind(lmm.doy_ME_MA_df, lmm.doy_ME_MAM_df, lmm.doy_ME_MAMJ_df, lmm.doy_ME_AM_df, lmm.doy_ME_AMJ_df, lmm.doy_ME_MJ_df)

write.csv(lmm_mid.ele_ery, 'C:/Users/User/Documents/Pheno_outputs/split_by_elevation/erythronium/lmm_mid.ele_ery.csv')


=============================================================================
###high elevation, MA
  
climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_MA = rowMeans(climate_new[,7:8]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_MA')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_MA))

climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_MA - T_spatial)

# Merge Datasets 
PhenoDOY_HE <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% c(7,8))

lmm.doy_HE_MA <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_HE, REML = TRUE)
AICc(lmm.doy_HE_MA)
summary(lmm.doy_HE_MA)

lmm.doy_HE_MA_df<-as.data.frame(summary(lmm.doy_HE_MA)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'MA', .before ='Fixed_Effects')

###high elevation, MAM

climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_MAM = rowMeans(climate_new[,7:9]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_MAM')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_MAM))

climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_MAM - T_spatial)


# Merge Datasets 
PhenoDOY_HE_MAM_ery <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% c(7,8))

lmm.doy_HE_MAM_ery <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_HE_MAM_ery, REML = TRUE)
AICc(lmm.doy_HE_MAM_ery)
summary(lmm.doy_HE_MAM_ery)

lmm.doy_HE_MAM_ery_df<-as.data.frame(summary(lmm.doy_HE_MAM_ery)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'MAM', .before ='Fixed_Effects')

###high elevation, MAMJ

climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_MAMJ = rowMeans(climate_new[,7:10]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_MAMJ')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_MAMJ))

climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_MAMJ - T_spatial)

# Merge Datasets 
PhenoDOY_HE <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% c(7,8))

lmm.doy_HE_MAMJ <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_HE, REML = TRUE)
AICc(lmm.doy_HE_MAMJ) 
summary(lmm.doy_HE_MAMJ)

lmm.doy_HE_MAMJ_df<-as.data.frame(summary(lmm.doy_HE_MAMJ)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'MAMJ', .before ='Fixed_Effects')


###high elevation, AM

climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_AM = rowMeans(climate_new[,8:9]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_AM')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_AM))

climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_AM - T_spatial)

# Merge Datasets 
PhenoDOY_HE <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% c(7,8))

lmm.doy_HE_AM <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_HE, REML = TRUE)
AICc(lmm.doy_HE_AM)
summary(lmm.doy_HE_AM)

lmm.doy_HE_AM_df<-as.data.frame(summary(lmm.doy_HE_AM)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'AM', .before ='Fixed_Effects')


###high elevation, AMJ

climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_AMJ = rowMeans(climate_new[,8:10]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_AMJ')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_AMJ))

climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_AMJ - T_spatial)

# Merge Datasets 
PhenoDOY_HE <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% c(7,8))


lmm.doy_HE_AMJ <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_HE, REML = TRUE)
AICc(lmm.doy_HE_AMJ) 
summary(lmm.doy_HE_AMJ)

lmm.doy_HE_AMJ_df<-as.data.frame(summary(lmm.doy_HE_AMJ)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'AMJ', .before ='Fixed_Effects')



###high elevation, MJ

climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)

climate_new <- climate_new %>%
  mutate(T_spring_MJ = rowMeans(climate_new[,9:10]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_MJ')%>%
  group_by(Site) %>%
  mutate(T_spatial = mean(T_spring_MJ))

climate_new <- climate_new %>%
  mutate(T_spatial_centred = T_spatial - mean (climate_new$T_spatial)) %>%
  mutate(T_temporal = T_spring_MJ - T_spatial)

# Merge Datasets 
PhenoDOY_HE <- full_join(canopy, ery) %>%
  left_join(climate_new, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(Plot %in% c(7,8))

lmm.doy_HE_MJ <- lmer(DOY ~ T_temporal + Strata + (T_temporal*Strata) + (1| Site) + (1| Year), data = PhenoDOY_HE, REML = TRUE)
AICc(lmm.doy_HE_MJ) 
summary(lmm.doy_HE_MJ)

lmm.doy_HE_MJ_df<-as.data.frame(summary(lmm.doy_HE_MJ)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'MJ', .before ='Fixed_Effects')


#Bind dataframes together

lmm_high.ele_ery <- rbind(lmm.doy_HE_MA_df, lmm.doy_HE_MAM_df, lmm.doy_HE_MAMJ_df, lmm.doy_HE_AM_df, lmm.doy_HE_AMJ_df, lmm.doy_HE_MJ_df)

write.csv(lmm_high.ele_ery, 'C:/Users/User/Documents/Pheno_outputs/split_by_elevation/erythronium//lmm_high.ele_ery.csv')



####plotting the graphs

library(ggeffects)
library(ggpubr)
install.packages("ggpubr")
install.packages("ggeffects")
#   Set ggplot theme so all plots match
theme_set(theme_classic() +
            theme(axis.text = element_text(colour = 'black', size = 12),
                  axis.title = element_text(colour = 'black', size = 12, face = 'bold'),
                  legend.title = element_blank(),
                  legend.text = element_text(colour = 'black', size = 12)
            ))

# Spring period MAM
pred.lmm.LE.MAM_ery <- ggpredict(lmm.doy_LE_MAM_ery, terms = c("T_temporal", "Strata"))
pred.lmm.ME.MAM_ery <- ggpredict(lmm.doy_ME_MAM_ery, terms = c("T_temporal", "Strata"))
pred.lmm.HE.MAM_ery <- ggpredict(lmm.doy_HE_MAM_ery, terms = c("T_temporal", "Strata"))

library(grid)
library(gridExtra)
install.packages("ggtext")
library(ggtext)


plotMAM_low_ery <- ggplot(data = pred.lmm.LE.MAM_ery) +
  geom_point(data = PhenoDOY_LE_MAM_ery, aes(x = T_temporal, y = DOY, colour = Strata), size = 2) +
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, fill = group), alpha = 0.5) +
  geom_line(aes(x = x, y = predicted, colour = group), size = 2) +
  labs(x = "Temporal temperature \n(\u00B0C)", 
       y = "Day of year of canopy closure <br> or <b><i>Erythronium</i></b> leaf expansion") +  # Use <br> for HTML line break
  scale_colour_manual(values = c("forestgreen", "goldenrod2")) +
  scale_fill_manual(values = c('forestgreen', 'goldenrod2')) +
  scale_y_continuous(limits = c(115,180)) +
  scale_x_continuous(breaks = seq(-1.5, 1, by = 0.5), labels = scales::label_number(accuracy = 0.1))+
  theme(
    legend.position = "none",
    axis.title.y = element_markdown()  # This allows Markdown formatting
  )+
  annotation_custom(grob = textGrob("(d)", gp = gpar(fontsize = 14, fontface = "bold")),
                    xmin = -1.5, ymin = 180) 
  plotMAM_low_ery
  
  ggsave("C:/Users/User/Desktop/combined_plot_low_ery.jpg", plot = plotMAM_low_ery, width = 3.5, height = 3.5, dpi = 300)
  
  
plotMAM_mid_ery <- ggplot(data = pred.lmm.ME.MAM_ery) +
  geom_point(data = PhenoDOY_ME_MAM_ery, aes(x = T_temporal, y = DOY, colour = Strata), size = 2) +
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, fill = group), alpha = 0.5) +
  geom_line(aes(x = x, y = predicted, colour = group), size = 2) +
  labs(x = "Temporal temperature \n(\u00B0C)", 
       y = "Day of year of canopy closure <br> or <b><i>Erythronium</i></b> leaf expansion") +  # Use <br> for HTML line break
  scale_colour_manual(values = c("forestgreen", "goldenrod2")) +
  scale_fill_manual(values = c('forestgreen', 'goldenrod2')) +
  scale_y_continuous(limits = c(115,180)) +
  scale_x_continuous(breaks = seq(-1.5, 1, by = 0.5), labels = scales::label_number(accuracy = 0.1))+
  theme(
    legend.position = "none",
    axis.title.y = ggtext::element_markdown(lineheight = 1.2)  # This will render HTML and Markdown
  ) +
  annotation_custom(grob = textGrob("(e)", gp = gpar(fontsize = 14, fontface = "bold")),
                    xmin = -1.5, ymin = 180)
  plotMAM_mid_ery
  ggsave("C:/Users/User/Desktop/combined_plot_mid_ery.jpg", plot = plotMAM_mid_ery, width = 3.5, height = 3.5, dpi = 300)
  

plotMAM_high_ery <- ggplot(data = pred.lmm.HE.MAM_ery) +
  geom_point(data = PhenoDOY_HE_MAM_ery, aes(x = T_temporal, y = DOY, colour = Strata), size = 2) +
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, fill = group), alpha = 0.5) +
  geom_line(aes(x = x, y = predicted, colour = group), size = 2) +
  labs(x = "Temporal temperature \n(\u00B0C)", 
       y = "Day of year of canopy closure <br> or <b><i>Erythronium</i></b> leaf expansion") +  # Use <br> for HTML line break
  scale_colour_manual(
    name = NULL,
    values = c("forestgreen", "goldenrod2"),
    labels = c("Canopy", "<i>Erythronium</i>")
  ) +
  scale_fill_manual(name= NULL, values = c('forestgreen', 'goldenrod2'), labels = c("Canopy", "<i>Erythronium</i>"))+
  scale_y_continuous(limits = c(115,180)) +
  scale_x_continuous(breaks = seq(-1.5, 1, by = 0.5), labels = scales::label_number(accuracy = 0.1))+
  theme(
    legend.position = "right",  # Change to "none" if you don't want legend
    legend.direction = "horizontal",
    legend.text = ggtext::element_markdown(size=15),  # Enables italic in legend
    legend.key.size = unit(1.2, "cm"),        
    axis.title.y = ggtext::element_markdown(lineheight = 1.2)
  )  +
  annotation_custom(grob = textGrob("(f)", gp = gpar(fontsize = 14, fontface = "bold")),
                    xmin = -1.5, ymin = 180)
  plotMAM_high_ery
  ggsave("C:/Users/User/Desktop/combined_plot_high_ery.jpg", plot = plotMAM_high_ery, width = 3.5, height = 3.5, dpi = 300)
  
  
  
  
#######################################


  # Load library (if not loaded yet)
  install.packages("cowplot")
  library(cowplot)
  library(grid)
  
  # Extract just the legend
  legend_only <- get_legend(plotMAM_high_ery)
  
  # Display it
  grid.newpage()
  grid.draw(legend_only)
  



