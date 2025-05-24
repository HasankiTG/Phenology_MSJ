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







