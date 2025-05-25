##Understory(Erythronium)-canopy full model
##Methodology:2.6.3.The effect of temperature on phenology of canopy closure and understory leaf expansion
##DOY ~ T_spatial_centred + T_temporal + Strata + T_spatial_centred*Strata + T_temporal*Strata + (1| Site) + (1| Year)

# Load required packages
library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(tidyverse)

# Canopy DOYs
canopy <- read.csv("C:/Users/User/Documents/canopy image analysis/Canopy_DOY_final.csv", header=TRUE) %>%
  mutate(Strata = 'Canopy',
         Site = gsub('C', '', Site)) %>%
  rename(DOY = DOY_canopy.greenup) 

# Erythronium DOYs
ery <- read.csv("C:/Users/User/Documents/erythronium/Extracted_DOYs_50/Ery_final_DOY_50.csv", header=TRUE) %>%
  mutate(Strata ='Understory',
         Site = gsub('G','',Site))

# Climate data 
climate_new <- read.csv("C:/Users/User/Documents/climatena/climate_new.csv", header=TRUE)


# Calculate T_spring for each site-year combinations
climate_new <- climate_new %>%
  mutate(T_spring_MA =rowMeans(climate_new[,7:8]),
         T_spring_MAM = rowMeans(climate_new[,7:9]),
         T_spring_MAMJ = rowMeans(climate_new[,7:10]),
         T_spring_AM = rowMeans(climate_new[,8:9]),
         T_spring_AMJ = rowMeans(climate_new[,8:10]),
         T_spring_MJ = rowMeans(climate_new[,9:10]),
         Site = gsub('-','', Site))%>%
  select('Year', 'Site', 'Transect', 'Plot', 'T_spring_MA', 'T_spring_MAM', 'T_spring_MAMJ', 'T_spring_AM', 'T_spring_AMJ', 'T_spring_MJ')


# Mean center corrected climate data and calculate T_spatial and T_temporal for each spring period
# 1| Reformat climate corrected to long structure
climate_new2 <- climate_new %>%
  pivot_longer(
    cols = starts_with("T_spring"),
    names_to = "SpringPeriod",
    names_prefix = "T_spring_",
    values_to = "T_spring")

# 2| Calculate mean centred T_spatial and T_temporal for each Spring Period
climate_new2_centred <- climate_new2 %>%
  group_by(Site, SpringPeriod) %>%
  mutate(T_spatial = mean(T_spring)) %>% 
  group_by(SpringPeriod) %>%
  mutate(T_spatial_mean = mean(T_spatial)) %>%
  mutate(T_spatial_centred = T_spatial - T_spatial_mean) %>%
  mutate(T_temporal = T_spring - T_spatial)

# Merge with Phenology Dataframes
PhenoDOY_ery <- full_join(canopy, ery) %>%
  full_join(climate_new2_centred, by = c("Year", "Site", "Transect", "Plot")) %>%
  group_by(Site) %>%
  mutate(Plot = as.factor(Plot),
         Plant = as.factor(Plant),
         Year = as.factor(Year))%>%
  filter(DOY != 'NA')

write.csv(PhenoDOY_ery, 'C:/Users/User/Documents/Pheno_outputs/PhenoDOY_ery.csv', row.names = FALSE)


# Run LMMs for each previousy identified spring period using T_spatial_centred and T_temproal 

install.packages("lmerTest")
library(lmerTest)
library(lme4)

lmmDOY_MA_ery <- lmer(DOY ~ T_spatial_centred + T_temporal + Strata + T_spatial_centred*Strata + T_temporal*Strata + (1| Site) + (1| Year), data = (PhenoDOY_ery %>% filter(SpringPeriod == 'MA')), REML = TRUE)
summary(lmmDOY_MA_ery)
AICc(lmmDOY_MA_ery)
lmmDOY_MA_ery <-as.data.frame(summary(lmmDOY_MA_ery)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'MA', .before ='Fixed_Effects')


lmmDOY_MAM_ery <- lmer(DOY ~ T_spatial_centred + T_temporal + Strata + T_spatial_centred*Strata + T_temporal*Strata + (1| Site) + (1| Year), data = (PhenoDOY_ery %>% filter(SpringPeriod == 'MAM')), REML = TRUE)
summary(lmmDOY_MAM_ery)
AICc(lmmDOY_MAM_ery)
lmmDOY_MAM_ery <-as.data.frame(summary(lmmDOY_MAM_ery)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'MAM', .before ='Fixed_Effects')

lmmDOY_MAMJ_ery <- lmer(DOY ~ T_spatial_centred + T_temporal + Strata + T_spatial_centred*Strata + T_temporal*Strata + (1| Site) + (1| Year), data = (PhenoDOY_ery %>% filter(SpringPeriod == 'MAMJ')), REML = TRUE)
summary(lmmDOY_MAMJ_ery)
AICc(lmmDOY_MAMJ_ery)
lmmDOY_MAMJ_ery <-as.data.frame(summary(lmmDOY_MAMJ_ery)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'MAMJ', .before ='Fixed_Effects')

lmmDOY_AM_ery <- lmer(DOY ~ T_spatial_centred + T_temporal + Strata + T_spatial_centred*Strata + T_temporal*Strata + (1| Site) + (1| Year), data = (PhenoDOY_ery %>% filter(SpringPeriod == 'AM')), REML = TRUE)
summary(lmmDOY_AM_ery)
AICc(lmmDOY_AM_ery)
lmmDOY_AM_ery <-as.data.frame(summary(lmmDOY_AM_ery)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'AM', .before ='Fixed_Effects')

lmmDOY_AMJ_ery <- lmer(DOY ~ T_spatial_centred + T_temporal + Strata + T_spatial_centred*Strata + T_temporal*Strata + (1| Site) + (1| Year), data = (PhenoDOY_ery %>% filter(SpringPeriod == 'AMJ')), REML = TRUE)
summary(lmmDOY_AMJ_ery)
AICc(lmmDOY_AMJ_ery)
lmmDOY_AMJ_ery <-as.data.frame(summary(lmmDOY_AMJ_ery)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'AMJ', .before ='Fixed_Effects')

lmmDOY_MJ_ery <- lmer(DOY ~ T_spatial_centred + T_temporal + Strata + T_spatial_centred*Strata + T_temporal*Strata + (1| Site) + (1| Year), data = (PhenoDOY_ery %>% filter(SpringPeriod == 'MJ')), REML = TRUE)
summary(lmmDOY_MJ_ery)
AICc(lmmDOY_MJ_ery)
lmmDOY_MJ_ery <-as.data.frame(summary(lmmDOY_MJ_ery)$coef) %>%
  rownames_to_column('Fixed_Effects') %>%
  mutate(SpringPeriod = 'MJ', .before ='Fixed_Effects')

lmm.full_ery <- rbind(lmmDOY_MA_ery, lmmDOY_MAM_ery, lmmDOY_MAMJ_ery, lmmDOY_AM_ery, lmmDOY_AMJ_ery, lmmDOY_MJ_ery)



ggsave("C:/Users/User/Desktop/ery_fulmodel_temporal.jpg", plot = plotMAM_temporal, width = 4, height = 4, dpi = 300)



