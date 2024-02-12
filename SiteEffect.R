#MALDIVES DATA and R-packages ============
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(readxl)) install.packages('readxl')

library(readxl)
library(tidyverse)

#LOAD ORIGINAL DATA:
Data = read.csv('Maldives_CarbonData.csv')

##Compute Mg/ha:
NewDATA <- Data %>% 
           filter(Duplicated == "no") %>%   #Remove duplicated records
  
         # One sediment volume was sub-sampled at each 10cm of the core.
            mutate(SliceLength_cm = ifelse(DepthTo_cm < 29, 10,30)) #Use 28-30 slice as representative of entire core till 50cm. See photos on Team: below 50cm is all shell layer.

unique(NewDATA$DepthTo_cm) # 2 12 30
unique(NewDATA$SliceLength_cm) #10 30 (28to30 extrapolated as 20 to 50 cm)

#Percent_OC based on Lab calculation (presumably wrong, see spreadsheet = "Job1599MacreadieFinal")
NewDATA$percent_OC <- ifelse(NewDATA$percent_OC == 0, 0.001, NewDATA$percent_OC)#convert 0 into 0.001 to run log-models if any

#DBD_gcm3 =Dry Bulk Density, #50 mm Internal Diameter and 2 cm height give us 39.27 cm3 for each sediment sample
NewDATA$DBD_gcm3 <- NewDATA$DryWeight_g / NewDATA$DryVolume.cm3
NewDATA$CarbonDensity_gcm3 <- NewDATA$DBD_gcm3 * NewDATA$percent_OC/100


#Compute Percent_OC2 based on Equations off LOI values:
#Blue Carbon Manual by Howard et al (English_Blue_Carbon_LR_190306.pdf): Table 3.7 Relationship between % LOI and % Corg for the different ecosystems.
#Variability within ecosystems may be #due to slight differences in methods used and/or characteristics of the soils.
Seagrass <- NewDATA %>% 
            filter(Ecosystem == "S") %>%
            mutate(percent_OC2 = 0.43 * LOI_percent - 0.33)



#Site Effect (Addressing reviewer comments):=========
#Line 270: This was low because the authors did not have enough data points  for Indo-Ocean region. =47 data points
#for Indo-Pacific seagrass meadows is not enough to provide accurate estimation.
#I would suggest to derive the OM vs C% relationship and then determine the stocks that will way different than these values. 

#One Way anova between sites in seagrass:
sg_aov <- aov(percent_OC2 ~ Island_FullName, Seagrass)
summary(sg_aov)
TukeyHSD(sg_aov)
residuals_sg <- residuals(mg_aov )# Extract residuals
z_scores_sg <- scale(residuals_sg)#  Calculate Z-scores
z_scores_sg # Display the Z-scores
write.csv(z_scores_sg, row.names =  F, file = "ZscoresSG.csv")

#One Way anova between sites in mangroves:
Mangroves  <- NewDATA %>% 
             filter(Ecosystem == "M") %>%
             mutate(percent_OC2 = 0.415 *  LOI_percent + 2.89 )

mg_aov <- aov(percent_OC2 ~ Island_FullName, Mangroves)
summary(mg_aov)
TukeyHSD(mg_aov)


residuals_mg <- residuals(mg_aov )# Extract residuals
z_scores_mg <- scale(residuals_mg)#  Calculate Z-scores
z_scores_mg # Display the Z-scores
write.csv(z_scores_mg, row.names =  F, file = "ZscoresMG.csv")

