#LOAD Libraries and data:
library(readxl)
library(tidyverse)
library(gridExtra)
library(ggpmisc)
library(Hmisc)

#Load C-data off Maldives sites:
Data = read.csv('Maldives_CarbonData.csv')

##Compute Mg/ha:
NewDATA <- Data %>% 
           filter(Duplicated == "no") %>%   #Remove duplicated records
  
         # One sediment volume was sub-sampled at each 10cm of the core.
            mutate(SliceLength_cm = ifelse(DepthTo_cm < 29, 10,30)) #Use 28-30 slice as representative of entire core till 50cm. See photos on Team: below 50cm is all shell layer.

unique(NewDATA$DepthTo_cm) # 2 12 30
unique(NewDATA$SliceLength_cm) #10 30 (28to30 extrapolated as 20 to 50 cm)


#Compute Percent_OC2 based on Equations off LOI values:
#Blue Carbon Manual by Howard et al (English_Blue_Carbon_LR_190306.pdf): Table 3.7 Relationship between % LOI and % Corg for the different ecosystems.
#Variability within ecosystems may be 
#due to slight differences in methods used and/or characteristics of the soils.

Seagrass <- NewDATA %>% 
            filter(Ecosystem == "S") %>%
            mutate(percent_OC2 = 0.43 * LOI_percent - 0.33)

Mangroves  <- NewDATA %>% 
             filter(Ecosystem == "M") %>%
             mutate(percent_OC2 = 0.415 *  LOI_percent + 2.89 )



NewDATA <- rbind(Seagrass, Mangroves)
NewDATA$percent_OC2<- ifelse(NewDATA$percent_OC2 == 0, 0.001, NewDATA$percent_OC2)#convert 0 into 0.001 to run log-models if any

#DBD_gcm3 =Dry Bulk Density, #50 mm Internal Diameter and 2 cm height give us 39.27 cm3 for each sediment sample
NewDATA$DBD_gcm3 <- NewDATA$DryWeight_g / NewDATA$DryVolume.cm3
NewDATA$CarbonDensity_gcm3 <- NewDATA$DBD_gcm3 * NewDATA$percent_OC/100
NewDATA$CarbonDensity_gcm3_Eq <- NewDATA$DBD_gcm3 * NewDATA$percent_OC2/100


#Compare OLD Carbon Stock with Equation-based stock: =======
#OLD Carbon Stock from isotope data
NewDATA$CarbonStock_Mgha_old <- (((NewDATA$CarbonDensity_gcm3  / 1000000 ) *100000000) * NewDATA$SliceLength_cm )
range(NewDATA$CarbonStock_Mgha, na.rm = T )# 2.18537 100.34179

#New Eq-based Carbon Stock:
NewDATA$CarbonStock_Mgha_eq <- (((NewDATA$CarbonDensity_gcm3_Eq  / 1000000 ) *100000000) * NewDATA$SliceLength_cm )
range(NewDATA$CarbonStock_Mgha_eq, na.rm = T )# 4.72245 374.11573

#New Core Carbon Stock (Based on Equation off LOI values)=======
#English_Blue_Carbon_LR_190306.pdf): Table 3.7
CoreCarbon <- NewDATA %>%  #Compute Carbon stock per core (sum all depths)
    group_by(CoreID, Island_FullName, ecosystem_full) %>%
    summarise(CoreCarbonStock = sum(CarbonStock_Mgha_eq, na.rm=T)) 
CoreCarbon

write.csv(CoreCarbon, row.names = F, file = "CarbonStockMaldives.csv") #Values sent in email

#PLOT  50 cm cores SOC stock by island on x-axis)========
Stock_Plot_50cm <- ggplot(NewDATA, aes(x = Island_FullName, y = CarbonStock_Mgha_eq, fill = ecosystem_full)) +
geom_boxplot(outlier.shape = NA)+
#stat_summary(fun.y = mean, geom = "bar") + 
#stat_summary(fun.data = mean_se, width=0.2, size = 1, geom = "errorbar")+

scale_fill_manual(values = c("#3399FF", "#339900"))+
  facet_grid(.~ ecosystem_full)+ #geom_jitter( size = 2,alpha = 0.4, aes(color = t))+
  labs(y = bquote('Carbon Stock  ' (tonnes*~ha^-1)), x="") +
  theme_bw() +
  ggtitle("Maldives (50 cm cores)") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        strip.text=element_text(size=18, face="bold"),
        #strip.background =  element_rect(fill = "yellow"),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5, hjust=0.5),
        plot.subtitle = element_text(size = 20, face = "bold", vjust = 0.5, hjust=0.5))

Stock_Plot_50cm
#ggsave(Stock_Plot,filename = "StockPlotMaldives_CarbonStock_Eq_tonnes_fixeds.jpg", height = 7,width = 10)


#PLOT3  100 cm cores SOC stock By island on x-axis)========
NewDATA_100cm <- NewDATA %>% mutate(SliceLength_cm = ifelse(DepthTo_cm < 30, 10,80)) #Use 28-30 slice as representative of entire core till 100cm!
#See photos on Team: below 50cm is all shell layer.

NewDATA_100cm$CarbonStock_Mgha_eq <- (((NewDATA_100cm$CarbonDensity_gcm3_Eq  / 1000000 ) *100000000) * NewDATA_100cm$SliceLength_cm )
range(NewDATA_100cm$CarbonStock_Mgha_eq, na.rm = T )#4.72245 997.64196


Stock_Plot_100cm <- ggplot(NewDATA_100cm, aes(x = Island_FullName, y = CarbonStock_Mgha_eq, fill = ecosystem_full)) +
geom_boxplot(outlier.shape = NA)+
  scale_fill_manual(values = c("#3399FF", "#339900"))+
  facet_grid(.~ ecosystem_full)+ #geom_jitter( size = 2,alpha = 0.4, aes(color = t))+
  labs(y = bquote('Carbon Stock  ' (tonnes*~ha^-1)), x="") +
  theme_bw() +
  ggtitle("Maldives (100 cm cores)") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        strip.text=element_text(size=18, face="bold"),
        #strip.background =  element_rect(fill = "yellow"),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5, hjust=0.5),
        plot.subtitle = element_text(size = 20, face = "bold", vjust = 0.5, hjust=0.5))

Stock_Plot_100cm

plot_50cm_100cm_carbon <- grid.arrange(Stock_Plot_50cm,Stock_Plot_100cm, ncol = 2)
ggsave(plot_50cm_100cm_carbon , dpi=600, width = 14, height = 8,filename = "PlotMaldives_50cm_100cm_Carbon.png")



#BARPLOT (calculated manually):=========
CoreCarbon2 <- CoreCarbon %>%
  group_by(ecosystem_full,Island_FullName) %>%
  summarise(AV = sd(CoreCarbonStock,na.rm=T),
  N = n(),
  SD = sd(CoreCarbonStock,na.rm=T),
  SE = SD/sqrt(N))

BARPLOT_50cm <-   ggplot(CoreCarbon2, aes(x = Island_FullName, y = AV, fill = ecosystem_full)) +
 geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = AV-SE, ymax = AV+SE),width=0.2)+

scale_fill_manual(values = c("#3399FF", "#339900"))+
  facet_grid(.~ ecosystem_full)+ #geom_jitter( size = 2,alpha = 0.4, aes(color = t))+
  labs(y = bquote('Carbon Stock  ' (tonnes*~ha^-1)), x="") +
  theme_bw() +
  ggtitle("Maldives (50 cm cores)") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        strip.text=element_text(size=18, face="bold"),
        #strip.background =  element_rect(fill = "yellow"),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5, hjust=0.5),
        plot.subtitle = element_text(size = 20, face = "bold", vjust = 0.5, hjust=0.5))

BARPLOT_50cm
ggsave(BARPLOT_50cm,filename = "BARPLOT_CarbonStock_Maldives.jpg", height = 7,width = 10)

