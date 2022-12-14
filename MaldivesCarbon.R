#MALDIVES DATA ============
library(readxl)
library(Hmisc)
library(tidyverse)


#Load C-data off Jawbone site:
getwd()#"C:/Users/poles/Documents/00Deakin_Docs/R/BCL_R/Maldives"
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/BCL_R/Maldives")

Data = read.csv('Maldives_CarbonData.csv')
names(Data)


##Compute Mg/ha:
NewDATA <- Data

NewDATA$OC_percent <- ifelse(NewDATA$OC_percent == 0, 0.001, NewDATA$OC_percent)#convert 0 into 0.001 to run log-models if any

NewDATA$CarbonDensity_gcm3 <- NewDATA$DBD_gcm3 * NewDATA$OC_percent/100

NewDATA$CarbonStock_Mgha <- (((NewDATA$CarbonDensity_gcm3  / 1000000 ) *100000000) * NewDATA$SliceLength_cm )

range(NewDATA$CarbonStock_Mgha, na.rm = T )# 0.7284566 33.4472622
table(Data$DepthTo_cm, Data$coreID)
unique(Data$DepthTo_cm)#depths_to of core slices = 2 12 30


#Core Carbon Stock=======
CoreCarbon <- NewDATA %>%  #Compute Carbon stock per core (sum all depths)
  
  group_by(coreID, Island, Ecosystem) %>%
  
  summarise(CoreCarbonStock = sum(CarbonStock_Mgha, na.rm=T)) 


CoreCarbon


ggplot(CoreCarbon, aes(x = Ecosystem, y = CoreCarbonStock, color = Ecosystem)) +
  
  geom_point( alpha = 0.4)+
  
  scale_color_manual(values = c("blue","darkgreen"))+
  
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1) +
  
  stat_summary(fun.y = "mean", size = 3, geom = "point")+
  
  facet_grid(.~ Island)+ #geom_jitter( size = 2,alpha = 0.4, aes(color = t))+
  
  labs(y = bquote('Carbon Stock  ' (Mg*~ha^-1)), x="") +
  
  theme_bw() +
  
  ggtitle("Maldives (30cm cores)", subtitle = "Islands: G, H,  M") +
  
  theme(axis.text.x = element_text(size = 12),
        
        axis.text.y = element_text(size = 12),
        
        axis.title.y = element_text(size = 16),
        
        axis.title.x = element_text(size = 16),
        
        legend.position = "none",
        
        strip.text=element_text(size=18, face="bold"),
        
        strip.background =  element_rect(fill = "yellow"),
        
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5, hjust=0.5),
        
        plot.subtitle = element_text(size = 20, face = "bold", vjust = 0.5, hjust=0.5))


ggsave("PlotMaldives_CarbonStock.jpg", height = 7,width = 10)




##Core IC% and OC%=======
Core_TOC <- NewDATA %>%  #Compute Carbon stock per core (sum all depths)
  
  select(IC_percent, OC_percent, coreID, Island, Ecosystem) %>%

  gather(IC_percent:OC_percent, key = "CarbonType", value = "Percent") %>%

  mutate(CarbonType2 = ifelse( CarbonType == "OC_percent", "organic", "inorganic"))


Core_TOC


ggplot(Core_TOC, aes(x = Ecosystem, y = Percent, color = CarbonType2)) +
  
  scale_color_manual(values = c("grey50","purple"))+
  
  geom_jitter( alpha = 0.7)+
  
  geom_boxplot(outlier.shape = NA) +
  
  facet_grid(.~ Island)+ #geom_jitter( size = 2,alpha = 0.4, aes(color = t))+
  
  labs(y = "Carbon (%)", x="", color = "Carbon type: ") +
  
  theme_bw() +
   
  ggtitle("Maldives (30cm cores)", subtitle = "Islands: G, M, H") +
  
  theme(axis.text.x = element_text(size = 14),
        
        axis.text.y = element_text(size = 14),
        
        axis.title.y = element_text(size = 16),
        
        axis.title.x = element_text(size = 16),
        
        legend.position = c(.5,.5),
        
        legend.text = element_text(size = 16),
        
        legend.title = element_text(size = 16, face="bold"),
        
        legend.key = element_rect( fill = "white", color = "black"),
        
        legend.box.background = element_rect(),
        
        legend.box.margin = margin(6, 6, 6, 6),
        
        strip.text=element_text(size=16, face = "bold"),
        
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5, hjust=0.5),
        
        plot.subtitle = element_text(size = 20, face = "bold", vjust = 0.5, hjust=0.5),
        
        strip.background =  element_rect(fill = "yellow"))


ggsave("PlotMaldives_CarbonPercent.jpg", height = 7,width = 10)

#write.csv(NewDATA, file = "MaldivesData.csv", row.names = F)
