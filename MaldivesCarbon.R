#MALDIVES DATA ============
library(readxl)
library(Hmisc)
library(tidyverse)
library(gridExtra)
library(ggpmisc)

#Load C-data off Maldives sites:
getwd()#"C:/Users/poles/Documents/00Deakin_Docs/R/BCL_R/Maldives"
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/BCL_R/Maldives")

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

Mangroves  <- NewDATA %>% 
             filter(Ecosystem == "M") %>%
             mutate(percent_OC2 = 0.415 *  LOI_percent + 2.89 )

NewDATA <- rbind(Seagrass, Mangroves)

NewDATA$percent_OC2<- ifelse(NewDATA$percent_OC2 == 0, 0.001, NewDATA$percent_OC2)#convert 0 into 0.001 to run log-models if any
NewDATA$CarbonDensity_gcm3_Eq <- NewDATA$DBD_gcm3 * NewDATA$percent_OC2/100

#Compare OLD Carbon Stock with Equation-based stock: =======
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

#Towards Result section:
CoreCarbonStock_Results <- NewDATA %>%  #Compute Carbon stock per core (sum all depths)
  group_by(CoreID, Island_FullName, ecosystem_full) %>%
  summarise(CoreCarbonStock = sum(CarbonStock_Mgha_eq, na.rm=T)) %>%
  group_by(ecosystem_full) %>%
  summarise(AV = mean(CoreCarbonStock, na.rm=T),
            N=n(),
            SD = sd(CoreCarbonStock, na.rm=T),
            SE = SD/sqrt(N))

CoreCarbonStock_Results

#PLOT1 (CD, DBD, OC by Island on x-axis) ======
names(NewDATA)
#Towards Result section:
CoreCarbon_CD_Results <- NewDATA %>%  #Compute Carbon stock per core (sum all depths)
  group_by(ecosystem_full) %>%
  summarise(AV = round(mean(CarbonDensity_gcm3_Eq, na.rm=T),2), #CarbonDensity_gcm3_Eq*1000 to get mg / cm3
            N=n(),
            SD = sd(CarbonDensity_gcm3_Eq, na.rm=T),
            SE = SD/sqrt(N))

CoreCarbon_CD_Results 

#Carbon Density
a <-ggplot(NewDATA, aes(x=as.factor(Island_FullName), y=CarbonDensity_gcm3_Eq,fill=ecosystem_full))+ #, fill = Island_Name
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#3399FF", "#339900"))+
  xlab("")+ ylab(bquote("Soil carbon density "  (g*~cm^-3)))+
  labs(fill = "Ecosystem: ")+
  ggtitle("a)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = c(.1,.9),
        legend.text = element_text(size = 10),
        legend.title = element_text(face = "bold", size=12),
        legend.key = element_rect( fill = "white", color = "black"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))

a

#Dry Bulk Density:
#Towards Result section:
CoreCarbon_DBD_Results <- NewDATA %>%  #Compute Carbon stock per core (sum all depths)
  group_by(ecosystem_full) %>%
  summarise(AV = round(mean(DBD_gcm3, na.rm=T),2),
            N=n(),
            SD = sd(DBD_gcm3, na.rm=T),
            SE = SD/sqrt(N))

CoreCarbon_DBD_Results 

b <-ggplot(NewDATA, aes(x=as.factor(Island_FullName), y=DBD_gcm3,fill=ecosystem_full))+  #, fill = Island_Name
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#3399FF", "#339900"))+
  xlab("")+ ylab(bquote("Soil bulk density "  (g*~cm^-3)))+
  ggtitle("b)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "none",
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "italic", size=10),
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))

b


#Organic Carbon (%):
#Towards Result section:
CoreCarbon_OC_Results <- NewDATA %>%  #Compute Carbon stock per core (sum all depths)
  group_by(ecosystem_full) %>%
  summarise(AV = round(mean(percent_OC2, na.rm=T),2),
            N=n(),
            SD = sd(percent_OC2, na.rm=T),
            SE = SD/sqrt(N))

CoreCarbon_OC_Results 

c<-ggplot(NewDATA, aes(x=as.factor(Island_FullName), y=percent_OC2,fill=ecosystem_full))+  #, fill = Island_Name
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#3399FF", "#339900"))+
  xlab("Sampling sites")+
  ylab("Soil carbon content (%) \n"  )+
  ggtitle("c)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "none",
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "italic", size=10),
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))


c

#Join all plots together:
plot1 <- grid.arrange(a,b,c, ncol = 1)
ggsave(plot1 , dpi=600, width = 12, height = 15,
       filename = "BoxPlot_Maldives_grams_fixed2.png")




#PLOT2 (Soil parameters by Depth on y-xis)=======
NewDATA1 <- NewDATA %>% 
unite("Depth", DepthFrom_cm:DepthTo_cm, sep = "-" , remove = F) #Create new Variable for plotting "Depth"
NewDATA1$Depth <- fct_rev(NewDATA1$Depth) #Reverse the sequence of Depth
names(NewDATA1)

#Carbon Densities:=======
#Towards Result section:
CoreCarbon_CD_Depth_Results <- NewDATA1 %>%  #Compute Carbon stock per core (sum all depths)
  group_by(ecosystem_full,Depth) %>%
  summarise(AV = round(mean(CarbonDensity_gcm3_Eq, na.rm=T),2),
            N=n(),
            SD = sd(CarbonDensity_gcm3_Eq, na.rm=T),
            SE = SD/sqrt(N))

CoreCarbon_CD_Depth_Results 


o1<-ggplot(NewDATA1, aes(y=Depth, x=CarbonDensity_gcm3_Eq))+ #for mg/cm3 go CarbonDensity_gcm3_Eq*1000
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Island_FullName),size=3)+  #aes(color = Island_Name)
  xlab(bquote("Soil carbon density "  (g*~cm^-3)))+
  scale_x_continuous(limits = c(0.0, 0.15))+
  ylab("Soil depth (cm) ")+
  labs(color = "Island Name:")+
  ggtitle("a)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold", size=12),
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))


o1


#Dry Bulk Density by Depth:====
#Towards Result section:
CoreCarbon_DBD_Depth_Results <- NewDATA1 %>%  #Compute Carbon stock per core (sum all depths)
  group_by(ecosystem_full, Depth) %>%
  summarise(AV = round(mean(DBD_gcm3, na.rm=T),2),
            N=n(),
            SD = sd(DBD_gcm3, na.rm=T),
            SE = SD/sqrt(N))

CoreCarbon_DBD_Depth_Results 

o2<-ggplot(NewDATA1, aes(y=Depth, x=DBD_gcm3))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Island_FullName),size=3)+   #aes(color = Island_Name)
  xlab(bquote("Soil bulk density "  (g*~cm^-3)))+
  ylab("Soil depth (cm) ")+
  labs(color = "Island Name:")+
  ggtitle("b)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold", size=12),
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))


o2


#OC,
#Towards Result section:
CoreCarbon_OC_depth_Results <- NewDATA1 %>%  #Compute Carbon stock per core (sum all depths)
  group_by(ecosystem_full, Depth) %>%
  summarise(AV = round(mean(percent_OC2, na.rm=T),2),
            N=n(),
            SD = sd(percent_OC2, na.rm=T),
            SE = SD/sqrt(N))

CoreCarbon_OC_depth_Results 



#Organic Carbon (%):======
#Towards Result section:
CoreCarbon_OC_Depth_Results <- NewDATA1 %>%  #Compute Carbon stock per core (sum all depths)
  group_by(ecosystem_full,Depth) %>%
  summarise(AV = round(mean(percent_OC2, na.rm=T),2),
            N=n(),
            SD = sd(percent_OC2, na.rm=T),
            SE = SD/sqrt(N))

CoreCarbon_OC_Depth_Results 

o3<-ggplot(NewDATA1, aes(y=Depth, x=percent_OC2))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Island_FullName),size=3)+    #aes(color = Island_Name)
  xlab("Soil carbon content (%) ")+
  ylab("Soil depth (cm) ")+
  labs(color = "Island Name:")+
  ggtitle("c)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "none",
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "bold", size=10),
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))


o3

plot_soil_depths <- grid.arrange(o1,o2,o3, ncol = 1)
ggsave(plot_soil_depths , dpi=600, width = 13, height = 15,
       filename = "PlotMaldivesDepths_grams_fixed2.png")

#PLOT3  (SOC Stock Bar Plot By island on x-axis)========
Stock_Plot <- ggplot(CoreCarbon, aes(x = Island_FullName, y = CoreCarbonStock, fill = ecosystem_full)) +
  stat_summary(fun.y = mean, geom = "bar") + 
  stat_summary(fun.data = mean_se, width=0.2, size = 1, geom = "errorbar")+
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

Stock_Plot
ggsave(Stock_Plot,filename = "StockPlotMaldives_CarbonStock_Eq_tonnes_fixeds.jpg", height = 7,width = 10)



#SOC Stock Bar Plot By Ecosystem on x-axis=======
ggplot(CoreCarbon, aes(x = ecosystem_full, y = CoreCarbonStock, fill = ecosystem_full)) +
  stat_summary(fun.y = mean, geom = "bar") + 
  stat_summary(fun.data = mean_se, width=0.2, size = 1, geom = "errorbar")+
  scale_fill_manual(values = c("#3399FF", "#339900"))+
  facet_grid(.~ Island_FullName)+ #geom_jitter( size = 2,alpha = 0.4, aes(color = t))+
  labs(y = bquote('Carbon Stock  ' (tonnes*~ha^-1)), x="") +
  theme_bw() +
  ggtitle("Maldives (50 cm cores)") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        strip.text=element_text(size=18, face="bold"),
        strip.background =  element_rect(fill = "yellow"),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5, hjust=0.5),
        plot.subtitle = element_text(size = 20, face = "bold", vjust = 0.5, hjust=0.5))

ggsave("01BarPlotMaldives_CarbonStock_Eq.jpg", height = 7,width = 10)
#95% CI SOC Stock Plot========
ggplot(CoreCarbon, aes(x = ecosystem_full, y = CoreCarbonStock, color = ecosystem_full)) +
  geom_point( alpha = 0.4)+
  scale_color_manual(values = c("#3399FF", "#339900"))+
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1) +
  stat_summary(fun.y = "mean", size = 3, geom = "point")+
  facet_grid(.~ Island_FullName)+ #geom_jitter( size = 2,alpha = 0.4, aes(color = t))+
  labs(y = bquote('Carbon Stock  ' (Mg*~ha^-1)), x="") +
  theme_bw() +
  ggtitle("Maldives (50 cm cores)") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        strip.text=element_text(size=18, face="bold"),
        strip.background =  element_rect(fill = "yellow"),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5, hjust=0.5),
        plot.subtitle = element_text(size = 20, face = "bold", vjust = 0.5, hjust=0.5))


#ggsave("PlotMaldives_CarbonStock_Eq.jpg", height = 7,width = 10)





