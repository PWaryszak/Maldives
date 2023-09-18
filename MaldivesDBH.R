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


#Carbon Density:
names(NewDATA)

ggplot(NewDATA, aes(x=as.factor(Island_FullName), y=CarbonDensity_gcm3_Eq,fill=ecosystem_full))+ #, fill = Island_Name
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




#Vincent's DBH DATA===========

#OUT DH DATA:
ggplot(NewDATA, aes(x=as.factor(Island_FullName), y=DBD_gcm3,fill=ecosystem_full))+ #, fill = Island_Name
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#3399FF", "#339900"))+
  xlab("")+ ylab(bquote("Dry bulk Density "  (g*~cm^-3)))+
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



#Vincent's DBD DATA======
taxa <- read_excel("Vincent_Maldives_DryBulkDensityData.xlsx", sheet = "DBD_CLEAN") #Get dataset cleaned (18-Sep-2023) from Vincent
head(taxa)

V <- taxa %>% gather( FM1:Hoan6, key = "site", value = "DBD_gcm3")
str(V)#264   4


a <- ggplot(NewDATA, aes(x=as.factor(Island_FullName), y=DBD_gcm3))+ #, fill = Island_Name
  geom_boxplot(outlier.shape = NA)+ ggtitle("OLD DATA (core length set to 50 cm)")+
  xlab("")+ ylab(bquote("Dry bulk Density "  (g*~cm^-3)))+
  theme_bw()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black",angle=45),
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

b <- ggplot(V, aes(x=site, y=DBD_gcm3))+ #, fill = Island_Name
  geom_boxplot(outlier.shape = NA)+ ggtitle("NEW DATA (variable core length)")+
    theme(axis.text.x=element_text(vjust=0.5,size=16, color="black",angle=45),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))


b

plot1 <- grid.arrange(a,b, ncol = 2)
plot1 
#ggsave(plot1 , dpi=600, width = 18, height = 12, filename = "BoxPlot_Maldives_DBD_OLD_NEW_DATA.png")


#Sthephie's data by depth:
c <- ggplot(V, aes(y=reorder(DepthSection_cm,desc(DepthSection_cm)), x=DBD_gcm3))+ #, fill = Island_Name
  geom_boxplot(outlier.shape = NA)+ ggtitle("NEW")+
    labs(y = "Core sections (cm)")+
      theme(axis.text.x=element_text(vjust=0.5,size=16, color="black",angle=45),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))


c

#Plot by section depths of Hoan3 site:
plot_Hoan3 <- ggplot(V[V$site=="Hoan3" , ], aes(y=reorder(DepthSection_cm,desc(DepthSection_cm)),  x=DBD_gcm3))+ #, fill = Island_Name
  geom_point(aes(size=DBD_gcm3))+ ggtitle("Hoan3 Site Only") +theme_bw() +
  theme(legend.position = "none")

plot_Hoan3

plot2 <- grid.arrange(c,plot_Hoan3, ncol = 2)
plot2
ggsave(plot2 , dpi=600, width = 18, height = 12, filename = "BoxPlot_Maldives_DBD_Stephie_BySection.png")



#Overall DBD Styudies:
#WEB: for 0.7 g/cm3 = https://www.mdpi.com/1999-4907/5/1/177
