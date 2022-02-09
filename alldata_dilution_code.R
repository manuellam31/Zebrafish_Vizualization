setwd("F:/DATA/Manuela/Zebrafish/Extractions_Experiment/")
library(readxl)
library(ggsci)
library(scales)
library(ggplot2)
library(RColorBrewer)

all_data <- read_excel("alldata_dilution.xlsx")
all_data <- as.data.frame(all_data)


library(ggsci)
library(scales)
ggplot(all_data, aes(x=Dilution,y = Intensity, group= Lipid))+
  geom_line(aes(color=Lipid))+
  geom_point(aes(color=Lipid))+
  scale_color_igv()+
  labs(x="Dilution",
       y="Intensity")+
  coord_trans(y="log10")+
  scale_y_continuous(labels = scientific)+
  scale_x_discrete(limits=rev)+
  theme_light()+
  theme(panel.background = element_rect(fill = "white", colour = "grey"),
        plot.title = element_text(hjust = 0.5,size=12),
        axis.text.x = element_text(size = 12),
        axis.text.y =  element_text(size = 10),
        axis.title = element_text(size=13),
        legend.text = element_text(size=13),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=14),
        strip.background = element_rect(fill="black"))+
  facet_grid(Polarity~Extraction, scales = "free")


masserrorzebrafish <- read_excel("masserrorzebrafish.xlsx")
mf<-data.frame(masserrorzebrafish)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


mf3 <- data_summary(mf, varname="Masserror", 
                    groupnames=c("Extraction", "Dilution"))
# Convert dose to a factor variable
mf3$Dilution=as.factor(mf3$Dilution)


ggplot(mf3, aes(x=Dilution, y=Masserror, fill=Extraction)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Masserror-sd, ymax=Masserror+sd), width=.2,
                position=position_dodge(.9))+
  theme_light()+
  geom_hline(yintercept = 5, linetype= "dotted", size=1)+
  theme(axis.title.x = element_blank(),
        axis.text = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank())+
  ylab("Mass Error(ppm)")+
  scale_fill_manual(values = c("#015CD2", "#AB0B06","#06B552"))+
  coord_polar()
