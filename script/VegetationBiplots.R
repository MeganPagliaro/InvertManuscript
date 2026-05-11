library(dplyr)
library(tidyr)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)


#Figure 1#####################
biplot <- read.csv("VegForMixSIAR.csv", header=T) 
View(biplot)
CN <- ggplot(biplot, aes(x = D13C, y = D15N, fill=SampleType))+
  geom_point(show.legend = FALSE, size=2, shape=21, color="black") +
  #scale_x_discrete(guide = guide_axis(angle = 65))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)) +
  xlab(expression(italic(delta)^13*C*"‰"))  + 
  ylab(expression(italic(delta)^15*N*"‰"))  
CN
#NS
NS <- ggplot(biplot, aes(x = D34S, y = D15N, fill=SampleType))+
  geom_point(show.legend = FALSE, size=2, shape=21, color="black") +
  #scale_x_discrete(guide = guide_axis(angle = 65))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)) +
  xlab(expression(italic(delta)^34*S*"‰"))  + 
  ylab(expression(italic(delta)^15*N*"‰"))  
NS
#CS
CS <- ggplot(biplot, aes(x = D13C, y = D34S, fill=SampleType))+
  geom_point(show.legend = FALSE, size=2, shape=21, color="black") +
  #scale_x_discrete(guide = guide_axis(angle = 65))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)) +
  theme(legend.title = element_blank(),
        legend.text=element_text(size=10)) +
  xlab(expression(italic(delta)^13*C*"‰"))  + 
  ylab(expression(italic(delta)^34*S*"‰"))   
CS


figbiplot1 <- ggarrange(CN,NS,CS,
                        ncol = 2, nrow = 2)
figbiplot1

###Figure S1###################
biplot2 <- read.csv("AllSamples4Biplot.csv", header=T) 
View(biplot2)
####April 14 Biplot
pal <- c("gold","springgreen3","tomato")
#biplot$SampleType <- factor(biplot$SampleType, levels = c("Fish", "Phytoplankton", "Emergent Vegetation"))
many <- c( "gray70", "#E31A1C","green4","palegreen3", "#6A3D9A", "#FF7F00", "black", "gold1", "skyblue2", "#FDBF6F", "dodgerblue2", "maroon")
black <- c("black", "black","black", "black", "black", "black","black", "black","black", "black","black", "black")
CN <- ggplot(biplot2, aes(x = D13C, y = D15N,
                         #fill=as.character(TaxaB),
                         #shape=as.character(Group),
                         #size=as.character(Group),
                         color=as.character(TaxaB))) +
  geom_point(show.legend = FALSE) +
  scale_shape_manual(values=c(21,21,21,21,21,21)) +
  scale_color_manual(values=alpha(many,.6)) +
  scale_size_manual(values=c(1, 1, 1, 1,1,1)) +
 # scale_fill_manual(values= alpha(many,.6)) +
  guides(color=guide_legend("Sources")) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)) +
  xlab(expression(italic(delta)^13*C*"‰"))  + 
  ylab(expression(italic(delta)^15*N*"‰"))  
CN
NS <- ggplot(biplot2, aes(x = D34S, y = D15N,
                         #fill=as.character(SampleSpecies),
                         #shape=as.character(SampleType),
                         #size=as.character(SampleType),
                         color=as.character(TaxaB))) +
  geom_point(show.legend = FALSE) +
  scale_shape_manual(values=c(21,21,21)) +
  scale_color_manual(values=alpha(many,.6)) +
  scale_size_manual(values=c(1, 1, 1)) +
  #scale_fill_manual(values= alpha(many,.6)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)) +
  xlab(expression(italic(delta)^34*S*"‰"))  + 
  ylab(expression(italic(delta)^15*N*"‰")) 
NS
CS <- ggplot(biplot2, aes(x = D34S, y = D13C,
                         #fill=as.character(SampleSpecies),
                         #shape=as.character(SampleType),
                         #size=as.character(SampleType),
                         color=as.character(TaxaB))) +
  geom_point(show.legend = FALSE) +
  scale_shape_manual(values=c(21,21,21)) +
  scale_color_manual(values=alpha(many,.6)) +
  scale_size_manual(values=c(1, 1, 1)) +
  #scale_fill_manual(values= alpha(many,.6)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)) +
  ylab(expression(italic(delta)^13*C*"‰"))  + 
  xlab(expression(italic(delta)^34*S*"‰"))  
CS

figbiplot2 <- ggarrange(CN,NS,CS,
                        ncol = 2, nrow = 2)
figbiplot2







###########TestCode##############
View(biplot)
CN <- ggplot(biplot, aes(x = D13C, y = D15N, fill=TaxaB))+
  geom_point(show.legend = FALSE, size=2, shape=21, color="black") +
  #scale_x_discrete(guide = guide_axis(angle = 65))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)) +
  xlab(expression(italic(delta)^13*C*"‰"))  + 
  ylab(expression(italic(delta)^15*N*"‰"))  
CN
#NS
NS <- ggplot(biplot, aes(x = D34S, y = D15N, fill=TaxaA))+
  geom_point(show.legend = FALSE, size=2, shape=21, color="black") +
  #scale_x_discrete(guide = guide_axis(angle = 65))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)) +
  xlab(expression(italic(delta)^34*S*"‰"))  + 
  ylab(expression(italic(delta)^15*N*"‰"))  
NS
#CS
CS <- ggplot(biplot, aes(x = D13C, y = D34S, fill=TaxaA))+
  geom_point(show.legend = FALSE, size=2, shape=21, color="black") +
  #scale_x_discrete(guide = guide_axis(angle = 65))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)) +
  theme(legend.title = element_blank(),
        legend.text=element_text(size=10)) +
  xlab(expression(italic(delta)^13*C*"‰"))  + 
  ylab(expression(italic(delta)^34*S*"‰"))   
CS


figbiplot1 <- ggarrange(CN,NS,CS,
                        ncol = 2, nrow = 2)
figbiplot1




theme(legend.position = c(0.205,0.11),
      legend.title = element_blank(),
      legend.text=element_text(size=10))

cn_plot <-ggplot(data = veg_data, aes(x=D13C, y=D15N, color=SampleType)) +
  geom_point() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 12)) +
  #scale_color_manual(values=c("cornflowerblue","darkgoldenrod"))+
  #facet_grid(Site~.) +
  #scale_x_discrete(guide = guide_axis(angle = 45)) +
  #scale_fill_manual(values=clrs6)+
  #facet_grid(Site~Species) +
  labs(y = "D15N", x = "D13C")
#rr_plot + geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray")
cn_plot

