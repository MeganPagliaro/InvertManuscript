#From R Code: InvertMixbySpp_V1.R
#copied 11 May 2026
#Author: Megan Pagliaro

######## Plotting the data_from hand_copied CSV #####################

library(dplyr)
library(tidyr)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(tidyverse)

#all <- read.csv("MixInvertOutput_7Oct25.csv", header=T)
#adding "all" column
all <- read.csv("MixInvertOutput_7Oct25_addAll.csv", header=T)
View(all)

#reorganizing the taxa so that it goes annelids>clams>crustaceans
all$Taxa <- factor(all$Taxa, levels=c("All",
                                      "Annelid (non-nereid)",
                                      "Nereididae",
                                      "Corbicula fluminea",
                                      "Potamocorbula amurensis",
                                      "Gnorimosphaeroma oregonense",
                                      "Gammarus sp", 
                                      "Sinocorophium sp",
                                      "Mysida"))

#choosing colors
colors2 <- c("darkgoldenrod","darkorchid3","cornflowerblue","darkolivegreen3")
#calculating means for graphing
table1 <- all %>%
  group_by(SiteType,Taxa,Source) %>%
  summarise(Mean = mean(Mean, na.rm = FALSE))
#rounding so only 3 didgits
table1_rounded <- table1 %>%
  mutate(Mean = round(Mean, digits = 3))
#graphing separating Reference and Restored
ggplot(all, aes(fill=Source, y=Mean, x=Taxa)) + 
  geom_bar(position="fill", stat="identity", width = 0.9) +
  scale_fill_manual(values = colors2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y="Mean Proportion", x="Taxa") + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_grid(SiteType~.,scales = "free", space="free") +
  geom_text(data=table1_rounded, aes(label=paste(Mean*100,"%")),
            position=position_stack(vjust=0.5), size=3.5) 


###GLM on Data####
all2 <- read.csv("MixInvertOutput_7Oct25.csv", header=T) # no "all column"
View(all2)

## glm
model <- glm(Mean ~ Taxa + Source*SiteType,
             family = gaussian, data = all2)
summary(model)

model2 <- glm(Mean ~ Taxa * Source + SiteType, family = gaussian, data = all2)
summary(model2)
########
#graphing no separation
table2 <- all %>%
  group_by(Taxa,Source) %>%
  summarise(Mean = mean(Mean, na.rm = FALSE))
#rounding so only 3 didgits
table2_rounded <- table2 %>%
  mutate(Mean = round(Mean, digits = 3))
ggplot(all, aes(fill=Source, y=Mean, x=Taxa)) + 
  geom_bar(position="fill", stat="identity", width = 0.9) +
  scale_fill_manual(values = colors2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y="Mean Proportion", x="Taxa") + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  geom_text(data=table2_rounded, aes(label=paste(Mean*100,"%")),
            position=position_stack(vjust=0.5), size=3.5) 



####t-test for resident versus transient ####
#across individuals
table_LH <- alloch.t %>%
  group_by(LifeHistory) %>%
  pivot_wider(names_from = LifeHistory, values_from = Mean)
View(table_LH)
t.test(table_LH$Resident, table_LH$Transient, paired = FALSE)  

#control by site
table_LH2 <- alloch.t %>%
  group_by(LifeHistory,Site) %>%
  summarise(mean_across_species = mean(Mean, na.rm = FALSE))
View(table_LH2)
reshaped_table3 <- table_LH2 %>%
  pivot_wider(names_from = LifeHistory, values_from = mean_across_species)
reshaped_table3 # OK we will compare one column to the other, to ask if there are consistent differnces in allochthony between site types (controlling for species identity)
t.test(reshaped_table3$Transient, reshaped_table3$Resident, paired = TRUE, alternative = "two.sided")  # yes, significant


########## Cohen's D#########
#### cohen's d
library(effectsize)
library(easystats)
options(es.use_symbols = TRUE)

alloch <- terrestrial_dataset<-SppAllSite %>%
  filter(energy_source=="Terrestrial")
View(alloch)
#separating species
amsh <- alloch %>%
  filter(taxa=="AmSh")
cohens_d(Mean ~ SiteType, data = amsh) #very small
#interpret_cohens_d(c(-0.133), rules = "cohen1988") #very small

GldSh <- alloch %>%
  filter(taxa=="GldSh")
cohens_d(Mean ~ SiteType, data = GldSh) #small
#interpret_cohens_d(c(-0.452), rules = "cohen1988") #small

InSil <- alloch %>%
  filter(taxa=="InSil")
cohens_d(Mean ~ SiteType, data = InSil) #small

PrSc <- alloch %>%
  filter(taxa=="PrSc")
cohens_d(Mean ~ SiteType, data = PrSc) #medium 
#interpret_cohens_d(c(-0.556), rules = "cohen1988") #medium

ScrSpl <- alloch %>%
  filter(taxa=="ScrSpl")
cohens_d(Mean ~ SiteType, data = ScrSpl) #small

StrBss <- alloch %>%
  filter(taxa=="StrBss")
cohens_d(Mean ~ SiteType, data = StrBss) # very small

ThrdSh <- alloch %>%
  filter(taxa=="ThrdSh")
cohens_d(Mean ~ SiteType, data = ThrdSh) # very small

ThreeSt <- alloch %>%
  filter(taxa=="ThreeSt")
cohens_d(Mean ~ SiteType, data = ThreeSt) #small

TriGby <- alloch %>%
  filter(taxa=="TriGby")
cohens_d(Mean ~ SiteType, data = TriGby) #small

TuPch <- alloch %>%
  filter(taxa=="TuPch")
cohens_d(Mean ~ SiteType, data = TuPch) # very small

YllGby <- alloch %>%
  filter(taxa=="YllGby")
cohens_d(Mean ~ SiteType, data = YllGby) # small


interpret_cohens_d(c(-0.04,0.46,0.09,-0.0009,0.38,0.13,0.01,0.36,0.10,0.15,0.20), rules = "cohen1988")


###### test code
all$Pair <- factor(all$Pair, levels=c("P3", "P2", "P1"),
                   labels=c("New restoration\n(seaward)", "Young restoration\n(middle)",
                            "Old restoration\n(landward)" ))

all$Site <- factor(all$Site, levels=c("Ryer", "Tule Red", "Chipps", "Wheeler", "Browns", "Sherman"),
                   labels=c("Ryer\n(Ref)", "Tule Red\n(Res)", "Chipps\n(Ref)", "Wheeler\n(Res)", "Browns\n(Ref)", "Sherman\n(Res)"))