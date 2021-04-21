## plots and dataviz for PAH data for deposited wildfire ash
## build several plots from raw data csvs and dfs 
## ash collected mid-late August 2020, on the roof of the Monterey Bay Aquarium in Pacific Grove, Calif 
## PAH analysis generated from diagnostic lab at UConn 

library(plyr)
library(dplyr)
library(data.table)
library(ggthemes)
library(ggplot2)
library(forcats)


themeKV <-theme_few()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin(0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.15, "cm"),element_line(colour = "black", size=.5),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0))


## make a simple bar plot of particle size masses from deposited ash
## need to understand the particle size distribution to understand possible influence on PAH quants
## should be figure 1c in the paper

PAH <- read.csv('./data/ash/mass_particle_ash_deposit.csv', header = T)
ggplot(PAH, aes(x = SIZE_ASH_mm, y = MASS_ash_prop)) +
  themeKV +
  geom_bar(stat="identity",alpha=0.4)
## alternative using x axis particle size as categorical variable  
ggplot(PAH, aes(x = SIZE_ASH, y = MASS_ash_prop)) +
  themeKV +
  geom_bar(stat="identity",alpha=0.4)


library(scales)
library(tidyverse)
## make a bar style plot summarizing, ranking the PAH quantification from ash
## should be figure ~ 1e in the paper
PAH2 <- read.csv('./data/ash/PAH_particle_ash_deposit.csv', header = T)
# head(PAH2)
PAH2 %>%
  ## reorder data so that highest concentration is on top
  mutate(PAH = fct_reorder(PAH, total)) %>% 
  ggplot(mapping= aes(x=PAH, y=total)) +
  themeKV +
  ##  geom_col() + ## choose geom_point below instead
  scale_y_log10(limits = c(8,2000)) +
  ##                breaks = trans_breaks("log10", function(x) 10^x)),
  ##                labels = trans_format("log10", math_format(10^.x))) +
  ##  annotation_logticks(sides="l") +
  geom_point(shape = 21, fill = "lightgray", color = "black", size = 3) +
  coord_flip()


## plot particle size vs PAH concentration, with log model through data
## should be figure ~ 1d in the paper
PAH3 <- read.csv('./data/ash/PAH_particle_vs_PAH.csv', header = T)
ggplot(data = PAH3, aes(x=size, y=ppb, group=total, color=total)) +
  themeKV +
  scale_color_manual(values=c("#000000", "#FF9900")) +
  geom_point(shape = 21, size = 5) +  
  geom_smooth(method="nls", se=FALSE, formula=y~a*log(x)+k,
              method.args=list(start=c(a=1, k=1)))+
  scale_y_continuous(limits = c(600,9400),
                     breaks = c(1000,2000,3000,4000,5000,6000,7000,8000,9000))



## make a box plot of HMs in deposited ash
## displaying raw values (n=4) with jitter
## should be figure 1_ in the paper

HM2 <- read.csv('./data/ash/ash_deposit_CAHFS_HMs.csv', header = T)
ggplot(HM2, aes(x = fct_reorder(element, metal_ppm, .fun=median, .desc=TRUE), y = metal_ppm)) +
  themeKV +
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        axis.ticks.length = unit(-.15, "cm"), 
        axis.text.x = element_text(margin = margin(t = 10, unit = "pt")),
        axis.title.y = element_text(margin = margin(-2,-2,-2,-2)),
        axis.text.y = element_text(hjust = 1, margin = margin(10, 10, 10, 10))) +
  geom_point(alpha=0.55, color="#FF9900", position="jitter", shape = 16, size = 4) +  
  geom_boxplot(alpha=0) +  ## alpha=0 is fully transparent interior of box
  scale_y_log10(limits = c(1,12000)) +
  xlab("metal") +
  ylab("ppm") # +
## just to check differences between sites
## facet_wrap(~collection_site, ncol=1)

