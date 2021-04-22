## plots and dataviz for JWS metadata paper
## build several plots from raw data csvs and DFs 
## Pretty standard stuff really 

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


## make a series of plots on the capture program
## should be figure 1a-hc in the paper

## tags deploymed by year, barplot
metaDF <- read.csv('./data/jws_tag_metadata.csv', header = T)
ggplot(metaDF, aes(x = YEAR_CAP)) +
  themeKV +
  geom_point(stat="count",alpha=0.65) +
  geom_line(stat="count",alpha=0.65) +
  xlab("deployment year") +
  ylab("sharks tagged (no. individuals)") +
  scale_y_continuous(limits = c(0,10),
                     breaks = c(0,2,4,6,8,10)) +
  scale_x_continuous(limits = c(2000,2020),
                     breaks = c(2000,2005,2010,2015,2020))
  
## tags deploymed by numerical month, barplot
metaDF <- read.csv('./data/jws_tag_metadata.csv', header = T)
ggplot(metaDF, aes(x = MONTH_CAP)) +
  themeKV +
  geom_bar(stat="count",alpha=0.4) +
  xlab("deployment month") +
  ylab("sharks tagged (no. individuals)") +
  scale_x_continuous(limits = c(0,12),
                     breaks = c(2,4,6,8,12))


## tags deploymed by latitude, density plot
ggplot(metaDF, aes(x = LAT_REL)) +
  themeKV +
  geom_density(alpha=0.4) +
  xlab("deployment latitude (°)") +
  scale_x_continuous(limits = c(27,37),
                     breaks = c(27,28,29,30,31,32,33,34,35,36,37)) +
  scale_y_reverse()+
  ## if want flip axes
    coord_flip()


## types of tags deployed, bar plot
tagDF <- read.csv('./data/jws_tag_types.csv', header = T)
ggplot(tagDF, aes(x = fct_infreq(TAG_MODEL),fill=TAG_TYPE)) +
  ## ggplot(tagDF, aes(x = TAG_MODEL)) +
  themeKV +
  geom_bar(stat="count",alpha=0.65) +
  xlab("tag model") +
  ylab("sharks tagged (no. individuals)") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30))


  
  
  
  



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

