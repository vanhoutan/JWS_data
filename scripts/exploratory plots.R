## plots and dataviz for JWS metadata paper
## build several plots from raw data csvs and DFs 
## Pretty standard stuff really 


library(plyr)
library(dplyr)
library(data.table)
library(ggthemes)
library(ggplot2)
library(tidyverse)
library(forcats)
library(tidyr)


themeKV <-theme_few()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin(0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.15, "cm"),element_line(colour = "black", size=.5),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0))
        

## make a series of plots on the capture program tagging metadata
## should be figure 1a-h in the paper

## first plot is the no. tags deployed time series
## line plot
metaDF <- read.csv('./data/jws_tag_metadata.csv', header = T)
ggplot(metaDF, aes(x = YEAR_CAP)) +
  themeKV +
  geom_line(stat="count",alpha=0.65) +
  geom_point(stat="count",alpha=0.65, size=4, color="orange") +
  xlab("deployment year") +
  ylab("sharks tagged (no. individuals)") +
  scale_y_continuous(limits = c(0,10),
                     breaks = c(0,2,4,6,8,10)) +
  scale_x_continuous(limits = c(2000,2020),
                     breaks = c(2000,2004,2008,2012,2016,2020))
  

## tags deploymed by numerical month, lolli + bar plot
## bar vers
metaDF <- read.csv('./data/jws_tag_metadata.csv', header = T)
ggplot(metaDF, aes(x = MONTH_CAP)) +
  themeKV +
  geom_bar(stat="count",alpha=0.4) +
  xlab("deployment month") +
  ylab("sharks tagged (no. individuals)") +
  scale_x_continuous(limits = c(0,12),
                     breaks = c(2,4,6,8,12))
## lollipop vers
metaDF <- read.csv('./data/jws_tag_metadata.csv', header = T)
metaDF %>%
group_by(MONTH_CAP) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = MONTH_CAP, y = count)) +
  themeKV +
  geom_segment(aes(x= MONTH_CAP, xend= MONTH_CAP, y=0, yend=count), alpha=0.65) +
  geom_point(alpha=0.65, size=4, color="orange") +
  xlab("deployment month") +
  ylab("sharks tagged (no. individuals)") +
  scale_y_continuous(limits = c(0,16),
                     breaks = c(0,4,8,12,16)) +
  scale_x_continuous(limits = c(1,12),
                     breaks = c(2,4,6,8,10,12))


## tags deploymed by latitude
## density plot
ggplot(metaDF, aes(x = LAT_REL)) +
  themeKV +
  geom_density(color = "orange", fill = "orange", alpha=0.4) +
  xlab("deployment latitude (°)") +
  scale_x_continuous(limits = c(27,37.5),
                     breaks = c(28,30,32,34,36)) +
  scale_y_reverse()+
   ## flip axes to mimic map + coastline orientation
    coord_flip()


## types of tags deployed
## bar plot
library(viridis)
tagDF <- read.csv('./data/jws_tag_types.csv', header = T)
ggplot(tagDF, aes(x = fct_infreq(TAG_MODEL),fill=TAG_TYPE)) +
  ## ggplot(tagDF, aes(x = TAG_MODEL)) +
  themeKV +
  geom_bar(stat="count",alpha=0.75) +
  scale_fill_viridis_d(
    alpha = 1,
    begin = 0.1,
    end = 0.75,
    direction = 1,
    option = "inferno",
    aesthetics = "fill"
  ) +
  xlab("tag model") +
  ylab("sharks tagged (no. individuals)") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30))


## number of tags deployed per shark
## donut plot
metaDF <- read.csv('./data/jws_tag_metadata.csv', header = T)
metaDF %>%
  group_by(TAGS_NO) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = 2, y = count, fill = count)) +
  geom_bar(stat="identity",color="white",show.legend = FALSE) +
  coord_polar("y", start = 0) +
##  geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
  theme_void() +
  scale_fill_viridis_c(
    alpha = 0.85,
    begin = 0.1,
    end = 0.75,
    direction = -1,
    option = "inferno",
    aesthetics = "fill") +
  xlim(0.5,2.5) +
  labs(title = "no. tags deployed per shark",
       ##       subtitle = "capture operation",
       caption = "1 + 2")  
  

## capture operation type
## donut plot
metaDF <- read.csv('./data/jws_tag_metadata.csv', header = T)
metaDF %>%
  group_by(INTERACTION) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = 2, y = count, fill = count)) +
  geom_bar(stat="identity",color="white",show.legend = FALSE) +
  coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_viridis_c(
    alpha = 0.85,
    begin = 0.1,
    end = 0.75,
    direction = -1,
    option = "inferno",
    aesthetics = "fill") +
  xlim(0.5,2.5) +
  labs(title = "capture operation",
##       subtitle = "capture operation",
       caption = "commercial fisheries + research")


## capture gear type
## lollipop
## cannot reorder y axis in descending, was trying to implement code at this URL
## https://learn.r-journalism.com/en/visualizing/customizing_charts/customizing-exporting-ggplot2/library(forcats)
metaDF <- read.csv('./data/jws_tag_metadata.csv', header = T)
metaDF %>%
  group_by(CAPTURE_GEAR) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = count, y=CAPTURE_GEAR)) +
  themeKV +
  geom_segment(aes(y=CAPTURE_GEAR,yend= CAPTURE_GEAR, x=0, xend=count), alpha=0.65) +
  geom_point(alpha=0.65, size=4, color="orange") +
  xlab("interaction gear") +
  ylab("sharks tagged (no. individuals)") +
  scale_x_continuous(limits = c(0,17),
                     breaks = c(0,4,8,12,16))



y=fct_reorder(CAPTURE_GEAR, actress_age, desc=TRUE)

## Target fishery 
## bar plot





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

