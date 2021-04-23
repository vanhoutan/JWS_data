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
metaDF <- read.csv('./data/jws_tag_metadata.csv', header = T)
metaDF %>%
  group_by(CAPTURE_GEAR) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = count, y=CAPTURE_GEAR)) +
  themeKV +
## reorder y axis in descending order based on count
  geom_segment(aes(y=fct_reorder(CAPTURE_GEAR, count),yend= CAPTURE_GEAR, x=0, xend=count), alpha=0.65) +
  geom_point(alpha=0.65, size=4, color="orange") +
  xlab("interaction gear") +
  ylab("sharks tagged (no. individuals)") +
  scale_x_continuous(limits = c(0,17),
                     breaks = c(0,4,8,12,16))


## Target fishery used to deploy tags
## bar plot
## would prefer to sort fill scale by count, but cannot figure out
## so hard wired into variable names in col by their count rank (1,2,3...)
library(viridis)
metaDF <- read.csv('./data/jws_tag_metadata.csv', header = T) 
ggplot(metaDF, aes(x = fct_infreq(TARGET_FISHERY), fill= TARGET_FISHERY)) +
  themeKV +
  geom_bar(stat="count", alpha=0.75) +
  scale_fill_viridis_d(
    alpha = 1,
    begin = 0.1,
    end = 0.75,
    direction = 1,
    option = "inferno",
    aesthetics = "fill"
  ) +
  xlab("fishery target species") +
## remove NA values from the diplayed counts
  scale_x_discrete(na.translate = FALSE) +
  ylab("sharks tagged (no. individuals)") +
  scale_y_continuous(breaks = c(0,4,8,12,16))
