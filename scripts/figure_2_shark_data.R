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
library(viridis)

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
## should be figure 2a-e in the paper


## TBL of sharks tagged, figure 2a 
## density plot
metaDF <- read.csv('./data/jws_tag_metadata.csv', header = T)
ggplot(metaDF, aes(x = TBL_cm)) +
  themeKV +
##  geom_density(color = "orange", fill = "orange", alpha=0.75) +
  geom_histogram(aes(y=..density..),fill = "orange", alpha=0.75) +
  xlab("shark total length (cm)") +
  scale_x_continuous(limits = c(100,440),
                     breaks = c(150,200,250,300,350,400))


## compare no. of female and male sharks tagged, figure 2b
## donut plot
metaDF <- read.csv('./data/jws_tag_metadata.csv', header = T)
metaDF %>%
  group_by(SEX) %>% 
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
  labs(title = "no. tags deployed",
       ##       subtitle = "capture operation",
       caption = "female + male + NA")  
  

## total days deployed from tagto pop, figure 2c 
## histogr plot
metaDF <- read.csv('./data/jws_tag_metadata.csv', header = T)
ggplot(metaDF, aes(x = DEPLOY_DAYS)) +
  themeKV +
  geom_density(color = "orange", fill = "orange", alpha=0.75) +
  geom_histogram(aes(y=..density..),alpha=0.25) +
  xlab("total deployment (no. days)") +
  scale_x_continuous(limits = c(-20,650),
                     breaks = c(0,100,200,300,400,500,600))


## days deployed per shark per year from tag to pop, figure 2d 
## box plot
metaDF <- read.csv('./data/jws_tag_metadata.csv', header = T)
## convert the variable 'YEAR_CAP' from a numeric to a factor variable
metaDF$YEAR_CAP <- as.factor(metaDF$YEAR_CAP)
head(metaDF)
ggplot(metaDF, aes(x = YEAR_CAP, y = DEPLOY_DAYS)) +
  themeKV +
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        axis.ticks.length = unit(-.15, "cm"), 
        axis.text.x = element_text(margin = margin(t = 10, unit = "pt")),
        axis.title.y = element_text(margin = margin(-2,-2,-2,-2)),
        axis.text.y = element_text(hjust = 1, margin = margin(10, 10, 10, 10))) +
  geom_point(alpha=0.55, color="#FF9900", position="jitter", shape = 16, size = 3) +  
  geom_boxplot(alpha=0) +  ## alpha=0 is fully transparent interior of box
  scale_y_continuous(limits = c(0,290),
                     breaks = c(0,40,80,120,160,200,240,280))+
  xlab("depployment year") +
  ylab("deployment (no. days)")


## total distance travelled between release and pop, Figure 2e
## density plot
metaDF <- read.csv('./data/jws_tag_metadata.csv', header = T)
ggplot(metaDF, aes(x = DIST_KM)) +
  themeKV +
##  geom_density(color = "orange", fill = "orange", alpha=0.75) +
  geom_histogram(aes(y=..density..),fill = "orange", alpha=0.75) +
  xlab("distance travelled (km)") +
  scale_x_continuous(breaks = c(0,400,800,1200,1600,2000))


## deployment duration versus distance travelled between release and pop, Figure 2f ???
## scatter plot
metaDF <- read.csv('./data/jws_tag_metadata.csv', header = T)
ggplot(metaDF, aes(x = DEPLOY_DAYS, y = DIST_KM)) +
  themeKV +
  geom_smooth(color="#FF9900", fill="#FF9900", method = "loess", formula = y ~ x, span = 1, se = TRUE)+
  geom_point(color="#000000", alpha=0.35, shape = 16, size = 3)




