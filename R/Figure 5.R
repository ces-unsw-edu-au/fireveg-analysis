library(RPostgreSQL)
library(ggplot2)
library(forcats)
library(dplyr)
library(data.table)
library(rpostgis)
library(tidyverse)
library(ggtext)
library(stringr)
library(vcd)
library(lubridate)
library(scales)
library(ggrepel)

#Load survey data
my.survey <- read.csv("data/my.surveys.csv",sep=",",
                     header=T, dec=".", stringsAsFactors=F, na.strings=c("","NA", "N/A"))

#Summary table
my.survey  %>%  
  group_by(Survey.Name) %>%
  summarise(t.loc = n_distinct(site_label),
            t.obs = n_distinct(observer),
            t.days = n_distinct(visit_date),
            t.taxa = sum(taxa, na.rm = TRUE),
            t.plots = n_distinct(subplots)
  )


#Table with vegetation types
type.veg <- data.frame(table(unlist(strsplit(my.survey$veg," / "))))

type.veg %>%
  ggplot(aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity")

#####
#Dumbbell Plot to vizualice relative positions 
#(like growth and decline) between two points in time.
#Now merge with fire information
my.fire<- read.csv("data/my.fire_20200316.csv",sep=",",
                   header=T, dec=".", stringsAsFactors=F, na.strings=c("","NA", "N/A"))

str(my.fire)

my.fire$visit_id <- as.factor(my.fire$visit_id)

#Transform date from character to date format
my.fire <-my.fire %>%
mutate(date = ymd(fire_date))

summary(my.fire$date)

#Fire events
my.fire %>%
group_by(visit_id) %>%
  summarise(fire = n_distinct(date))

#Totals
my.fire %>%
  select(date) %>%
  summarise(fire = n()) # 45 events of fire from 

#Temporal plot of fire events
tmp1 <- my.fire %>%
  mutate(date = ymd(fire_date)) %>%
  group_by(date) %>%
  summarise(n = n()) %>%
  mutate(csum=cumsum(n)) 

tmp1$date.fire <- tmp1$date


tmp1 <- tmp1 %>%
  separate(date.fire, c("year", "month", "day"))

#Define seasons base on months
tmp1$season <- ifelse (tmp1$month %in% c('12','01','02'), 
                      "SUMMER",
                      ifelse (tmp1$month %in% c('03','04','05'), 
                              "AUTUMN",
                              ifelse (tmp1$month %in% c('06','07','08'), 
                                      "WINTER",
                                      ifelse (tmp1$month %in% c('09','10','11'), 
                                              "SPRING", NA))))

tmp1 %>% 
  ggplot(aes(x = date, csum)) +
  geom_area() +
  labs(x = "", y = "Accumulated number of fire events") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  annotate("rect", xmin=c("1980-12-01", "1981-02-28"), xmax=c("1990-12-01", "1991-02-28"),
           ymin=c(0,50), ymax=c(0,50), alpha=0.2, color="blue", fill="blue")
  
geom_label_repel(aes(label = season), fontface = "bold")




#Lollipop
#Fire frequency
ggplot(my.fire, aes(days_since_fire, visit_id)) +
  geom_segment(aes(x = 0, y = visit_id, xend = days_since_fire, 
                   yend = visit_id), color = "grey50", size = 0.5) +
  geom_point(size = 1) +
  xlab ("Days since the last fire")+
  ylab ("")

