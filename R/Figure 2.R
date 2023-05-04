#Set working directory
library(RPostgreSQL)
library(ggplot2)
library(forcats)
library(dplyr)
library(data.table)
library(rpostgis)
library(tidyverse)
library(ggtext)


#######Load the files  #######
#Species and traits data
my.table <- read.csv("database_fire_20200530.csv",sep=",",
                     header=T, dec=".", stringsAsFactors=F, na.strings=c("","NA", "N/A"))

#Fire data
my.fire<- read.csv("my.fire_20200316.csv",sep=",",
                     header=T, dec=".", stringsAsFactors=F, na.strings=c("","NA", "N/A"))
my.fire$visit_id <- as.factor(my.fire$visit_id)
#Subset only the last fire
#my.fire2<-my.fire[c(1,4,5, 10,12,15,18, 22,26,27,34,35,38,43),]

####Calculate traits from field data###########

tmp <- my.table %>% distinct(species, .keep_all = TRUE)
tmp <- tmp[!is.na(tmp$species), ]
dim(tmp) #84 spp.
tmp$species

#Select resprouter species
resprout_organ3 <- data.frame(tmp$species, tmp$resprout_organ)
colnames(resprout_organ3)<-c("species1", "resprout_organ")
str(resprout_organ3)

#Seedbank
seedbank3 <- data.frame(tmp$species, tmp$seedbank)
colnames(seedbank3)<-c("species", "seedbank")
str(seedbank3)

#N total live resprouts
N1<- as.data.frame(aggregate(my.table$resprouts_live, list(my.table$species), sum))
colnames(N1)<-c("species", "N1")
dim(N1)

#N reproductive live resprouts
N2<- as.data.frame(aggregate(my.table$resprouts_reproductive, list(my.table$species), sum))
colnames(N2)<-c("species", "N2")

#N total live recruits
N5<- as.data.frame(aggregate(my.table$recruits_live, list(my.table$species), sum))
colnames(N5)<-c("species", "N5")

#N reproductive live recruits
N6<- as.data.frame(aggregate(my.table$recruits_reproductive, list(my.table$species), sum))
colnames(N6)<-c("species", "N6")

#N Dead sprouts
N7<- as.data.frame(aggregate(my.table$resprouts_died, list(my.table$species), sum))
colnames(N7)<-c("species", "N7")

#N dead recruits
N8<- as.data.frame(aggregate(my.table$recruits_died, list(my.table$species), sum))
colnames(N8)<-c("species", "N8")

#N fire killed (resprouts?)
N9<- as.data.frame(aggregate(my.table$resprouts_kill, list(my.table$species), sum))
colnames(N9)<-c("species", "N9")
dim(N9)

#Combine in a data frame
plant.traits <-cbind.data.frame(seedbank3, resprout_organ3, N1, N2, N5, N6, N7, N8, N9)
str(plant.traits)
#Delete column species
drops <- c("species", "plant_species")
plant.traits <-plant.traits[ , !(names(plant.traits) %in% drops)]
#rhizoma.traits["species"] <- NULL

#Species characteristics
plant.traits$n.fire.mortality <- (plant.traits$N1 + plant.traits$N7 + plant.traits$N9)
plant.traits$fire.mortality <- (100*plant.traits$N9)/(plant.traits$N1 + plant.traits$N7 + plant.traits$N9)
plant.traits$n.sprout.surv <-(plant.traits$N1 + plant.traits$N7)
plant.traits$sprout.surv <- (100*plant.traits$N1)/(plant.traits$N1 + plant.traits$N7)
plant.traits$seed_adult <- (plant.traits$N5 + plant.traits$N8)/(plant.traits$N1 + plant.traits$N7)
plant.traits$n.recruit.surv <- (plant.traits$N5 + plant.traits$N8) 
plant.traits$recruit.surv <- (100*plant.traits$N5)/(plant.traits$N5 + plant.traits$N8) 
plant.traits$n.reprod.resprout <- plant.traits$N2
plant.traits$reprod.resprout <- (100*plant.traits$N2)/(plant.traits$N2 + plant.traits$N1) 
plant.traits$n.reprod.recruit <-plant.traits$N6 # the formula said N5 but this is the number of lives recruits
plant.traits$reprod.recruit <- 100*(plant.traits$N6/plant.traits$N5)
plant.traits$surv.dens <- (plant.traits$N2/625)
plant.traits$recruit.dens <-(plant.traits$N5/625)

str(plant.traits)
plant.traits <- plant.traits[!is.na(plant.traits$resprout_organ), ]
plant.traits <- plant.traits[!is.na(plant.traits$seedbank), ]

#write.csv(plant.traits, "plant.traits.csv")


#####
#Dumbbell Plot to visualize relative positions 
#(like growth and decline) between two points in time.

my.spp<- data.frame(species = my.table$species,
                    visit_id = my.table$visit_id,
                    N2_tf = my.table$resprouts_reproductive,
                    N5_tf = my.table$recruits_live,
                    resprout_organ = my.table$resprout_organ)

str(my.spp)
#Now merge with fire information
my.time<- merge(my.spp, my.fire, by = "visit_id")
my.time[1:5,]

###Fire frequency
my.fire %>%
  pivot_longer(cols = c(visit_date, fire_date)) %>% #get into long format
  rename(Event = name,
         Date = value) %>% 
  mutate
  pivot_wider(names_from = Event, values_from = Date) 
  
  
ggplot(my.fire) +
  geom_segment(data =my.fire, aes(x = fire_date, y = visit_id,
                   yend = visit_id, xend = visit_date),
               color = "blue",
               size = 3, #Note that I sized the segment to fit the points
               alpha = .2) +
  geom_point(aes(x = fire_date, y = visit_id), size = 4, show.legend = TRUE)


prueba %>%
  ggplot(aes(x = days_since_fire, y = visit_id)) +
           geom_line(aes(group = Event))+
           geom_point(aes(color = Event), size=4) +
           theme(legend.position="top")

         







####
#SEEDERS SPECIES
####
ggplot(my.time, aes(N5_tf, days_since_fire)) +
  geom_segment(aes(x = 0, y = days_since_fire, xend = N5_tf, 
                   yend = days_since_fire), color = "grey50", size = 0.5) +
  geom_point(size = 1) + coord_flip()

table(my.time$species, my.time$N5_tf>1)

#Pultenaea blakelyi
ggplot(subset(my.time, species %in% c("Pultenaea blakelyi")), aes(N5_tf, days_since_fire)) +
  geom_segment(aes(x = 0, y = days_since_fire, xend = N5_tf, 
                   yend = days_since_fire), color = "grey50", size = 0.5) +
  geom_point(size = 1) + coord_flip() + 
  labs(title = "Pultenaea blakelyi", subtitle = "Total of live recruits")

#Entolasia marginata
ggplot(subset(my.time, species %in% c("Entolasia marginata")), aes(N5_tf, days_since_fire)) +
  geom_segment(aes(x = 0, y = days_since_fire, xend = N5_tf, 
                   yend = days_since_fire), color = "grey50", size = 0.5) +
  geom_point(size = 1) + coord_flip() + 
  xlab("Total of live recruits (N5)") + 
  ylab("Days since fire")+
  labs(title = "Entolasia marginata", subtitle = "Total of live recruits")

#Plectrarnthus parviflorus
ggplot(subset(my.time, species %in% c("Plectranthus parviflorus")), aes(N5_tf, days_since_fire)) +
  geom_segment(aes(x = 0, y = days_since_fire, xend = N5_tf, 
                   yend = days_since_fire), color = "grey50", size = 0.5) +
  geom_point(size = 1) + coord_flip() + 
  labs(title = "Plectranthus parviflorus", subtitle = "Total of live recruits")

#Resprouters
table(my.time$species, my.time$N2_tf>1)
#Lomandra longifolia
ggplot(subset(my.time, species %in% c("Lomandra longifolia")), aes(N2_tf, days_since_fire)) +
  geom_segment(aes(x = 0, y = days_since_fire, xend = N2_tf, 
                   yend = days_since_fire), color = "grey50", size = 0.5) +
  geom_point(size = 1) + coord_flip() + 
  ylab("Days since fire") + 
  xlab("Resprouts reproductive (N2)")+
  labs(title = "Lomandra longifolia", subtitle = "Resprouts reproductive (N2)")

  #Lepyrodia scariosa
  ggplot(subset(my.time, species %in% c("Lepyrodia scariosa")), aes(N2_tf, days_since_fire)) +
    geom_segment(aes(x = 0, y = days_since_fire, xend = N2_tf, 
                     yend = days_since_fire), color = "grey50", size = 0.5) +
    geom_point(size = 1) + coord_flip() + 
    ylab("Days since fire") + 
    xlab("Resprouts reproductive (N2)")+
    labs(title = "Lepyrodia scariosa", subtitle = "Resprouts reproductive (N2)")
  

##END