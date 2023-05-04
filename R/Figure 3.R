library("V.PhyloMaker")

#Install ggtree to visualization
#if (!require("BiocManager", quietly = TRUE))
 # install.packages("BiocManager")

#BiocManager::install("ggtree")
library(ggtree)
library(tidyr)
library(dplyr)
library(viridis)
library(ggplot2)
library(ggnewscale)
library("RColorBrewer")
library(psych)

#Loads the data
my.spp <- readRDS("data/Summary-traits-species.rds", refhook = NULL) 

describe(my.spp)

my.spp %>%
  select(family,spp, germ8:surv1) %>%
  group_by(spp) %>%
  summarise(n = n())

example <- readRDS("data/Summary-traits-family.rds", refhook = NULL) %>%
  select(fam,nspp,germ8:surv1)

##### 1 Build a phylogenetic tree ####
# Build a tree using V.PhyloMaker 
tree <- phylo.maker(sp.list=example , tree=GBOTB.extended, 
                      nodes=nodes.info.1, scenarios="S2", r = 1, output.tree = T)


#### 2 Create a summary table of traits by species ####
slc <- tree$scenario.2$run.1$tip.label
df <- example %>% slice(match(example$fam,slc)) %>% 
  select(germ8:surv1)

rownames(df) <- slc

####  Plot a base phylogenetic tree ####
my_breaks <- c(0, 50,100, 200, 400, 600)

base_tree <-
  ggtree(tree$scenario.2$run.1, layout="fan", open.angle=20, size = 0.1)

gheatmap(base_tree, df, offset=1, width=.3,
               colnames_angle = 95, colnames_offset_y = 0.0001, 
         colnames_offset_x = 0, font.size = 2) + 
  scale_fill_gradient(name = "count", trans = "log",
                      breaks = my_breaks, labels = my_breaks) +
  geom_tiplab(align = TRUE, linesize = 0, offset = 50, size= 1.5)
  
  





