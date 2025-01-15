# AIM: Create an practical example of use cases of the Fire Veg database. 
# We provided 2 examples:
# One of trait quantification for specific species (e.g., respruters and seeders)
# Second, the presence/absence of particular organ types. 

# 0. Load the packages ----
library(tidyverse) # For everyday data analyses including manipulation and plotting
library(ggplot2)   # For plotting
library(dplyr)     # For grammar of data manipulation
library(ggtext)
library(patchwork) # For combining plots
library(RColorBrewer) # For define colors palletes
library(ggridges)  # For plotting density ridges
library(ggdist)    # For plotting densities

# 1. Read the figshare tables ----
field_records <- read_csv("practical_examples/input/fireveg-field-records.csv")

# 2. Select the top 20 ----
# Species with more localities and plots
top_species <- field_records |>
  group_by(species) |>
  summarise(
    n_localities = n_distinct(visit_id),
    n_visits = n_distinct(visit_date),
    n_plots = n_distinct(sample_nr)
  ) |>
  slice_max(n_localities, n = 20) |>
  pull(species)


# 3. Calculate traits by species -----
# Number of individuals with a given trait
spp_trait <- field_records |>
  filter(species %in% top_species) |>
  mutate(spp_type = case_when(
    resprout_organ %in% c("None") ~ "Seeder", 
    resprout_organ %in% c("Apical", "Basal", "Epicormic", "Lignotuber", "Short rhizome", "Stolon", "Tuber", "Tussock") ~ "Resprouter")
  ) |>
  group_by(species, visit_id, visit_date, spp_type) |>
  summarise(n1 = sum(resprouts_live, na.rm = TRUE),         # N total live resprouts (N1)
            n2 = sum(resprouts_reproductive, na.rm = TRUE), # N reproductive live resprouts (N2)
            n5 = sum(recruits_live, na.rm = TRUE),          # N total live recruits (N5)
            n6 = sum(recruits_reproductive, na.rm = TRUE),  # N reproductive live recruits (N6)
            n7 = sum(resprouts_died, na.rm = TRUE),         # N Dead resprouts (N7). This variable is all 0
            n8 = sum(recruits_died, na.rm = TRUE),          # N dead recruits (N8)
            n9 = sum(resprouts_kill, na.rm = TRUE)      # N fire killed resprouts (N9)
) |>
  mutate(prop_fire_mortality = n9 /(n1 + n7 + n9),
         prop_sprout_surv = n7/ (n1 + n7),
         seed_adult = (n5 + n8) / (n1 + n7),
         pro_recruit_surv = n5 / (n5 + n8),
         prop_reprod_recruit = n6 / max(n5)
  ) 

str(spp_trait)
n_distinct(spp_trait$species)


# Examle 1: Distribution of traits in the top 20 spp ----
plot_mortality <- spp_trait |>
  ggplot(aes(x = prop_fire_mortality, y = species, fill = spp_type)) +
  geom_density_ridges(scale = 0.4) +
  theme_ridges() +
  labs(title = "Fire mortality",
       x = "Proportion",
       y = "",
       fill = "Species Type") +
  scale_fill_manual(values = c("#6E016B", "#000000")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.5)) +
  theme(legend.position = "none")

plot_recruit_surv <- spp_trait |>
  ggplot(aes(x = pro_recruit_surv, y = species, fill = spp_type)) +
  geom_density_ridges(scale = 0.4) +
  theme_ridges() +
  labs(title = "Recruit survival",
       x = "Proportion",
       y = "",
       fill = "Species Type") +
  scale_fill_manual(values = c("#6E016B", "#000000")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.5)) +
  theme(legend.position = "bottom",
        axis.text.y = element_blank())

plot_sprout_surv <- spp_trait |>
  ggplot(aes(x = prop_sprout_surv, y = species, fill = spp_type)) +
  geom_density_ridges(scale = 0.4) +
  theme_ridges() +
  labs(title = "Sprout survival",
       x = "Proportion",
       y = "",
       fill = "Species Type") +
  scale_fill_manual(values = c("#6E016B", "#000000")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.5)) +
  theme(legend.position = "none",
        axis.text.y = element_blank())

plot_reprod_recruit <- spp_trait |>
  ggplot(aes(x = prop_reprod_recruit, y = species, fill = spp_type)) +
  geom_density_ridges(scale = 0.4) +
  theme_ridges() +
  labs(title = "Reproductive recruits",
       x = "Proportion",
       y = "",
       fill = "Species Type") +
  scale_fill_manual(values = c("#6E016B", "#000000")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.5)) +
  theme(legend.position = "none",
        axis.text.y = element_blank())

plot_mortality + plot_sprout_surv + plot_recruit_surv + plot_reprod_recruit +
  plot_layout(ncol = 4)

ggsave("practical_examples/plots/example_dist_trait_by_topspp.png", width = 15, height = 10)

# Example 2: Spatial variation ----
unique(field_records$visit_id)

library(stringr)

# Filter the records based on visit_id and the year
west_east_locations <- field_records |>
  mutate(region = case_when(
    str_starts(visit_id, "S20") ~ "West",
    TRUE ~ "East"
  )) |>
  group_by(species, visit_id, visit_date, region) |>
  summarise(n1 = sum(resprouts_live, na.rm = TRUE),         # N total live resprouts (N1)
            n2 = sum(resprouts_reproductive, na.rm = TRUE), # N reproductive live resprouts (N2)
            n5 = sum(recruits_live, na.rm = TRUE),          # N total live recruits (N5)
            n6 = sum(recruits_reproductive, na.rm = TRUE),  # N reproductive live recruits (N6)
            n7 = sum(resprouts_died, na.rm = TRUE),         # N Dead resprouts (N7). This variable is all 0
            n8 = sum(recruits_died, na.rm = TRUE),          # N dead recruits (N8)
            n9 = sum(resprouts_kill, na.rm = TRUE)      # N fire killed resprouts (N9)
  ) |>
  mutate(prop_fire_mortality = n9 /(n1 + n7 + n9),
         prop_sprout_surv = n7/ (n1 + n7),
         seed_adult = (n5 + n8) / (n1 + n7),
         pro_recruit_surv = n5 / (n5 + n8),
         prop_reprod_recruit = n6 / max(n5)
  ) |>
  ungroup()


west_east_locations |> select(visit_id, region) |> table()







plant_traits <- field_records |>
  # Resprout organ counts
  count(species, resprout_organ) |>
  pivot_wider(names_from = resprout_organ, values_from = n, values_fill = 0) |>
  rename_with(~ paste0("resprout_", make.names(.)), -species) |>
  
  # Seedbank type counts
  left_join(
    field_records |>
      count(species, seedbank) |>
      pivot_wider(names_from = seedbank, values_from = n, values_fill = 0) |>
      rename_with(~ paste0("seedbank_", make.names(.)), -species),
    by = "species"
  ) |>
  
  left_join(
    field_records |>
      group_by(species) |>
      summarise(n1 = sum(resprouts_live, na.rm = TRUE),         # N total live resprouts (N1)
                n2 = sum(resprouts_reproductive, na.rm = TRUE), # N reproductive live resprouts (N2)
                n5 = sum(recruits_live, na.rm = TRUE),          # N total live recruits (N5)
                n6 = sum(recruits_reproductive, na.rm = TRUE),  # N reproductive live recruits (N6)
                n7 = sum(resprouts_died, na.rm = TRUE),         # N Dead resprouts (N7)
                n8 = sum(recruits_died, na.rm = TRUE),          # N dead recruits (N8)
                n9 = sum(resprouts_kill, na.rm = TRUE),         # N fire killed resprouts (N9)
                .groups = "drop"), by = "species"
  ) 


str(plant_traits)

# 3. Species characteristics -----
plant_traits <- plant_traits |>
  mutate(n_fire_mortality = n1 + n7 + n9,
         prop_fire_mortality = n9 /(n1 + n7 + n9),
         n_sprout_surv = n1 + n7,
         prop_sprout_surv = n7/ (n1 + n7),
         seed_adult = (n5 + n8) / (n1 + n7),
         n_recruit_surv = n5 + n8,
         pro_recruit_surv = n5 / (n5 + n8),
         n_reprod_resprod = n1,
         n_reprod_recruit = n6, # the formula said N5 but this is the number of lives recruits
         prop_reprod_recruit = n6 / max(n5),
         surv_dens = n2 / 625, # plot area
         recruit_dens = n5 / 625 # plot area
  ) 

str(plant_traits)


# 4. Seeders and resprouters -----

# SEEDERS = spp. without reprout organ (seeder). That is with resprout_None > 0
# RESPROUTERS = spp. with resprout organ = rhizome

plant_traits <- plant_traits |>
  mutate(spp_type = case_when(
    resprout_None > 0 ~ "Seeder", 
    resprout_Basal > 0 ~ "Resprouter",
    resprout_Stolon > 0 ~ "Resprouter",
    resprout_Tuber > 0 ~ "Resprouter" ,
    resprout_Short.rhizome > 0 ~ "Resprouter",
    resprout_Epicormic > 0 ~ "Resprouter",
    resprout_Tussock > 0 ~ "Resprouter",
    resprout_Apical > 0 ~ "Resprouter",
    resprout_Lignotuber > 0 ~ "Resprouter")
  ) |>
  mutate(seedbank_type = case_when(
    seedbank_Transient > 0 ~ "Transient",
    seedbank_Non.canopy > 0 ~ "Non_canopy",
    seedbank_Soil.persistent > 0 ~ "Soil_persistent",
    seedbank_Canopy > 0 ~ "Canopy",
    seedbank_NA. > 0 ~ "No_reported")
  ) 


plant_traits <- plant_traits |>
  mutate_at(c('seedbank_type', 'spp_type'), as.factor) |>
  mutate(seedbank_type_ord = fct_relevel(seedbank_type, 
                                         "Non_canopy", 
                                         "Soil_persistent", 
                                         "Transient", 
                                         "Canopy", 
                                         "No_reported" ))


# Summary table
plant_traits |> count(spp_type)
plant_traits |> count(seedbank_type_ord)

# 5. Some plots ----
## Example 1: Organ type ----
# We illustrate the prevalence of particular organ types, differentiating between seeders (none organ type)
# and resprouters with different type of organ type.

plant_traits <- plant_traits |>
  mutate(organ_type = case_when(
    resprout_None > 0 ~ "None", 
    resprout_Basal > 0 ~ "Basal",
    resprout_Stolon > 0 ~ "Stolon",
    resprout_Tuber > 0 ~ "Tuber" ,
    resprout_Short.rhizome > 0 ~ "Rhizome",
    resprout_Epicormic > 0 ~ "Epicormic",
    resprout_Tussock > 0 ~ "Tussock",
    resprout_Apical > 0 ~ "Apical",
    resprout_Lignotuber > 0 ~ "Lignotuber")
  ) |>
  mutate_at(c('organ_type'), as.factor) |>
  mutate(organ_type_ord = fct_relevel(organ_type, 
                                      "Apical",
                                      "Epicormic",
                                      "Lignotuber",
                                      "Stolon", 
                                      "Tuber", 
                                      "Tussock", 
                                      "Basal", 
                                      "None"
  ))

# Get the hex color codes
# brewer.pal(7, "BuPu")

# Define pallet of color for resprouter organ
pal_fill_org_type <- c(
  "Apical" = "#EDF8FB",
  "Epicormic" = "#BFD3E6",
  "Lignotuber" = "#9EBCDA",
  "Stolon" = "#8C96C6", 
  "Tuber" = "#8C6BB1", 
  "Tussock" = "#88419D", 
  "Basal" = "#6E016B", 
  "None" = "black"
)

# Define pallet of color for seedbank
# Get the hex color codes
display.brewer.pal(7, "Greys")
brewer.pal(7, "Greys")

pal_fill_seedbank <- c(
  "Non_canopy" = "#525252",
  "Soil_persistent" =  "#737373",  
  "Transient" = "#BDBDBD", 
  "Canopy" =  "#D9D9D9",
  "No_reported" = "#F7F7F7" )

# Now, let's do the plot

plot_organ_type <- plant_traits |> count(organ_type_ord) |>
  filter_at(vars(organ_type_ord, n), all_vars(!is.na(.))) |>
  ggplot(aes(values = n, fill = organ_type_ord)) +
  waffle::geom_waffle(
    n_rows = 4,        # Number of squares in each row
    color = "white",   # Border color
    flip = F, na.rm = TRUE, 
    make_proportional = T,
    show.legend = T) +
  labs(title = "<b>Organ type</b>",
       subtitle = "A third of species are <br><b><span style='color:#000000;'>seeders</span></b> with none resprout organ. Among resprouter species, <br><b><span style='color:#6E016B;'>basal</span></b> organ is more common.") +
  coord_equal() +
  theme_classic() +
  scale_fill_manual(values = pal_fill_org_type) +
  theme(
    plot.title=element_markdown(), # Enable markdown for title and subtitle
    plot.subtitle=element_markdown(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position ="bottom",
    legend.title = element_blank()
  ) 


plot_seedbank <- plant_traits |> count(seedbank_type_ord) |>
  filter_at(vars(seedbank_type_ord, n), all_vars(!is.na(.))) |>
  ggplot(aes(values = n, fill = seedbank_type_ord)) +
  waffle::geom_waffle(
    n_rows = 4,        # Number of squares in each row
    color = "white",   # Border color
    flip = F, na.rm = TRUE, 
    make_proportional = T,
    show.legend = T) +
  labs(title = "<b>Seedbank type</b>",
       subtitle = "The most frequent seedbank type is <br><b><span style='color:#525252;'>Non_canopy</span></b> followed by <br><b><span style='color:#737373;'>soil persistent</span></b> organ is more common.") +
  coord_equal() +
  theme_classic() +
  scale_fill_manual(values = pal_fill_seedbank) +
  theme(
    plot.title=element_markdown(), # Enable markdown for title and subtitle
    plot.subtitle=element_markdown(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position ="bottom",
    legend.title = element_blank()
  ) 

plot_organ_type + plot_seedbank

ggsave("plots/example_application1.png", width = 12, height = 8)


## Example 2: Seeder & resprouter distribution ----
# We illustrate the distribution of fire mortality (proportion) and recruit survival among seeders and resprouters species
# highlighting differences in their fire-adaptive strategies.

plot_fire_mortality <- plant_traits |>
  filter_at(vars(prop_fire_mortality, spp_type), all_vars(!is.na(.))) |>
  ggplot(aes(x = prop_fire_mortality, y = spp_type, fill = spp_type)) +
  geom_density_ridges(scale = 0.75) +
  theme_ridges() + 
  labs(title = "<b>Fire mortality</b>",
       subtitle = "<br><b><span style='color:#000000;'>Seeders</span></b> species have a broader distribution with higher proportions of fire mortality, while <br><b><span style='color:#6E016B;'>resprouter</span></b> are more concentrated at lower fire mortality values, reflecting differences in their fire-adaptive strategies.",
       x = "Fire mortality (proportion)",
       y = "") +
  scale_fill_manual(values = c("#6E016B", "#000000")) +
  theme(
    plot.title=element_markdown(hjust = 0.5), # Enable markdown for title and subtitle
    plot.subtitle=element_markdown(hjust = 0.5),
    legend.position ="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

plot_recruit_survival <- plant_traits |>
  filter_at(vars(pro_recruit_surv, spp_type), all_vars(!is.na(.))) |>
  ggplot(aes(x = pro_recruit_surv, y = spp_type, fill = spp_type)) +
  geom_density_ridges(scale = 0.75) +
  theme_ridges() + 
  labs(title = "<b>Recruit survival</b>",
       subtitle = "Both <br><b><span style='color:#000000;'>seeders</span></b> and <br><b><span style='color:#6E016B;'>resprouter</span></b> species have similar high recruit survival values.",
       x = "Recruit survival (proportion)",
       y = "") +
  scale_fill_manual(values = c("#6E016B", "#000000")) +
  theme(
    plot.title = element_markdown(hjust = 0.5), # Enable markdown for title and subtitle
    plot.subtitle = element_markdown(hjust = 0.5),
    legend.position ="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.y = element_blank()
  )

plot_fire_mortality + plot_recruit_survival 

ggsave("plots/example_application2.png", width = 15, height = 8)

# Why don’t resprouters ever panic during a wildfire? Because they know they’ve got roots in the matter! 

# END