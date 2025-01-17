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
library(stringr)

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
    TRUE ~ "Resprouter")
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



# 4. Example: Distribution of traits in the top 20 spp ----
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

plot_mortality + plot_recruit_surv + plot_reprod_recruit +
  plot_layout(ncol = 4)

ggsave("practical_examples/plots/example_dist_trait_by_topspp.png", width = 10, height = 8)


# 5. Example: Spatial variation ----
unique(field_records$visit_id)

# Filter the records based on visit_id and the year
west_east_locations <- field_records |>
  filter(species %in% top_species) |>
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

# Fire mortality
# brewer.pal(5, "OrRd")

spatial_fire_mortality <- west_east_locations |>
  ggplot(aes(visit_id, species, fill= prop_fire_mortality)) + 
  geom_tile() +
  scale_fill_gradient(low= "#FEF0D9", high= "#B30000", na.value = "white") +
  labs(x = "Sampling localities",
       y = "",
       title = "Fire mortality",
       fill = "Proportion") +
  facet_grid(~ region, scales = "free_x", space = "free_x") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.position = "bottom")


# Recruit survival
# brewer.pal(5, "Blues")

# Reorder visit_id based on pro_recruit_surv within each region
spatial_dist_recruitsurv <- west_east_locations |>
  ggplot(aes(visit_id, species, fill = pro_recruit_surv)) + 
  geom_tile() +
  scale_fill_gradient(low = "#BDD7E7", high = "#08519C", na.value = "white") +
  labs(x = "Sampling localities",
       y = "",
       title = "Recruit survival",
       fill = "Proportion") +
  facet_grid(~ region, scales = "free_x", space = "free_x") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        axis.text.y = element_blank(),
        legend.position = "bottom")

# Recruit survival
# brewer.pal(5, "Greens")

spatial_dist_reprodrecruit <- west_east_locations |>
  ggplot(aes(visit_id, species, fill= prop_reprod_recruit)) + 
  geom_tile() +
  scale_fill_gradient(low= "#EDF8E9", high= "#006D2C", na.value = "white") +
  labs(x = "Sampling localities",
       y = "",
       title = "Reproductive recruit",
       fill = "Proportion") +
  facet_grid(~ region, scales = "free_x", space = "free_x") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        axis.text.y = element_blank(),
        legend.position = "bottom")

spatial_fire_mortality + spatial_dist_recruitsurv + spatial_dist_reprodrecruit

ggsave("practical_examples/plots/spatial_dist.png", width = 15, height = 8)


# 6. Example: Organ type & Seedbank ----
# We illustrate the prevalence of particular organ types, differentiating between seeders (none organ type)
# and resprouters with different type of organ type.

all_spp_traits <- field_records |>
  mutate(spp_type = case_when(
    resprout_organ %in% c("None") ~ "Seeder", 
    TRUE ~ "Resprouter")
  ) |>
  group_by(species, visit_id, visit_date, spp_type, resprout_organ, seedbank ) |>
  summarise(n1 = sum(resprouts_live, na.rm = TRUE),         # N total live resprouts (N1)
            n2 = sum(resprouts_reproductive, na.rm = TRUE), # N reproductive live resprouts (N2)
            n5 = sum(recruits_live, na.rm = TRUE),          # N total live recruits (N5)
            n6 = sum(recruits_reproductive, na.rm = TRUE),  # N reproductive live recruits (N6)
            n7 = sum(resprouts_died, na.rm = TRUE),         # N Dead resprouts (N7). This variable is all 0
            n8 = sum(recruits_died, na.rm = TRUE),          # N dead recruits (N8)
            n9 = sum(resprouts_kill, na.rm = TRUE)          # N fire killed resprouts (N9)
  ) |>
  mutate(prop_fire_mortality = n9 /(n1 + n7 + n9),
         prop_sprout_surv = n7/ (n1 + n7),
         seed_adult = (n5 + n8) / (n1 + n7),
         pro_recruit_surv = n5 / (n5 + n8),
         prop_reprod_recruit = n6 / max(n5)
  ) |>
  ungroup()

# Summary table
all_spp_traits |> count(spp_type)
all_spp_traits |> count(seedbank)
all_spp_traits |> count(resprout_organ)

# Get the hex color codes
# brewer.pal(8, "BuPu")

# Define pallet of color for resprouter organ
all_spp_traits |> count(resprout_organ) |> arrange(n)

pal_fill_org_type <- c(
  "Apical" = "#EDF8FB",
  "Epicormic"  = "#BFD3E6",
  "Short rhizome" = "#9EBCDA",
  "Stolon" = "#8C96C6", 
  "Tuber" = "#8C6BB1", 
  "Tussock" = "#88419D", 
  "Lignotuber" = "#810F7C", 
  "Basal" = "#4D004B",
  "None" = "black"
)

# Now, let's do the plot

plot_organ_type <- all_spp_traits|> 
  count(resprout_organ) |>
  arrange(n) |> 
  filter_at(vars(resprout_organ, n), all_vars(!is.na(.))) |>
  ggplot(aes(values = n, fill = resprout_organ)) +
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

# Define pallet of color for seedbank
# Get the hex color codes
display.brewer.pal(7, "Greys")
brewer.pal(7, "Greys")

pal_fill_seedbank <- c(
  "Soil-persistent" = "#525252",
  "Non-canopy" =  "#737373",
  "Canopy" =  "#969696",
  "Transient" = "#D9D9D9" )

plot_seedbank <- all_spp_traits |>
  count(seedbank) |>
  arrange(n) |>
  filter_at(vars(seedbank, n), all_vars(!is.na(.))) |>
  ggplot(aes(values = n, fill = seedbank)) +
  waffle::geom_waffle(
    n_rows = 4,        # Number of squares in each row
    color = "white",   # Border color
    flip = F, na.rm = TRUE, 
    make_proportional = T,
    show.legend = T) +
  labs(title = "<b>Seedbank type</b>",
       subtitle = "The most frequent seedbank type is <br><b><span style='color:#D9D9D9;'>Transient</span></b> followed by <br><b><span style='color:#737373;'>Non-canopy</span></b> organ is more common.") +
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

ggsave("practical_examples/plots/dist_seeders_resprouters.png", width = 12, height = 8)

# Why don’t resprouters ever panic during a wildfire? Because they know they’ve got roots in the matter! 

# END