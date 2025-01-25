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
library(readxl)    # For reading data in Excel format

here::i_am("Notebooks/00-Overview-trait-information.ipynb")

# 1. Read the figshare tables ----
field_records <- read_csv(here::here(data_dir, "figshare_data", "fireveg-field-records.csv"), show_col_types = FALSE)

species_list <- read_excel(here::here(data_dir, "figshare_data","fireveg-field-report-model.xlsx"), sheet = 4)
sites_record <- read_excel(here::here(data_dir, "figshare_data","fireveg-field-report-model.xlsx"), sheet = 2)

# 2. Last fire -----
last_fire <- sites_record |>
  mutate(last_fire = if_else(
    grepl("^(2 years|3 years|1 year)", `Time since last fire (days)`),
    "recent",
    "older"
  )) |>
  select("Site label", "last_fire") |>
  rename(visit_id = `Site label`)

head(last_fire)


# 3. Calculate traits by species -----
# Number of individuals with a given trait. All the spp in the datset
full_spp_trait <- field_records |>
  mutate(spp_type = case_when(
    resprout_organ %in% c("None") ~ "Seeder", 
    TRUE ~ "Resprouter")
  ) |>
  left_join(species_list, by = c("species" = "Scientific name (as entered)")) |> # Add family information
  left_join(last_fire, by = "visit_id") |>  # Add fire information
  filter(last_fire == "recent") |> # Calculate the metrics only for these sites with time since last fire <= 3 years
  group_by(species, visit_id, visit_date, spp_type, resprout_organ, seedbank, Family, last_fire) |>
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
  as_tibble() 
  
# Summary table
full_spp_trait |> count(spp_type)
full_spp_trait |> count(seedbank)
full_spp_trait |> count(resprout_organ)
full_spp_trait |> count(Family)
full_spp_trait |> count(last_fire)

# 4. Select the top 20 spp ----
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


# 5. Select the top 4 families -----
# Which are the families with more spp?
species_list |>
  group_by(Family) |>
  summarise(
    n_spp = n_distinct(`Scientific name (as entered)`)
  ) |>
  arrange(desc(n_spp)) |>
  slice_max(n_spp, n = 5 ) |>
  na.omit(Family)

top_families <- full_spp_trait |>
  group_by(Family) |>
  summarise(n_spp = n_distinct(species)) |>
  arrange(desc(n_spp)) |>
  slice_max(n_spp, n = 6 ) |>
  na.omit(Family) |>
  pull(Family)


# 6. Example 1: Organ type & Seedbank ----
# We illustrate the prevalence of particular organ types, differentiating between seeders (none organ type)
# and resprouters with different type of organ type across the top 5 families included in the field observation data.

plot_organ_type <- full_spp_trait|> 
  filter(Family %in% top_families) |>
  filter(!is.na(resprout_organ)) |>
  mutate(Family = recode(Family, 
                         "Fabaceae (Faboideae)" = "Fabaceae")) |>
  group_by(Family, resprout_organ) |>
  summarise(n_species = n_distinct(species)) |>
  arrange(n_species) |> 
  mutate(resprout_organ = fct_reorder(resprout_organ, desc(n_species))) |>
  ggplot(aes(x = resprout_organ, y = n_species)) +
  geom_bar(stat="identity", fill = "black") +
  facet_grid(~ Family) +
  coord_flip() +
  ylim(0, 60) +
  labs(y = "Number of species",
       x = "",
       title = "<b>Distribution of species count by resprout organ<b>",
       subtitle = "Variation in resprouting strategies among the top 5 families") +
  theme_classic() +
  theme(plot.title=element_markdown(), # Enable markdown for title and subtitle
        plot.subtitle=element_markdown())


plot_seedbank <- full_spp_trait|> 
  filter(Family %in% top_families) |>
  filter(!is.na(seedbank)) |>
  mutate(Family = recode(Family, 
                         "Fabaceae (Faboideae)" = "Fabaceae")) |>
  group_by(Family, seedbank) |>
  summarise(n_species = n_distinct(species)) |>
  arrange(n_species) |> 
  mutate(seedbank = fct_reorder(seedbank, desc(n_species))) |>
  ggplot(aes(x = seedbank, y = n_species)) +
  geom_bar(stat="identity", fill = "black") +
  facet_grid(~ Family) +
  ylim(0, 60) +
  coord_flip() +
  labs(y = "Number of species",
       x = "",
       title = "<b>Distribution of species count by seedbank type<b>",
       subtitle = "Variation in seedbank strategies among the top 5 families") +
  theme_classic() +
  theme(plot.title = element_markdown(), # Enable markdown for title and subtitle
        plot.subtitle = element_markdown())

plot_organ_type / plot_seedbank

ggsave("practical_examples/plots/dist_seeders_resprouters.png", width = 10, height = 8)


# 7. Example 2: Distribution of traits in the top 20 spp ----

plot_mortality <- full_spp_trait |>
  filter(species %in% top_species) |> # Only the top 20 spp.
  mutate(species = reorder(species, spp_type, FUN = function(x) ifelse(x[1] == "Seeder", 1, 2))) |>
  ggplot(aes(x = prop_fire_mortality, y = species, fill = spp_type, color = spp_type)) +
  geom_density_ridges(scale = 0.4) +
  theme_ridges() +
  labs(title = "Fire mortality",
       x = "Proportion",
       y = "",
       fill = "Species Type") +
  scale_fill_manual(values = c("Resprouter" = "#9EBCDA", "Seeder" = "#000000")) +
  scale_color_manual(values = c("Resprouter" = "#9EBCDA", "Seeder" = "#000000")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.5)) +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "italic") )

plot_recruit_surv <- full_spp_trait |>
  filter(species %in% top_species) |>
  mutate(species = reorder(species, spp_type, FUN = function(x) ifelse(x[1] == "Seeder", 1, 2))) |>
  ggplot(aes(x = pro_recruit_surv, y = species, fill = spp_type, color = spp_type)) +
  geom_density_ridges(scale = 0.4) +
  theme_ridges() +
  labs(title = "Recruit survival",
       x = "Proportion",
       y = "",
       fill = "Species Type") +
  scale_fill_manual(values = c("#9EBCDA", "#000000")) +
  scale_color_manual(values = c("#9EBCDA", "#000000")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.5)) +
  theme(legend.position = "none",
        axis.text.y = element_blank())

plot_reprod_recruit <- full_spp_trait |>
  filter(species %in% top_species) |>
  filter(last_fire == "recent") |>
  mutate(species = reorder(species, spp_type, FUN = function(x) ifelse(x[1] == "Seeder", 1, 2))) |>
  ggplot(aes(x = prop_reprod_recruit, y = species, fill = spp_type, color = spp_type)) +
  geom_density_ridges(scale = 0.4) +
  theme_ridges() +
  labs(title = "Reproductive recruits",
       x = "Proportion",
       y = "",
       fill = "Species Type") +
  scale_fill_manual(values = c("#9EBCDA", "#000000")) +
  scale_color_manual(values = c("#9EBCDA", "#000000")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.5)) +
  theme(legend.position = "none",
        axis.text.y = element_blank())

plot_mortality + plot_recruit_surv + plot_reprod_recruit +
  plot_layout(ncol = 3) +
  plot_annotation(title = 'Distribution of traits in the top 20 spp in the Mallee data',
                  subtitle = 'Resprouter species in light blue and seeders in black. Only sites with time since the last fire sites ≤3 years were included in traits calculation'
  ) &
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12) )

ggsave("practical_examples/plots/dist_trait_by_topspp.png", width = 12, height = 8)


# 8. Example 3: Spatial variation ----
unique(field_records$visit_id)

# Filter the records based on visit_id
scotia_tarawi_locations <- field_records |>
  filter(species %in% top_species) |>
  mutate(spp_type = case_when(
    resprout_organ %in% c("None") ~ "Seeder", 
    TRUE ~ "Resprouter")
  ) |>
  mutate(region = case_when(
    str_starts(visit_id, "S20") ~ "Scotia",
    str_starts(visit_id, "T19") ~ "Tarawi",
    str_starts(visit_id, "T20") ~ "Tarawi",
    TRUE ~ "Other"
  )) |>
  filter(region != "Other") |>
  left_join(last_fire, by = "visit_id") |>  # Add fire information
  filter(last_fire == "recent") |> # Calculate the metrics only for these sites with time since last fire <= 3 years
  group_by(species, visit_id, visit_date, region, spp_type, last_fire) |>
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

# Checking
scotia_tarawi_locations |> count(region)
scotia_tarawi_locations |> count(spp_type)
scotia_tarawi_locations |> count(last_fire) # recent      644

# Fire mortality
# brewer.pal(5, "Greys")
spatial_fire_mortality <- scotia_tarawi_locations |>
  mutate(species = reorder(species, spp_type, FUN = function(x) ifelse(x[1] == "Seeder", 1, 2))) |>
  ggplot(aes(visit_id, species, fill= prop_fire_mortality)) + 
  geom_tile() +
  scale_fill_gradient(low= "#F7F7F7", high= "#252525", na.value = "white") +
  labs(x = "Sampling localities",
       y = "",
       title = "Fire mortality",
       fill = "Proportion") +
  facet_grid(~ region, scales = "free_x", space = "free_x") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.position = "bottom",
        axis.text.y = element_text(size = 10, face = "italic"))

# Recruits survival
spatial_dist_recruitsurv <- scotia_tarawi_locations |>
  ggplot(aes(visit_id, species, fill = pro_recruit_surv)) + 
  geom_tile() +
  scale_fill_gradient(low= "#F7F7F7", high= "#252525", na.value = "white") +
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

# Reproductive recruits
spatial_dist_reprodrecruit <- scotia_tarawi_locations |>
  ggplot(aes(visit_id, species, fill= prop_reprod_recruit)) + 
  geom_tile() +
  scale_fill_gradient(low= "#F7F7F7", high= "#252525", na.value = "white") +
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

# Ensamble the plots
spatial_fire_mortality + spatial_dist_recruitsurv + spatial_dist_reprodrecruit +
  plot_layout(ncol = 3) +
  plot_annotation(title = 'Spatial distribution of traits in the top 20 spp in the Mallee data',
                  subtitle = 'Sampling localities are separated into Scotia and Tarawi. Only sites with time since the last fire sites ≤3 years were included in traits calculation'
  ) &
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12) )

ggsave("practical_examples/plots/spatial_dist.png", width = 15, height = 8)


# Why don’t resprouters ever panic during a wildfire? Because they know they’ve got roots in the matter! 

# END