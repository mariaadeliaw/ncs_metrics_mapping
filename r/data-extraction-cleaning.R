# Load necessary libraries
library(tidyverse)
library(janitor)
library(dplyr)
library(stringr)

# dataset prep ------------------------------------------------------------

url <- 'https://docs.google.com/spreadsheets/d/1r_ait8M3KpA5iF-Ysq_9hiqQhuTQmkpwUgQozdqm7Ho/edit?gid=236606241#gid=236606241'
# Load your dataset
# df_raw <- read_csv("temp/temp-result.csv")  # replace with your actual filename
df_raw <- googlesheets4::read_sheet(url)

# Define the columns you want to fill
columns_to_fill <- c(
  "location_detail", "country", "study_object", "land_use_context",
  "modified_condition_of_the_population_by_human_intervention_prior_to_the_study",
  "intervention_in_the_study_has_to_be_an_activity_that_modifies_the_population_study_for_the_sake_of_the_study",
  "ncs_category_griscom_2020", "study_design", "comparator",
  "statistical_analysis"
)

# Clean column names first
df_clean <- df_raw %>%
  janitor::clean_names() %>%
  mutate(doi = str_trim(doi))  # ensure no trailing/leading spaces

# Create a lookup table of the first non-NA row per DOI
doi_lookup <- df_clean %>%
  group_by(doi) %>%
  slice(1) %>%
  ungroup() %>%
  select(doi, all_of(columns_to_fill))

# Join it back to the original dataframe and fill missing values
df_filled <- df_clean %>%
  left_join(doi_lookup, by = "doi", suffix = c("", "_first")) %>%
  mutate(across(
    all_of(columns_to_fill),
    ~coalesce(.x, get(paste0(cur_column(), "_first")))
  )) %>%
  select(-ends_with("_first"))

# analysis ----------------------------------------------------------------

### Analysis ###
# Clean and recode blank entries
df_outcomes <- df_filled %>% 
  select(-column_19) %>% 
  select(-timestamp) %>% 
  mutate(across(everything(), ~na_if(.x, ""))) %>%
  mutate(across(where(is.character), str_squish))

# standardizing outcome group

df_outcomes <- df_outcomes %>%
  mutate(outcome_group = case_when(
    # Soil and Water Quality
    str_detect(outcome, regex("soil|pH|CEC|ECe|SAR|nutrient|organic matter|infiltration|redox|bulk density", ignore_case = TRUE)) ~ "Soil and Water Quality",
    
    # Plant Productivity and Growth
    str_detect(outcome, regex("yield|biomass production|growth|plant height|dry matter|leaf area|crown", ignore_case = TRUE)) ~ "Plant Productivity and Growth",
    
    # Biodiversity and Species Composition
    str_detect(outcome, regex("bird communities|survival|funcitional properties|species importance value|structural parameter|biodiversity|species richness|diversity|functional trait|community structure|species composition|taxonomic|shannon|simpson|rao", ignore_case = TRUE)) ~ "Biodiversity and Species Composition",
    
    # Biomass and Carbon Stock
    str_detect(outcome, regex("volumetric stock|carbon stock|aboveground biomass|AGB|tree volume|biomass partitioning|wood density|tree biomass", ignore_case = TRUE)) ~ "Biomass and Carbon Stock",
    
    # GHG Emissions and Climate Metrics
    str_detect(outcome, regex("table depth|water level|carbon storage|carbon footprint|GHG|CO2|CH4|N2O|greenhouse gas|emission|carbon intensity|NEP|net ecosystem productivity|methane", ignore_case = TRUE)) ~ "GHG Emissions and Climate Metrics",
    
    # Economic and Social Metrics
    str_detect(outcome, regex("agronomic|NPV|profit|cost-benefit|economic|income|social return|well-being|livelihood", ignore_case = TRUE)) ~ "Economic and Social Metrics",
    
    # Landscape and Land Use Change
    str_detect(outcome, regex("cover change|connectivity|land use|LULC|fragmentation|streamflow|fire|land cover|scenario|height", ignore_case = TRUE)) ~ "Landscape and Land Use Change",
    
    # Remote Sensing and Modeling Accuracy
    str_detect(outcome, regex("accuracy|kappa|OA|segmentation|classification|LiDAR|model performance|error matrix", ignore_case = TRUE)) ~ "Remote Sensing and Modeling Accuracy",
    
    # Restoration Effectiveness and Feasibility
    str_detect(outcome, regex("suitability|regeneration potential|feasibility|restoration success|planning|scenario modeling|effectiveness|success rate|opportunity cost", ignore_case = TRUE)) ~ "Restoration Effectiveness and Feasibility",
    
    # Ecosystem Services
    str_detect(outcome, regex("forest value|water availability|ecosystem service|pollination|habitat provision|erosion control|water purification|nutrient cycling|recreation|flood regulation|cultural service|provisioning|regulating|supporting", ignore_case = TRUE)) ~ "Ecosystem Services",
    
    # Fallback
    TRUE ~ "Other"
  ))



#standardizing method
df_outcomes <- df_outcomes %>%
  mutate(method_category = case_when(
    # Remote sensing & geospatial analysis
    str_detect(data_collection_methodology_method, regex("satellite|lidar|LiDAR|remote sensing|imagery|SAR|NDVI|classification|OBIA|point cloud|image preprocessing|spatial analysis|GIS", ignore_case = TRUE)) ~ "Remote Sensing / Geospatial",
    
    # Field-based ecological / biophysical
    str_detect(data_collection_methodology_method, regex("field measurement|transect|plot|vegetation monitoring|DBH|diameter tape|clinometer|transect walk|tree ring|sampling point|timed point count|quadrat|soil sampling|canopy|seedling|sapling|monolith|Gentry|insect sampling", ignore_case = TRUE)) ~ "Field-based Ecological",
    
    # Laboratory-based analyses
    str_detect(data_collection_methodology_method, regex("laboratory|lab analysis|dry matter analysis|carbon analyzer|chemical analysis|DNA extraction|sequencing|bioinformatic|pipette method|radiocarbon|elemental analysis|microbial|taxonomy", ignore_case = TRUE)) ~ "Laboratory",
    
    # Modeling, simulation, and machine learning
    str_detect(data_collection_methodology_method, regex("model|simulation|CGE|Dyna-CLUE|COMAP|TERM|TEM|GoFor|maxent|ecosystem model|system model|machine learning|random forest|ensemble|supervised learning|training|regression|projection|scenario|equation|prediction", ignore_case = TRUE)) ~ "Modeling / Simulation",
    
    # Socioeconomic and participatory
    str_detect(data_collection_methodology_method, regex("focus group|workshop|interview|stakeholder|participatory|survey|respondent|METT|assessment toolkit|preference|opportunity cost|economic dependency", ignore_case = TRUE)) ~ "Socioeconomic / Participatory",
    
    # Industrial or production-level processes
    str_detect(data_collection_methodology_method, regex("standard industrial production|LCA|life cycle assessment|ISO 14067|footprint", ignore_case = TRUE)) ~ "Production / Life Cycle",
    
    # Mixed or other
    TRUE ~ "Mixed / Other"
  ))

df_outcomes <- df_outcomes %>%
  mutate(land_use_context = str_to_title(land_use_context)) # standardize capitalization
# viz
# count group outcome types
df_outcomes %>%
  count(outcome_group, sort = TRUE) %>%
  ggplot(aes(x = fct_rev(outcome_group), y = n)) +
  geom_col(fill = "#e76f51") +
  coord_flip() +
  labs(title = "Grouped Outcome Types", x = "Outcome Category", y = "Count")

library(tidyr)

library(ggplot2)
# Methodology vs Outcome Group
df_outcomes %>%
  count(method_category, outcome_group) %>%
  ggplot(aes(x = method_category, y = n, fill = outcome_group)) +
  geom_col(position = "dodge") +
  labs(
    title = "Method Category vs Outcome Group",
    x = "Method Category",
    y = "Count",
    fill = "Outcome Group"
  ) +
  theme_minimal()

# Methodology vs Ecosystem Type
df_outcomes %>%
  count(method_category, land_use_context) %>%
  ggplot(aes(x = method_category, y = n, fill = land_use_context)) +
  geom_col(position = "dodge") +
  labs(
    title = "Method Category vs Ecosystem Type",
    x = "Method Category",
    y = "Count",
    fill = "Ecosystem Type"
  ) +
  theme_minimal()

# Heatmap of Outcome × Method × Ecosystem
df_outcomes %>%
  count(method_category, outcome_group, land_use_context) %>%
  ggplot(aes(x = outcome_group, y = land_use_context, fill = n)) +
  geom_tile(color = "white") +
  facet_wrap(~ method_category) +
  scale_fill_viridis_c() +
  labs(
    title = "Outcome vs Ecosystem by Method Type",
    x = "Outcome Group",
    y = "Ecosystem Type",
    fill = "Count"
  ) +
  theme_minimal()

clipr::write_clip(df_outcomes)