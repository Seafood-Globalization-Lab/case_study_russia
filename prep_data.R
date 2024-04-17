
# libraries
library(tidyverse)
library(countrycode)
library(exploreARTIS)
library(DBI)
library(janitor)
library(broom)
library(dbscan)
library(GGally)
library(ggrepel)
library(ggpubr)

# Setup-------------------------------------------------------------------------
outdir <- "data"

# Initial database pulls--------------------------------------------------------
# Database connection
con <- dbConnect(RPostgres::Postgres(),
                 dbname=Sys.getenv("DB_NAME"),
                 host="localhost",
                 port="5432",
                 user=Sys.getenv("DB_USERNAME"),
                 password=Sys.getenv("DB_PASSWORD"))

# Check that connection is established by checking which tables are present
dbListTables(con)

# ARTIS dataframe
custom_artis_query <- "SELECT * FROM snet WHERE 
((hs_version = 'HS96' AND year >= 1996 AND year <= 2003) OR
(hs_version = 'HS02' AND year >= 2004 AND year <= 2012) OR
(hs_version = 'HS12' AND year >= 2013 AND year <= 2020))"

artis <- dbGetQuery(con, custom_artis_query) %>%
  select(-record_id)

custom_consumption_query <- "SELECT * FROM complete_consumption WHERE 
((hs_version = 'HS96' AND year >= 1996 AND year <= 2003) OR
(hs_version = 'HS02' AND year >= 2004 AND year <= 2012) OR
(hs_version = 'HS12' AND year >= 2013 AND year <= 2020))"

# consumption dataframe
consumption <- dbGetQuery(con, custom_consumption_query) %>%
  select(-record_id) 

# Production dataframe
prod <- dbGetQuery(con, "SELECT * FROM production") %>%
  select(-record_id) 

# Product metadata dataframe
product <- dbGetQuery(con, "SELECT * FROM products") %>%
  select(-record_id) 

# Species metadata
sciname_metadata <- dbGetQuery(con, "SELECT * FROM sciname") %>%
  select(-record_id)

# Improved taxa resolution metadata
sciname_hs_modified <- dbGetQuery(con, "SELECT * FROM code_max_resolved_taxa") %>%
  select(-record_id)

# Close database connection
dbDisconnect(con)
rm(list = c("con"))

artis <- artis %>%
  # Add variables for whether source/importer/exporter imposed sanctions following annexation of Crimea
  mutate(sanctions_towards_russia = case_when(
    importer_iso3c %in% c("USA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROM", "SVK", "SVN", "ESP", "SWE", "GBR", "JPN", "ALB", "ISL", "AUS", "MNT", "UKR", "CAN", "NOR", "CHE", "MDA")  ~ "Yes",
    TRUE ~ "No"
  )) %>%
  mutate(exporter_sanctions_towards_russia = case_when(
    exporter_iso3c %in% c("USA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROM", "SVK", "SVN", "ESP", "SWE", "GBR", "JPN", "ALB", "ISL", "AUS", "MNT", "UKR", "CAN", "NOR", "CHE", "MDA")  ~ "Yes",
    TRUE ~ "No")) %>% 
  left_join(sciname_metadata %>% 
              select(sciname, common_name), 
            by = "sciname") %>% 
  mutate(source_sanctions_towards_russia = case_when(
    source_country_iso3c %in% c("USA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROM", "SVK", "SVN", "ESP", "SWE", "GBR", "JPN", "ALB", "ISL", "AUS", "MNT", "UKR", "CAN", "NOR", "CHE", "MDA")  ~ "Yes",
    TRUE ~ "No")) %>% 
  # Join on common names
  left_join(sciname_metadata %>% 
              select(sciname, common_name), 
            by = "sciname") 


consumption <- consumption %>%
  # Filter consumption to when Russia is involved
  filter(source_country_iso3c == "RUS"|exporter_iso3c == "RUS"|consumer_iso3c == "RUS") %>%
  # Add variables for whether source/importer/exporter imposed sanctions following annexation of Crimea
  mutate(sanctions_towards_russia = case_when(
    consumer_iso3c %in% c("USA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", 
                          "POL", "PRT", "ROM", "SVK", "SVN", "ESP", "SWE", "GBR", "JPN", "ALB", "ISL", "AUS", "MNT", "UKR", "CAN", "NOR", "CHE", "MDA")  ~ "Russia unfriendly countries",
    TRUE ~ "Russia friendly countries"
  )) %>%
  mutate(exporter_sanctions_towards_russia = case_when(
    exporter_iso3c %in% c("USA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROM", "SVK", "SVN", "ESP", "SWE", "GBR", "JPN", "ALB", "ISL", "AUS", "MNT", "UKR", "CAN", "NOR", "CHE", "MDA")  ~ "Russia unfriendly countries",
    TRUE ~ "Russia friendly countries")) %>%
  mutate(source_sanctions_towards_russia = case_when(
    source_country_iso3c %in% c("USA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROM", "SVK", "SVN", "ESP", "SWE", "GBR", "JPN", "ALB", "ISL", "AUS", "MNT", "UKR", "CAN", "NOR", "CHE", "MDA")  ~ "Russia unfriendly countries",
    TRUE ~ "Russia friendly countries")) %>% 
  # Add common names (for both sciname and sciname_hs_modified)
  left_join(sciname_metadata %>% 
              select(sciname, common_name), 
            by = "sciname") %>%
  left_join(sciname_metadata %>% 
              select(sciname, "common_name_hs_modified" = "common_name"), 
            by = c("sciname_hs_modified" = "sciname"))

write.csv(consumption, file.path(outdir, "case_study_consumption.csv"), row.names = FALSE)
# Figure 1----------------------------------------------------------------------
# Consumption of Russian products by sanctioning and non-sanctioning countries

consumption_russian_ts <- consumption %>%
  filter(consumer_iso3c != "RUS", 
         source_country_iso3c == "RUS") %>%
  mutate(path = case_when(
    exporter_iso3c == "RUS" ~ "Direct import", 
    TRUE ~ "Import from intermediate partner"
  )) %>%
  mutate(sanctions_towards_russia = case_when(
    sanctions_towards_russia == "Russia friendly countries" ~ "Friendly countries' consumption\nof seafood from Russia",
    TRUE ~ "Unfriendly countries' consumption\nof seafood from Russia"
  )) %>% 
  group_by(year, path, sanctions_towards_russia) %>%
  summarise(consumption_live_t = sum(consumption_live_t)) %>%
  ungroup()

write.csv(consumption_russian_ts, file.path(outdir, "fig1_consumption_from_russia.csv"), row.names = FALSE)


# Russian consumption of seafood products from sanctioning and non-sanctioning
consumption_by_russia_ts <- consumption %>%
  filter(consumer_iso3c == "RUS", 
         source_country_iso3c != "RUS") %>%
  mutate(path = case_when(
    source_country_iso3c == exporter_iso3c ~ "Direct import", 
    TRUE ~ "Import from intermediate partner"
  )) %>% 
  mutate(source_sanctions_towards_russia = case_when(
    source_sanctions_towards_russia == "Russia friendly countries" ~ "Russian consumption of seafood\nfrom friendly countries",
    TRUE ~ "Russian consumption of seafood\nfrom unfriendly countries"
  )) %>%
  group_by(year, path, source_sanctions_towards_russia) %>%
  summarise(consumption_live_t = sum(consumption_live_t)) %>%
  ungroup()

write.csv(consumption_by_russia_ts, file.path(outdir, "fig1_consumption_by_russia.csv"), row.names = FALSE)

# Figure 2----------------------------------------------------------------------
rus_to_chn <- artis %>%
  # Isolating Chinese imports that were originally caught in Russia
  filter(source_country_iso3c == "RUS", importer_iso3c == "CHN") %>%
  group_by(year, sciname) %>%
  summarise(russia_live_t = sum(live_weight_t, na.rm = TRUE))

notrus_to_chn <- artis %>%
  # Isolating Chinese imports that were NOT originally caught in Russia
  filter(source_country_iso3c != "RUS", importer_iso3c == "CHN") %>%
  group_by(year, sciname) %>%
  summarise(notrussia_live_t = sum(live_weight_t, na.rm = TRUE))

chn_to_usa <- artis %>%
  filter(exporter_iso3c == "CHN", importer_iso3c == "USA") %>%
  group_by(year, sciname) %>%
  summarise(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
  left_join(rus_to_chn, by = c("year", "sciname")) %>%
  left_join(notrus_to_chn, by = c("year", "sciname")) %>%
  replace_na(list(russia_live_t = 0, notrussia_live_t = 0)) %>% 
  # Calculate the minimum by year and species that had to come from Russia
  mutate(min_russia = case_when(
    # If non-Russian product exceeds the volume sent to the US, 
    # then min that had to be Russian is 0
    (notrussia_live_t >= live_weight_t) ~ 0,
    # If non-Russian product cannot explain all, then fill up remaining with Russian product
    (live_weight_t - notrussia_live_t) >= russia_live_t ~ russia_live_t,
    # Otherwise fill up remaining with Russian product
    TRUE ~ (live_weight_t - notrussia_live_t)
  )) %>% 
  # Calculate the max that could have come from Russia
  mutate(max_russia = case_when(
    # If Russian product can explain full amount, max is total trade flow
    russia_live_t >= live_weight_t ~ live_weight_t, 
    # If Russian product cannont explain all, fill up with Russian product to get max
    russia_live_t < live_weight_t ~ russia_live_t
  ))

write.csv(chn_to_usa, file.path(outdir, "chn_to_usa.csv"), row.names = FALSE)

rus_chn_usa <- artis %>%
  filter(source_country_iso3c == "RUS", exporter_iso3c == "CHN", importer_iso3c == "USA") %>%
  group_by(year, sciname) %>%
  summarise(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(chn_to_usa %>%
              select(year, sciname, min_russia, max_russia)) %>%
  mutate(live_weight_t = live_weight_t / 1e3,
         min_russia = min_russia / 1e3,
         max_russia = max_russia / 1e3)

write.csv(rus_chn_usa, file.path(outdir, "rus_chn_usa.csv"), row.names = FALSE)

# Top species US consumes sourced from Russia
species_rank_df <- consumption %>%
  filter(consumer_iso3c == "USA", source_country_iso3c == "RUS", year %in% 2015:2019) %>%
  mutate(path = case_when(
    exporter_iso3c == "RUS" ~ "Direct\nimport", 
    TRUE ~ "Import from\nintermediate partner"
  )) %>% 
  rename("importer_iso3c" = "consumer_iso3c",
         "live_weight_t" = "consumption_live_t") %>%
  mutate(common_name_hs_modified = case_when(
    common_name_hs_modified == "alaska pollock(=walleye poll.)" ~ "Alaska Pollock",
    common_name_hs_modified == "atlantic cod" ~ "Atlantic Cod",
    common_name_hs_modified == "pink(=humpback) salmon" ~ "Pink Salmon",
    common_name_hs_modified == "haddock" ~ "Haddock",
    common_name_hs_modified == "pacific cod" ~ "Pacific Cod",
    TRUE ~ common_name_hs_modified
  )) %>%
  mutate(common_name_hs_modified = gsub(" ", "\n", common_name_hs_modified)) %>%
  mutate(live_weight_t = live_weight_t / length(2015:2019)) %>%
  mutate(live_weight_t = live_weight_t / 1e3)

write.csv(species_rank_df, file.path(outdir, "species_ranking.csv"), row.names = FALSE)
