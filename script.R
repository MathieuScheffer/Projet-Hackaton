library(rsdmx)
library(tidyverse)
library(geodata)
library(raster)
library(leaflet)
library(RColorBrewer)
library(sf)
library(htmltools)
library(eurostat)
library(RCurl)
library(highcharter)
library(forecast)
library(tseries)

##########################################################################################################################################
# Population par canton - 2021 : 1er leaflet
##########################################################################################################################################

# URL des données provenant du STATEC
url_ratio1 <- "https://lustat.statec.lu/rest/data/LU1,DSD_CENSUS_GROUP1_3@DF_B1607,1.0/..A10..Y_GE85+Y65T84+Y50T64+Y30T49+Y15T29+_T+Y_LT15.................?startPeriod=2021&endPeriod=2021"

# Structure de la table
structure_ratio1 <- "https://lustat.statec.lu/rest/dataflow/LU1/DSD_CENSUS_GROUP1_3@DF_B1607/1.0?references=all"

sdmx_structure_ratio1 <- readSDMX(structure_ratio1)

df_ratio1 <- as.data.frame(readSDMX(url_ratio1))

# Recupérer les labels FR de la structure pour les codes GEO
codes_list_geo <- sdmx_structure_ratio1@codelists@codelists[[2]]@Code

df_codes_geo <- data.frame(
  id = sapply(codes_list_geo, function(x) x@id),
  geo_name_fr = sapply(codes_list_geo, function(x) x@name$fr),
  stringsAsFactors = FALSE
)

# Recupérer les labels FR de la structure pour les codes AGE
codes_list_age <- sdmx_structure_ratio1@codelists@codelists[[1]]@Code

df_codes_age <- data.frame(
  id = sapply(codes_list_age, function(x) x@id),
  age_name_fr = sapply(codes_list_age, function(x) x@name$fr),
  stringsAsFactors = FALSE
)

# Canton : GEO 7 - digits
# SOURCE 1
df_ratio1_tf <- df_ratio1 %>%
  filter(nchar(GEO) == 7) %>%
  left_join(df_codes_geo, by = c("GEO" = "id")) %>%
  left_join(df_codes_age, by = c("AGE" = "id")) %>%
  dplyr::select(geo_name_fr, obsValue, age_name_fr) %>%
  mutate(age_name_fr = case_when(
    age_name_fr %in% c("De 65 à 84 ans", "85 ans et plus") ~ "65 ans et plus",
    TRUE ~ age_name_fr
  )) %>%
  group_by(geo_name_fr, age_name_fr) %>%
  summarise(val_total = sum(obsValue, na.rm = TRUE), .groups = "drop") %>%
  filter(age_name_fr %in% c("65 ans et plus", "Total")) %>%
  pivot_wider(
    names_from = age_name_fr,
    values_from = val_total
  ) %>%
  mutate(pct_65_plus = (`65 ans et plus` / Total) * 100) %>%
  dplyr::select(geo_name_fr, pct_65_plus)

##########################################################################################################################################
# Immigration par canton - 2021 : 1er leaflet
################################################################################################
url_ratio2 <- "https://lustat.statec.lu/rest/data/LU1,DSD_CENSUS_GROUP7_10@DF_B1626,1.0/..A10._T...........FOR+_T.....LU00008+LU00009+LU00007+LU00012+LU00004+LU00003+LU00011+LU00002+LU00010+LU00006+LU00001+LU00005+_T..?startPeriod=2021&endPeriod=2021"

# Structure de la table
structure_ratio2 <- "https://lustat.statec.lu/rest/dataflow/LU1/DSD_CENSUS_GROUP7_10@DF_B1626/1.0?references=all"

sdmx_structure_ratio2 <- readSDMX(structure_ratio2)

df_ratio2 <- as.data.frame(readSDMX(url_ratio2))

# Canton : GEO 7 - digits
# SOURCE 2
df_ratio2_tf <- df_ratio2 %>%
  filter(nchar(GEO) == 7) %>%
  left_join(df_codes_geo, by = c("GEO" = "id")) %>%
  dplyr::select(geo_name_fr, obsValue, C_BIRTH) %>%
  group_by(geo_name_fr, C_BIRTH) %>%
  summarise(val_total = sum(obsValue, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = C_BIRTH,
    values_from = val_total
  ) %>%
  mutate(immigre = (FOR / `_T`) * 100) %>%
  dplyr::select(geo_name_fr, immigre)

##########################################################################################################################################
# Depenses publiques du gouvernement luxembourgeois, 1995-2024 - pct du total
##########################################################################################################################################
source_dep_publique <- "gov_10a_exp"

df_dep_publique <- get_eurostat(source_dep_publique,
                              type = "code",
                              time_format = "num",
                              cache = FALSE,
                              keepFlags = TRUE) %>%
  filter(geo == "LU" & 
           cofog99 %in% c("GF07", "GF10", "GF1002") & 
           unit == "PC_TOT" & 
           sector == "S13" &
           na_item == "TE")

df_dep_publique_tf <- label_eurostat(df_dep_publique, lang = "fr", fix_duplicated = TRUE) %>%
  filter(cofog99 %in% c("Santé", "Vieillesse")) %>%
  dplyr::select(TIME_PERIOD, cofog99, values) %>%
  rename("Value" = "values")

df_other_social <- df_dep_publique %>%
  filter(cofog99 %in% c("GF10", "GF1002")) %>%
  dplyr::select(TIME_PERIOD, cofog99, values) %>%
  pivot_wider(names_from = cofog99, values_from = values) %>%
  mutate(`Autres dépenses sociales` = round(GF10 - GF1002, digit = 1)) %>%
  pivot_longer(
    cols = c(GF10, GF1002, `Autres dépenses sociales`),
    names_to = "cofog99",
    values_to = "Value"
  ) %>%
  dplyr::select(TIME_PERIOD, cofog99, Value) %>%
  filter(cofog99 == "Autres dépenses sociales")

df_other_expenditure <- df_dep_publique %>%
  filter(cofog99 %in% c("GF07", "GF10")) %>%
  dplyr::select(TIME_PERIOD, cofog99, values) %>%
  pivot_wider(names_from = cofog99, values_from = values) %>%
  mutate(`Autres dépenses` = round(100 - GF07 - GF10, digit = 1)) %>%
  pivot_longer(
    cols = c(GF07, GF10, `Autres dépenses`),
    names_to = "cofog99",
    values_to = "Value"
  ) %>%
  dplyr::select(TIME_PERIOD, cofog99, Value) %>%
  filter(cofog99 == "Autres dépenses")

df_merged_depense_publique_cate <- rbind(df_dep_publique_tf, df_other_social, df_other_expenditure)

# SOURCE 3
df_depense_publique_cate <- df_merged_depense_publique_cate %>%
  tidyr::pivot_wider(
    names_from = cofog99,
    values_from = Value
  )

##########################################################################################################################################
# Depenses publiques du gouvernement luxembourgeois, 1995-2024 - millions euro
##########################################################################################################################################
source_dep_publique_milleur <- "gov_10a_exp"

df_dep_publique_milleur <- get_eurostat(source_dep_publique_milleur,
                                        type = "code",
                                        time_format = "num",
                                        cache = FALSE,
                                        keepFlags = TRUE) %>%
  filter(geo == "LU" & 
           cofog99 %in% c("TOTAL","GF07", "GF10", "GF1002") & 
           unit == "MIO_EUR" & 
           sector == "S13" &
           na_item == "TE")

df_dep_publique_milleur_tf <- label_eurostat(df_dep_publique_milleur, lang = "fr", fix_duplicated = TRUE) %>%
  filter(cofog99 %in% c("Santé", "Vieillesse")) %>%
  dplyr::select(TIME_PERIOD, cofog99, values) %>%
  rename("Value" = "values")

df_other_social_milleur <- df_dep_publique_milleur %>%
  filter(cofog99 %in% c("GF10", "GF1002")) %>%
  dplyr::select(TIME_PERIOD, cofog99, values) %>%
  pivot_wider(names_from = cofog99, values_from = values) %>%
  mutate(`Autres dépenses sociales` = round(GF10 - GF1002, digit = 1)) %>%
  pivot_longer(
    cols = c(GF10, GF1002, `Autres dépenses sociales`),
    names_to = "cofog99",
    values_to = "Value"
  ) %>%
  dplyr::select(TIME_PERIOD, cofog99, Value) %>%
  filter(cofog99 == "Autres dépenses sociales")

df_other_expenditure_milleur <- df_dep_publique_milleur %>%
  filter(cofog99 %in% c("TOTAL","GF07", "GF10")) %>%
  dplyr::select(TIME_PERIOD, cofog99, values) %>%
  pivot_wider(names_from = cofog99, values_from = values) %>%
  mutate(`Autres dépenses` = round(TOTAL - GF07 - GF10, digit = 1)) %>%
  pivot_longer(
    cols = c(TOTAL, GF07, GF10, `Autres dépenses`),
    names_to = "cofog99",
    values_to = "Value"
  ) %>%
  dplyr::select(TIME_PERIOD, cofog99, Value) %>%
  filter(cofog99 == "Autres dépenses")

df_merged_depense_publique_cate_milleur <- rbind(df_dep_publique_milleur_tf, df_other_social_milleur, df_other_expenditure_milleur)

# SOURCE 4
df_depense_publique_cate_milleur <- df_merged_depense_publique_cate_milleur %>%
  tidyr::pivot_wider(
    names_from = cofog99,
    values_from = Value
  )

##########################################################################################################################################
##########################################################################################################################################

##########################################################################################################################################
# Population de Luxembourg (historique et projection)
##########################################################################################################################################
# Projection
source_provision <- "proj_23np"

df_proj_23np <- get_eurostat(source_provision,
                             type = "code",
                             time_format = "num",
                             cache = FALSE,
                             keepFlags = TRUE) %>%
  filter(geo == "LU", sex == "T", age == "TOTAL")

df_projection_clean <- df_proj_23np %>%
  filter(
    geo == "LU",
    age == "TOTAL",
    sex == "T",
    projection %in% c("BSL","LFRT","LMRT","HMIGR","LMIGR")
  )

df_wide_proj <- df_projection_clean %>%
  filter(TIME_PERIOD >= 2024) %>%
  dplyr::select(projection, TIME_PERIOD, values) %>%
  pivot_wider(
    names_from = TIME_PERIOD,
    values_from = values
  )

# Historique
source_historique <- "demo_pjangroup"

df_demo_pjangroup <- get_eurostat(source_historique,
                                  type = "code",
                                  time_format = "num",
                                  cache = FALSE,
                                  keepFlags = TRUE) %>%
  filter(geo == "LU", sex =="T", age =="TOTAL")

df_hist <- df_demo_pjangroup %>%
  filter(geo == "LU", age == "TOTAL", sex == "T") %>%
  dplyr::select(year = TIME_PERIOD, population = values) %>%
  mutate(projection = "HIST") %>%
  dplyr::select(projection, year, population)

df_hist_wide <- df_hist %>%
  dplyr::select(projection, year, population) %>%
  pivot_wider(names_from = year, values_from = population)

df_final <- bind_rows(df_hist_wide, df_wide_proj)

df_final <- df_final %>%
  dplyr::select(projection, sort(as.numeric(names(.)[-1])) |> as.character())

# SOURCE 5
df_long <- df_final %>%
  pivot_longer(
    cols=-projection,
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(year = as.numeric(year),
         projection = case_when(
           projection == "BSL" ~ "Ligne de base",
           projection == "HIST" ~ "Historique",
           projection == "HMIGR" ~ "Forte migration",
           projection == "LMIGR" ~ "Faible migration",
           projection == "LFRT" ~ "Faible fertilité",
           projection == "LMRT" ~ "Forte fertilité",
           TRUE ~ projection)
  ) 

##########################################################################################################################################
##########################################################################################################################################

##########################################################################################################################################
# Population de Luxembourg (historique et projection) - 65 ans et plus
##########################################################################################################################################
# Projection
source_provision <- "proj_23np"

df_proj_23np <- get_eurostat(source_provision,
                             type = "code",
                             time_format = "num",
                             cache = FALSE,
                             keepFlags = TRUE) %>%
  filter(geo == "LU", sex == "T", age == "Y_GE65")

df_projection_clean <- df_proj_23np %>%
  filter(
    geo == "LU",
    age == "Y_GE65",
    sex == "T",
    projection %in% c("BSL","LFRT","LMRT","HMIGR","LMIGR")
  )

df_wide_proj <- df_projection_clean %>%
  dplyr::select(projection, TIME_PERIOD, values) %>%
  pivot_wider(
    names_from = TIME_PERIOD,
    values_from = values
  )

# DELETE 2022, 2023 and 2024
df_wide_proj <- df_wide_proj %>%
  dplyr::select(-'2022',-'2023',-'2024')

# HISTORIQUE #
source_historique <- "demo_pjangroup"

df_demo_pjangroup <- get_eurostat(source_historique,
                                  type = "code",
                                  time_format = "num",
                                  cache = FALSE,
                                  keepFlags = TRUE) %>%
  filter(geo == "LU", sex =="T", age %in% c("Y65-69","Y70-74","Y_GE75"))


# group age classes
df_65plus <- df_demo_pjangroup %>%
  filter(geo == "LU", age %in% c("Y65-69","Y70-74","Y_GE75"), sex == "T") %>%
  group_by(TIME_PERIOD) %>%
  summarise(
    projection="HIST",
    age="Y65+",
    population=sum(values, na.rm=TRUE),
    .groups="drop"
  ) %>%
  rename(year=TIME_PERIOD)

df_65plus_wide <- df_65plus %>%
  pivot_wider(
    names_from = year,
    values_from = population
  )

df_65plus_wide <- df_65plus_wide %>%
  dplyr::select(-'age')

df_final <- bind_rows(df_65plus_wide, df_wide_proj) %>%
  dplyr::select(projection, sort(as.numeric(names(.)[-1])) |> as.character())

# SOURCE 6
df_pop2100_65plus <- df_final %>%
  pivot_longer(
    cols=-projection,
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(year = as.numeric(year),
         projection = case_when(
           projection == "BSL" ~ "Ligne de base",
           projection == "HIST" ~ "Historique",
           projection == "HMIGR" ~ "Forte migration",
           projection == "LMIGR" ~ "Faible migration",
           projection == "LFRT" ~ "Faible fertilité",
           projection == "LMRT" ~ "Forte fertilité",
           TRUE ~ projection)
  )
##########################################################################################################################################
##########################################################################################################################################

##########################################################################################################################################
# Part des résidents et étrangers dans les pensions
##########################################################################################################################################
url <- "https://raw.githubusercontent.com/MathieuScheffer/Projet-Hackaton/Resources/ap-2-107-rg-evonbpensresid-cat.xlsx"

temp_file <- tempfile(fileext = ".xlsx")

download.file(url, destfile = temp_file, mode = "wb")

DDB.Brute <- read_excel(temp_file, skip = 7)

DDB <- DDB.Brute[c(1,2,6:7,13:14)]

DDB <- DDB[1:16, ]

DDBclean <- DDB[3:16,]

DDBclean <- DDBclean %>% 
  dplyr::select("Année","...7","...14") %>% 
  rename("resident"="...7","etranger"="...14")

PRésidents <- as.numeric(DDBclean$resident)
PEtrangers <- as.numeric(DDBclean$etranger)

PR <- ts(PRésidents, start = 2010, frequency = 1)
PE <- ts(PEtrangers, start = 2010, frequency = 1)

annees <- as.numeric(2010:2023)
annees_futures <- as.numeric(2024:2030)

# Pour Résidents
modele_PR <- lm(PR ~ annees)
# Pour Etrangers
modele_PE <- lm(PE ~ annees)

# Prédictions linéaires
prevision_PR <- predict(modele_PR, newdata = data.frame(annees = annees_futures))
prevision_PE <- predict(modele_PE, newdata = data.frame(annees = annees_futures))

df <- data.frame(
  Annee = c(annees, annees_futures),
  Resident = c(as.numeric(PR), prevision_PR),
  Etranger = c(as.numeric(PE), prevision_PE),
  Type = c(rep("Historique", length(annees)), rep("Prévision", length(annees_futures)))
)

# SOURCE 7
result_lm <- df %>%
  pivot_longer(cols = c("Resident", "Etranger"),
               names_to = "Categorie",
               values_to = "Valeur")
##########################################################################################################################################
##########################################################################################################################################