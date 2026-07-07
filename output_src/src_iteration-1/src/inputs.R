source("H:/utils/m_functions.R")
source("H:/utils/demog_functions.R")
library(data.table)
library(glue)
library(ggplot2)

stap_cols <- c("rinpersoon", "geslacht", "leeftijd", "gem", "wc", 
               "inkomen_klasse", "seswoa_cat")

stap_raw_cols <- c("wmo_gem_huishoudelijke_hulp", "wmo_gem_ondersteuning_thuis",
                   "wmo_gem_hulpmiddelen_diensten", "wmo_gem_verblijf_opvang",
                   "belanginkbronpers", "belanginkbronhh", "bedrijfstak_10cat", 
                   "huishoudnr", "aantpphh", "huishsamstsocec")

labels_stap_raw <- c("belanginkbronpers", "belanginkbronhh",
                     "bedrijfstak_10cat")
years <- 2020:2021

lbz_vars <- c("RINPERSOON", "LBZIcd10hoofddiagnose", "LBZOpnamedatum", 
              "LBZICopnamedag", "LBZICaantaldagen", "LBZOntslagdatum")

med_vars <- c("RINPERSOON", "ATC4")
atc4_list <- c("L04A", "L01X", "A10A", "A10B", "C10A", "C10B", "A16A", "H01C",
               "H04A", "C01A", "C01B", "C01C", "C01D", "C01E", "C07A", "C08C",
               "C09A")
immuno <- c("A07E", "H02A", "H02B", "J06B", "L01A", "L01B", "L01C", "L01D", "L01X",
            "L04A", "M01A", "M01B", "M01C")
diabetes <- c("A10A", "A10B")
metabool <- c("A10A", "A10B", "C10A", "C10B", "A16A", "H01C",
              "H04A")
hartziekte <- c("C01A", "C01B", "C01C", "C01D", "C01E", "C07A", "C08C",
                "C09A")

# covid uccodes
covid_code <- "U071" 
presumed_covid_code <- "U072"
