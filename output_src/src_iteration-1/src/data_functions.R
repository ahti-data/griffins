# Griffins code
source("src/tijn/inputs.R")

# Wrangle wijk22 file so its easier to use
wijk22 <- readxl::read_excel("H:/data/crosswalks/Amsterdam Gemeente & CBS indelingen 2017 .xlsx")
keep <- c("WK_CODE", "Stadsdelen", "AMS_Wijk 22")
wijk22 <- as.data.table(wijk22)[, ..keep]
setnames(wijk22, c("WK_CODE", "Stadsdelen", "AMS_Wijk 22"), c("wc", "stadsdeel", "wijk22"))
wijk22[, wc := substr(wc, 3, nchar(wc))]
wijk22[, stadsdeel := substr(stadsdeel, 3, nchar(stadsdeel))]
wijk22[, wijk22 := substr(wijk22, 6, nchar(wijk22))]
wijk22 <- unique(wijk22, by = "wc")

#### Demog ####
demog <- function(years){
  dt <- data.table()
  for (yr in years){
    
    # Combine stapeling data
    #stap_year <- yr - 1
    stap_path <- paste0("H:/data/demog/", yr - 1, "/rin_demog.parquet")
    print(stap_path)
    stap <- arrow::read_parquet(stap_path,
                                all_of(stap_cols))
    #print(names(stap))
    stap <- format_data(stap)
    stap <- stap[gem == "0363"]
    stap[, leeftijd := as.numeric(haven::zap_labels(leeftijd))]
    
    # Make a few different leeftijd cat vars
    stap[, leeftijd_8 := fcase(
      leeftijd < 18, "0-17",
      leeftijd < 30, "18-29",
      leeftijd < 40, "30-39",
      leeftijd < 50, "40-49",
      leeftijd < 60, "50-59",
      leeftijd < 70, "60-69",
      leeftijd < 80, "70-79",
      leeftijd > 79, "80+"
    )]
    
    stap[, leeftijd_3 := fcase(
      leeftijd < 60, "0-59",
      leeftijd < 70, "60-69",
      leeftijd > 69, "70+"
    )]
    
    # leeftijd_labels <- c(paste0(seq(0, 95, by = 5), "-", seq(4,84,by=5)), "100+")
    # stap[, leeftijd_20 := cut(leeftijd,
    #                               breaks = c(seq(0, 100, by = 5), Inf),
    #                               right = F,
    #                               include.lowest = T,
    #                               labels = leeftijd_labels)]
    # 
    
    stap[, seswoa_small := fcase(
      seswoa_cat == "0-10%", "0-10%",
      seswoa_cat == "50-75%", "50-75%",
      seswoa_cat == "75-100%", "75-100%",
      seswoa_cat == "Onbekend", "Onbekend",
      default = "10-50%"
    )]
    
    # Melt income groups together
    stap[, inkomen_klasse_small := fcase(
      inkomen_klasse == "tot_120", "tot_120", 
      inkomen_klasse == "120_160", "120_280",
      inkomen_klasse == "160_200", "120_280",
      inkomen_klasse == "200_240", "120_280",
      inkomen_klasse == "240_280", "120_280",
      inkomen_klasse == "280_400", "280_400", 
      inkomen_klasse == "400+", "400+",
      default = "onbekend_inst_student"
    )]
    
    stap <- merge(stap, wijk22, by = "wc", all.x=T)
    
    
    print("read demog stapeling")
    stap_raw <- read_demog_stapeling(yr - 1, 
                                     cols = stap_raw_cols, 
                                     labelled_cols = labels_stap_raw)
    stap_raw <- format_data(stap_raw)
    
    stap <- merge(stap, stap_raw, all.x = T, by = "rinpersoon")
    
    stap[, file_year := yr]
    
    stap[, wmo := as.integer(rowSums(.SD, na.rm=T) > 0),
         .SDcols = c("wmo_gem_huishoudelijke_hulp", "wmo_gem_ondersteuning_thuis",
                     "wmo_gem_hulpmiddelen_diensten", "wmo_gem_verblijf_opvang")]
    
    stap[, hh_group := fcase(
      huishsamstsocec == "Institutioneel huishouden", "inst_hh",
      aantpphh == 1, "1",
      aantpphh == 2, "2",
      aantpphh %in% c(3,4), "3-4",
      aantpphh > 4, "5+"
    )]
    
    print("computing hh met kind schoolleeftijd")
    hh <- stap[leeftijd <= 18, .(
      hh_kind_0_3 = as.integer(any(leeftijd <= 3)),
      hh_kind_4_11 = as.integer(any(leeftijd >= 4 & leeftijd <= 11)),
      hh_kind_12_18 = as.integer(any(leeftijd >= 12))
    ), by = .(huishoudnr, file_year)]
    
    stap[hh, on = .(huishoudnr, file_year),`:=` (
      hh_kind_0_3 = i.hh_kind_0_3,
      hh_kind_4_11 = i.hh_kind_4_11,
      hh_kind_12_18 = i.hh_kind_12_18
    )]
    
    setnafill(stap, fill = 0L, 
              cols = c("hh_kind_0_3", "hh_kind_4_11", "hh_kind_12_18"))
    
    stap[, hh_kind_group := paste0(
      fifelse(hh_kind_0_3 == 1, "0-3", ""),
      fifelse(hh_kind_4_11 == 1, " & 4-11", ""),
      fifelse(hh_kind_12_18 == 1, " & 12-18", "")
    )]
    
    stap[, hh_kind_group := sub("^ & ", "", hh_kind_group)]
    stap[, hh_kind_group := fifelse(hh_kind_group == "", "geen_kind", hh_kind_group)]
    
    stap[, c("wmo_gem_huishoudelijke_hulp", "wmo_gem_ondersteuning_thuis",
             "wmo_gem_hulpmiddelen_diensten", "wmo_gem_verblijf_opvang") := NULL]
    
    print("add herkomst")
    stap <- add_herkomst(stap)
    
    stap[, herkomstland := fcase(
      herkomst7 == "Overig Afrika, Azië, Amerika en Oceanië", "Overig buiten Europa", 
      herkomst7 == "Europa (exclusief Nederland)", "Europa (exclusief Nederland)",
      herkomst7 == "Suriname", "Buiten Europa (Tur., Mar., Sur., Ind., NL Cariben)",
      herkomst7 == "Marokko", "Buiten Europa (Tur., Mar., Sur., Ind., NL Cariben)",
      herkomst7 == "Indonesië", "Buiten Europa (Tur., Mar., Sur., Ind., NL Cariben)",
      herkomst7 == "Nederlands-Caribisch gebied", "Buiten Europa (Tur., Mar., Sur., Ind., NL Cariben)",
      herkomst7 == "Turkije", "Buiten Europa (Tur., Mar., Sur., Ind., NL Cariben)",
      herkomst7 == "Nederland", "Nederland"
    )]
    
    stap[, belanginkbronpers := as.character(belanginkbronpers)]
    stap[belanginkbronpers == "Behoort tot huishouden zonder waargenomen inkomen/niet in populatie stapelingsmonitor (31-12-JJJJ)",
          belanginkbronpers := "Niet Waargenomen"]
    stap[, wmo := factor(wmo, levels = c(0,1), labels = c("geen wmo", "wmo"))]
    
    dt <- rbindlist(list(dt, stap), use.names = T)
  }
  return(dt)
}

demog_dt <- demog(years)
setindex(demog_dt, NULL)
arrow::write_parquet(demog_dt, "H:/_Current_projects/griffins/data/tijn/demog.parquet")

#demog <- r_parquet_get_dt("H:/_Current_projects/griffins/data/tijn/demog.parquet")


#### Medicijngebruik ####
med_gebruik <- function(years){
  keep <- c("rinpersoon", "diabetes_combi", "astma_copd", 
            "hypertensie_smal", "cholesterol")
  dt <- data.table()
  for (yr in years){
    aan_path <- paste0("H:/data/aandoeningen/", yr, "/aandoeningen.parquet")
    aan_dt <- r_parquet_get_dt(aan_path)
    aan_dt[, rinpersoon := as.numeric(rinpersoon)]
    aan_dt <- aan_dt[, ..keep]
    aan_dt[, hyper_chol_combi := hypertensie_smal + cholesterol]
    aan_dt[, c("hypertensie_smal", "cholesterol") := NULL]
    aan_dt <- aan_dt[rowSums(aan_dt == 1) > 0]
    
    
    path <- get_path_newest(
      file.path("G:/GezondheidWelzijn/MEDICIJNTAB", 
                yr), 
      string_pattern=yr,
      extension=".csv")
    print(path)
    
    # Do immuno separately because we use different ATC4 codes than aandoeningen uses
    med_dt <- fread(path, select = med_vars)
    med_dt <- format_data(med_dt)
    med_dt <- med_dt[atc4 %chin% immuno]
    med_dt[, immuno := 1]
    
    med_dt <- merge(med_dt, aan_dt, by = "rinpersoon", all = T)
    
    # Save 1 row per person
    per_person <- med_dt[, lapply(.SD, max),
                         by = .(rinpersoon),
                         .SDcols = c("immuno", "diabetes_combi",
                                     "astma_copd", "hyper_chol_combi")]
    setnafill(per_person, fill = 0)
    per_person[, comorbiditeit := 1]
    per_person[, file_year := yr]
    
    dt <- rbindlist(list(dt, per_person), use.names = T)
  }
  return(dt)
}
comorb_dt <- med_gebruik(years)

demog_medicijn <- merge(demog_dt, comorb_dt, all.x = T, by = c("rinpersoon", "file_year"))
setnafill(demog_medicijn, fill = 0, cols = c("immuno", "diabetes_combi", "astma_copd", 
                                             "hyper_chol_combi", "comorbiditeit"))
setindex(demog_medicijn, NULL)
arrow::write_parquet(demog_medicijn, "H:/_Current_projects/griffins/data/tijn/demog_medicijn.parquet")

#setindex(comorb_dt, NULL)
#arrow::write_parquet(comorb_dt, "H:/_Current_projects/griffins/data/tijn/medicijnen.parquet")

# comorbiditeit per age
totals <- demog_medicijn[, .N, by = leeftijd_cat]
med_per_age <- demog_medicijn[comorbiditeit == 1, .N, by = leeftijd_cat][totals, on = "leeftijd_cat"][, pct := N / i.N * 100]
med_per_age


#### Admissions ####
covid_admissions <- function(years){
  
  dt <- data.table()
  for (yr in years) {
    path <- get_path_newest(
      file.path("G:/GezondheidWelzijn/LBZBASISTAB", 
                yr), 
      string_pattern=yr,
      extension=".csv")
    print(path)
    
    lbz_dt <- fread(path, select = lbz_vars)
    
    lbz_dt <- format_data(lbz_dt)
    
    setnames(lbz_dt, "lbzicd10hoofddiagnose", "icd10")
    
    lbz_dt <- lbz_dt[icd10 %chin% c("U071", "U072")]
    
    lbz_dt[, ic := fifelse(lbzicaantaldagen > 0, 1, 0)]
    
    lbz_dt[, lbzopnamedatum := as.Date(as.character(lbzopnamedatum), 
                                       format = "%Y%m%d")]
    lbz_dt[, lbzontslagdatum := as.Date(as.character(lbzontslagdatum), 
                                       format = "%Y%m%d")]
    lbz_dt[, aantal_dagen := as.numeric(lbzontslagdatum- lbzopnamedatum)]
    
    lbz_dt[, year := lubridate::year(lbzopnamedatum)]
    lbz_dt[, week := lubridate::week(lbzopnamedatum)]
    
    # Save max 1 hospitalization per week
    lbz_dt <- unique(lbz_dt, by = c("rinpersoon", "year", "week"))
    
    lbz_dt[, wave := fcase(
      year == 2020 & week < 19, "wave1",
      year == 2020 & week < 39, "iw1",
      year == 2020 & week > 38, "wave2",
      year == 2021 & week < 4, "wave2",
      year == 2021 & week > 3 & week < 33, "iw2",
      year == 2021 & week > 32, "wave3"
    )]
    
    # This can be different from "year"
    lbz_dt[, file_year := yr]
    
    dt <- rbindlist(list(dt, lbz_dt), use.names = T)
  }
  return(dt)
}

adm <- covid_admissions(years)

#demog_medicijn <- r_parquet_get_dt("H:/_Current_projects/griffins/data/tijn/demog_medicijn.parquet")
# Merge demog to hospitalisations and write
admissions_demog <- merge(adm, demog_medicijn, all = F, by = c("rinpersoon", "file_year"))
setindex(admissions_demog, NULL)
arrow::write_parquet(admissions_demog, "H:/_Current_projects/griffins/data/tijn/hospitalisations_demog.parquet")

admissions_demog <- r_parquet_get_dt("data/tijn/hospitalisations_demog.parquet")


#### DEATHS ####

death_file <- "G:/Bevolking/GBAOVERLIJDENTAB/2023/GBAOVERLIJDEN2023TABV1.csv"
deaths <- fread(death_file, select = c("RINPERSOON", "GBADatumOverlijden"))
deaths <- deaths[GBADatumOverlijden >= 20200101 & GBADatumOverlijden < 20220101]
deaths <- format_data(deaths)
deaths[, gbadatumoverlijden := as.Date(as.character(gbadatumoverlijden), format = "%Y%m%d")]
deaths[, file_year := lubridate::year(gbadatumoverlijden)]
deaths[, week := lubridate::week(gbadatumoverlijden)]
deaths[, wave := fcase(
  file_year == 2020 & week < 19, "wave1",
  file_year == 2020 & week < 39, "iw1",
  file_year == 2020 & week > 38, "wave2",
  file_year == 2021 & week < 4, "wave2",
  file_year == 2021 & week > 3 & week < 33, "iw2",
  file_year == 2021 & week > 32, "wave3"
)]

# Combine death causes per year to 1 dt
combine_yearly_death_causes <- function(years) {
  
  dt <- data.table()
  for (yr in years) {
    path <- get_path_newest(
      file.path("G:/GezondheidWelzijn/DOODOORZTAB", 
                yr), 
      string_pattern=yr,
      extension=".csv")
    
    death_causes_dt <- fread(path, select = c("RINPERSOON", "UCCODE"))
    
    death_causes_dt <- format_data(death_causes_dt)
    death_causes_dt <- death_causes_dt[uccode %chin% c(covid_code, presumed_covid_code)]
    
    dt <- rbindlist(list(dt, death_causes_dt), use.names = T)
    
  }
  return(dt)
}

death_causes <- combine_yearly_death_causes(years)

deaths <- merge(deaths, death_causes, all=F)

deaths_demog <- merge(deaths, demog_medicijn, all = F, by = c("rinpersoon", "file_year"))
deaths_demog[, year := file_year]
                     
setindex(deaths_demog, NULL)
arrow::write_parquet(deaths_demog, "H:/_Current_projects/griffins/data/tijn/deaths_demog.parquet")

deaths_demog <- r_parquet_get_dt("data/tijn/deaths_demog.parquet")

# Combine admissions and deaths
setnames(admissions_demog, "lbzopnamedatum", "datum")
setnames(deaths_demog, "gbadatumoverlijden", "datum")

admissions_demog[, type := "opname"]
deaths_demog[, type := "sterfte"]

escalaties <- rbindlist(list(
  admissions_demog, deaths_demog), fill = T)
escalaties[, c("ic", "icd10", "lbzicopnamedag", "lbzicaantaldagen", "lbzontslagdatum", "uccode") := NULL]

overlijdens <- escalaties[type == "sterfte", .(rinpersoon, datum_overlijden = datum)]
escalaties <- merge(escalaties, overlijdens, by = "rinpersoon", all.x = T)

# Drop opnames that occur less than a month before a death because its the same 
# escalation
escalaties <- escalaties[
  !(type == "opname" &
      !is.na(datum_overlijden) &
      datum >= datum_overlijden - 30 &
      datum <= datum_overlijden)
]

escalaties[, datum_overlijden := NULL]

setindex(escalaties, NULL)
arrow::write_parquet(escalaties, "H:/_Current_projects/griffins/data/tijn/escalaties.parquet")


#### Covid tests ####
covid_tests_file_20 <- "G:/GezondheidWelzijn/GGDCOVID19BM/geconverteerde data/HPZonedata_2020V1.csv"
covid_tests_file_21 <- "G:/GezondheidWelzijn/GGDCOVID19BM/geconverteerde data/HPZonedata_2021V1.csv"
test_files <- list(covid_tests_file_20, covid_tests_file_21)

covid_tests <- function(files) {
  
  dt <- data.table()
  for (f in files) {
    covid_dt <- format_data(fread(f))
    covid_dt <- covid_dt[typeuitslagcovid19test %in% c(1, 2)]
    #covid_dt <- covid_dt[rinpersoon != 0 & !is.na(rinpersoon)]
    covid_dt[, datum_besmetting := as.Date(tijdstiprapportagecovid19besmetting)]
    covid_dt[, year := lubridate::year(datum_besmetting)]
    covid_dt[, week := lubridate::week(datum_besmetting)]
    #covid_dt[, datum_test := as.Date(as.character(datumcovid19testafname), format = "%Y%m%d")]
    covid_dt[, c("tijdstiprapportagecovid19besmetting", "rinpersoons", "datumcovid19testafname") := NULL]
    #covid_dt[, dagen_tussen_besm_test := as.integer(datum_besmetting - datum_test)]
    
    # Save max 1 positive test per week
    covid_dt <- unique(covid_dt, by = c("rinpersoon", "year", "week"))
    
    covid_dt[, wave := fcase(
      year == 2020 & week < 19, "wave1",
      year == 2020 & week < 39, "iw1",
      year == 2020 & week > 38, "wave2",
      year == 2021 & week < 4, "wave2",
      year == 2021 & week > 3 & week < 33, "iw2",
      year == 2021 & week > 32, "wave3"
    )]
    
    if (f == covid_tests_file_20) {
      covid_dt[, file_year := 2020]
    }
    else {
      covid_dt[, file_year := 2021]
    }
    
    dt <- rbindlist(list(dt, covid_dt), use.names = T)
  }
  return(dt)
}

test_dt <- covid_tests(test_files)

# Merge demog to hospitalisations and write
tests_demog <- merge(test_dt, demog_medicijn, all = F, by = c("rinpersoon", "file_year"))
setindex(tests_demog, NULL)
arrow::write_parquet(tests_demog, "H:/_Current_projects/griffins/data/tijn/positive_tests_demog.parquet")

#test_dt1 <- r_parquet_get_dt("H:/_Current_projects/griffins/data/tijn/positive_tests_demog.parquet")


# voeg variabele toe die checkt of persoon maand voor escalatie een positieve test had
tests_demog <- r_parquet_get_dt("H:/_Current_projects/griffins/data/tijn/positive_tests_demog.parquet")
escalaties <- r_parquet_get_dt("H:/_Current_projects/griffins/data/tijn/escalaties.parquet")

escalaties[, datum_min := datum - 30]

escalaties[, positief_getest := as.integer(
  tests_demog[escalaties,
     on = .(rinpersoon, datum_besmetting >= datum_min, datum_besmetting <= datum),
     .N > 0,
     by = .EACHI]$V1
)]

setindex(escalaties, NULL)
arrow::write_parquet(escalaties, "H:/_Current_projects/griffins/data/tijn/escalaties_pos_test.parquet")








#### COMPUTE RATES ####


# Count (IC) hospitalisations per year per week

# First compute unique hospitalisations per week per person
#agg1 <- adm[, .(N_hosp = .N, ic = sum(ic)), by = .(rinpersoon, year, week)][order(rinpersoon,year,week)]

# Compute hospitalisations per week
adm_agg <- adm[, .(N_hosp = .N, N_hosp_ic = sum(ic)), by = .(year, week)][order(year,week)]

#adm_agg1 <- agg1[, .(N_hosp = .N, N_hosp_ic = N_hosp_ic), by = (year,week)]


covid_outcomes <- merge(covid_outcomes, test_dt, all = F, by = c("rinpersoon", "year"))

# Count tests per year/week
test_agg <- test_dt[, .(N_tests = .N), by = .(year, week)][order(year,week)]


#### Merge and compute rates ####

# Merge tests and hospitalisations
covid_outcomes <- merge(adm_agg, test_agg, all = F, by = c("year", "week"))

# Add lagged periods of tests for easy compare
# covid_outcomes[, test_lag_sum := frollsum(shift(N_tests,1), n=2, align="right", fill = NA)]
# covid_outcomes[, test_lag_mean := frollmean(shift(N_tests,1), n=2, align="right", fill = NA)]
covid_outcomes[, N_tests_lag1 := shift(N_tests, n=1)]
covid_outcomes[, N_tests_lag2 := shift(N_tests, n=2)]

# Week/year var
covid_outcomes[, weekyear := paste(substr(year, 3,4),"-W",sprintf("%02d",week))]

# Add population per month (now simple, just population size at start of the year for every month)
covid_outcomes[, population := fifelse(year == 2020, aantal_inwoners_2020, aantal_inwoners_2021)]

# Compute rates
covid_outcomes[, `:=`(
  rate_hosp = N_hosp / population,
  # rate_hosp_ic = N_hosp_ic / population,
  rate_tests = N_tests / population,
  # rate_tests_lag_sum = test_lag_sum / population,
  # rate_tests_lag_mean = test_lag_mean / population,
  rate_tests_lag1 = N_tests_lag1 / population,
  rate_tests_lag2 = N_tests_lag2 / population
)]

#make_quantile_var <- function(dt, )
  
covid_outcomes[, rate_hosp_percentile := as.integer(cut(
  rate_hosp,
  breaks = quantile(rate_hosp, probs = 0:10/10, na.rm=T),
  include.lowest = T,
  labels = 1:10
))]

covid_outcomes[, rate_tests_lag1_percentile := as.integer(cut(
  rate_tests_lag1,
  breaks = quantile(rate_tests_lag1, probs = 0:10/10, na.rm=T),
  include.lowest = T,
  labels = 1:10
))]


# Standardised rates
covid_outcomes[, `:=`(
  st_rate_hosp = scale(rate_hosp),
  st_rate_tests_lag1 = scale(rate_tests_lag1)
)]

# Compute deltas
covid_outcomes[, `:=`(
  # D_hosp_test_sum = rate_tests_lag_sum - rate_hosp,
  # D_hosp_test_mean = rate_tests_lag_mean - rate_hosp,
  D_hosp_test = rate_tests_lag1 - rate_hosp
  # D_hosp_ic_test_sum = rate_tests_lag_sum - rate_hosp_ic,
  # D_hosp_ic_test_mean = rate_tests_lag_mean - rate_hosp_ic,
  # D_hosp_ic_test = rate_tests_lag - rate_hosp_ic
)]

covid_outcomes[, `:=`(
  D_hosp_test_percentile = rate_tests_lag1_percentile - rate_hosp_percentile,
  D_hosp_test_st = st_rate_tests_lag1 - st_rate_hosp
)]

 
# Plot hospitalisation rate / test rate
p <- ggplot(covid_outcomes, aes(weekyear, D_hosp_test_st))+
  geom_line(group = 1) +
  geom_point() +
  scale_x_discrete(breaks = covid_outcomes$weekyear[seq(1, nrow(covid_outcomes), by=4)]) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme_minimal()
p

ggsave(filename = "H:/recovac/data/griffins/standardised.png",
       plot = p,
       width = 14,
       height = 5,
       dpi = 300)


p <- ggplot(covid_outcomes[1:84], aes(weekyear, rate_hosp/ rate_tests_lag1)) + 
  geom_line(group = 1) +
  geom_point() +
  scale_x_discrete(breaks = covid_outcomes$weekyear[seq(1, nrow(covid_outcomes), by=4)]) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme_minimal()
p

# Plot Delta rate percentiles
p <- ggplot(covid_outcomes, aes(weekyear, D_hosp_test_percentile))+
  geom_line(group=1) +
  geom_point() +
  scale_y_continuous(n.breaks = 10) +
  scale_x_discrete(breaks = covid_outcomes$weekyear[seq(1, nrow(covid_outcomes), by=4)]) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(
    x = "week",
    y = "Delta rate percentile") +
  theme_minimal() +
  theme_bw()
p

ggsave(filename = "H:/recovac/data/griffins/deiles.png",
       plot = p,
       width = 14,
       height = 5,
       dpi = 300)



# Plot Delta rate percentiles, hospitalisation rate and test rate
p <- ggplot(covid_outcomes, aes(weekyear)) +
  geom_line(aes(y = D_hosp_test_percentile, color = "Delta rate p(test-hosp)"), linewidth = 1, group=1) +
  geom_line(aes(y = rate_tests_lag1_percentile, color = "test rate p"), linewidth = 1, alpha=0.5, group=1) +
  geom_line(aes(y = rate_hosp_percentile, color = "hosp rate p"), linewidth = 1,alpha=0.5, group=1) +
  geom_point(aes(y = D_hosp_test_percentile), size = 1.5) + 
  scale_y_continuous(n.breaks = 10) +
  scale_x_discrete(breaks = covid_outcomes$weekyear[seq(1, nrow(covid_outcomes), by=4)]) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(
    x = "week",
    y = "Delta rate decile") +
  scale_color_manual(
    name = "Rate",
    values = c(
      "Delta rate p(test-hosp)" = "black",
      "test rate p" = "blue",
      "hosp rate p" = "red")) +
  theme_minimal() +
  theme_bw()

ggsave(filename = "H:/recovac/data/griffins/deciles all.png",
       plot = p,
       width = 14,
       height = 5,
       dpi = 300)



