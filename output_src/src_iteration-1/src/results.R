# Save results
source("H:/utils/m_functions.R")
source("H:/utils/demog_functions.R")
source("src/tijn/analysis_functions.R")
library(data.table)
library(glue)
library(ggplot2)
library(openxlsx)
demog <- r_parquet_get_dt("data/tijn/demog_medicijn.parquet")
#hosps <- r_parquet_get_dt("data/tijn/hospitalisations_demog.parquet")
#deaths <- r_parquet_get_dt("data/tijn/deaths_demog.parquet")
escalaties <- r_parquet_get_dt("data/tijn/escalaties_pos_test.parquet")

tests <- r_parquet_get_dt("data/tijn/positive_tests_demog.parquet")
demog[, totaal := "Amsterdam"]
hosps[, totaal := "Amsterdam"]
deaths[, totaal := "Amsterdam"]
tests[, totaal := "Amsterdam"]

subgroups <- c("herkomstland", "herkomst7", "inkomen_klasse_small", "seswoa_cat",
               "seswoa_small", "wmo", "wijk22", "stadsdeel", "comorbiditeit", 
               "hh_group", "hh_kind_group", "leeftijd_3", "geslacht",
               "hh_kind_0_3", "hh_kind_4_11", "hh_kind_12_18")

std_var_combis <- list(
  c("leeftijd_3"),
  c("geslacht"),
  c("leeftijd_3", "geslacht"),
  c("leeftijd_3", "comorbiditeit"),
  c("leeftijd_3", "geslacht", "comorbiditeit")
)

#### Test plotting ####
# res <- compute_rates(demog, tests, hosps, deaths, 
#                      subgroup = "wijk22", 
#                      time_unit = "wave",
#                      std_vars = c("leeftijd_3", "geslacht"))

res <- compute_escalatie_rates(demog, tests, escalaties, 
                     subgroup = "inkomen_klasse_small",
                     std_vars = c("leeftijd_3", "geslacht"))


res_no_waves <- compute_escalatie_rates_no_waves(demog, tests, escalaties, 
                                        subgroup = "wijk22",
                                        std_vars = c("leeftijd_3", "geslacht"))

p <- plot_ratio_wave(dt = res[wave != "wave1" & inkomen_klasse_small != "onbekend_inst_student"],
                     subgroup = "inkomen_klasse_small",
                     ratio_col = "ratio_test_esc",
                     crude_ratio_col = "crude_ratio_test_esc",
                     top_n = 10)
p


p <- plot_scatter_wave(res[wave != "wave1" & 
                             inkomen_klasse_small != "onbekend_inst_student"], 
                       "inkomen_klasse_small",
                       x_col = "std_rate_test", y_col = "std_rate_death")
p


escalaties[, .(escalaties = .N, positief_getest = sum(positief_getest)), by = wave]


#indelingen <- list(
#   "8 groepen" = "leeftijd_8",
#   "5 groepen" = "leeftijd_5",
#   "4 groepen" = "leeftijd_4",
#   "3 groepen" = "leeftijd_3"
# )
# 
# resultaten <- rbindlist(lapply(names(indelingen), function(naam) {
#   res <- compute_rates(demog, tests, hosps, deaths, 
#                        subgroup = "inkomen_klasse_small", 
#                        time_unit = "wave",
#                        std_vars = c(indelingen[[naam]], "geslacht", "herkomstland"))
#   res[, indeling := naam]
#   res
# }), fill = T)
# 
# resultaten[, wave := factor(wave, levels = c("wave1", "iw1", "wave2", "iw2", "wave3"))]
# 
# p <- ggplot(resultaten[wave != "wave1" & inkomen_klasse_small != "onbekend_inst_student"], 
#        aes(x = wave, y = std_rate_hosp, color = indeling,
#                        group = indeling)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap( ~inkomen_klasse_small) +
#   theme_bw() #+
#   #labs(color = get(inkomen_klasse_small))
# p
# 
# ggsave("H:/_Current_projects/griffins/data/tijn/plots/leeftijd_cat/inkomen_hosp_rate_leeftijd_geslacht_herkomstland.png",
#        p,
#        width = 10,
#        height = 5,
#        dpi = 300)


#### SAVE DATA ####

out_vars_pos <- c("n_esc_tested_total", "n_esc_not_tested", 
                  "std_rate_esc_tested","crude_pct_tested",
                  "std_detectie_esc", "std_detectie_pop")
  
out_vars <- c("std_rate_test", "std_rate_esc", "crude_rate_test", "pop_total",
              "n_tests_total", "crude_rate_esc", "n_esc_total", "n_esc_tested_total",
              "n_esc_not_tested","ratio_esc_test", "crude_ratio_esc_test", 
              "ratio_test_esc", "crude_ratio_test_esc",  "std_rate_esc_tested",
              "crude_pct_tested", "std_detectie_esc", "std_detectie_pop")
round_vars <- c("pop_total", "n_tests_total", "n_esc_total")

round_vars_pos <- c("n_esc_tested_total", "n_esc_not_tested")

for (sv in std_var_combis) {
  print(sv)
  wb <- createWorkbook()
  
  for (sg in subgroups) {
    # skip if subgroup in std_vars
    if (sg %in% sv) next
    
    res <- compute_escalatie_rates(demog, tests, escalaties, 
                                   subgroup = sg,
                                   std_vars = sv)
    res <- res[wave != "wave1"]
    if (sg == "wijk22") {
      res <- res[!is.na(wijk22)]
      res[wijk22 == "buiten beschouwing", (out_vars) := NA]
      
    }
    if (sg == "stadsdeel") {
      res <- res[!is.na(stadsdeel)]
      res[stadsdeel == "Westpoort", (out_vars) := NA]
    }
    
    # check if any round_vars are < 10, if so -> make whole row NA
    res[rowSums(res[, .SD < 10, .SDcols = round_vars]) > 0, (out_vars) := NA]
    res[rowSums(res[, .SD < 10, .SDcols = round_vars_pos]) > 0, (out_vars_pos) := NA]
    
    # round to fives
    res[, (round_vars) := lapply(.SD, function(x) round(x / 5) * 5), .SDcols = round_vars]
    res[, (round_vars_pos) := lapply(.SD, function(x) round(x / 5) * 5), .SDcols = round_vars_pos]
    
    addWorksheet(wb, sg)
    writeData(wb, sg, res)
  }
  
  resw22 <- compute_escalatie_rates_no_waves(demog, tests, escalaties,
                                             subgroup = "wijk22",
                                             std_vars = sv)
  resw22 <- resw22[!is.na(wijk22)]
  resw22[wijk22 == "buiten beschouwing", (out_vars) := NA]
  
  # check if any round_vars are < 10, if so -> make NA
  resw22[rowSums(resw22[, .SD < 10, .SDcols = round_vars]) > 0, (out_vars) := NA]
  resw22[rowSums(resw22[, .SD < 10, .SDcols = round_vars_pos]) > 0, (out_vars_pos) := NA]
  
  # round to fives
  resw22[, (round_vars) := lapply(.SD, function(x) round(x / 5) * 5), .SDcols = round_vars]
  resw22[, (round_vars_pos) := lapply(.SD, function(x) round(x / 5) * 5), .SDcols = round_vars_pos]
  
  addWorksheet(wb, "no waves wijk22")
  writeData(wb, "no waves wijk22", resw22)
  
  res_stadsdeel <- compute_escalatie_rates_no_waves(demog, tests, escalaties,
                                             subgroup = "stadsdeel",
                                             std_vars = sv)
  res_stadsdeel <- res_stadsdeel[!is.na(stadsdeel)]
  res_stadsdeel[stadsdeel == "Westpoort", (out_vars) := NA]
  
  # check if any round_vars are < 10, if so -> make NA
  res_stadsdeel[rowSums(res_stadsdeel[, .SD < 10, .SDcols = round_vars]) > 0, (out_vars) := NA]
  res_stadsdeel[rowSums(res_stadsdeel[, .SD < 10, .SDcols = round_vars_pos]) > 0, (out_vars_pos) := NA]
  
  # round to fives
  res_stadsdeel[, (round_vars) := lapply(.SD, function(x) round(x / 5) * 5), .SDcols = round_vars]
  res_stadsdeel[, (round_vars_pos) := lapply(.SD, function(x) round(x / 5) * 5), .SDcols = round_vars_pos]
  
  addWorksheet(wb, "no waves stadsdeel")
  writeData(wb, "no waves stadsdeel", res_stadsdeel)
  
  output_map <- "H:/_Current_projects/griffins/data/tijn/results"
  file_name <- file.path(output_map, paste0("rates_ratios_metN_pos_test_", paste(sv, collapse = "_"), ".xlsx"))
  saveWorkbook(wb, file_name, overwrite = T)
  
}
