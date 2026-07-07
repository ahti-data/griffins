#### Standard Population ####
# standard_pop <- demog[, .N, by = .(file_year, leeftijd_cat, geslacht)]
# standard_pop[, total_year := sum(N), by = file_year]
# standard_pop[, weight := N / total_year]

add_file_year <- function(dt, time_unit) {
  # Just used to overwrite file year for wave 2 to 1 file year
  if (time_unit == "week") {
    dt[, file_year := year]
  } else {
    wave_to_year <- data.table(
      wave = c("wave1", "iw1", "wave2", "iw2", "wave3"),
      file_year = c(2020,2020,2020,2021,2021)
    )
    dt <- merge(dt, wave_to_year, by = "wave", all.x =T)
  }
  return(dt)
}

compute_rates <- function(demog, tests, hosps, deaths, subgroup, 
                          time_unit = "wave",
                          std_vars = c("leeftijd_cat", "geslacht")) {
  # Compute standardized and crude hospitalization, death and test rates and ratios
  
  # Compute weights of each profile in the population
  standard_pop <- demog[, .N, by = c("file_year", std_vars)]
  standard_pop[, weight := N / sum(N), by = file_year]
  
  if (time_unit == "week") {
    time_cols <- c("year", "week")
  } else {
    time_cols <- "wave"
  }
  
  group_cols_event <- c(time_cols, subgroup, std_vars)
  join_cols_denom <- c("file_year", subgroup, std_vars)
  group_cols_final <- c(time_cols, subgroup)
  
  # Populuation per year, subgroup std_vars combinatie
  denom <- demog[, .(pop = .N), by = join_cols_denom]
  
  # Events per time unit
  numerator_tests <- tests[, .(n_tests = .N), by= group_cols_event]
  numerator_hosp <- hosps[, .(n_hosps = .N), by = group_cols_event]
  numerator_death <- deaths[, .(n_deaths = .N), by = group_cols_event]
  # print(numerator_tests[order(n_tests, wave)], nrow = Inf)
  # print(numerator_death[order(n_deaths, wave)], nrow=Inf)
  
  numerator_tests <- add_file_year(numerator_tests, time_unit = time_unit)
  numerator_hosp <- add_file_year(numerator_hosp, time_unit = time_unit)
  numerator_death <- add_file_year(numerator_death, time_unit = time_unit)
  
  # Merge demog with tests, hospitalisation and deaths to get population per stratification/time
  # combination and compute rates 
  numerator_tests <- merge(numerator_tests, denom, by = join_cols_denom, all.x=T)
  numerator_tests[, rate := n_tests / pop]
  
  numerator_hosp <- merge(numerator_hosp, denom, by = join_cols_denom, all.x=T)
  numerator_hosp[, rate := n_hosps / pop]
  
  numerator_death <- merge(numerator_death, denom, by = join_cols_denom, all.x=T)
  numerator_death[, rate := n_deaths / pop]
  
  # Add weights computed in standard pop to stratifications
  weight_cols <- c("file_year", std_vars, "weight")
  numerator_tests <- merge(numerator_tests, standard_pop[, ..weight_cols],
                           by = c("file_year", std_vars), all.x=T)
  numerator_hosp <- merge(numerator_hosp, standard_pop[, ..weight_cols],
                          by = c("file_year", std_vars), all.x=T)
  numerator_death <- merge(numerator_death, standard_pop[, ..weight_cols],
                          by = c("file_year", std_vars), all.x=T)
  
  
  # Standardised rates
  std_test <- numerator_tests[, .(std_rate_test = sum(rate * weight, na.rm=T)),
                                by = group_cols_final]
  std_hosp <- numerator_hosp[, .(std_rate_hosp = sum(rate * weight, na.rm=T)),
                                by = group_cols_final]
  std_death <- numerator_death[, .(std_rate_death = sum(rate * weight, na.rm=T)),
                             by = group_cols_final]

  
  # Crude (non-standardized) rates. Compute this from total population to prevent
  # not including stratifications with 0 tests/hosps/deaths for total N of people in denominator
  
  denom_total <- denom[, .(pop_total = sum(pop)), by = c("file_year", subgroup)]
  
  crude_numerator_test <- tests[, .(n_tests_total = .N),
                                by = c(time_cols, subgroup)]
  crude_numerator_hosp <- hosps[, .(n_hosp_total = .N),
                                by = c(time_cols, subgroup)]
  crude_numerator_death <- deaths[, .(n_death_total = .N),
                                by = c(time_cols, subgroup)]
  
  crude_numerator_test <- add_file_year(crude_numerator_test, time_unit = time_unit)
  crude_numerator_hosp <- add_file_year(crude_numerator_hosp, time_unit = time_unit)
  crude_numerator_death <- add_file_year(crude_numerator_death, time_unit = time_unit)
  
  # Merge with complete pop
  crude_test <- merge(crude_numerator_test, denom_total, 
                      by = c("file_year", subgroup),
                      all.x = T)
  crude_test[, crude_rate_test := n_tests_total / pop_total]
  crude_test <- crude_test[, c(group_cols_final, "crude_rate_test"), with = F]
  
  crude_hosp <- merge(crude_numerator_hosp, denom_total, 
                      by = c("file_year", subgroup),
                      all.x = T)
  crude_hosp[, crude_rate_hosp := n_hosp_total / pop_total]
  crude_hosp <- crude_hosp[, c(group_cols_final, "crude_rate_hosp"), with = F]
  
  crude_death <- merge(crude_numerator_death, denom_total, 
                      by = c("file_year", subgroup),
                      all.x = T)
  crude_death[, crude_rate_death := n_death_total / pop_total]
  crude_death <- crude_death[, c(group_cols_final, "crude_rate_death"), with = F]
  
  # Merge everything
  res <- merge(std_test, std_hosp, by = group_cols_final, all = T)
  res <- merge(res, std_death, by = group_cols_final, all = T)
  res <- merge(res, crude_test, by = group_cols_final, all.x=T)
  res <- merge(res, crude_hosp, by = group_cols_final, all.x=T)
  res <- merge(res, crude_death, by = group_cols_final, all.x=T)
  
  rate_cols <- c("std_rate_test", "std_rate_hosp", "std_rate_death",
                 "crude_rate_test", "crude_rate_hosp", "crude_rate_death")
  for (col in rate_cols) {
    set(res, which(is.na(res[[col]])), col, 0)
  }
  #setnafill(res, fill = 0, rate_cols)
  
  res[, ratio_hosp_test := fifelse(std_rate_test > 0,  std_rate_hosp / std_rate_test, NA_real_)]
  res[, crude_ratio_hosp_test := fifelse(crude_rate_test > 0, crude_rate_hosp / crude_rate_test, NA_real_)]
  res[, ratio_test_hosp := fifelse(std_rate_hosp > 0,  std_rate_test / std_rate_hosp, NA_real_)]
  res[, crude_ratio_test_hosp := fifelse(crude_rate_hosp > 0, crude_rate_test / crude_rate_hosp, NA_real_)]
  
  res[, ratio_death_test := fifelse(std_rate_test > 0,  std_rate_death / std_rate_test, NA_real_)]
  res[, crude_ratio_death_test := fifelse(crude_rate_test > 0, crude_rate_death / crude_rate_test, NA_real_)]
  res[, ratio_test_death := fifelse(std_rate_death > 0,  std_rate_test / std_rate_death, NA_real_)]
  res[, crude_ratio_test_death := fifelse(crude_rate_death > 0, crude_rate_test / crude_rate_death, NA_real_)]
  # res[, ratio := fifelse(std_rate_test > 0,  std_rate_test / std_rate_hosp, NA)]
  # res[, crude_ratio := fifelse(crude_rate_test > 0, crude_rate_test / crude_rate_hosp, NA)]
  res[, subgroup_type := subgroup]
  
  return(res)
}


compute_escalatie_rates <- function(demog, tests, escalaties, subgroup,
                                    std_vars = c("leeftijd_cat", "geslacht")) {
  # Compute standardized and crude hospitalization, death and test rates and ratios
  
  group_cols_event <- c("wave", subgroup, std_vars)
  join_cols_denom <- c("file_year", subgroup, std_vars)
  group_cols_final <- c("wave", subgroup)
  
  # Compute weights of each profile in the population
  standard_pop <- demog[, .N, by = c("file_year", std_vars)]
  standard_pop[, weight := N / sum(N), by = file_year]
  
  # Populuation per year, subgroup std_vars combinatie
  denom <- demog[, .(pop = .N), by = join_cols_denom]
  
  wave_to_year <- data.table(
    wave = c("wave1", "iw1", "wave2", "iw2", "wave3"),
    file_year = c(2020, 2020, 2020, 2021, 2021)
  )
  
  # Events per time unit
  numerator_tests <- tests[, .(n_tests = .N), by= group_cols_event]
  numerator_esc <- escalaties[, .(n_escalaties = .N), by = group_cols_event]
  numerator_esc_tested <- escalaties[positief_getest == 1, .(n_esc_tested = .N), by = group_cols_event]
  # print(numerator_tests[order(n_tests, wave)], nrow = Inf)
  # print(numerator_esc[order(n_escalaties, wave)], nrow=Inf)
  
  numerator_tests <- merge(numerator_tests, wave_to_year, all.x=T)
  numerator_esc <- merge(numerator_esc, wave_to_year, all.x=T)
  numerator_esc_tested <- merge(numerator_esc_tested, wave_to_year, all.x = T)
  
  # Merge demog with tests, hospitalisation and deaths to get population per stratification/time
  # combination and compute rates 
  numerator_tests <- merge(numerator_tests, denom, by = join_cols_denom, all.x=T)
  numerator_tests[, rate := n_tests / pop]
  
  numerator_esc <- merge(numerator_esc, denom, by = join_cols_denom, all.x=T)
  numerator_esc[, rate := n_escalaties / pop]
  
  numerator_esc_tested <- merge(numerator_esc_tested, denom, by = join_cols_denom,
                                all.x = T)
  numerator_esc_tested[, rate := n_esc_tested / pop]
  
  #print(numerator_esc[wave != "wave1"][order(n_escalaties)], nrow = Inf)
  # print(numerator_esc[, sum(n_escalaties)])
  
  # Add weights computed in standard pop to stratifications
  weight_cols <- c("file_year", std_vars, "weight")
  numerator_tests <- merge(numerator_tests, standard_pop[, ..weight_cols],
                           by = c("file_year", std_vars), all.x=T)
  numerator_esc <- merge(numerator_esc, standard_pop[, ..weight_cols],
                          by = c("file_year", std_vars), all.x=T)
  numerator_esc_tested <- merge(numerator_esc_tested, standard_pop[, ..weight_cols],
                                by = c("file_year", std_vars), all.x=T)
  
  # Standardised rates
  std_test <- numerator_tests[, .(std_rate_test = sum(rate * weight, na.rm=T)),
                              by = group_cols_final]
  #print(std_test)
  std_esc <- numerator_esc[, .(std_rate_esc = sum(rate * weight, na.rm=T)),
                             by = group_cols_final]
  std_esc_tested <- numerator_esc_tested[, .(std_rate_esc_tested = sum(rate * weight, na.rm=T)),
                                         by = group_cols_final]
  
  # Crude (non-standardized) rates. Compute this from total population to prevent
  # not including stratifications with 0 tests/hosps/deaths for total N of people in denominator
  
  denom_total <- denom[, .(pop_total = sum(pop)), by = c("file_year", subgroup)]
  
  crude_numerator_test <- tests[, .(n_tests_total = .N),
                                by = c("wave", subgroup)]
  crude_numerator_esc <- escalaties[, .(n_esc_total = .N),
                                by = c("wave", subgroup)]
  crude_numerator_esc_tested <- escalaties[positief_getest == 1,
                                           .(n_esc_tested_total = .N),
                                    by = c("wave", subgroup)]
  
  crude_numerator_test <- merge(crude_numerator_test, wave_to_year, by = "wave", all.x=T)
  crude_numerator_esc <- merge(crude_numerator_esc, wave_to_year, by = "wave", all.x=T)
  
  # Merge with complete pop
  crude_test <- merge(crude_numerator_test, denom_total, 
                      by = c("file_year", subgroup),
                      all.x = T)
  crude_test[, crude_rate_test := n_tests_total / pop_total]
  crude_test <- crude_test[, c(group_cols_final, "crude_rate_test", "pop_total",
                               "n_tests_total"), with = F]
  
  crude_esc <- merge(crude_numerator_esc, denom_total, 
                      by = c("file_year", subgroup),
                      all.x = T)
  crude_esc[, crude_rate_esc := n_esc_total / pop_total]
  crude_esc <- crude_esc[, c(group_cols_final, "crude_rate_esc"), 
                         with = F]
  
  # crude detection % = escalaties met test / alle escalaties
  crude_detection <- merge(crude_numerator_esc[, c(group_cols_final, "n_esc_total"), with = F],
                           crude_numerator_esc_tested,by = group_cols_final, all.x=T)
  crude_detection[is.na(n_esc_tested_total), n_esc_tested := 0]
  crude_detection[, crude_pct_tested := fifelse(n_esc_total > 0,
                                                n_esc_tested_total / n_esc_total, NA_real_)]
  crude_detection <- crude_detection[, c(group_cols_final, "n_esc_total", 
                                         "n_esc_tested_total", "crude_pct_tested"),
                                     with = F]
  
  # direct gestandaardiseerde proportie escalatie
  esc_standard <- escalaties[, .N, by = c("wave", std_vars)]
  esc_standard <- merge(esc_standard, wave_to_year, by = "wave", all.x = T)
  esc_standard <- esc_standard[, .(N = sum(N)), by = c("file_year", std_vars)]
  esc_standard[, esc_weight := N / sum(N), by = file_year]
  
  det_stratum <- merge(
    numerator_esc[, c(group_cols_event, "file_year", "n_escalaties"), with = F],
    numerator_esc_tested[, c(group_cols_event, "n_esc_tested"), with = F],
    by = group_cols_event, all.x= T)
  det_stratum[is.na(n_esc_tested), n_esc_tested := 0]
  det_stratum[, p := n_esc_tested / n_escalaties]
  det_stratum <- merge(det_stratum,
                       esc_standard[, c("file_year", std_vars, "esc_weight"), with = F],
                       by = c("file_year", std_vars), all.x = T)
  
  # gewogen gemiddelde gehernormaliseerd over strata die subgroep echt heeft
  std_det_esc <- det_stratum[, .(std_detectie_esc = 
                                   sum(p* esc_weight, na.rm = T) /
                                   sum(esc_weight, na.rm = T)),
                             by = group_cols_final]
  
  
  
  
  
  # Merge everything
  res <- merge(std_test, std_esc, by = group_cols_final, all = T)
  res <- merge(res, std_esc_tested, by = group_cols_final, all.x = T)
  res <- merge(res, crude_test, by = group_cols_final, all.x=T)
  res <- merge(res, crude_esc, by = group_cols_final, all.x=T)
  res <- merge(res, crude_detection, by = group_cols_final, all.x = T)
  res <- merge(res, std_det_esc, by = group_cols_final, all.x = T)
  
  rate_cols <- c("std_rate_test", "std_rate_esc", "std_rate_esc_tested",
                 "crude_rate_test", "crude_rate_esc", "std_detectie_esc")
  for (col in rate_cols) {
    set(res, which(is.na(res[[col]])), col, 0)
  }
  
  #setnafill(res, fill = 0, rate_cols)
  
  res[, ratio_esc_test := fifelse(std_rate_test > 0,  
                                  std_rate_esc / std_rate_test, NA_real_)]
  res[, crude_ratio_esc_test := fifelse(crude_rate_test > 0, 
                                        crude_rate_esc / crude_rate_test, NA_real_)]
  res[, ratio_test_esc := fifelse(std_rate_esc > 0, 
                                  std_rate_test / std_rate_esc, NA_real_)]
  res[, crude_ratio_test_esc := fifelse(crude_rate_esc > 0, 
                                        crude_rate_test / crude_rate_esc, NA_real_)]
  res[, std_detectie_pop := fifelse(std_rate_esc > 0,
                                     std_rate_esc_tested / std_rate_esc, NA_real_)]
  res[, n_esc_not_tested := n_esc_total - n_esc_tested_total]
  
  # res[, ratio := fifelse(std_rate_test > 0,  std_rate_test / std_rate_hosp, NA)]
  # res[, crude_ratio := fifelse(crude_rate_test > 0, crude_rate_test / crude_rate_hosp, NA)]
  res[, subgroup_type := subgroup]
  
  return(res)
}

compute_escalatie_rates_no_waves <- function(demog, tests, escalaties, subgroup,
                                            std_vars = c("leeftijd_cat", "geslacht")) {
  # Compute standardized and crude hospitalization, death and test rates and ratios
  
  group_cols_event <- c(subgroup, std_vars)
  join_cols_denom <- c(subgroup, std_vars)
  group_cols_final <- subgroup
  
  # Compute weights of each profile in the population
  standard_pop <- demog[, .N, by = c(std_vars)]
  standard_pop[, weight := N / sum(N)]
  
  # Populuation per year, subgroup std_vars combinatie
  denom <- demog[, .(pop = .N), by = join_cols_denom]
  
  # Events per time unit
  numerator_tests <- tests[, .(n_tests = .N), by= group_cols_event]
  numerator_esc <- escalaties[, .(n_escalaties = .N), by = group_cols_event]
  numerator_esc_tested <- escalaties[positief_getest == 1, .(n_esc_tested = .N), 
                                     by = group_cols_event]
  
  
  # Merge demog with tests, hospitalisation and deaths to get population per stratification/time
  # combination and compute rates 
  numerator_tests <- merge(numerator_tests, denom, by = join_cols_denom, all.x=T)
  numerator_tests[, rate := n_tests / pop]
  
  numerator_esc <- merge(numerator_esc, denom, by = join_cols_denom, all.x=T)
  numerator_esc[, rate := n_escalaties / pop]
  #print(numerator_esc[order(n_escalaties)][1:50])
  # print(numerator_esc[, sum(n_escalaties)])
  
  numerator_esc_tested <- merge(numerator_esc_tested, denom, by = join_cols_denom, all.x=T)
  numerator_esc_tested[, rate := n_esc_tested / pop]
  
  
  # Add weights computed in standard pop to stratifications
  weight_cols <- c(std_vars, "weight")
  numerator_tests <- merge(numerator_tests, standard_pop[, ..weight_cols],
                           by = std_vars, all.x=T)
  numerator_esc <- merge(numerator_esc, standard_pop[, ..weight_cols],
                         by = std_vars, all.x=T)
  numerator_esc_tested <- merge(numerator_esc_tested, standard_pop[, ..weight_cols],
                         by = std_vars, all.x=T)
  
  # Standardised rates
  std_test <- numerator_tests[, .(std_rate_test = sum(rate * weight, na.rm=T)),
                              by = group_cols_final]
  
  std_esc <- numerator_esc[, .(std_rate_esc = sum(rate * weight, na.rm=T)),
                           by = group_cols_final]
  std_esc_tested <- numerator_esc_tested[, .(std_rate_esc_tested = sum(rate * weight, na.rm=T)),
                           by = group_cols_final]
  
  # Crude (non-standardized) rates. Compute this from total population to prevent
  # not including stratifications with 0 tests/hosps/deaths for total N of people in denominator
  
  denom_total <- denom[, .(pop_total = sum(pop)), by = subgroup]
  
  crude_numerator_test <- tests[, .(n_tests_total = .N), by = subgroup]
  crude_numerator_esc <- escalaties[, .(n_esc_total = .N), by = subgroup]
  crude_numerator_esc_tested <- escalaties[positief_getest == 1, 
                                           .(n_esc_tested_total = .N), 
                                           by = subgroup]
  
  # Merge with complete pop
  crude_test <- merge(crude_numerator_test, denom_total,
                      by = subgroup, all.x = T)
  crude_test[, crude_rate_test := n_tests_total / pop_total]
  crude_test <- crude_test[, c(group_cols_final, "crude_rate_test", "pop_total",
                               "n_tests_total"), with = F]
  
  crude_esc <- merge(crude_numerator_esc, denom_total, 
                     by = subgroup, all.x = T)
  crude_esc[, crude_rate_esc := n_esc_total / pop_total]
  crude_esc <- crude_esc[, c(group_cols_final, "crude_rate_esc"), with = F]
  
  crude_det <- merge(crude_numerator_esc[, c(group_cols_final, "n_esc_total"), with = F],
                     crude_numerator_esc_tested, by = subgroup, all.x= T)
  crude_det[is.na(n_esc_tested_total), n_esc_tested_total := 0]
  crude_det[, crude_pct_tested := fifelse(n_esc_total > 0, 
                                          n_esc_tested_total / n_esc_total, NA_real_)]
  crude_det <- crude_det[, c(group_cols_final, "n_esc_total", "n_esc_tested_total",
                             "crude_pct_tested"), with = F]
  
  esc_standard <- escalaties[, .N, by = std_vars]
  esc_standard[, esc_weight := N / sum(N)]
  
  det_stratum <- merge(
    numerator_esc[, c(group_cols_event, "n_escalaties"), with = F],
    numerator_esc_tested[, c(group_cols_event, "n_esc_tested"), with = F],
    by = group_cols_event, all.x= T)
  det_stratum[is.na(n_esc_tested), n_esc_tested := 0]
  det_stratum[, p := n_esc_tested / n_escalaties]
  det_stratum <- merge(det_stratum,
                       esc_standard[, c(std_vars, "esc_weight"), with = F],
                       by = std_vars, all.x = T)
  
  # gewogen gemiddelde gehernormaliseerd over strata die subgroep echt heeft
  std_det_esc <- det_stratum[, .(std_detectie_esc = 
                                   sum(p* esc_weight, na.rm = T) /
                                   sum(esc_weight, na.rm = T)),
                             by = group_cols_final]
  
  
  # Merge everything
  res <- merge(std_test, std_esc, by = group_cols_final, all = T)
  res <- merge(res, std_esc_tested, by = group_cols_final, all.x= T)
  res <- merge(res, crude_test, by = group_cols_final, all.x=T)
  res <- merge(res, crude_esc, by = group_cols_final, all.x=T)
  res <- merge(res, crude_det, by = group_cols_final, all.x=T)
  res <- merge(res, std_det_esc, by = group_cols_final, all.x=T)
  
  
  rate_cols <- c("std_rate_test", "std_rate_esc", "std_rate_esc_tested",
                 "crude_rate_test", "crude_rate_esc")
  for (col in rate_cols) {
    set(res, which(is.na(res[[col]])), col, 0)
  }
  
  res[, ratio_esc_test := fifelse(std_rate_test > 0,  std_rate_esc / std_rate_test, NA_real_)]
  res[, crude_ratio_esc_test := fifelse(crude_rate_test > 0, crude_rate_esc / crude_rate_test, NA_real_)]
  res[, ratio_test_esc := fifelse(std_rate_esc > 0,  std_rate_test / std_rate_esc, NA_real_)]
  res[, crude_ratio_test_esc := fifelse(crude_rate_esc > 0, crude_rate_test / crude_rate_esc, NA_real_)]
  res[, std_detectie_pop := fifelse(std_rate_esc > 0,
                                    std_rate_esc_tested / std_rate_esc, NA_real_)]
  res[, n_esc_not_tested := n_esc_total - n_esc_tested_total]
  res[, subgroup_type := subgroup]
  #res[, n_total := ]
  
  return(res)
}


#### Plot ratios per wave ####
plot_ratio_wave <- function(dt, subgroup, ratio_col = "ratio_test_hosp", 
                            crude_ratio_col = NULL, top_n = 10) {
  
  dt <- copy(dt)
  wave_order <- c("wave1", "iw1", "wave2", "iw2", "wave3")
  dt[, wave := factor(wave, levels = wave_order)]
  
  #crude_col <- paste0("crude_", ratio_col)
  
  #top_sg_col <- if ("ratio_hosp_test" %in% names(dt) && plot_std) ratio_col else crude_col
 
  top_sg <- dt[, .(avg = mean(get(ratio_col), na.rm = T)),
               by = subgroup][order(-avg)][1:top_n][[subgroup]]
  dt_plot <- dt[get(subgroup) %in% top_sg]
  
  p <- ggplot(dt_plot, aes(x = wave, color = get(subgroup),
                           group = get(subgroup)))
  
  if (!is.null(crude_ratio_col)) {
    p <- p +
      geom_line(aes(y = get(ratio_col), linetype = "Gestandaardiseerd"), linewidth = 1) +
      geom_point(aes(y = get(ratio_col)), size = 1.5) +
      geom_line(aes(y = get(crude_ratio_col), linetype = "Crude"), linewidth = 1) +
      geom_point(aes(y = get(crude_ratio_col)), size = 1.5, shape = 1) + 
      scale_linetype_manual(values = c("Gestandaardiseerd" = "solid",
                                       "Crude" = "dashed"))
  # } else if (plot_std) {
  #   p <- p + 
  #     geom_line(aes(y = get(ratio_col)), linewidth = 1) +
  #     geom_point(aes(y = get(ratio_col)), size = 1.5)
  } else {
    p <- p +
      geom_line(aes(y = get(ratio_col)), linewidth = 1) +
      geom_point(aes(y = get(ratio_col)), size = 1.5)#, shape = 1)
  }
  
  p <- p +
    labs(color = subgroup, x = "Wave", y = "Ratio") + 
    scale_y_continuous(n.breaks = 6) + 
    theme_minimal() +
    theme_bw()# +
    #theme(legend.position = "bottom",
     #     axis.text.x = element_text(angle = 30, hjust = 1))
  
  return(p)
}

#### Discrepancy plot ####
# We will probably end up not using this function
plot_disc_week <- function(dt, subgroup, top_n = 10) {
  # This function computes and plots Z-scores and difference in Z-scores for
  # different rates
  dt <- copy(dt)
  
  dt[, year_week := paste(substr(year, 3,4),"-W",sprintf("%02d",week))] #week, 1, sep = "-"), format = "%G-%V-%u")]
  #dt[, week_year := paste(substr(year, 3,4),"-W",sprintf("%02d",week))]
  #dt[, week_year := as.Date(paste0(year, "-01-01")) + (week - 1) * 7]
  
  # Compute Z-scores
  dt[, z_test := (std_rate_test - mean(std_rate_test)) / sd(std_rate_test)]
  dt[, z_hosp := (std_rate_hosp - mean(std_rate_hosp)) / sd(std_rate_hosp)]
  dt[, discrepancy := z_test - z_hosp]
  
  #top_sg <- dt[, .(avg = mean(abs(discrepancy), na.rm=T)),
  #             by = subgroup][order(-avg)][1:top_n][[subgroup]]
  
  #dt_plot <- dt[get(subgroup) %in% top_sg]
  
  p <- ggplot(dt, aes(x = year_week, y = discrepancy, group =1)) + #, y = discrepancy, color = get(subgroup))) +
    geom_line(linewidth = .7) +
    #geom_line(aes(y = z_hosp, color = "z opname"), linewidth = .7, linetype = "dashed") +
    #geom_line(aes(y = discrepancy)) + #, color = "discrepantie")) + 
    # scale_color_manual(values = c("z test" = "blue", "z opname" = "red",
    #                               "discrepantie" = "black")) +
    labs(title = paste0("Discrepancy (z_test - z_hosp)"),
         y = "Discrepancy score") +
    scale_x_discrete(breaks = dt$year_week[seq(1, nrow(dt), by=4)]) + 
    #scale_x_discrete(breaks = dt$week_year[seq(1, nrow(dt), by=4)]) +
    #scale_x_continuous(n.breaks = 10) +
    theme(axis.text.x = element_text(angle=60, hjust=1)) +
    theme_minimal() +
    theme_bw()
  
  return(p)
}

#### Scatter plot ####
plot_scatter_wave <- function(dt, subgroup, x_col = "std_rate_test", y_col = "std_rate_hosp") {
  dt <- copy(dt)
  wave_order <- c("wave1", "iw1", "wave2", "iw2", "wave3")
  dt[, wave := factor(wave, levels = wave_order)]
  
  # Labels
  labels <- c(
    std_rate_test = "Test rate (gestandaardiseerd)",
    std_rate_hosp = "Opname rate (gestandaardiseerd)",
    std_rate_death = "Sterfte rate (gestandaardiseerd)",
    crude_rate_test = "Test rate (crude)",
    crude_rate_hosp = "Opname rate (crude)",
    crude_rate_death = "Sterfte rate (crude)"
  )
  x_label <- fifelse(x_col %in% names(labels), labels[x_col], x_col)
  y_label <- fifelse(y_col %in% names(labels), labels[y_col], y_col)
  
  dt[, overall_ratio := sum(get(y_col), na.rm=T) / sum(get(x_col), na.rm=T), 
     by = wave]
  
  p <- ggplot(dt, aes(get(x_col), y = get(y_col), color = get(subgroup))) +
    geom_point(size = 2) +
    geom_abline(aes(slope = overall_ratio, intercept = 0),
                linetype = "dashed", color = "red", linewidth = .5) +
    facet_wrap(~wave) +
    scale_y_continuous(labels = scales::number) +
    scale_x_continuous(labels = scales::number) +
    labs(x = x_label,
         y = y_label,
         color = subgroup) +
    theme_minimal() +
    theme_bw()
}


#### Test plotting ####
# res <- compute_rates(demog, tests, hosps, deaths, 
#                      subgroup = "inkomen_klasse_small", 
#                      time_unit = "wave",
#                      std_vars = c("leeftijd_cat", "geslacht", "comorbiditeit", "herkomstland"))
# 
# 
# 
# p <- plot_ratio_wave(dt = res[wave != "wave1" & !is.na(hh_group)],
#                      subgroup = "hh_group",
#                      ratio_col = "ratio_test_death",
#                      crude_ratio_col = "crude_ratio_test_death",
#                      top_n = 10)
# p
# 
# p <- plot_scatter_wave(res[wave != "wave1" & !is.na(hh_group)], "hh_group",
#                        x_col = "std_rate_test", y_col = "std_rate_hosp")
# p
# 
# # CHECK OUTCOME
# n_tests <- tests[herkomstland == "Nederland" & wave == "iw1", .N]
# n_pop <- demog[herkomstland == "Nederland" & file_year == 2020, .N]
# handm <- n_tests / n_pop
# cr_nl <- res1[herkomstland == "Nederland" & wave == "iw1", crude_rate_test]
# 
# assertthat::assert_that(handm == cr_nl)
#denominators <- demog[, .(pop = .N), by = c("file_year")]