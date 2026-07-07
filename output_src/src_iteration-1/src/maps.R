#### MAPS ####

## wc -> wijk22/stadsdeel mapping
wijk22 <- readxl::read_excel("H:/data/crosswalks/Amsterdam Gemeente & CBS indelingen 2017 .xlsx")
keep <- c("WK_CODE", "Stadsdelen", "AMS_Wijk 22")
wijk22 <- as.data.table(wijk22)[, ..keep]
setnames(wijk22, c("WK_CODE", "Stadsdelen", "AMS_Wijk 22"), c("wc", "stadsdeel", "wijk22"))
wijk22[, wc := substr(wc, 3, nchar(wc))]
wijk22[, stadsdeel := substr(stadsdeel, 3, nchar(stadsdeel))]
wijk22[, wijk22 := substr(wijk22, 6, nchar(wijk22))]
wijk22 <- unique(wijk22, by = "wc")
wijk22$wc <- paste0("WK", wijk22$wc)
fwrite(wijk22, file = "data/tijn/wijk22_stadsdeel_mapping.csv")

# Read shp file
wijken_2021 <- "K:/Utilities/Tools/GISHulpbestanden/Gemeentewijkbuurt/2021/wk_2021.shp"
codes_wijken <- sf::st_read(wijken_2021)

# wijk 22 data
wijk22_dt <- as.data.table(readxl::read_excel("data/tijn/results/rates_ratios_leeftijd_3_geslacht.xlsx", sheet = "wijk22"))

map_wijk22_geom <- codes_wijken |>
  dplyr::left_join(wijk22, by = c("STATCODE" = "wc")) |>
  dplyr::filter(!is.na(wijk22)) |>
  dplyr::group_by(wijk22) |>
  dplyr::summarise(geometry = sf::st_union(geometry))

map_wijk22 <- dplyr::inner_join(map_wijk22_geom, wijk22_dt, by = "wijk22")

# Stadsdeel data
stadsdeel_dt <- as.data.table(readxl::read_excel("data/tijn/results/rates_ratios_leeftijd_3_geslacht.xlsx", sheet = "stadsdeel"))

stadsdeel_geom <- codes_wijken |>
  dplyr::left_join(wijk22, by = c("STATCODE" = "wc")) |>
  dplyr::filter(!is.na(stadsdeel)) |>
  dplyr::group_by(stadsdeel) |>
  dplyr::summarise(geometry = sf::st_union(geometry))

map_stadsdeel <- dplyr::inner_join(stadsdeel_geom, stadsdeel_dt, by = "stadsdeel")

# Plot map
map <- ggplot(map_wijk22) + #map_wijk22[map_wijk22$wave == "wave2",]) +
  geom_sf(aes(fill = ratio_test_esc), linewidth = .7) +
  scale_fill_gradient(high = "darkblue",
                      low = "lightblue") +
  #scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  facet_wrap(~wave) +
  labs(fill = "Ratio (tests / opnames)") +
  theme_void()

map

ggsave("H:/_Current_projects/griffins/data/tijn/plots/maps/all_waves_wijk_test_hosp.png")


# Save maps
# map_list <- list(
#   map_wijk = map_wijk,
#   map_wijk22 = map_wijk22
# )
# 
# ratio_vars <- c("ratio_test_hosp", "ratio_hosp_test", "ratio_test_death", "ratio_death_test")
# 
# map_wijk <- map_wijk |> 
#   dplyr::mutate(dplyr::across(dplyr::all_of(ratio_vars), ~ dplyr::na_if(., 0)))
# 
# map_wijk22 <- map_wijk22 |> 
#   dplyr::mutate(dplyr::across(dplyr::all_of(ratio_vars), ~ dplyr::na_if(., 0)))
# 
# fill_labels <- c(
#   ratio_test_hosp = "Ratio (tests/opnames)",
#   ratio_hosp_test = "Ratio (opnames/tests)",
#   ratio_test_death = "Ratio (tests/sterfte)",
#   ratio_death_test = "Ratio (sterfte/tests)"
# )
# 
# waves <- c("iw1", "wave2", "iw2", "wave3")
# class(map_list)
# 
# df <- map_list[["map_wijk"]]
# class(df)
# dplyr::filter(df, wave == "iw1")
# 
# for (map_name in names(map_list)) {
#   df <- map_list[[map_name]]
#   #print(paste(map_name, ":", class(df)))
#   
#   for (ratio in ratio_vars) {
#     for (w in waves) {
#       #print(paste(map_name, ratio, w, ":", class(df)))
#       
#       map <- ggplot(df[df$wave == w,]) + #ggplot(dplyr::filter(map_data, wave == w))
#         geom_sf(aes(fill = .data[[ratio]]), linewidth = .7) +
#         scale_fill_gradient(high = "darkblue",
#                             low = "lightblue") +
#         labs(fill = fill_labels[[ratio]]) +
#         theme_void()
#       
#       ggsave(
#         filename = paste0("H:/_Current_projects/griffins/data/tijn/plots/maps/kaart_", map_name, "_", ratio, "_", w, ".png"),
#         plot = map,
#         width = 8,
#         height = 6,
#         dpi = 300
#       )
#     }
#   }
# }