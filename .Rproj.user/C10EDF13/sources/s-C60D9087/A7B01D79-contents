library(tidyverse)
library(highcharter)
library(RColorBrewer)
data("GNI2014")

hctreemap2(data = GNI2014,
           group_vars = c("continent", "iso3"),
           size_var = "GNI",
           color_var = "GNI",
           layoutAlgorithm = "squarified",
           levelIsConstant = FALSE,
           levels = list(
             list(level = 1, dataLabels = list(enabled = TRUE)),
             list(level = 2, dataLabels = list(enabled = FALSE)),
             list(level = 3, dataLabels = list(enabled = FALSE))
           )) %>% 
  hc_colorAxis(stops = color_stops(colors = viridisLite::viridis(10, direction = -1))) %>% 
  hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
                            Pop: {point.value:,.0f}<br>
                            GNI: {point.colorValue:,.0f}") %>% 
  hc_chart(backgroundColor="#FFFFFF", borderColor = "transparent", 
           style=list(fontFamily = "Roboto Condensed")) %>% 

  htmlwidgets::saveWidget(here::here("img", "nein.html"))

abastece %<>% 
   mutate(segmento = case_when(
     segmento == "rurales" ~ "Municipios rurales",
     segmento == "capitales" ~ "Municipios capitales y El Alto",
     segmento == "intermedias" ~ "Ciudades intermedias"
   ))

hctreemap2(data = abastece,
           group_vars = c("segmento", "abastece"),
           size_var = "prop",
           color_var = "prop",
           layoutAlgorithm = "squarified",
           levelIsConstant = FALSE,
           levels = list(
             list(level = 1, dataLabels = list(enabled = TRUE)),
             list(level = 2, dataLabels = list(enabled = FALSE)),
             list(level = 3, dataLabels = list(enabled = FALSE))
           )) %>% 
  hc_colorAxis(stops = color_stops(colors = viridisLite::viridis(10, direction = 1))) %>% 
  hc_plotOptions(series = list(
    dataLabels = list(
      style = list(textOutline = FALSE,
                   fontSize =15)
    )
  )) %>% 
  hc_tooltip(pointFormat = "<b>{point.segmento}</b><br>
                            Valor: {point.colorValue:,.0f} %", 
             style = list(fontFamily = "Roboto Condensed", fontSize = 14)) %>% 
  hc_chart(backgroundColor="#FFFFFF", borderColor = "transparent", 
           style=list(fontFamily = "Roboto Condensed", fontSize = 12)) %>% 
  hc_legend(enabled = FALSE)   %>% 
  hc_colors("transparent") %>% 
  hc_title(text = "¿Cuál es su fuente de abastecimiento?",
           align =  "center", style=list(fontFamily = "Roboto Condensed",
                                         fontSize = 19)) %>% 
  hc_subtitle(text = "Ponderado al 100% por cada segmento territorial en base al número de respuestas",
           align =  "center", style=list(fontFamily = "Roboto Condensed",
                                         fontSize = 17)) %>% 
  htmlwidgets::saveWidget(here::here("img", "nein.html"))

data(GNI2014)


