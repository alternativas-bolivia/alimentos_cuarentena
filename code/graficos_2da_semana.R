# sobre:visualizaciones semana 2
source("code/semana_2.R")
library(waffle)
library(hrbrthemes)

# grafico mapa
mapa <- jsonlite::fromJSON("input/municipios.339.geojson", simplifyVector = F)
donde %<>% rename(value = n)
colores <- viridis::viridis(4, begin = 0, direction = -1)

highchart(type = "map") %>% 
  hc_plotOptions(map = list(
    allAreas = F,
    joinBy = "CODIGO",
    mapData = mapa
  )) %>% 
  hc_add_series(name = "Municipios capitales y El Alto", data = donde %>% filter(segmento == "capitales"), 
                color = colores[1], borderColor = "transparent") %>%
  hc_add_series(name = "Ciudades intermedias", data = donde %>% filter(segmento == "intermedias"), 
                color = colores[2], borderColor = "transparent") %>%
  hc_add_series(name = "Municipios rurales", data = donde %>% filter(segmento == "rurales"), 
                color = colores[3], borderColor = "transparent") %>%
  hc_add_series(name = "Sin cobertura de encuesta", data = donde %>% filter(segmento == "Sin cobertura de encuesta"), 
                color = colores[4], borderColor = "transparent") %>%
  hc_title(text = "67 municipios son parte de la segunda encuesta",
              align =  "center", style=list(fontFamily = "Roboto Condensed",
                                            fontSize = 19)) %>% 
  hc_tooltip(enabled = T, valueDecimals = 0, borderWidth = 0.01,
             pointFormat = paste("<br>Municipio: <b>{point.municipio}</b><br>
                               Encuestados: <b>{point.value} </b>"),
             style = list(fontFamily = "Roboto Condensed",
                        fontSize = 15)) %>%  
  hc_legend(style = list(fontFamily = "Roboto Condensed",
                         fontSize = 15)) %>% 
  hc_chart(backgroundColor="#FFFFFF", style=list(fontFamily = "Roboto Condensed",
                                                 color = "black")) %>% 
  htmlwidgets::saveWidget(here::here("img/semana_2/donde.html")) 
  
  
# ocupación: 661
ocupacion %>% 
  ungroup() %>% 
  filter(!is.na(segmento)) %>% 
  group_split(segmento ) %>% 
  map(., ~mutate(., prop = prop.table(n)*100)) %>% 
  map(., ~mutate_if(., is.numeric, round, 0)) -> ocupacion


ocupacion[[1]] %>% 
  ggplot(aes(label = cual_es_su_ocupacion_actual, values = prop)) +
  geom_pictogram(n_rows = 10, aes(colour = cual_es_su_ocupacion_actual), flip = TRUE, make_proportional = T) +
  scale_color_manual(
    name = NULL,
    values = viridisLite::viridis(7, direction = -1),
    labels = c("Empleada(o) sector privado", "Estudiante", "Otros", "Productor", "Profesional independiente", "Servidor/Autoridad Pública")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = rev(c("chair","briefcase", "tractor", "user-alt", "graduation-cap", "cash-register")),
    labels = c("Empleada(o) sector privado", "Estudiante", "Otros", "Productor", "Profesional independiente", "Servidor/Autoridad Pública")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75)) +
  ggsave("img/semana_2/ocupacion_capitales.jpg", width = 8, height = 8)
  
ocupacion[[3]] %>%
  mutate(prop = case_when(
    cual_es_su_ocupacion_actual == "Servidor/Autoridad Pública" ~ 1,
    T ~ prop
  )) %>% 
  ggplot(aes(label = cual_es_su_ocupacion_actual, values = prop)) +
  geom_pictogram(n_rows = 10, aes(colour = cual_es_su_ocupacion_actual), flip = TRUE, make_proportional = T) +
  scale_color_manual(
    name = NULL,
    values = viridisLite::viridis(7, direction = -1),
    labels = c("Empleada(o) sector privado", "Estudiante", "Otros", "Productor", "Profesional independiente", "Servidor/Autoridad Pública")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = rev(c("chair","briefcase", "tractor", "user-alt", "graduation-cap", "cash-register")),
    labels = c("Empleada(o) sector privado", "Estudiante", "Otros", "Productor", "Profesional independiente", "Servidor/Autoridad Pública")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75)) +
  ggsave("img/semana_2/ocupacion_rurales.jpg", width = 8, height = 8)


ocupacion[[2]] %>% 
  ggplot(aes(label = cual_es_su_ocupacion_actual, values = prop)) +
  geom_pictogram(n_rows = 10, aes(colour = cual_es_su_ocupacion_actual), flip = TRUE, make_proportional = T) +
  scale_color_manual(
    name = NULL,
    values = viridisLite::viridis(7, direction = -1),
    labels = c("Empleada(o) sector privado", "Estudiante", "Otros", "Productor", "Profesional independiente", "Servidor/Autoridad Pública")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = rev(c("chair","briefcase", "tractor", "user-alt", "graduation-cap", "cash-register")),
    labels = c("Empleada(o) sector privado", "Estudiante", "Otros", "Productor", "Profesional independiente", "Servidor/Autoridad Pública")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75)) +
  ggsave("img/semana_2/ocupacion_intermedias.jpg", width = 8, height = 8)

# abastecimiento
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
  htmlwidgets::saveWidget(here::here("img/semana_2/", "abastecimiento.html"))

# disponibilidad
disponibilidad %>% 
  mutate(segmento = case_when(
    segmento == "rurales" ~ "Municipios rurales",
    segmento == "capitales" ~ "Municipios capitales y El Alto",
    segmento == "intermedias" ~ "Ciudades intermedias"
  )) %>% 
  mutate(
    prop = case_when(
      segmento == "Ciudades intermedias" & disponibilidad == "Nunca fue normal" ~ 3, 
      T ~ prop
    )
  ) %>%
  mutate(
    prop = case_when(
      segmento == "Municipios rurales" & disponibilidad == "Nunca fue normal" ~ 14, 
      T ~ prop
    )
  ) %>% 
  ggplot(aes(fill = disponibilidad, values = prop)) +
  geom_waffle(n_rows = 10, size = 0.5, colour = "white", flip = T) +
  scale_fill_manual(
    name = NULL,
    values = viridis::viridis(4),
    labels = c("Normal", "Cada vez menos", "Hay escacez", "Nunca fue normal")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="", plot_title_size = 30, subtitle_face = 20, caption_size = 15, subtitle_size = 20, 
                 strip_text_face = 20, plot_title_face = 20
                  ) +
  theme_enhance_waffle() +
  theme(
    legend.text = element_text(size = 15, hjust = 0), 
    legend.position = "bottom"
  ) +
  facet_wrap(~segmento) +
  labs(
    title = "¿Hay disponibilidad de alimentos?",
    subtitle = "Ponderado al 100%",
    caption = "cada cuadrado equivale al 1%"
  ) +
  ggsave("img/semana_2/disponibilidad.jpg", width = 20, height = 8) 

# escasez
escasez_productos %>% 
  filter(segmento == "capitales") -> temp

temp %>% 
  ggplot(aes(label = escasea, values = prop)) +
  geom_pictogram(n_rows = 10, aes(colour = escasea), flip = T, make_proportional = T) +
  scale_color_manual(
    name = NULL, 
    values = rep("#666666", 8),
    labels = c("Aceites, grasas", "Carnes, huevos", "Cereales", "Frutas", "Azúcares", "Legumbres", "Productos lácteos", "Verduras")
  ) +
  scale_label_pictogram(
    name = NULL,
    values =  c("bacon", "drumstick-bite", "cookie", "apple-alt", "candy-cane", "seedling",  "cheese", "carrot"),
    labels = c("Aceites, grasas", "Carnes, huevos", "Cereales", "Frutas", "Azúcares", "Legumbres", "Productos lácteos", "Verduras")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 14, hjust = 0, vjust = 0.75)) +
  theme(legend.position = "bottom") + 
  ggsave("img/semana_2/escasez_capitales.jpg", height = 8, width = 8)


escasez_productos %>% 
  filter(segmento == "intermedias") -> temp

temp %>% 
  ggplot(aes(label = escasea, values = prop)) +
  geom_pictogram(n_rows = 10, aes(colour = escasea), flip = T, make_proportional = T) +
  scale_color_manual(
    name = NULL, 
    values = rep("#666666", 8),
    labels = c("Aceites, grasas", "Carnes, huevos", "Cereales", "Frutas", "Azúcares", "Legumbres", "Productos lácteos", "Verduras")
  ) +
  scale_label_pictogram(
    name = NULL,
    values =  c("bacon", "drumstick-bite", "cookie", "apple-alt", "candy-cane", "seedling",  "cheese", "carrot"),
    labels = c("Aceites, grasas", "Carnes, huevos", "Cereales", "Frutas", "Azúcares", "Legumbres", "Productos lácteos", "Verduras")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 14, hjust = 0, vjust = 0.75)) +
  theme(legend.position = "bottom") + 
  ggsave("img/semana_2/escasez_intermedias.jpg", height = 8, width = 8)


escasez_productos %>% 
  filter(segmento == "rurales") -> temp

temp %>% 
  ggplot(aes(label = escasea, values = prop)) +
  geom_pictogram(n_rows = 10, aes(colour = escasea), flip = T, make_proportional = T) +
  scale_color_manual(
    name = NULL, 
    values = rep("#666666", 8),
    labels = c("Aceites, grasas", "Carnes, huevos", "Cereales", "Frutas", "Azúcares", "Legumbres", "Productos lácteos", "Verduras")
  ) +
  scale_label_pictogram(
    name = NULL,
    values =  c("bacon", "drumstick-bite", "cookie", "apple-alt", "candy-cane", "seedling",  "cheese", "carrot"),
    labels = c("Aceites, grasas", "Carnes, huevos", "Cereales", "Frutas", "Azúcares", "Legumbres", "Productos lácteos", "Verduras")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 14, hjust = 0, vjust = 0.75)) +
  theme(legend.position = "bottom") + 
  ggsave("img/semana_2/escasez_rural.jpg", height = 8, width = 8)

# dificultades
dificultades %>% 
  ungroup() %>% 
  mutate(prop = prop.table(n) * 100) %>% 
  mutate(segmento = case_when(
    segmento == "rurales" ~ "Municipios rurales",
    segmento == "capitales" ~ "Municipios capitales y El Alto",
    segmento == "intermedias" ~ "Ciudades intermedias"
  )) -> dificultades
  
  
hctreemap2(data = dificultades,
             group_vars = c("segmento", "dificultades"),
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
                            Valor: {point.colorValue:,.3f} %", 
             style = list(fontFamily = "Roboto Condensed", fontSize = 14)) %>% 
  hc_chart(backgroundColor="#FFFFFF", borderColor = "transparent", 
           style=list(fontFamily = "Roboto Condensed", fontSize = 12)) %>% 
  hc_legend(enabled = FALSE)   %>% 
  hc_colors("transparent") %>% 
  hc_title(text = "¿Dificultades para abastecerse?",
           align =  "center", style=list(fontFamily = "Roboto Condensed",
                                         fontSize = 19)) %>% 
  hc_subtitle(text = "Ponderado al 100% por cada segmento territorial en base al número de respuestas",
              align =  "center", style=list(fontFamily = "Roboto Condensed",
                                            fontSize = 17)) %>% 
  htmlwidgets::saveWidget(here::here("img/semana_2/", "dificultades.html"))

# abastecimiento hast el 15 de abril

abril_15 %>% 
  ungroup() %>% 
  mutate(segmento = case_when(
    segmento == "rurales" ~ "Municipios rurales",
    segmento == "capitales" ~ "Municipios capitales y El Alto",
    segmento == "intermedias" ~ "Ciudades intermedias"
  )) %>% 
  group_split(segmento) %>% 
  map(., ~ mutate(., prop = prop.table(n)*100)) %>% 
  map(., ~ mutate_if(., is.numeric, round, 0)) %>% 
  bind_rows() %>% 
  mutate(prop = case_when(
    segmento == "Municipios capitales y El Alto" & abril_15 == "No habrá desabastecimiento" ~ 23,
    T ~ prop
  )) %>% 
  ggplot(aes(fill = abril_15, values = prop)) +
  geom_waffle(n_rows = 10, size = 0.5, colour = "white", flip = T) +
  scale_fill_manual(
    name = NULL,
    values = viridis::viridis(4),
    labels = c("A corto plazo habrá desabastecimiento", "A largo plazo habrá desabastecimiento", "No habrá desabastecimiento")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="", plot_title_size = 30, subtitle_face = 20, caption_size = 15, subtitle_size = 20, 
                 strip_text_face = 20, plot_title_face = 20
  ) +
  theme_enhance_waffle() +
  theme(
    legend.text = element_text(size = 15, hjust = 0), 
    legend.position = "bottom"
  ) +
  facet_wrap(~segmento) +
  labs(
    title = "¿Habrá desabastecimiento hasta el final de la cuarentena?",
    subtitle = "Ponderado al 100%",
    caption = "cada cuadrado equivale al 1%"
  ) +
  ggsave("img/semana_2/abril_15.jpg", width = 20, height = 8) 

# gam
gam %>% 
  ungroup() %>% 
  mutate(segmento = case_when(
    segmento == "rurales" ~ "Municipios rurales",
    segmento == "capitales" ~ "Municipios capitales y El Alto",
    segmento == "intermedias" ~ "Ciudades intermedias"
  )) %>% 
  group_split(segmento) %>% 
  map(., ~ mutate(., prop = prop.table(n)*100)) %>% 
  map(., ~ mutate_if(., is.numeric, round, 0)) %>% 
  bind_rows() %>% 
  ggplot(aes(fill = gam, values = prop)) +
  geom_waffle(n_rows = 10, size = 0.5, colour = "white", flip = T) +
  scale_fill_manual(
    name = NULL,
    values = viridis::viridis(6),
    labels = c("Control de precios", "Coordinación con actores locales", "Coordinación con otros niveles de gobierno", 
               "Coordinación con productores locales", "Nada", "No sabe")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="", plot_title_size = 30, subtitle_face = 20, caption_size = 15, subtitle_size = 20, 
                 strip_text_face = 20, plot_title_face = 20
  ) +
  theme_enhance_waffle() +
  theme(
    legend.text = element_text(size = 15, hjust = 0), 
    legend.position = "bottom"
  ) +
  facet_wrap(~segmento) +
  labs(
    title = "¿Qué hace su gobierno local?",
    subtitle = "Ponderado al 100%",
    caption = "cada cuadrado equivale al 1%"
  ) +
  ggsave("img/semana_2/gam.jpg", width = 20, height = 8) 
  


  
  





  






  

