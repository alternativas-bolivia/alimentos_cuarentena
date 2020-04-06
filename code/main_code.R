# sobre:limpieza base
library(tidyverse)
library(magrittr)
library(janitor)

df <- rio::import("input/Matriz abastecimiento cuarentena 2.xlsx") %>% clean_names()

# wrangling
df$ocupacion %<>% gsub("Servidor/Autoridad pública", "Servidor/Autoridad Pública", .)
df$ocupacion %<>% gsub("Cuentapropista o profesional independiente", "Profesional independiente", .)
df$ocupacion %<>% gsub("Independiente", "Profesional independiente", .)  
df$ocupacion %<>% gsub("Dependiente", "Empleada(o) sector privado", .)  
df$ocupacion %<>% gsub("Empleado sector privado", "Empleada(o) sector privado", .)  
df$ocupacion %<>% gsub("Empleado del hogar", "Empleada(o) del hogar", .)  
df$ocupacion %<>% gsub("Cáritas|Ninguno", "Otros", .)  
df$ocupacion %<>% replace_na(., "Otros")

df %<>% 
  fill(departamento, municipio)


# quienes responde la encuesta waffle
df %>% 
  count(ocupacion) %>%
  arrange(desc(n)) %>% 
  ggplot(aes(label = ocupacion, values = n)) +
  geom_pictogram(n_rows = 7, aes(colour = ocupacion), flip = TRUE, make_proportional = F) +
  scale_color_manual(
    name = NULL,
    values = viridisLite::cividis(7, direction = -1),
    labels = c("Empleada(o) del hogar", "Empleada(o) sector privado", "Estudiante", "Otros", "Productor", "Profesional independiente", "Servidor/Autoridad Pública")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = rev(c("chair","briefcase", "tractor", "user-alt", "graduation-cap", "cash-register", "blender")),
    labels = c("Empleada(o) del hogar", "Empleada(o) sector privado", "Estudiante", "Otros", "Productor", "Profesional independiente", "Servidor/Autoridad Pública")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75)) +
  labs(
    title = "¿Quienes respondieron la encuesta?",
    subtitle = "56 personas",
    caption = "Fundación Alternativas"
  ) +
  ggsave("img/quien_responde.jpg", width = 7, height = 7)
  
# donde se hace
ine <- rio::import("/Users/rafalopezv/Dropbox/alfa1/datos_extras/datos_municipales/input/codigos.ine.xlsx")

(df$municipio %>% unique)[!(df$municipio %>% unique) %in% ine$MUNICIPIO]

df$municipio %<>% gsub("Coroico \\(Carmen Pampa\\)", "Coroico", .)
df$municipio %<>% gsub("Caranavi \\(Unión Berea\\)", "Caranavi", .)
df$municipio %<>% gsub("Belén de Urmiri", "Urmiri", .)
df$municipio %<>% gsub("Rurreneabaque", "Rurrenabaque", .)
df$municipio %<>% gsub("Cobija \\(El Castañal\\)", "Cobija", .)
df$municipio %<>% gsub("Villa Montes", "Villamontes", .)
df$municipio %<>% gsub("Salinas de Garcí Mendoza", "Salinas de Garci Mendoza", .)
df$municipio %<>% gsub("Villa bella", "Guayaramerín", .)

(df$municipio %>% unique)[!(df$municipio %>% unique) %in% ine$MUNICIPIO]

ine %>% 
  select(CODIGO, municipio = MUNICIPIO) %>% 
  left_join(df,. ) -> df

# Cargar mapa
mapa <- sf::st_read("/Users/rafalopezv/Dropbox/alfa1/geo_mapas/municipios.339.geojson", 
            stringsAsFactors = F)

mapa %<>% 
  mutate(
    cobertura = case_when(
       CODIGO %in% (df$CODIGO %>% unique)~ "si",
       is.na(CODIGO) ~ "lagos",
       T ~ "no"
    )
  ) 

mapa[mapa$cobertura == "lagos", "cobertura"] <- NA

# gráfico mapa
ggplot(mapa) +
  geom_sf(aes(fill = cobertura), color = "#7e7e7e", size = 0.05) + 
  scale_fill_viridis_d(na.value = "white", option = "D", begin = 0, end = 0.7, 
                       direction = 1, labels = c("Sin cobertura", " Municipios de la encuesta", " lagos, salares")) + 
  labs(
    title = "37 municipios son parte de la encuesta",
    caption = "Fundación Alternativas"
  ) + 
  hrbrthemes::theme_ipsum_rc(grid = F, axis = F) +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "white", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA), 
    panel.background = element_rect(fill = "white", color = NA), 
    legend.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank()
  ) +
  ggsave("img/mapa.jpg", width = 6.5, height = 6.5)

# donde se abastece
df$en_esta_epoca_de_cuarentena_donde_se_abastece_ud_y_su_familia %>% 
  unique()

df$en_esta_epoca_de_cuarentena_donde_se_abastece_ud_y_su_familia %<>% gsub("Mercados Barriales", "Mercado Barrial", .)
df$en_esta_epoca_de_cuarentena_donde_se_abastece_ud_y_su_familia %<>% gsub("tiendas de barrio", "Tiendas de barrio", .)
df$en_esta_epoca_de_cuarentena_donde_se_abastece_ud_y_su_familia %<>% gsub("Produccion propia", "Producción propia", .)
df$en_esta_epoca_de_cuarentena_donde_se_abastece_ud_y_su_familia %<>% gsub("Mercado de otro municipio", "Mercado de otro Municipio", .)
df$en_esta_epoca_de_cuarentena_donde_se_abastece_ud_y_su_familia %<>% gsub("Mercado Central de Villa Tunari", "Mercado Municipal", .)
df$en_esta_epoca_de_cuarentena_donde_se_abastece_ud_y_su_familia %<>% gsub("Mercado de otro Municipio\n\nFerias", "Mercado de otro Municipio", .)
df$en_esta_epoca_de_cuarentena_donde_se_abastece_ud_y_su_familia %<>% gsub("Mercado Municipal\n\n\nFerias", "Mercado Municipal", .)


df %>% 
    count(en_esta_epoca_de_cuarentena_donde_se_abastece_ud_y_su_familia) %>% 
    rename(abast = en_esta_epoca_de_cuarentena_donde_se_abastece_ud_y_su_familia) %>% 
    filter(!is.na(abast), abast != "-") %>% 
    arrange(desc(n)) %>%
    mutate(
      sigla = case_when(
        str_detect(abast, "Mercado") ~ "Mercados",
        str_detect(abast, "Ferias") ~ "Ferias o tiendas de barrio",
        str_detect(abast, "Tiendas de barrio") ~ "Ferias o tiendas de barrio",
        T ~ "Producción propia"
      )
    ) %>% 
    count(sigla, wt = n) %>% 
  arrange(n) %>% 
mutate(n = c(10, 29, 61)) %>% 
  ggplot(aes(label = sigla, values = n)) +
  geom_pictogram(n_rows = 10, aes(colour = sigla), flip = F, make_proportional = F) +
  scale_color_manual(
    name = NULL,
    values = c("#440154FF", "#21908CFF", "#FDE725FF"),
    labels = c("Ferias o tiendas de barrio: 29%", "Mercados: 61%","Producción propia: 10%")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = rev(c("user",   "shopping-basket", "store")),
    labels = c("Ferias o tiendas de barrio: 29%", "Mercados: 61%","Producción propia: 10%")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") + 
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 12, hjust = 0, vjust = 0.75)) +
  labs(
    title = "¿Cuál es su fuente de abastecimiento durante la cuarentena?",
    subtitle = "Ponderado al 100%",
    caption = "Fundación Alternativas"
  ) + 
  theme(legend.position = "top") +
  ggsave("img/fuente_abastecimiento.jpg", width = 11, height = 8)
  
# ahora los detalles ferias y tiendas de barrio
df %>% 
  count(en_esta_epoca_de_cuarentena_donde_se_abastece_ud_y_su_familia) %>% 
  rename(abast = en_esta_epoca_de_cuarentena_donde_se_abastece_ud_y_su_familia) %>% 
  filter(!is.na(abast), abast != "-") %>% 
  arrange(desc(n)) %>%
  mutate(
    sigla = case_when(
      str_detect(abast, "Mercado") ~ "Mercados",
      str_detect(abast, "Ferias") ~ "Ferias o tiendas de barrio",
      str_detect(abast, "Tiendas de barrio") ~ "Ferias o tiendas de barrio",
      T ~ "Producción propia"
    )
  ) %>% 
  filter(sigla == "Ferias o tiendas de barrio") %>% 
mutate(n = c(71, 29)) %>% 
  ggplot(aes(label = abast, values = n)) +
  geom_pictogram(n_rows = 10, aes(colour = abast), flip = F, make_proportional = F) +
  scale_color_manual(
    name = NULL,
    values = c("#440154FF", "#9202b5"),
    labels = c("Ferias: 71%", "Tiendas de barrio: 29%")
  ) +
  scale_label_pictogram(
    name = NULL,
    values =  "store",
    labels = c("Ferias: 71%", "Tiendas de barrio: 29%")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="", base_size = 14) +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 14, hjust = 0, vjust = 0.75)) +
  theme(legend.position = "left") +
  ggsave("img/ferias_tiendas.jpg", width = 12, height = 7)
  
  
# ahora los detalles de mercados
df %>% 
  count(en_esta_epoca_de_cuarentena_donde_se_abastece_ud_y_su_familia) %>% 
  rename(abast = en_esta_epoca_de_cuarentena_donde_se_abastece_ud_y_su_familia) %>% 
  filter(!is.na(abast), abast != "-") %>% 
  arrange(desc(n)) %>%
  mutate(
    sigla = case_when(
      str_detect(abast, "Mercado") ~ "Mercados",
      str_detect(abast, "Ferias") ~ "Ferias o tiendas de barrio",
      str_detect(abast, "Tiendas de barrio") ~ "Ferias o tiendas de barrio",
      T ~ "Producción propia"
    )
  ) %>% 
  filter(sigla == "Mercados") %>% 
  ggplot(aes(label = abast, values = n)) +
  geom_pictogram(n_rows = 10, aes(colour = abast), flip = F, make_proportional = T) +
  scale_color_manual(
    name = NULL, 
    values = c("#21908CFF", "#2bb9b4", "#8ae4e1"),
    labels = c("Mercado Municipal: 20%", "Mercado de otro Municipio: 13%", "Mercado barrial: 67%")
  ) +
  scale_label_pictogram(
    name = NULL,
    values =  "shopping-basket",
    labels = c("Mercado Municipal: 20%", "Mercado de otro Municipio: 13%", "Mercado barrial: 67%")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 14, hjust = 0, vjust = 0.75)) +
  theme(legend.position = "right") +
  ggsave("img/mercados.jpg", width = 12, height = 7)


# columna 2
df$disponibilidad %>% 
  unique()

df %<>% rename(disponibilidad = en_su_opinion_hay_disponibilidad_de_alimentos_en_su_municipio)
df$disponibilidad %<>% gsub("Hay abastecimiento pero cada vez menos|Hay abastecmiento pero cada vez menos", "Cada vez menos", .)
df$disponibilidad %<>% gsub("Aún existe abastecimiento normal|Aun existe abastecimiento normal|Aun existe abastecimiento", "Normal", .)
df$disponibilidad %<>% gsub("El abastecimiento nunca ha sido normal", "Nunca fue normal", .)

df %>% 
  filter(!is.na(disponibilidad)) %>% 
  count(disponibilidad) %>% 
  mutate(prop = round(prop.table(n)*100), 0) %>% 
  arrange(prop) %>% 
  mutate(num = 1:nrow(.)) %>% 
  ggplot(aes(fill = fct_reorder(disponibilidad, num, .desc = T), values = prop)) +
  geom_waffle(n_rows = 10, size = 0.5, colour = "white", flip = T) +
  scale_fill_manual(
    name = NULL,
    values = viridis::viridis(4),
    labels = c("Normal: 56%", "Cada vez menos: 38%", "Hay escacez: 4%", "Nunca fue normal: 2")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  labs(title = "¿Hay disponibilidad de alimentos?",
       subtitle = "Ponderado al 100%") +
  theme(legend.text = element_text(size = 12, hjust = 0)) +
  ggsave("img/disponibilidad.jpg", width = 9, height = 7)


# dificultades
df  %>% 
  select(7, 8) %>% 
  gather() %>% 
  filter(!is.na(value)) %>% 
  count(value)  %>% 
  arrange(desc(n)) -> temp

temp$value %<>% gsub("Productos ya no estan disponibles",  "Algunos productos ya no están disponibles", .)
temp$value %<>% gsub("Algunos productos ya no estan disponibles",  "Algunos productos ya no están disponibles", .)
temp$value %<>% gsub("Restriccion Vehicular",  "Restricción vehicular", .)
temp$value %<>% gsub("Algunos productos ya no están llegando",  "Algunos productos ya no están disponibles", .)
temp$value %<>% gsub("Los más perjudicados están en la zona periurbana o comunidades aisladas",  "Perjudicados estan en la zona periurbana o comunidades alejadas", .)
temp$value %<>% gsub("Perjudicados estan en la zona periurbana o comunidades aisladas",  "Perjudicados estan en la zona periurbana o comunidades alejadas", .)


temp %<>% 
  count(value, wt = n)

temp %>% 
  mutate(
    sigla = case_when(
      value == "Algunos productos ya no están disponibles" ~ "Acceso de productos", 
      value == "El horario de abastecimiento es limitado" ~ "Acceso de productos",
      value == "Han subido mucho los precios" ~ "Acceso de productos",
      value == "No hay donde comprar alimentos en mi barrio" ~ "Acceso de productos",
      value == "Restricción vehicular" ~ "Restricción vehicular",
      value == "Ninguna" ~ "Ninguna",
      value == "Perjudicados estan en la zona periurbana o comunidades alejadas" ~ "Ninguna"
    )
  ) %>% 
  count(sigla, wt = n) %>% 
  arrange(n) %>% 
  mutate(
    num = 1:nrow(.),
    por = round(prop.table(n) * 100, 1),
    por = c(15, 42, 43)
  ) %>% 
  ggplot(aes(fill = fct_reorder(sigla, num, .desc = T), values = por)) +
  geom_waffle(n_rows = 10, size = 0.5, colour = "white", flip = F) +
  scale_fill_manual(
    name = NULL,
    values = viridis::viridis(3),
    labels = c("Acceso a productos: 43%", "Reestricción vehicular: 42%", "Ninguna: 15%")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  labs(title = "¿Dificultades para abastecerse?",
       subtitle = "Ponderado al 100%") +
  theme(legend.text = element_text(size = 12, hjust = 0)) +
  ggsave("img/disifultades.jpg", width = 9, height = 7)

#-----------
df %>% 
  count(en_su_opinion_actualmente_existen_suficientes_alimentos_en_el_municipio_hasta_el_15_de_abril) %>% 
  arrange(desc(n))


df %>% 
  count(que_mecanismos_esta_adoptandos_el_gam_para_garantizar_abastecimiento) %>% 
  arrange(desc(n))
#-----------

