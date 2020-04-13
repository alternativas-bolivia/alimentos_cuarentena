# sobre: comparación semanas

ocu1 <- read_csv("output/comparacion/ocupacion_s1.csv")
ocu2 <- read_csv("output/comparacion/ocupacion_s2.csv")

bind_rows(ocu1, ocu2) %>% 
  arrange(semana, segmento, prop) %>% 
  mutate(
    num = 1:nrow(.), 
    segmento = case_when(
      segmento == "capitales" ~ "Ciudades capitales",
      segmento == "intermedias" ~ "Ciudades intermedias",
      segmento == "rurales" ~ "Municipios rurales"
    )
  ) %>% 
  ggplot(aes(fct_reorder(cual_es_su_ocupacion_actual, num, .desc = T) , prop, fill = semana)) +
  geom_bar(stat="identity", position = "dodge", alpha = 0.8) + 
  facet_wrap(~segmento) + 
  hrbrthemes::theme_ipsum_rc() +
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  ) +
  labs(
    title = "Procedencia de las respuestas", 
    subtitle = "Comparación entre encuestas de las semanas 1 y 2",
    x = "", 
    y = "porcentaje (%)"
  ) +
  scale_y_continuous(breaks = seq(0, 50, 10)) + 
  ggsave("img/comparacion/ocupacion.jpg", width = 12, height = 10)

# abastceimiento

a1 <- read_csv("output/comparacion/abastece_s1.csv")
a2 <- read_csv("output/comparacion/abastece_s2.csv")

bind_rows(a1, a2) %>% 
  group_by(segmento, categoria, semana) %>% 
  count(wt = n) %>% 
  spread(semana, n) %>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  ungroup() %>% 
  group_split(segmento) %>% 
  map(., ~mutate(., `Semana 1` = prop.table(`Semana 1`)*100,
                 `Semana 2` = prop.table(`Semana 2`)*100)) %>% 
  bind_rows() %>% 
  mutate_if(is.numeric, round, 2) %>% 
  ungroup() %>% 
  gather(semana, valor, -segmento, -categoria) %>% 
  arrange(segmento, categoria, semana) %>% 
  mutate(num = 1:nrow(.)) %>% 
  ggplot(aes(fct_reorder(categoria, num, .desc = F) , valor, fill = semana)) +
  geom_bar(stat="identity", position = "dodge", alpha = 0.8) + 
  facet_wrap(~segmento) + 
  hrbrthemes::theme_ipsum_rc() +
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  ) +
  labs(
    title = "Fuente de abastacimiento de alimentos", 
    subtitle = "Comparación entre encuestas de las semanas 1 y 2",
    x = "", 
    y = "porcentaje (%)"
  ) +
  ggsave("img/comparacion/abastecimiento.jpg", width = 10, height = 8)

# abril_15  
ab1 <- read_csv("output/comparacion/abril_15_s1.csv")
ab2 <- read_csv("output/comparacion/abril_15_s2.csv")

bind_rows(ab1, ab2) %>% 
  arrange(semana, segmento, prop) %>% 
  mutate(num = 1:nrow(.)) %>% 
  mutate(abril_15 = str_replace(abril_15, pattern = "habrá desabastecimiento", "")) %>% 
  ggplot(aes(fct_reorder(abril_15, num, .desc = T) , prop, fill = semana)) +
  geom_bar(stat="identity", position = "dodge", alpha = 0.8) + 
  facet_wrap(~segmento) + 
  hrbrthemes::theme_ipsum_rc() +
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  ) +
  labs(
    title = "¿Habrá desabastecimiento de alimentos hasta el 15 de abril?", 
    subtitle = "Comparación entre encuestas de las semanas 1 y 2",
    x = "", 
    y = "porcentaje (%)"
  ) +
  scale_y_continuous(breaks = seq(0, 50, 10)) + 
  ggsave("img/comparacion/abril_15.jpg", width = 10, height = 8)

# dificultades
d1 <- read_csv("output/comparacion/dificultades_s1.csv")
d2 <- read_csv("output/comparacion/dificultades_s2.csv")

bind_rows(d1, d2) %>% 
  group_by(segmento, dificultades, semana) %>% 
  filter(dificultades %in% c("Algunos productos ya no están disponibles", "Especulación y precios altos", 
                             "Restricción vehicular", "Distancia a centros de abastecimiento")) %>% 
  count(wt = n) %>% 
  spread(semana, n) %>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  ungroup() %>% 
  group_split(segmento) %>% 
  map(., ~mutate(., `Semana 1` = prop.table(`Semana 1`)*100,
                 `Semana 2` = prop.table(`Semana 2`)*100)) %>% 
  bind_rows() %>% 
  mutate_if(is.numeric, round, 2) %>% 
  ungroup() %>% 
  gather(semana, valor, -segmento, -dificultades) %>% 
  arrange(segmento, dificultades, semana) %>% 
  mutate(num = 1:nrow(.)) %>% 
  ggplot(aes(fct_reorder(dificultades, num, .desc = F) , valor, fill = semana)) +
  geom_bar(stat="identity", position = "dodge", alpha = 0.8) + 
  facet_wrap(~segmento) + 
  hrbrthemes::theme_ipsum_rc() +
  theme(
    legend.position = "bottom", 
    legend.title = element_blank()
  ) +
  coord_flip() +
  labs(
    title = "¿Dificultades para abastecerse de alimentos?", 
    subtitle = "Comparación entre encuestas de las semanas 1 y 2",
    x = "", 
    y = "porcentaje (%)"
  ) +
  ggsave("img/comparacion/dificultades.jpg", width = 10, height = 8)

# disponibilidad
dis1 <- read_csv("output/comparacion/disponibilidad_s1.csv")
dis2 <- read_csv("output/comparacion/disponibilidad_s2.csv")

bind_rows(dis1, dis2) %>% 
  arrange(semana, segmento, prop) %>% 
  mutate(num = 1:nrow(.)) %>% 
  ggplot(aes(fct_reorder(disponibilidad, num, .desc = T) , prop, fill = semana)) +
  geom_bar(stat="identity", position = "dodge", alpha = 0.8) + 
  facet_wrap(~segmento) + 
  hrbrthemes::theme_ipsum_rc() +
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  ) +
  labs(
    title = "¿Hay disponibilidad de alimentos?", 
    subtitle = "Comparación entre encuestas de las semanas 1 y 2",
    x = "", 
    y = "porcentaje (%)"
  ) +
  scale_y_continuous(breaks = seq(0, 50, 10)) + 
  ggsave("img/comparacion/disponibilidad.jpg", width = 12, height = 10)

# escasez
es1 <- read_csv("output/comparacion/escasez_s1.csv")
es2 <- read_csv("output/comparacion/escasez_s2.csv")

bind_rows(es1, es2) %>% 
  arrange(semana, segmento, prop) %>% 
  mutate(num = 1:nrow(.)) %>% 
  mutate(segmento = case_when(
    segmento == "capitales" ~ "Ciudades capitales y El Alto",
    segmento == "intermedias" ~ "Ciudades intermedias",
    segmento == "rurales" ~ "Municipios rurales",
  )) %>% 
  ggplot(aes(fct_reorder(escasea, num, .desc = T) , prop, fill = semana)) +
  geom_bar(stat="identity", position = "dodge", alpha = 0.8) + 
  facet_wrap(~segmento) + 
  hrbrthemes::theme_ipsum_rc() +
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  ) +
  labs(
    title = "¿Qué grupo de alimentos es el que escasea y al que le cuesta acceder?", 
    subtitle = "Comparación entre encuestas de las semanas 1 y 2",
    x = "", 
    y = "porcentaje (%)"
  ) +
  scale_y_continuous(breaks = seq(0, 50, 10)) + 
  ggsave("img/comparacion/escasez.jpg", width = 12, height = 10)

# gam
g1 <- read_csv("output/comparacion/gam_s1.csv")
g2 <- read_csv("output/comparacion/gam_s2.csv")


bind_rows(g1, g2) %>% 
  arrange(semana, segmento, prop) %>% 
  mutate(num = 1:nrow(.)) %>% 
  ggplot(aes(fct_reorder(gam, num, .desc = T) , prop, fill = semana)) +
  geom_bar(stat="identity", position = "dodge", alpha = 0.8) + 
  facet_wrap(~segmento) + 
  hrbrthemes::theme_ipsum_rc() +
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  ) +
  labs(
    title = "¿Qué hace su gobierno local?", 
    subtitle = "Comparación entre encuestas de las semanas 1 y 2",
    x = "", 
    y = "porcentaje (%)"
  ) +
  scale_y_continuous(breaks = seq(0, 50, 10)) + 
  ggsave("img/comparacion/gam.jpg", width = 12, height = 10)









  
  



  




    
  

  


  
  

  

  
  
  
  





