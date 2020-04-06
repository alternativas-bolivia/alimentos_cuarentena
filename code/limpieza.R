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

# donde se abastece
df %<>% rename(abastecimiento = en_esta_epoca_de_cuarentena_donde_se_abastece_ud_y_su_familia)

df$abastecimiento %<>% gsub("Mercados Barriales", "Mercado Barrial", .)
df$abastecimiento %<>% gsub("tiendas de barrio", "Tiendas de barrio", .)
df$abastecimiento %<>% gsub("Produccion propia", "Producción propia", .)
df$abastecimiento %<>% gsub("Mercado de otro municipio", "Mercado de otro Municipio", .)
df$abastecimiento %<>% gsub("Mercado Central de Villa Tunari", "Mercado Municipal", .)
df$abastecimiento %<>% gsub("Mercado de otro Municipio\n\nFerias", "Mercado de otro Municipio", .)
df$abastecimiento %<>% gsub("Mercado Municipal\n\n\nFerias", "Mercado Municipal", .)
df$abastecimiento %<>% gsub("-", NA, .)

df %<>% 
  mutate(
    abstecimiento_categorias = case_when(
      str_detect(abastecimiento, "Mercado") ~ "Mercados",
      str_detect(abastecimiento, "Ferias") ~ "Ferias o tiendas de barrio",
      str_detect(abastecimiento, "Tiendas de barrio") ~ "Ferias o tiendas de barrio",
      str_detect(abastecimiento, "Producción propia") ~ "Producción propia",
      T ~ abastecimiento
    )
  ) 

# disponibilidad
df %<>% rename(disponibilidad = en_su_opinion_hay_disponibilidad_de_alimentos_en_su_municipio)
df$disponibilidad %<>% gsub("Hay abastecimiento pero cada vez menos|Hay abastecmiento pero cada vez menos", "Cada vez menos", .)
df$disponibilidad %<>% gsub("Aún existe abastecimiento normal|Aun existe abastecimiento normal|Aun existe abastecimiento", "Normal", .)
df$disponibilidad %<>% gsub("El abastecimiento nunca ha sido normal", "Nunca fue normal", .)

# dificultades
df %<>% rename(dificultades = actualmente_cuales_son_las_dificultades_mas_grandes_que_tienen_los_ciudadanos_en_el_municipio_para_abastecerse_respuesta_1)

df$dificultades %<>% gsub("Productos ya no estan disponibles",  "Algunos productos ya no están disponibles", .)
df$dificultades %<>% gsub("Algunos productos ya no estan disponibles",  "Algunos productos ya no están disponibles", .)
df$dificultades %<>% gsub("Restriccion Vehicular",  "Restricción vehicular", .)
df$dificultades %<>% gsub("Algunos productos ya no están llegando",  "Algunos productos ya no están disponibles", .)
df$dificultades %<>% gsub("Los más perjudicados están en la zona periurbana o comunidades aisladas",  "Perjudicados estan en la zona periurbana o comunidades alejadas", .)
df$dificultades %<>% gsub("Perjudicados estan en la zona periurbana o comunidades aisladas",  "Perjudicados estan en la zona periurbana o comunidades alejadas", .)


df %<>% rename(dificultades_1 = actualmente_cuales_son_las_dificultades_mas_grandes_que_tienen_los_ciudadanos_en_el_municipio_para_abastecerse_respuesta_2)

df$dificultades_1 %<>% gsub("Productos ya no estan disponibles",  "Algunos productos ya no están disponibles", .)
df$dificultades_1 %<>% gsub("Algunos productos ya no estan disponibles",  "Algunos productos ya no están disponibles", .)
df$dificultades_1 %<>% gsub("Restriccion Vehicular",  "Restricción vehicular", .)
df$dificultades_1 %<>% gsub("Algunos productos ya no están llegando",  "Algunos productos ya no están disponibles", .)
df$dificultades_1 %<>% gsub("Los más perjudicados están en la zona periurbana o comunidades aisladas",  "Perjudicados estan en la zona periurbana o comunidades alejadas", .)
df$dificultades_1 %<>% gsub("Perjudicados estan en la zona periurbana o comunidades aisladas",  "Perjudicados estan en la zona periurbana o comunidades alejadas", .)


# suficiente
df %<>% rename(suficiente = en_su_opinion_actualmente_existen_suficientes_alimentos_en_el_municipio_hasta_el_15_de_abril)

df$suficiente %<>% gsub("A corto plazo habrá desabastecimiento\n|A corto plaza habrá desabastecimiento", "A corto plazo habrá desabastecimiento", .)
df$suficiente %<>% gsub("A largo plazo habra desabastecimiento", "A largo plazo habrá desabastecimiento", .)
df$suficiente %<>% gsub("No habra desabastecimiento", "No habrá desabastecimiento", .)


# mecanismos de gobiernos

df %<>% rename(mecanismos = que_mecanismos_esta_adoptandos_el_gam_para_garantizar_abastecimiento)

df$mecanismos %<>% gsub("Coordinacion con otros niveles del Estado", "Coordinacion con otros niveles de gobierno", . )
df$mecanismos %<>% gsub("Coordinación actores locales \n\nControl de precios en mercados", "Coordinación con actores locales", . )
df$mecanismos %<>% gsub("Coordinación actores locales", "Coordinación con actores locales", . )
df$mecanismos %<>% gsub("Control de precios en los mercados", "Control de precios en mercados", . )
df$mecanismos %<>% gsub("Coordinacion con otros niveles de gobierno", "Coordinación con otros niveles de gobierno", . )
df$mecanismos %<>% gsub("No se está haciendo nada|No se esta haciendo nada|Nos estan haciendo nada|No están haciendo nada", "Nada", . )
df$mecanismos %<>% gsub("Están ayudando a los que transportan alimento, pero igual no llega normal\\.", "Ayuda con transporte", . )
df$mecanismos %<>% gsub("No sé|No se|No nos han comunicado", "No sabe", . )

df %>% 
  select(-CODIGO, -abstecimiento_categorias) %>% 
  rio::export("output/base_limpia.xlsx")

df %>% 
  select(-CODIGO, -abstecimiento_categorias) %>% 
  write_csv("output/base_limpia.csv")


