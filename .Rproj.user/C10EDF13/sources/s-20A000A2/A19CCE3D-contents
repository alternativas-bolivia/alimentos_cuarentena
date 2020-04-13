# sobre: limpieza general de la base estandarizada en google forms para generación gráfica mas eficiente
library(tidyverse)
library(magrittr)
library(janitor)
library(waffle)

# cargado de bases
df <- read_csv("input/semana_2/Percepciones ciudadanas acerca del Abastecimiento de alimentos durante la Emergencia Sanitaria COVID-19.csv")
df %<>% clean_names()
ine <- rio::import("input/codigos.ine.xlsx") %>% 
  clean_names() %>% 
  select(departamento, municipio, CODIGO = codigo)
segmento <- read_csv("input/segmentos.csv") %>% rename(CODIGO = codigo)


# quitar columnas inútiles
df %<>% 
  select(-matches("temporal|apellido|telefono")) 

# unificación columna de municipios e incorporación de códigos
df %>% 
  select(matches("en_que_municipio_")) -> temp

temp %<>% 
  apply(., 2, is.na) %>% 
  as.data.frame() 

temp$suma <- rowSums(temp) 
temp$num <- 1:nrow(temp)

temp %>% 
  filter(suma == 9) %>% 
  pull(num) -> temp

df %>% 
  select(matches("departamento|en_que_municipio_")) %>% 
  mutate(num = 1:nrow(.)) %>% 
  gather(sigla, valor, -en_que_departamento_reside_actualmente, -num) %>% 
  arrange(num) %>% 
  filter(!is.na(valor)) %>% 
  select(num, municipio = valor) %>% 
  bind_rows(., tibble(
    num = temp, 
    municipio = rep(NA, length(temp))
  )) %>% 
  arrange(num) %>% 
  pull(municipio) -> temp_1

df %<>% select(-contains("en_que_municipio_"))

# añadir la columna de municipio
df %<>% 
  mutate(municipio = temp_1) %>% 
  rename(departamento = en_que_departamento_reside_actualmente) %>% 
  select(departamento, municipio, everything()) %>% 
  mutate(departamento = toupper(departamento)) 

df %<>% merge(., ine, all.x = T)  
  
# añadir segmento territorial
df %<>% merge(., segmento, all.x = T) 


# ---------------------------------
# división por respuestas
# ---------------------------------
# dónde
donde <- df %>% 
  group_by(CODIGO, municipio, segmento) %>% 
  count() %>% 
  filter(!is.na(segmento)) 

ine %>% 
  filter(!CODIGO %in% (donde$CODIGO %>% unique)) %>% 
  select(CODIGO, municipio) %>% 
  mutate(
    segmento = "Sin cobertura de encuesta",
    n = 0
  ) %>% 
  bind_rows(donde, .) -> donde

# ocupación
ocupacion <- df %>% 
  group_by(cual_es_su_ocupacion_actual, segmento) %>% 
  count()

# abastece
df %>% 
  rename(abastece = en_esta_epoca_de_cuarentena_donde_se_abastece_ud_y_su_familia) %>% 
  group_by(abastece, segmento) %>% 
  count() %>% 
  ungroup() -> abastece


temp <- grep(pattern = "super", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Supermercado"

abastece$abastece %<>% gsub("Súper mercado|Ketal|SAS y mercado central", "Supermercado", .)
abastece$abastece %<>% gsub("Micrimercado", "Supermercado", .)

temp <- grep(pattern = "propia", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Producción propia"

temp <- grep(pattern = "product", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Directo de productores"

temp <- grep(pattern = "Un grupo campesino organizado por la sub gobernación trajo una canasta a la.puerta de mi casa", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Directo de productores"

temp <- grep(pattern = "Distribución de los pructores", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Directo de productores"

temp <- grep(pattern = "Mecanismo alternativo de alimentación: Canasta Campesina Alantuya", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Directo de productores"

temp <- grep(pattern = "Tiendas", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Tiendas de barrio"

temp <- grep(pattern = "En tiedas y mercado barrial y municipal", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Tiendas de barrio"

abastece %<>% 
  filter(!abastece %in% c("De nada", "En el campo", "No hay por aqui ni feria", 
                          "Solo en casa", "Taller", "Lo que llega a la urbanizacion")) 

temp <- grep(pattern = "Llegan camiones con pesido para varios vecinos", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Camiones o vendedores en vehiculos"

temp <- grep(pattern = "Mercado Campesino|Mercado Bolivar", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Mercado Municipal"

temp <- grep(pattern = "Mercados moviles", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Camiones o vendedores en vehiculos"


abastece %>% 
  mutate(categoria = case_when(
    str_detect(abastece, "Mercado") ~ "Mercados",
    str_detect(abastece, "Supermercado") ~ "Supermercados",
    str_detect(abastece, "Cami") ~ "Venta móvil",
    str_detect(abastece, "Tiendas|Ferias") ~ "Tiendas de barrio y ferias",
    str_detect(abastece, "Directo") ~ "Directo de productores",
    str_detect(abastece, "Producción propia") ~ "Producción propia",
    T ~ abastece
  )) %>% 
  filter(!is.na(segmento)) %>% 
  mutate(prop = prop.table(n)* 100) -> abastece
  
# disponibilidad
df %>% 
  rename(disponibilidad = en_su_opinion_hay_disponibilidad_de_alimentos_en_su_municipio) %>% 
  group_by(segmento, disponibilidad) %>% 
  count() %>% 
  filter(!is.na(segmento)) %>% 
  ungroup() %>% 
  group_split(segmento) %>% 
  map(., ~ mutate(., prop = prop.table(n)*100)) %>% 
  bind_rows() %>% 
  mutate_if(is.numeric, round, 0) -> disponibilidad

# productos escasos
df %>% 
  rename(escasea = que_grupo_de_alimentos_es_el_que_mas_escasea_y_al_que_le_cuesta_acceder_puede_marcar_mas_de_una_opcion) %>% 
  select(segmento, escasea) %>% 
  filter(!is.na(segmento))  %>% 
  separate_rows(escasea, sep = ",", convert = TRUE) %>% 
  separate_rows(escasea, sep = ";", convert = TRUE) %>% 
  mutate_if(is.character, trimws) %>% 
  filter(!is.na(escasea)) %>% 
  mutate(
    escasea = str_replace(escasea, "derivados y legumbres secas", "Legumbres secas"),
    escasea = str_replace(escasea, "grasas y azúcares", "Grasas y azúcares")
  ) %>% 
  group_by(segmento, escasea) %>% 
  count() %>% 
  ungroup() %>% 
  group_split(segmento) %>%
  map(., ~ mutate(., prop = prop.table(n)*100)) %>% 
  bind_rows() %>% 
  mutate_if(is.numeric, round, 0) -> escasez_productos
  
  
# dificultades
df %>% 
  rename(dificultades = actualmente_cuales_son_las_dificultades_mas_grandes_que_tienen_los_ciudadanos_en_el_municipio_para_abastecerse_puede_marcar_mas_de_una_opcion) %>% 
  select(segmento, dificultades) %>% 
  filter(!is.na(segmento))  %>% 
  separate_rows(dificultades, sep = ";", convert = TRUE) %>% 
  group_by(segmento, dificultades) %>% 
  count() -> dificultades
  
temp <- grep(pattern = "filas", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Largas filas"

temp <- grep(pattern = "Colas largas", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Largas filas"

	
temp <- grep(pattern = "precios", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Especulación y precios altos"

temp <- grep(pattern = "Subio un poco", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Especulación y precios altos"

temp <- grep(pattern = "Precio elevado", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Especulación y precios altos"

temp <- grep(pattern = "No hay dinero|Falta de ingresos" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Falta de ingresos" 

temp <- grep(pattern = "Falta de dinero" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Falta de ingresos" 

temp <- grep(pattern = "La gente no tiene dinero, vive del día, de la FRONTERA, ahora está cerrada\\." , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Falta de ingresos" 

temp <- grep(pattern = "dista" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Distancia a centros de abastecimiento" 

temp <- grep(pattern = "No hay dificultades en este momento, es época de cosecha y hay abundancia" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Ninguna" 

temp <- grep(pattern = "El abastecimiento con horarios obliga a un contacto lesivo" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Corto el tiempo para abastecimiento" 

temp <- grep(pattern = "Más perjudicados están en la zona periurbana o comunidades alejadas|Los que vivin en las comunidades alejadas de la provincias tienen dificultades para avastecerse de azucares fideos aceite y otros alimentos" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Distancia a centros de abastecimiento" 

temp <- grep(pattern = "Perjudicados están en la zona periurbana o comunidades alejadas" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Distancia a centros de abastecimiento" 

temp <- grep(pattern = "Ir a los mercados es un peligro inminente no se capacitó a las vendedoras para protegerse ni a los ciudadanos para respetar las normas, no hay control policial ni militar" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Higiene en la venta" 

temp <- grep(pattern = "Higiene de las vendedoras" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Higiene en la venta" 

temp <- grep(pattern = "Gente y supermercados no hacen cumplir las restricciones por el número deCI" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Falta de coordinación" 

dificultades %<>% 
  filter(!dificultades %in% c("Desigualdades sociales y familias que viven al día", "La gente que vive del dia morirá de hambre"))
		
# desabastecimiento
df %>% 
  rename(abril_15 = en_su_opinion_existen_suficientes_alimentos_en_el_municipio_hasta_el_15_de_abril) %>% 
  group_by(segmento, abril_15) %>% 
  count() %>% 
  filter(!is.na(segmento)) -> abril_15

# gam
df %>% 
  rename(gam = que_mecanismos_esta_adoptando_su_gobierno_municipal_para_garantizar_el_abastecimiento_de_alimentos) %>% 
  mutate(gam = str_replace(gam, "Coordinacion con productores locales", "Coordinación con productores locales")) %>% 
  group_by(segmento, gam) %>% 
  count() %>% 
  filter(!is.na(segmento)) -> gam

rm(temp, ine, segmento)











  
  




  
  
  
  
  



  
  
  














