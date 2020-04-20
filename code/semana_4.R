# sobre: limpieza general de la base estandarizada en google forms para generación gráfica mas eficiente
library(tidyverse)
library(magrittr)
library(janitor)
library(waffle)

# cargado de bases
df <- read_csv("input/semana_4/Percepciones ciudadanas acerca del Abastecimiento de alimentos durante la Emergencia Sanitaria COVID-19.csv")

df %<>% 
  clean_names() %>% 
  slice(-(1:2033))
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

abastece$abastece %<>% gsub("soper", "Supermercado", .)


temp <- grep(pattern = "product", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Directo de productores"

temp <- grep(pattern = "Canastas campesinas que llevan a domicilio|Canastas campesinas que llevan a domicilio", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Directo de productores"

temp <- grep(pattern = "Mercado Campesino", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Directo de productores"

temp <- grep(pattern = "feria", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Ferias"

temp <- grep(pattern = "Almacén de zona rural", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Tiendas de barrio"


temp <- grep(pattern = "Mercado mutualista", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Mercados Barrial"

temp <- grep(pattern = "Mercados Barrial", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Mercado Barrial"


temp <- grep(pattern = "camion|movil|móvil", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Camiones o vendedores en vehiculos"

temp <- grep(pattern = "vende", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Camiones o vendedores en vehiculos"

temp <- grep(pattern = "Delivery|Pues me trae alimentos una doctorita aveses|Servicio de catering|Traen a domicilio", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Pedidos por aplicación"

temp <- grep(pattern = "puesto de venta de la zona", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Tiendas de barrio"


abastece %<>% 
  filter(!abastece %in% c("trabajo", "CH", "ch", "Bolognia", "Desde La Paz", "En el mercado de mi zona y en esta epoca mucho mas en los alrededores",  
                          "Labores de casa", "Limpieza", "No puedo  trabajar xq tengo un bb en casa", "Pero casi no boy por que no estoy trabajando",
                          "Pero ya no hay", "Trabajo")) 



temp <- (which(abastece$abastece == "D"))
abastece <- abastece[-temp, ]

temp <- grep(pattern = "La Paz", x = abastece$abastece, ignore.case = T)
abastece <- abastece[-temp, ]

temp <- grep(pattern = "Bolognia", x = abastece$abastece, ignore.case = T)
abastece <- abastece[-temp, ]


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


temp <- grep(pattern = "dinero", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Falta de ingresos"

temp <- grep(pattern = "No hay plata", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Falta de ingresos"


temp <- grep(pattern = "No tienen recursos economicos|Económico", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Falta de ingresos"

temp <- grep(pattern = "filas", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Largas filas"


temp <- grep(pattern = "Las colas son muy largas, y no te alcanza el tiempo para las compras", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Especulación y precios altos"

temp <- grep(pattern = "prec", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Especulación y precios altos"

temp <- grep(pattern = "especulación", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Especulación y precios altos"

temp <- grep(pattern = "El huevo esta muy caro|TODO ES CARISIMO.", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Especulación y precios altos"

temp <- grep(pattern = "distancia|lejos", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Distancia a los centro de abasto"

temp <- grep(pattern = "ningun" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Ninguna" 


temp <- grep(pattern = "No hay dificultad.|no hay desabastecimiento aún" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Ninguna" 


temp <- grep(pattern = "Artículos de aseo|Hay algunas cosas que se acaban rápido|Los productores ya no cuentan con insumos|No hay mucha opcion de comida vegetariana \\(ej\\. carne de soya\\)" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Algunos productos ya no están disponibles" 

temp <- grep(pattern = "No venden gasolina para trasladarnosa los centros de abasto" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Restricción vehicular" 

temp <- grep(pattern = "Los vendedores no llegan al lugar y si llegan es con poca variedad de productos y caros" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Restricción vehicular" 


temp <- grep(pattern = "No hay productos de limpieza lavandina ni alcohol en gel preocupante" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Algunos productos ya no están disponibles" 

temp <- grep(pattern = "No hay gas" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Algunos productos ya no están disponibles" 

temp <- grep(pattern = "Tengo peones en el campo no ay alimento para perros y gatos" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Algunos productos ya no están disponibles" 

temp <- grep(pattern = "Trabajos  tds desempleados" , x = dificultades$dificultades, ignore.case = T)
dificultades <- dificultades[-temp, ]

temp <- grep(pattern = "Culpa de la sosa cerro mercados" , x = dificultades$dificultades, ignore.case = T)
dificultades <- dificultades[-temp, ]

temp <- grep(pattern = "Falta de cuidado en el manejo de los alimentos, puestos en el suelo." , x = dificultades$dificultades, ignore.case = T)
dificultades <- dificultades[-temp, ]

temp <- grep(pattern = "que los mayores no pueden salir" , x = dificultades$dificultades, ignore.case = T)
dificultades <- dificultades[-temp, ]

temp <- grep(pattern = "Estamos en limite de dos municipios ,alfinal del municipio de scz -inicio del municipio de la guardia .al parecer es esta situacion hace que nos dejen en el olvido respecto a todo tipo de ayuda o a los mercado moviles y otros." , x = dificultades$dificultades, ignore.case = T)
dificultades <- dificultades[-temp, ]

temp <- which(dificultades$dificultades == "I")
dificultades <- dificultades[-temp, ]

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

