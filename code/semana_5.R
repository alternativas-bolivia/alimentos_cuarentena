# sobre: limpieza general de la base estandarizada en google forms para generación gráfica mas eficiente
library(tidyverse)
library(magrittr)
library(janitor)
library(waffle)

# cargado de bases
df <- read_csv("input/semana_5/Percepciones ciudadanas acerca del Abastecimiento de alimentos durante la Emergencia Sanitaria COVID-19.csv")

df %<>% 
  clean_names() %>% 
  slice(-(1:2967))
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

abastece$abastece %<>% gsub("Hipermaxi|Súper Mercado", "Supermercado", .)

temp <- grep(pattern = "feri", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Ferias"

temp <- grep(pattern = "Tiendas , mercado y vecinos que traen productos", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Tiendas de barrio"

temp <- grep(pattern = "Mercado barrial y producción propia|Mercado Calatayud|Mercado central Rodriguez y de barrio", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Mercados Barrial"

temp <- grep(pattern = "Mercados Barrial|Mercado 1ro de marzo|Mercado de otro barrio y productores ambulantes", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Mercado Barrial"

temp <- grep(pattern = "Mercado de riberalta|Mercado municipal y barrial|Nuevo mercado los pozos", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Mercado Municipal"

temp <- grep(pattern = "mov|móv", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Camiones o vendedores en vehiculos"

temp <- grep(pattern = "Camionetas q revenden los productos|Llegan vehículos con productos|Mercado ambulante|Pasan por ni zona diferentrs distribuidores\\.", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Camiones o vendedores en vehiculos"

temp <- grep(pattern = "Camionetas q revenden los productos|Llegan vehículos con productos|Mercado ambulante|Pasan por ni zona diferentrs distribuidores\\.", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Camiones o vendedores en vehiculos"

temp <- grep(pattern = "Vendedores de la Alcaldia en el barrio, delivery", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Camiones o vendedores en vehiculos"

temp <- grep(pattern = "Envió a domicilio|Mi barrio hace pedidos|Online|PedidosYa delivery", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Pedidos por aplicación"

temp <- grep(pattern = "Huertos periurbanos El Alto", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Producción propia"

temp <- grep(pattern = "Ambulantes", x = abastece$abastece, ignore.case = T)
abastece[temp, "abastece"] <- "Camiones o vendedores en vehiculos"

abastece %<>% 
  filter(!abastece %in% c("Vivo en el penal de San Roque", "ama de casa", "Ama de casa", "En las casas",
                          "Nnn", "No llega víveres, ni productos necesarios para higiene", "No tengo dinero para comprar nada",
                          "Rio mulato", "Soy estudiante me encuentro sola en la ciudad mis padres se encuentran en el campo y ellos alla con lo que cosechan se estan abasteciendo",
                          "TODO CARISIMO", "Vivo en el penal de San Roque")) 


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

temp <- grep(pattern = "Falta de recursos económicos para comprar|Familias que no acceden sin RREE|Limitados recursos", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Falta de ingresos"


temp <- grep(pattern = "No hay plata|Por factor económico|Varias personas en mi barrio viven del dia y no son asalariados|Economíca|Falta de recursos para conprar", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Falta de ingresos"


temp <- grep(pattern = "precios", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Especulación y precios altos"

temp <- grep(pattern = "Han subido mucho|Q paso todo caro|TODO ESTÁ CARISIMO NO ALCANSA EL BOLSILLO... DEBERÍAN TOMAR CARTAS EN EL ASUNTO LA GENTE QUE ENVUAN EN CAMIONES SE ESTÁN APROVECHANDO DEL PUEBLO... PORFAVOR FÍJENSE", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Especulación y precios altos"

temp <- grep(pattern = "Todo está CARISIMO porfavor ayuden al pueblo|Sube el precio paulatinamente y por ello mantienen alimentos a la venta en mal estado y a veces podrido y t venden recién a un muy bajo precio, comida enlatada como atún o sardinas leche evaporada etc desaparece y quien tiene lo vende muy caro", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Especulación y precios altos"

temp <- grep(pattern = "dist|lej", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Distancia a los centro de abasto"

temp <- grep(pattern = "No  hay  supermercados,  mayptistas  cerca|no hay mercado cerca por mi barrio para abastecerse lo cerraron el que quedaba mas cerca clausurado|Gente de la zonas leganas deberían de contar con mercados móviles 1 ves por Semama", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Distancia a los centro de abasto"


temp <- grep(pattern = "No  hay  supermercados,  mayptistas  cerca|no hay mercado cerca por mi barrio para abastecerse lo cerraron el que quedaba mas cerca clausurado|Gente de la zonas leganas deberían de contar con mercados móviles 1 ves por Semama", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Distancia a los centro de abasto"

temp <- grep(pattern = "Vivimos en el cantón paurito serca de cotoca|No llega al pueblo nada, la gente viajaba a Uyuni normalmente para abastecerse pero ahora que no se puede la gente no tiene donde comprar lo que le hace falta", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Distancia a los centro de abasto"

temp <- grep(pattern = "No llegan a la zona con esos productos", x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Distancia a los centro de abasto"


temp <- grep(pattern = "A Pando llega la verdura y frutas desde Perú y Brasil, ahora no están pasando. No contamos con productos de industria nacional porque el costo es muy elevado por el transporte\\." , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Algunos productos ya no están disponibles" 

temp <- grep(pattern = "El mercado esta cerrado|No hay materiales de limpieza como lavandina|Cloro|riberalta" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Algunos productos ya no están disponibles" 

temp <- grep(pattern = "No hay recursos económicos" , x = dificultades$dificultades, ignore.case = T)
dificultades[temp, "dificultades"] <- "Falta de ingresos" 

temp <- grep(pattern = "Baja calidad de alimentos nutritivos|El problema es la doña Sosa|Están vendiendo el pollo podrido y el pan con moho|Kara Kara alto pampa san Miguel" , x = dificultades$dificultades, ignore.case = T)
dificultades <- dificultades[-temp, ]

temp <- grep(pattern = "Mucha gente sin medidas de Bio seguridad|No hay ayuda en el Penal de San Roque, hay restricción de visitas|No vienen las ferias a mi zona\\.|Poder salir un solo día de compras" , x = dificultades$dificultades, ignore.case = T)
dificultades <- dificultades[-temp, ]

temp <- grep(pattern = "Enfermedad|huertas" , x = dificultades$dificultades, ignore.case = T)
dificultades <- dificultades[-temp, ]

# desabastecimiento
df %>% 
  rename(abril_15 = en_su_opinion_existen_suficientes_alimentos_en_el_municipio_hasta_el_final_de_la_cuarentena) %>% 
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

