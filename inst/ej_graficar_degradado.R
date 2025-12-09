library(encuestar) #si se necesita para el diseño muestral
library(survey)
#library(morantviz)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggsankey)
library(stringr)
library(tidyr)
library(ggalluvial)


devtools::load_all(path = "../morantviz/")

# Dicc del ejemplo
dicc <- tibble::tribble(
  ~codigo                 , ~nombre                  , ~pregunta                             ,
  "conoce_pm_astiazaran"  , "Astiazarán"            , "Conoce o ha escuchado de (...)"      ,
  "conoce_pm_delrio"      , "Del Río"               , "Conoce o ha escuchado de (...)"      ,
  "conoce_pm_lia"         , "Lía Limón"            , "Conoce o ha escuchado de (...)"      ,
  "conoce_pm_javier"      , "Javier López Casarín" , "Conoce o ha escuchado de (...)"      ,
  "opinion_pm_astiazaran" , "Astiazarán"            , "¿Cuál es su opinión sobre (...)?" ,
  "opinion_pm_delrio"     , "Del Río"               , "¿Cuál es su opinión sobre (...)?"
)

### Colores
colores <- tibble::tribble(
  ~respuesta  , ~color    ,
  "Sí"       , "#0c4c8a" ,
  "No"        , "#ecf0f1" ,
  "Buena"     , "#27ae60" ,
  "Mala"      , "#c0392b" ,
  "Muy buena" , "#2ecc71" ,
  "Muy mala"  , "#e74c3c" ,
  "Regular"   , "#f1c40f" ,
  "Ns/Nc"     , "#95a5a6" ,
  "M"         , "#2980b9" ,
  "F"         , "#8e44ad" ,
  "18A24"     , "#16a085" ,
  "25A39"     , "#f39c12" ,
  "40A59"     , "#d35400" ,
  "60YMAS"    , "#2c3e50"
)


## Inicialización
g <- Encuesta$new(
  diseno = encuesta_demo$muestra$diseno,
  diccionario = dicc,
  colores = colores,
  color_principal = "pink",
  tema = tema_morant()
)

################################### degradado continuo ###################################

## Paleta de colores para el degradado continuo
colores_base <- c("#FEE0D2", "#FC9272", "#DE2D26")


g$contar_variables(
  variables = c(
    "conoce_pm_astiazaran",
    "conoce_pm_delrio",
    "conoce_pm_lia",
    "conoce_pm_javier"
  ),
  confint = F
)$filtrar_respuesta(
  valor = "Sí"
)$pegar_diccionario()$pegar_color()$envolver_etiquetas(
  columna = "nombre",
  ancho = 13
)$reordenar_columna(
  columna = "nombre",
  tipo = "manual",
  "Astiazarán",
  after = 1
)$reordenar_columna(columna = "nombre", tipo = "asc")$degradado_continuo(
  colores_base,
  col_max = "#27ae60",
  freq = 'media'
)$graficar_barras_h(x = "nombre") ## Color maximo
