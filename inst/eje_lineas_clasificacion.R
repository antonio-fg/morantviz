################################### graficar lineas clasificación ###################################

usethis::use_readme_rmd(open = FALSE)
library(encuestar) #si se necesita para el diseño muestral
library(survey)
#library(morantviz)
library(dplyr)
library(ggplot2)
library(patchwork)


## Dicc del ejemplo
dicc <- tibble::tribble(
  ~codigo                 , ~nombre                  , ~pregunta                             ,
  "conoce_pm_astiazaran"  , "Astiazarán"            , "Conoce o ha escuchado de (...)"      ,
  "conoce_pm_delrio"      , "Del Río"               , "Conoce o ha escuchado de (...)"      ,
  "conoce_pm_lia"         , "Lía Limón"            , "Conoce o ha escuchado de (...)"      ,
  "conoce_pm_javier"      , "Javier López Casarín" , "Conoce o ha escuchado de (...)"      ,
  "opinion_pm_astiazaran" , "Astiazarán"            , "¿Cuál es su opinión sobre (...)?" ,
  "opinion_pm_delrio"     , "Del Río"               , "¿Cuál es su opinión sobre (...)?"
)


g <- Encuesta$new(
  diseno = encuesta_demo$muestra$diseno,
  diccionario = dicc,
  colores = NULL,
  color_principal = "pink",
  tema = tema_morant()
)

g$contar_variables(
  variables = c(
    "conoce_pm_astiazaran",
    "conoce_pm_delrio",
    "conoce_pm_lia",
    "conoce_pm_javier"
  ),
  confint = F
)
g$graficar_lineas_clasificacion(titulo = 'Líneas de clasificación')
#g$graficar_lineas_clasificacion(x_var='respuesta',y_var='media')

g$tbl
