

usethis::use_readme_rmd( open = FALSE )
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
dicc <- tibble::tribble(~codigo, ~nombre, ~pregunta,
                        "conoce_pm_astiazaran", "Astiazarán", "Conoce o ha escuchado de (...)",
                        "conoce_pm_delrio", "Del Río", "Conoce o ha escuchado de (...)",
                        "conoce_pm_lia", "Lía Limón", "Conoce o ha escuchado de (...)",
                        "conoce_pm_javier", "Javier López Casarín", "Conoce o ha escuchado de (...)",
                        "opinion_pm_astiazaran", "Astiazarán","¿Cuál es su opinión sobre (...)?",
                        "opinion_pm_delrio", "Del Río","¿Cuál es su opinión sobre (...)?"
)

### Colores
colores <- tibble::tribble(
  ~respuesta,      ~color,
  "Sí",            "#0c4c8a",
  "No",            "#ecf0f1",
  "Buena",         "#27ae60",
  "Mala",          "#c0392b",
  "Muy buena",     "#2ecc71",
  "Muy mala",      "#e74c3c",
  "Regular",       "#f1c40f",
  "Ns/Nc",         "#95a5a6"
)


## Inicialización
g <- Encuesta$new(diseno = encuesta_demo$muestra$diseno,
                diccionario = dicc,
                colores = colores,
                color_principal = "pink",
                tema = tema_morant())


################################### graficar lineas clasificación ###################################

g$
  contar_variables(variables = c("problema_principal"), confint = F)$
  filtrar_respuesta(valor = c("Falta de agua","Falta de servicios de salud","Falta de alumbrado","Tráfico vehicular"))$
  pegar_diccionario()$
  pegar_color()$
  graficar_lineas(x ="respuesta") 




################################### graficar varias lineas de clasificacion ###################################

### Definimos el color para la columna que queramos editar, en este caso nombre
colores_candidatos <- tibble::tribble(
  ~nombre,                ~color,
  "Astiazarán",           "#9D7AD2",  # morado
  "Del Río",              "#27ae60",  # verde
  "Lía Limón",            "#e74c3c",  # rojo
  "Javier López Casarín", "#f1c40f"   # amarillo
)

# modificamos los colores que se habían modificado
g$colores <- colores_candidatos

g$
  contar_variables(variables = c("conoce_pm_astiazaran", "conoce_pm_delrio", "conoce_pm_lia", "conoce_pm_javier"), confint = F)$
  pegar_diccionario()$
  pegar_color(columna ="nombre")$ # Solo pegamos con respecto a la columna que queremos
  graficar_lineas(x ="respuesta") 
