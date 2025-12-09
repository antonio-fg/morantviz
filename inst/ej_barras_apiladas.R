######################################################################
# Barras apiladas
######################################################################

#' Graficar barras apiladas horizontales por categoría
#'
#' - Cada barra representa una categoría (por ejemplo, el nombre de
#'   una persona/candidato).
#' - Dentro de cada barra, los segmentos apilados corresponden a los
#'   niveles de `fill` (por ejemplo, partidos políticos).
#' - Los colores de cada segmento se toman de la columna `color` y se
#'   convierten en una paleta nombrada para asegurar un mapeo estable
#'   (misma categoría → mismo color).
#' - Se muestran etiquetas de porcentaje dentro de cada segmento,
#'   solamente cuando el valor es mayor o igual a 5 %.
#' - La leyenda de colores se muestra al pie de la gráfica.
#'
#' @param x Nombre de la columna categórica que define cada barra
#'   (por ejemplo `"nombre"`).
#' @param freq Nombre de la columna numérica que se usa como altura
#'   acumulada de los segmentos (por ejemplo `"pct"`). Debe estar en
#'   escala de 0 a 1 si se quiere interpretar como porcentaje.
#' @param letra_tam Tamaño de la tipografía para las etiquetas de
#'   porcentaje dentro de los segmentos.
#' @param fill Nombre de la columna categórica que define los
#'   segmentos apilados dentro de cada barra (por ejemplo
#'   `"respuesta"`).
#'
#' @return Un objeto `ggplot` guardado en `self$grafica`.
#'
#' @details
#' La función asume que:
#' - `self$tbl` contiene los datos, con al menos las columnas
#'   indicadas por `x`, `freq`, `fill` y `color`.
#' - La columna `color` tiene un código de color (hex o nombre R)
#'   asociado a cada nivel de `fill`. Internamente se construye un
#'   vector nombrado `fill -> color` usando `distinct()` y
#'   `deframe()` para garantizar un mapeo consistente en la escala
#'   manual de relleno.
#' - `self$diccionario$pregunta[1]` contiene el texto de la pregunta
#'   que se usará como `caption` de la gráfica.
#' - `self$tema` es un objeto de tema (por ejemplo creado con
#'   `theme()` o una función propia) que define el estilo base de la
#'   gráfica. La función fuerza adicionalmente que la leyenda de
#'   relleno se muestre en la parte inferior y sin título.

library(tidyverse)
library(scales)
devtools::load_all(path = "../morantviz/")

### Colores
colores <- tibble::tribble(
  ~respuesta                  , ~color    , # Partidos
  "MORENA"                    , "#A6032F" ,
  "PAN"                       , "#0339a6" ,
  "Movimiento Ciudadano (MC)" , "#F27405" ,
  "PRI"                       , "#038C33" ,
  "Partido Verde (PVEM)"      , "#98BF5E" ,
  "PT"                        , "#D91136" ,
  "Ninguno"                   , "black"   ,
  "Ns/Nc"                     , "gray60"
)

## Función para generar respuestas aleatorias basada en una lista de categorías
problemas <- function(cat, llave, n) {
  seleccion <- sample(cat, n, replace = T)
  tibble(
    id = seq_len(n),
    !!sym(llave) := seleccion
  )
}

## Lista de categorias
categorias <- c(
  "Ns/Nc",
  "PAN",
  "PRI",
  "Partido Verde (PVEM)",
  "Movimiento Ciudadano (MC)",
  "Ninguno",
  "PT",
  "MORENA"
)

## Base de datos
bd_cruz_rel <- problemas(cat = categorias, llave = "cruz_rel", n = 100)
bd_andrea_rel <- problemas(cat = categorias, llave = "andrea_rel", n = 100)
bd_juan_rel <- problemas(cat = categorias, llave = "juan_rel", n = 100)
bd_chavez_rel <- problemas(cat = categorias, llave = "chavez_rel", n = 100)


## Join de las bases de datos
bd_respuestas <- list(
  bd_cruz_rel,
  bd_andrea_rel,
  bd_juan_rel,
  bd_chavez_rel
) |>
  reduce(full_join, by = "id")


### Diccionario de problemas
dicc <- tibble::tribble(
  ~codigo      , ~nombre                  , ~pregunta                                                          , ~respuestas                                                                           ,
  "cruz_rel"   , "Cruz Pérez Cuéllar"   , "¿Con qué partido político relaciona a (...) en la actualidad?" , "Ns/Nc_PAN_PRI_Partido Verde (PVEM)_Movimiento Ciudadano (MC)_Ninguno_PT_MORENA_otra" ,
  "andrea_rel" , "Andea Chávez"          , "¿Con qué partido político relaciona a (...) en la actualidad?" , "Ns/Nc_PAN_PRI_Partido Verde (PVEM)_Movimiento Ciudadano (MC)_Ninguno_PT_MORENA_otra" ,
  "juan_rel"   , "Juan Carlos Loera"      , "¿Con qué partido político relaciona a (...) en la actualidad?" , "Ns/Nc_PAN_PRI_Partido Verde (PVEM)_Movimiento Ciudadano (MC)_Ninguno_PT_MORENA_otra" ,
  "chavez_rel" , "Layra Chávez Jiménez" , "¿Con qué partido político relaciona a (...) en la actualidad?" , "Ns/Nc_PAN_PRI_Partido Verde (PVEM)_Movimiento Ciudadano (MC)_Ninguno_PT_MORENA_otra"
)

### Inicialización del objeto g
g <- Graficar$new(
  bd = bd_respuestas,
  diccionario = dicc,
  colores = colores,
  color_principal = "#340e63",
  tema = tema_morant()
)


g$contar_variables(
  variables = c("cruz_rel", "andrea_rel", "juan_rel", "chavez_rel")
)$pegar_diccionario()$pegar_color()$calcular_pct()$barras_apiladas(x = "nombre")


################################### Graficas apiladas con saldo ###################################

barras_apiladas <- g$contar_variables(
  variables = c("cruz_rel", "andrea_rel", "juan_rel", "chavez_rel")
)$pegar_diccionario()$pegar_color()$calcular_pct()$barras_apiladas(x = "nombre")


ns_nc <- g$filtrar_respuesta(valor = "Ns/Nc")$graficar_barras_h(
  x = "nombre",
  y = "pct"
) +
  theme_void() +
  labs(caption = NULL, title = "No sabe / No contesta") +
  theme(text = element_text(family = "Poppins"))


barras_apiladas + ns_nc + plot_layout(widths = c(3, 1))

##widths = c(3, 1) significa:
#la gráfica de la izquierda ocupa 3 partes
#la de la derecha 1 parte
#total 4 partes; izquierda ≈ 75%, derecha ≈ 25%, el cambio tambien es continuo, entonces se puede usar 2.8 o valores similares
