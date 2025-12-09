######################################################################
# Graficar líneas
######################################################################

#' Graficar líneas por grupo con etiquetas de porcentaje
#'
#' - Cada grupo se dibuja con su propio color y línea.
#' - Las etiquetas de porcentaje se muestran en cada punto.
#' - Para evitar que las etiquetas se encimen cuando los valores son
#'   similares, se aplica un pequeño offset vertical
#'   (`rango_offset`) dentro de cada categoría del eje X.
#' - Opcionalmente permite filtrar qué grupos se grafican.
#'
#' @param x Nombre de la columna que se usa en el eje X
#'   (por defecto `"respuesta"`).
#' @param freq Nombre de la columna numérica que se usa en el eje Y
#'   (por ejemplo `"pct"` o `"media"`).
#' @param grupo Nombre de la columna que identifica los grupos
#'   (por ejemplo `"nombre"` o `"codigo"`).
#' @param grupos_seleccion Vector opcional con los valores de `grupo`
#'   que se quieren mostrar. Si es `NULL`, se grafican todos.
#' @param vjust Ajuste vertical de las etiquetas de texto.
#' @param size Tamaño de la tipografía de las etiquetas.
#' @param rango_offset Desplazamiento vertical máximo (en unidades de
#'   `freq`) que se usa para separar las etiquetas dentro de cada
#'   categoría del eje X. Valores típicos: 0.01–0.02.
#'
#' @return Un objeto `ggplot` guardado en `self$grafica`.
#'
#' @details
#' La función asume que:
#' - `self$tbl` contiene los datos, con al menos las columnas
#'   indicadas en `x`, `freq`, `grupo` y `color`.
#' - `self$tbl$pregunta[1]` contiene el texto de la pregunta para
#'   usarlo como `caption` de la gráfica.

### Librerias
devtools::load_all(path = "../morantviz/")
library(rlang)
library(dplyr)
library(purrr)
library(ggrepel)


################################### Graficar líneas para diferentes llaves ###################################
#
# Para este caso cada llave representa un línea de color diferente. Para definir el color correspondiente, en la
# paleta de colores se puede especificar el color deseado con la llave o con el nombre

### Colores
colores <- tibble::tribble(
  ~respuesta            , ~color      ,
  "Álvaro Obregón"    , "#0c4c8a"   ,
  "Venustiano Carranza" , "#95a5a6"   ,
  "Iztapalapa"          , "#1b8a0cff"
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
  "Baches en calles y avenidas",
  "Delincuencia",
  "Falta de agua",
  "Falta de alumbrado",
  "Falta de recolección de basura",
  "Falta de servicios de salud",
  "Ns/Nc",
  "Otro",
  "Parques y jardines descuidados",
  "Tráfico vehicular",
  "Transporte público deficiente"
)

## Base de datos
bd_problemas_ao <- problemas(cat = categorias, llave = "problemas_ao", n = 100)
bd_problemas_vc <- problemas(categorias, llave = "problemas_vc", n = 100)
bd_problemas_iz <- problemas(categorias, llave = "problemas_iz", n = 100)


## Join de las bases de datos
bd_respuestas <- list(
  bd_problemas_ao,
  bd_problemas_vc,
  bd_problemas_iz
) |>
  reduce(full_join, by = "id")


### Diccionario de problemas
dicc <- tibble::tribble(
  ~codigo        , ~nombre               , ~pregunta                                                         ,
  "problemas_ao" , "Álvaro Obregón"    , "¿Cúal considera que es el problema principal de la alcaldía?" ,
  "problemas_vc" , "Venustiano Carranza" , "¿Cuál considera que es el problema principal de la alcaldía?" ,
  "problemas_iz" , "Iztapalapa"          , "¿Cuál considera que es el problema principal de la alcaldía?" ,
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
  variables = c("problemas_ao", "problemas_vc", "problemas_iz")
)$pegar_diccionario()$calcular_pct()$pegar_colorPorGrupo(
  "nombre"
)$filtrar_respuesta(
  valor = c(
    "Baches en calles y avenidas",
    "Falta de agua",
    "Falta de recolección de basura"
  )
)$graficar_lineas(
  x = "respuesta",
  freq = "pct",
  grupo = "nombre",
  grupos_seleccion = c("Álvaro Obregón", "Venustiano Carranza", "Iztapalapa"),
  letra_tam = 8,
  rango_offset = 0.03
)


################################### Graficar líneas para una misma llave y diferemtes respuestas ###################################

#Para este caso se considera solo una llave y diferentes respuestas

## Función de respuestas aleatorias (ligeramente diferente a la de arriba)
problemas <- function(cat, llave, n) {
  seleccion <- sample(cat, n, replace = T)
  tibble(
    !!sym(llave) := seleccion
  )
}
## Respuestas
categorias <- c(
  "Aprueba mucho",
  "Aprueba poco",
  "Desaprueba poco",
  "Desaprueba mucho"
)

## Generación de las respuestas para el caso de aprobacióm de Rosario entre el 2024 y 2026

bd_aprob_rosario_2024 <- problemas(
  cat = categorias,
  llave = "aprob_rosario",
  n = 100
) |>
  mutate(año = 2024)
bd_aprob_rosario_2025 <- problemas(
  cat = categorias,
  llave = "aprob_rosario",
  n = 100
) |>
  mutate(año = 2025)
bd_aprob_rosario_2026 <- problemas(
  cat = categorias,
  llave = "aprob_rosario",
  n = 100
) |>
  mutate(año = 2026)


## binding de las respuestas por año
bd_total_rosario <- bind_rows(
  bd_aprob_rosario_2024,
  bd_aprob_rosario_2025,
  bd_aprob_rosario_2026
) |>
  mutate(id = row_number()) |>
  relocate(id, .before = everything()) |>
  select(-matches("año\\.")) %>%
  mutate(año = as.character(año))


### Diccionario
dicc <- tibble::tribble(
  ~codigo         , ~pregunta                                      ,
  "aprob_rosario" , "¿Qué le parece la gorbernatura de Rosario?"
)

## Colores de las respuestas
colores <- tibble::tribble(
  ~respuesta         , ~color      ,
  "Aprueba mucho"    , "#1b8a0cff" ,
  "Aprueba poco"     , "#0c8a7fff" ,
  "Desaprueba poco"  , "#67b4d7ff" ,
  "Desaprueba mucho" , "#ae2e27ff"
)


### Inicialización de la g
g <- Graficar$new(
  bd = bd_total_rosario,
  diccionario = dicc,
  colores = colores,
  color_principal = "#340e63",
  tema = tema_morant()
)

## Utilización de contar variables por grupos para separarlas por año
g$contar_variables_porGrupos(
  variables = c("aprob_rosario"),
  grupos = c("año")
)$pegar_diccionario()$calcular_pct()$pegar_color()$graficar_lineas(
  x = "año",
  freq = "pct",
  grupo = "respuesta",
  letra_tam = 7,
  rango_offset = 0.037
)


### Otras formas de graficar usando facet_wrap(~ "especificar columna")
g$contar_variables_porGrupos(
  variables = c("aprob_rosario"),
  grupos = c("año")
)$pegar_diccionario()$calcular_pct()$pegar_color()$graficar_lineas(
  x = "año",
  freq = "pct",
  grupo = "respuesta",
  letra_tam = 7,
  rango_offset = 0.037
)

g$contar_variables_porGrupos(
  variables = c("aprob_rosario"),
  grupos = c("año")
)$pegar_diccionario()$calcular_pct()$pegar_color()$graficar_lineas(
  x = "año",
  freq = "pct",
  grupo = "respuesta",
  letra_tam = 7,
  rango_offset = 0.00
) +
  facet_wrap(~respuesta)

## Agregar esto para quitar el eje Y cuando se quiera graficar dos gráficas

#theme(
#    axis.title.y  = element_blank(),
#    axis.text.y   = element_blank(),
#    axis.ticks.y  = element_blank())
