### Librerias
devtools::load_all(path = "../morantviz/")
library(tidyverse)
library(scales)
################################### Ejemplo de barras (horizontales y verticales) ###################################

### Colores
colores <- tibble::tribble(
  ~respuesta  , ~color    , # Partidos
  "Facebook"  , "#A6032F" ,
  "Instagram" , "#0339a6" ,
  "TikTok"    , "#F27405"
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
  "Muy efectivos",
  "No considero que hay una diferencia",
  "Nada efectivos"
)

## Base de datos
bd_efectivo <- problemas(cat = categorias, llave = "efectivo", n = 100)


### Diccionario de problemas
dicc <- tibble::tribble(
  ~codigo    , ~pregunta                                                 , ~respuestas                                                        ,
  "efectivo" , "¿Qué tan efectivos son los reportes de la consultora?" , "Muy efectivos_No considero que hay una diferencia_Nada efectivos"
)

### Inicialización del objeto g
g <- Graficar$new(
  bd = bd_efectivo,
  diccionario = dicc,
  colores = colores,
  color_principal = "#340e63",
  tema = tema_morant()
)

g$contar_variables(
  variables = "efectivo",
  confint = F
)$pegar_diccionario()$pegar_color()$calcular_pct()$graficar_barras_h(
  x = "respuesta",
  y = "pct"
)

g$contar_variables(
  variables = "efectivo",
  confint = F
)$pegar_diccionario()$pegar_color()$calcular_pct()$graficar_barras_h(
  x = "respuesta",
  y = "pct",
  ancho_cap = 15,
  ancho_etiquetas = 5
)


################################### Ejemplo de graficar lineas ###################################

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
  rango_offset = 0.05
) +
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
    rango_offset = 0.05,
    ancho_cap = 5,
    ancho_etiquetas = 1
  )


################################### Ejemplo de graficar Waffle ###################################

dicc <- tibble::tribble(
  ~codigo                  , ~nombre                  , ~pregunta                             ,
  "conoce_pm_astiazaran"   , "Astiazarán"            , "Conoce o ha escuchado de (...)"      ,
  "conoce_pm_delrio"       , "Del Río"               , "Conoce o ha escuchado de (...)"      ,
  "conoce_pm_lia"          , "Lía Limón"            , "Conoce o ha escuchado de (...)"      ,
  "conoce_pm_javier"       , "Javier López Casarín" , "Conoce o ha escuchado de (...)"      ,
  "opinion_pm_astiazaran"  , "Astiazarán"            , "¿Cuál es su opinión sobre (...)?" ,
  "opinion_pm_delrio"      , "Del Río"               , "¿Cuál es su opinión sobre (...)?" ,
  "identificacion_partido" , ""                       , "¿Con que partido se identifica?"
)


colores <- tibble::tribble(
  ~respuesta                  , ~color      ,
  "Sí"                       , "#0c4c8a"   ,
  "No"                        , "#ecf0f1"   ,
  "Buena"                     , "#27ae60"   ,
  "Mala"                      , "#c0392b"   ,
  "Muy buena"                 , "#2ecc71"   ,
  "Muy mala"                  , "#e74c3c"   ,
  "Regular"                   , "#f1c40f"   ,
  "Ns/Nc"                     , "#95a5a6"   ,
  "MORENA"                    , "#B8385C"   ,
  "PAN"                       , "#0C3B8C"   ,
  "PRI"                       , "#2ECC71"   ,
  "PRD"                       , "#F39C12"   ,
  "Movimiento Ciudadano (MC)" , "#E67E22"   ,
  "PT"                        , "#C0392B"   ,
  "Partido Verde (PVEM)"      , "#069441ff" ,
  "Ninguno"                   , "#34495E"   ,
  "Otros"                     , "#c1cbccff"
)


g <- Encuesta$new(
  diseno = diseno_demo,
  diccionario = dicc,
  colores = colores,
  color_principal = "pink",
  tema = tema_morant()
)


g$contar_variables_porGrupos(
  variables = c(
    "conoce_pm_astiazaran",
    "conoce_pm_delrio",
    "conoce_pm_lia",
    "conoce_pm_javier"
  ),
  grupos = c("region"),
  confint = F
)$filtrar_respuesta(valor = c("Sí"))$pegar_diccionario()$pegar_color()

g$reordenar_columna(columna = "nombre", tipo = "manual", orden)
g$generar_coordenadas(
  eje_x = "region",
  eje_y = "nombre",
  valor = "media"
)

g$graficar_waffle(
  nombre_x = "Región",
  eje_x = "region",
  eje_y = "nombre",
  caption = F,
  ancho_cap = 15,
  ancho_etiquetas_x = 5,
  ancho_etiquetas_y = 5
)


################################### Barras divergentes ###################################

dicc <- tibble::tribble(
  ~codigo                  , ~nombre                  , ~pregunta                             ,
  "conoce_pm_astiazaran"   , "Astiazarán"            , "Conoce o ha escuchado de (...)"      ,
  "conoce_pm_delrio"       , "Del Río"               , "Conoce o ha escuchado de (...)"      ,
  "conoce_pm_lia"          , "Lía Limón"            , "Conoce o ha escuchado de (...)"      ,
  "conoce_pm_javier"       , "Javier López Casarín" , "Conoce o ha escuchado de (...)"      ,
  "opinion_pm_astiazaran"  , "Astiazarán"            , "¿Cuál es su opinión sobre (...)?" ,
  "opinion_pm_delrio"      , "Del Río"               , "¿Cuál es su opinión sobre (...)?" ,
  "identificacion_partido" , ""                       , "¿Con que partido se identifica?"
)


colores <- tibble::tribble(
  ~respuesta                  , ~color      ,
  "Sí"                       , "#0c4c8a"   ,
  "No"                        , "#ecf0f1"   ,
  "Buena"                     , "#27ae60"   ,
  "Mala"                      , "#c0392b"   ,
  "Muy buena"                 , "#2ecc71"   ,
  "Muy mala"                  , "#e74c3c"   ,
  "Regular"                   , "#f1c40f"   ,
  "Ns/Nc"                     , "#95a5a6"   ,
  "MORENA"                    , "#B8385C"   ,
  "PAN"                       , "#0C3B8C"   ,
  "PRI"                       , "#2ECC71"   ,
  "PRD"                       , "#F39C12"   ,
  "Movimiento Ciudadano (MC)" , "#E67E22"   ,
  "PT"                        , "#C0392B"   ,
  "Partido Verde (PVEM)"      , "#069441ff" ,
  "Ninguno"                   , "#34495E"   ,
  "Otros"                     , "#c1cbccff"
)


g <- Encuesta$new(
  diseno = diseno_demo,
  diccionario = dicc,
  colores = colores,
  color_principal = "pink",
  tema = tema_morant()
)


g$contar_variables(
  variables = c("opinion_pm_astiazaran", "opinion_pm_delrio"),
  confint = F
)$filtrar_respuesta(
  valor = c("Muy buena", "Buena", "Regular", "Mala", "Muy mala")
)$pegar_diccionario()$pegar_color()$reordenar_columna(
  columna = "respuesta",
  tipo = "manual",
  c(positivas, regular, negativas)
)$partir_regular(opcion = "Regular")$cambiarSigno_freq(
  negativo = c("Mala", "Muy mala")
)$etiquetar_regular(regular = "Regular", freq = "media")$reordenar_columna(
  columna = "nombre",
  tipo = "suma"
)$graficar_barras_divergente(
  x = "nombre",
  regular = "Regular",
  positivas = c("Buena", "Muy buena"),
  negativas = c("Mala", "Muy mala"),
  caption = T
)


# opinión barras divergente sin regular -----------------------------------

g$contar_variables(
  variables = c("opinion_pm_astiazaran", "opinion_pm_delrio"),
  confint = F
)$pegar_diccionario()$filtrar_respuesta(
  valor = c("Muy buena", "Buena", "Mala", "Muy mala")
)$pegar_color()$partir_regular(opcion = "Regular")$reordenar_columna(
  columna = "respuesta",
  tipo = "manual",
  c("Muy buena", "Buena", "Mala", "Muy mala")
)$cambiarSigno_freq(negativo = c("Mala", "Muy mala"))$reordenar_columna(
  columna = "nombre",
  tipo = "suma"
)$etiquetar_regular(regular = "", freq = "media")$graficar_barras_divergente(
  regular = NULL,
  positivas = c("Buena", "Muy buena"),
  negativas = c("Mala", "Muy mala"),
  caption = T,
  ancho_cap = 25,
  ancho_etiquetas = 1
)


# conocimiento con porcentaje ----------------------------------------------

g$contar_variables(
  variables = c("conoce_pm_astiazaran", "conoce_pm_delrio"),
  confint = F
)$filtrar_respuesta(
  valor = "Sí"
)$pegar_diccionario()$pegar_color()$envolver_etiquetas(
  columna = "nombre",
  ancho = 13
)$reordenar_columna(columna = "nombre", tipo = "manual", orden)


g$saldos_opinion(
  sufijo_opinion = "opinion_pm",
  cat_ns_nc = "Ns/Nc",
  sufijo_conoce = "conoce_pm",
  cat_conoce = "Sí",
  actores = c("astiazaran", "delrio"),
  positivas = c("Muy buena", "Buena"),
  negativas = c("Mala", "Muy mala"),
  regular = "Regular",
  y = "media",
  letra_tam = 7,
  letra_tam_con = 20,
  ancho_etiquetas = 1,
  ancho_ns_nc = 2
)


################################### Barras divergentes ###################################

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
)$pegar_diccionario()$pegar_color()$calcular_pct()$barras_apiladas(
  x = "nombre",
  caption = T,
  ancho_cap = 20,
  ancho_etiquetas = 5
)


################################### Lollipops ###################################

dicc <- tibble::tribble(
  ~codigo                  , ~nombre                  , ~pregunta                             ,
  "conoce_pm_astiazaran"   , "Astiazarán"            , "Conoce o ha escuchado de (...)"      ,
  "conoce_pm_delrio"       , "Del Río"               , "Conoce o ha escuchado de (...)"      ,
  "conoce_pm_lia"          , "Lía Limón"            , "Conoce o ha escuchado de (...)"      ,
  "conoce_pm_javier"       , "Javier López Casarín" , "Conoce o ha escuchado de (...)"      ,
  "opinion_pm_astiazaran"  , "Astiazarán"            , "¿Cuál es su opinión sobre (...)?" ,
  "opinion_pm_delrio"      , "Del Río"               , "¿Cuál es su opinión sobre (...)?" ,
  "identificacion_partido" , ""                       , "¿Con que partido se identifica?"
)


colores <- tibble::tribble(
  ~respuesta                  , ~color      ,
  "Sí"                       , "#0c4c8a"   ,
  "No"                        , "#ecf0f1"   ,
  "Buena"                     , "#27ae60"   ,
  "Mala"                      , "#c0392b"   ,
  "Muy buena"                 , "#2ecc71"   ,
  "Muy mala"                  , "#e74c3c"   ,
  "Regular"                   , "#f1c40f"   ,
  "Ns/Nc"                     , "#95a5a6"   ,
  "MORENA"                    , "#B8385C"   ,
  "PAN"                       , "#0C3B8C"   ,
  "PRI"                       , "#2ECC71"   ,
  "PRD"                       , "#F39C12"   ,
  "Movimiento Ciudadano (MC)" , "#E67E22"   ,
  "PT"                        , "#C0392B"   ,
  "Partido Verde (PVEM)"      , "#069441ff" ,
  "Ninguno"                   , "#34495E"   ,
  "Otros"                     , "#c1cbccff"
)


g <- Encuesta$new(
  diseno = diseno_demo,
  diccionario = dicc,
  colores = colores,
  color_principal = "pink",
  tema = tema_morant()
)


g$contar_variables(
  variables = c("opinion_pm_delrio"),
  confint = F
)$pegar_diccionario()$pegar_color()$graficar_lollipops("respuesta") +

  g$contar_variables(
    variables = c("opinion_pm_delrio"),
    confint = F
  )$pegar_diccionario()$pegar_color()$graficar_lollipops(
    "respuesta",
    ancho_cap = 15,
    ancho_etiquetas = 1
  )
