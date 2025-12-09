######################################################################
# Parametrización de visualizaciones
######################################################################

# La actualización de la parametrización de las visualizaciones permite modificar el tamaño y posiciones
# de las letras detro de todas las gráficas de MorantViz con excepción de la grafica de Waffle.
# En este script de ejemplo, se muestra una comparación entre las gráficas con sus valores predeterminados (panal izquierdo) y
# con la nueva parametrización (panel derecho).

## LOS VALORES SE EXAGERAN PARA VER LA APLICACIÓN DEL MÉTODO ##

library(stringr)
library(tidyr)
library(ggalluvial)
library(treemapify)
library(patchwork)
library(ggsankey)


devtools::load_all(path = "../morantviz/")

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


g <- Encuesta$new(
  diseno = diseno_demo,
  diccionario = dicc,
  colores = colores,
  color_principal = "pink",
  tema = tema_morant()
)


################################### Barras verticales (parametrización) ###################################

## Valores predefinidos
g$contar_variables(
  variables = "opinion_pm_astiazaran",
  confint = T
)$pegar_color()$graficar_barras_v(x = "respuesta", letra_tam = 5, vjust = -.1) +
  labs(title = "Tamaño predefinido") +

  g$contar_variables(
    variables = "opinion_pm_astiazaran",
    confint = T
  )$pegar_color()$graficar_barras_v(
    x = "respuesta",
    letra_tam = 10,
    vjust = -10
  ) +
  labs(title = "Nueva paremetrización")


################################### Lollipop (parametrización) ###################################

## Valores predefinidos
g$contar_variables(
  variables = c("opinion_pm_delrio"),
  confint = F
)$pegar_diccionario()$pegar_color()$graficar_lollipops(
  "respuesta",
  letra_tam = 6,
  hjust = -0.5,
  bola_tam = 5
) +
  labs(title = "Tamaño predefinido") +

  g$contar_variables(
    variables = c("opinion_pm_delrio"),
    confint = F
  )$pegar_diccionario()$pegar_color()$graficar_lollipops(
    "respuesta",
    letra_tam = 10,
    hjust = -2,
    bola_tam = 9
  ) +
  labs(title = "Nueva parametrización")


################################### Gauge (parametrización) ###################################

## Valores predeterminados
g$contar_variables(
  variables = "conoce_pm_javier",
  confint = T
)$pegar_diccionario()$pegar_color()$graficar_gauge(letra_tam = 12) +
  labs(title = "Tamaño predeterminado") +

  ## Valores predeterminados
  g$contar_variables(
    variables = "conoce_pm_javier",
    confint = T
  )$pegar_diccionario()$pegar_color()$graficar_gauge(letra_tam = 20) +
  labs(title = "Nueva parametrización")


################################### Barras divergentes (parametrización) ###################################

g$contar_variables(
  variables = c("opinion_pm_astiazaran", "opinion_pm_delrio"),
  confint = F
)$filtrar_respuesta(
  valor = c("Muy buena", "Buena", "Regular", "Mala", "Muy mala")
)$pegar_diccionario()$pegar_color()$reordenar_columna(
  columna = "respuesta",
  tipo = "manual",
  c("Muy buena", "Buena", "Regular", "Mala", "Muy mala")
)$partir_regular(opcion = "Regular")$cambiarSigno_freq(
  negativo = c("Mala", "Muy mala")
)$reordenar_columna(columna = "nombre", tipo = "suma")$etiquetar_regular(
  regular = "Regular",
  freq = "media"
)$graficar_barras_divergente(
  regular = "Regular",
  positivas = c("Buena", "Muy buena"),
  negativas = c("Mala", "Muy mala"),
  letra_tam = 25,
  vjust = .5
) +
  labs(title = "Tamaño predeterminado") +

  g$contar_variables(
    variables = c("opinion_pm_astiazaran", "opinion_pm_delrio"),
    confint = F
  )$filtrar_respuesta(
    valor = c("Muy buena", "Buena", "Regular", "Mala", "Muy mala")
  )$pegar_diccionario()$pegar_color()$reordenar_columna(
    columna = "respuesta",
    tipo = "manual",
    c("Muy buena", "Buena", "Regular", "Mala", "Muy mala")
  )$partir_regular(opcion = "Regular")$cambiarSigno_freq(
    negativo = c("Mala", "Muy mala")
  )$reordenar_columna(columna = "nombre", tipo = "suma")$etiquetar_regular(
    regular = "Regular",
    freq = "media"
  )$graficar_barras_divergente(
    regular = "Regular",
    positivas = c("Buena", "Muy buena"),
    negativas = c("Mala", "Muy mala"),
    letra_tam = 40,
    vjust = .96
  ) +
  labs(title = "Nueva parametrización")


################################### Bloques (parametrización) ###################################

g$contar_variables_porGrupos(
  variables = c("identificacion_partido"),
  grupos = c("sexo"),
  confint = F
)$pegar_color()$envolver_etiquetas(
  columna = "respuesta",
  10
)$pegar_diccionario()$graficar_bloque(freq = "media", letra_tam = 2) +
  labs(title = "Tamaño predeterminado") +

  g$contar_variables_porGrupos(
    variables = c("identificacion_partido"),
    grupos = c("sexo"),
    confint = F
  )$pegar_color()$envolver_etiquetas(
    columna = "respuesta",
    10
  )$pegar_diccionario()$graficar_bloque(freq = "media", letra_tam = 20) +
  labs(title = "Nueva parametrización")


################################### Piramide (parametrización) ###################################

g$contar_variables_porGrupos(
  variables = c("rango_edad"),
  grupos = c("sexo"),
  confint = F
)
g$tbl <- g$tbl |>
  mutate(
    respuesta = case_when(
      respuesta == "18A24" ~ "18-24",
      respuesta == "25A39" ~ "25-39",
      respuesta == "40A59" ~ "40-59",
      respuesta == "60YMAS" ~ "60+",
      TRUE ~ respuesta
    )
  )


g$graficar_piramide(
  cantidad_puntos = 30,
  tam_punto = 6,
  tam_texto_etiqueta_porcentaje = 6,
  separacion_texto = 1.5,
  espaciado = c(0.05, 0.05),
  tam_texto_rango_edad = 6
) +
  labs(title = "Tamaño predeterminado") +

  g$graficar_piramide(
    cantidad_puntos = 30,
    tam_punto = 16,
    tam_texto_etiqueta_porcentaje = 12,
    separacion_texto = 4.5,
    espaciado = c(0.05, 0.05),
    tam_texto_rango_edad = 12
  ) +
  labs(title = "Nueva parametrización")


################################### Lineas (parametrización) ###################################

g$contar_variables(
  variables = c("problema_principal"),
  confint = F
)$filtrar_respuesta(
  valor = c(
    "Falta de agua",
    "Falta de servicios de salud",
    "Falta de alumbrado",
    "Tráfico vehicular"
  )
)$pegar_diccionario()$pegar_color()$graficar_lineas(
  x = "respuesta",
  letra_tam = 5,
  hjust = -.1,
  vjust = -1,
  bola_tam = 3
) +
  labs(title = "Tamaño predeterminado") +

  g$contar_variables(
    variables = c("problema_principal"),
    confint = F
  )$filtrar_respuesta(
    valor = c(
      "Falta de agua",
      "Falta de servicios de salud",
      "Falta de alumbrado",
      "Tráfico vehicular"
    )
  )$pegar_diccionario()$pegar_color()$graficar_lineas(
    x = "respuesta",
    letra_tam = 10,
    hjust = -.4,
    vjust = -4,
    bola_tam = 6
  ) +
  labs(title = "Nueva parametrización")


################################### Sankey (parametrización) ###################################

grupo <- c('sexo', 'rango_edad')

g$contar_variables_porGrupos(
  variables = c("conoce_pm_astiazaran"),
  grupos = grupo,
  confint = F
)$pegar_diccionario()$graficar_sankey(grupo, letra_tam = 3.5) +
  labs(title = "Valores predeteminados") +

  g$contar_variables_porGrupos(
    variables = c("conoce_pm_astiazaran"),
    grupos = grupo,
    confint = F
  )$pegar_diccionario()$graficar_sankey(grupo, letra_tam = 13.5) +
  labs(title = "Nueva parametrización")


################################### Barras horizontales (parametrización) ###################################

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
)$reordenar_columna(columna = "nombre", tipo = "asc")$graficar_barras_h(
  x = "nombre",
  letra_tam = 5,
  hjust = -.1
) +
  labs(title = "Valores predeteminados") +
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
  )$reordenar_columna(columna = "nombre", tipo = "asc")$graficar_barras_h(
    x = "nombre",
    letra_tam = 10,
    hjust = -.9
  ) +
  labs(title = "Nueva parametrización")


################################### Saldos opinión (parmatrización) ###################################

g$saldos_opinion(
  sufijo_opinion = "opinion_pm",
  cat_ns_nc = "Ns/Nc",
  sufijo_conoce = "conoce_pm",
  cat_conoce = "Sí",
  actores = c("astiazaran", "delrio"),
  positivas = c("Muy buena", "Buena"),
  negativas = c("Mala", "Muy mala"),
  regular = "",
  letra_tam = 90,
  hjust = -.9
)
