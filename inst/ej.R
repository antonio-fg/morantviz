# usethis::use_mit_license( "Emilio Morones" )  # You can set another license here
# usethis::use_readme_rmd( open = FALSE )
# usethis::use_code_of_conduct()
# usethis::use_lifecycle_badge( "Experimental" )
# usethis::use_news_md( open = FALSE )
#
# usethis::use_vignette("morantviz")
# devtools::build_vignettes()
#
# usethis::use_package( "survey" )
# usethis::use_package( "ggplot2" )
# usethis::use_package( "patchwork" )
# usethis::use_data(dicc)
# usethis::use_data(colores)
# diseno_demo <- encuesta_demo$muestra$diseno
# usethis::use_data(diseno_demo)
# usethis::use_data(tema_morant)
# Ejemplo -----------------------------------------------------------------

#library(morantviz)

library(stringr)
library(tidyr)
library(ggalluvial)
library(treemapify)


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
# conocimiento barras horizontal ------------------------------------------

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
)$reordenar_columna(columna = "nombre", tipo = "asc")


g$graficar_barras_h(x = "nombre")

g$tbl


# opinión barras facet ----------------------------------------------------

g$contar_variables(
  variables = c("opinion_pm_astiazaran", "opinion_pm_delrio"),
  confint = F
)$pegar_diccionario()$pegar_color()$reordenar_columna(
  columna = "respuesta",
  tipo = "manual",
  c("Buena", "Regular", "Mala", "Muy mala", "Ns/Nc", )
)$reordenar_columna(
  columna = "nombre",
  tipo = "manual",
  c("Del Río", "Astiazarán")
)


g$graficar_barras_h(x = "respuesta") +
  facet_wrap(~nombre)

####ejemplo barras verticales
g$contar_variables(variables = "opinion_pm_astiazaran", confint = T)
g$pegar_color()
g$graficar_barras_v(x = "respuesta")


#ejemplo lollipops
g$contar_variables(
  variables = c("opinion_pm_delrio"),
  confint = F
)$pegar_diccionario()$pegar_color()


g$graficar_lollipops("respuesta") +
  tema_morant(base_family = "KuFam") +
  facet_wrap(~nombre)

####ejemplo barras verticales
g$contar_variables(variables = "opinion_pm_astiazaran", confint = T)
g$pegar_color()
g$graficar_barras_v(x = "respuesta")


g$contar_variables(variables = "conoce_pm_javier", confint = T)
g$tbl
g$pegar_color()
g$tbl
g$graficar_gauge()

g$contar_variables(variables = "conoce_pm_astiazaran", confint = T)
g$tbl
g$filtrar_respuesta(variable = "respuesta", valor = "Sí")
g$tbl
g$pegar_color()
g$tbl
g$graficar_gauge() +
  labs(title = "ejemplo", caption = "caption") +
  theme(plot.title = element_text(family = "KuFam", size = 25))


######Pirámide
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


g$graficar_piramide(espaciado = c(0.05, 0.05))


# opinión barras divergente ------------------------------------------------

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
  regular = "Regular"
)

g$tbl
op <- g$graficar_barras_divergente(
  regular = "Regular",
  positivas = c("Buena", "Muy buena"),
  negativas = c("Mala", "Muy mala")
)


orden <- g$tbl$nombre |> levels()


# opinión barras divergente sin regular -----------------------------------

g$contar_variables(
  variables = c("opinion_pm_astiazaran", "opinion_pm_delrio"),
  confint = F
)$filtrar_respuesta(
  valor = c("Muy buena", "Buena", "Mala", "Muy mala")
)$pegar_diccionario()$pegar_color()$reordenar_columna(
  columna = "respuesta",
  tipo = "manual",
  c("Muy buena", "Buena", "Mala", "Muy mala")
)$partir_regular(opcion = "Regular")$cambiarSigno_freq(
  negativo = c("Mala", "Muy mala")
)$reordenar_columna(columna = "nombre", tipo = "suma")$etiquetar_regular(
  regular = "",
  freq = "media"
)

g$tbl
op <- g$graficar_barras_divergente(
  regular = NULL,
  positivas = c("Buena", "Muy buena"),
  negativas = c("Mala", "Muy mala")
)
op
# conocimiento con porcentaje ----------------------------------------------

g$
  contar_variables(variables = c("conoce_pm_astiazaran", "conoce_pm_delrio"), confint = F)$
  filtrar_respuesta(valor = "Sí")$
  pegar_diccionario()$
  pegar_color()$
  envolver_etiquetas(columna = "nombre", ancho = 13)$
# reordenar_columna(columna = "nombre", tipo = "manual", "Astiazarán", after = 1)
reordenar_columna(columna = "nombre", tipo = "manual", orden)

conoc <- g$tbl |>
  ggplot(aes(x = nombre, y = 1)) +
  geom_tile(aes(fill = media), color = "white", show.legend = F) +
  ggfittext::geom_fit_text(
    aes(label = scales::percent(media, 1)),
    contrast = T
  ) +
  coord_flip() +
  labs(x = NULL, y = NULL, title = "Conocimiento") +
  # tema_morant() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  theme_void() +
  theme(text = element_text(family = "Poppins"))

conoc

# ns/nc barras horizontal -------------------------------------------------

g$contar_variables(
  variables = c("opinion_pm_astiazaran", "opinion_pm_delrio"),
  confint = F
)$filtrar_respuesta(
  valor = c("Ns/Nc")
)$pegar_diccionario()$pegar_color()$reordenar_columna(
  columna = "nombre",
  tipo = "manual",
  orden
)


ns_nc <- g$graficar_barras_h(x = "nombre") +
  theme_void() +
  labs(caption = NULL, title = "No sabe / No contesta") +
  theme(text = element_text(family = "Poppins"))


ns_nc
# p + conoc + ns_nc -------------------------------------------------------

op + conoc + ns_nc + plot_layout(ncol = 3, widths = c(3, 1, 1))

g$saldos_opinion(
  sufijo_opinion = "opinion_pm",
  cat_ns_nc = "Ns/Nc",
  sufijo_conoce = "conoce_pm",
  cat_conoce = "Sí",
  actores = c("astiazaran", "delrio"),
  positivas = c("Muy buena", "Buena"),
  negativas = c("Mala", "Muy mala"),
  regular = "Regular"
)


g$saldos_opinion(
  sufijo_opinion = "opinion_pm",
  cat_ns_nc = "Ns/Nc",
  sufijo_conoce = "conoce_pm",
  cat_conoce = "Sí",
  actores = c("astiazaran", "delrio"),
  positivas = c("Muy buena", "Buena"),
  negativas = c("Mala", "Muy mala"),
  regular = ""
)

sufijo_opinion = "opinion_pm"
cat_ns_nc = "Ns/Nc"
sufijo_conoce = "conoce_pm"
cat_conoce = "Sí"
actores = c("astiazaran", "delrio")
positivas = c("Muy buena", "Buena")
negativas = c("Mala", "Muy mala")
regular = "Regular"

opinion <- paste(sufijo_opinion, actores, sep = "_")

g$contar_variables(variables = opinion, confint = F)$filtrar_respuesta(
  valor = c(positivas, negativas, regular)
)$pegar_diccionario()$pegar_color()$reordenar_columna(
  columna = "respuesta",
  tipo = "manual",
  c(positivas, regular, negativas)
)$partir_regular(opcion = regular)$cambiarSigno_freq(
  negativo = negativas
)$reordenar_columna(columna = "nombre", tipo = "suma")$etiquetar_regular(
  regular = regular,
  freq = "media"
)

g$graficar_barras_divergente(
  regular = regular,
  positivas = rev(positivas),
  negativas = negativas
)


g$contar_variables(
  variables = c("conoce_pm_astiazaran"),
  confint = T
)$filtrar_respuesta(valor = c("Sí"))
g$tbl


diseno_demo$variables |> glimpse()
# cruce -------------------------------------------------------------------

g$contar_variables_porGrupos(
  variables = c("rango_edad"),
  grupos = c("sexo"),
  confint = F
)
g$tbl

# multirespuesta ----------------------------------------------------------
g$contar_variables_porGrupos(
  variables = c("conoce_pm_astiazaran", "conoce_pm_delrio"),
  grupos = c("sexo", "region"),
  confint = F
)


g$tbl


# graficar waffle cruzado ----------------------------------

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
  eje_y = "nombre"
)


# graficar waffle una variable ------------------------------
g$contar_variables(
  variables = c(
    "opinion_pm_astiazaran",
    "opinion_pm_delrio"
  ),
  confint = T
)
g$pegar_diccionario()
g$tbl
g$pegar_color()
g$tbl
g$reordenar_columna(
  columna = "respuesta",
  tipo = "manual",
  c("Muy buena", "Buena", "Regular", "Mala", "Muy mala")
)

g$generar_coordenadas(
  eje_x = "nombre",
  eje_y = "respuesta",
  valor = "media"
)

g$graficar_waffle(
  eje_x = "nombre",
  eje_y = "respuesta"
)
#Graficar Bloques Con Facet-------------------------------
library("stringr")
g$contar_variables_porGrupos(
  variables = c("identificacion_partido"),
  grupos = c("sexo"),
  confint = F
)
g$pegar_color()
g$envolver_etiquetas(columna = "respuesta", 10)
g$pegar_diccionario()

library("treemapify")

g$graficar_bloque(freq = "media") +
  facet_wrap(~sexo) +
  labs(title = "Grafica de bloques de identificación de partidos por sexo")

#Graficar Bloques -------------------------------

g$contar_variables(variables = c("identificacion_partido"))
g$pegar_color()
g$pegar_diccionario()

g$graficar_bloque(freq = "media")
