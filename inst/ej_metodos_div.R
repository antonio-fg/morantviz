
# ========================================================================================================
# Métodos para barras divergentes 
# ========================================================================================================
#'
#' PROYECTO: Moranviz
#' SCRIPT: ej_metodos_div.R
#' DESCRIPCIÓN: 4 métdos para realizar el panel de Ns/Nc + Saldo + Cocimiento + emsamblador de gráficas
#' ------------------------------------------------------------------------------------------------------
#' Fecha de creación: 11-Mayo-2026
#' AUTOR: [José Antonio Flores / Morant Consultores S.A]
#' VERSIÓN: 1.0.0
#' -------------------------------------------------------------------------------------------------------

devtools::load_all(path = "../morantviz/")

dicc <- tibble::tribble(
  ~codigo                  , ~nombre                  , ~pregunta                             ,~respuestas,
  "conoce_pm_astiazaran"   , "Astiazarán"            , "Conoce o ha escuchado de (...)"      ,"Sí_No",
  "conoce_pm_delrio"       , "Del Río"               , "Conoce o ha escuchado de (...)"      ,"Sí_No",
  "conoce_pm_lia"          , "Lía Limón"            , "Conoce o ha escuchado de (...)"      ,"Sí_No",
  "conoce_pm_javier"       , "Javier López Casarín" , "Conoce o ha escuchado de (...)"      ,"Sí_No",
  "opinion_pm_astiazaran"  , "Astiazarán"            , "¿Cuál es su opinión sobre (...)?" , "Muy_buena_Buena_Regular_Mala_Muy_mala",
  "opinion_pm_delrio"      , "Del Río"               , "¿Cuál es su opinión sobre (...)?" , "Muy_buena_Buena_Regular_Mala_Muy_mala",
  "identificacion_partido" , ""                       , "¿Con que partido se identifica?",  "MORENA_Movimiento Ciudadano (MC)_Partido Verde (PVEM)_PRD_PRI_PT_Ninguno_Otro"
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


################################### Ns/Nc ###################################


g$contar_variables(
  variables = c("opinion_pm_astiazaran", "opinion_pm_delrio"),
  confint = F
)$filtrar_respuesta(
  variable = "respuesta",
  valor = c("Muy buena", "Buena", "Regular", "Mala", "Muy mala"))$
pegar_diccionario()$pegar_color()$reordenar_columna(
  columna = "respuesta",
  tipo = "manual",
  c("Muy buena", "Buena", "Regular", "Mala", "Muy mala"),freq = 'media'
)$partir_regular(opcion = "Regular")$cambiarSigno_freq(
  negativo = c("Mala", "Muy mala"),freq = 'media'
)$reordenar_columna(columna = "nombre", tipo = "suma")$etiquetar_regular(
  regular = "Regular",freq = 'media'
)$graficar_barras_divergente(
  regular = "Regular",
  positivas = c("Buena", "Muy buena"),
  negativas = c("Mala", "Muy mala"),
  encadenar = T
)$
  crear_ns_nc_plot(titulo_tam = 10)$
  ensamblar_graficas()
 


################################### Ns/Nc + Saldos ###################################

g$contar_variables(
  variables = c("opinion_pm_astiazaran", "opinion_pm_delrio"),
  confint = F
)$filtrar_respuesta(
  variable = "respuesta",
  valor = c("Muy buena", "Buena", "Regular", "Mala", "Muy mala"))$
pegar_diccionario()$pegar_color()$reordenar_columna(
  columna = "respuesta",
  tipo = "manual",
  c("Muy buena", "Buena", "Regular", "Mala", "Muy mala"),freq = 'media'
)$partir_regular(opcion = "Regular")$cambiarSigno_freq(
  negativo = c("Mala", "Muy mala"),freq = 'media'
)$reordenar_columna(columna = "nombre", tipo = "suma")$etiquetar_regular(
  regular = "Regular",freq = 'media'
)$graficar_barras_divergente(
  regular = "Regular",
  positivas = c("Buena", "Muy buena"),
  negativas = c("Mala", "Muy mala"),
  encadenar = T
)$
  crear_ns_nc_plot(titulo_tam = 10)$
  crear_saldo_plot()$
  ensamblar_graficas()


################################### Ns/Nc + Saldo + Panel de conocimiento ###################################


g$contar_variables(
  variables = c("opinion_pm_astiazaran", "opinion_pm_delrio"),
  confint = T
)$filtrar_respuesta(
  variable = "respuesta",
  valor = c("Muy buena", "Buena", "Regular", "Mala", "Muy mala"))$
pegar_diccionario()$pegar_color()$reordenar_columna(
  columna = "respuesta",
  tipo = "manual",
  c("Muy buena", "Buena", "Regular", "Mala", "Muy mala"),freq = 'media'
)$partir_regular(opcion = "Regular")$cambiarSigno_freq(
  negativo = c("Mala", "Muy mala"),freq = 'media'
)$reordenar_columna(columna = "nombre", tipo = "suma")$etiquetar_regular(
  regular = "Regular",freq = 'media'
)$graficar_barras_divergente(
  regular = "Regular",
  positivas = c("Buena", "Muy buena"),
  negativas = c("Mala", "Muy mala"),
  encadenar = T
)$
  crear_ns_nc_plot(titulo_tam = 10)$
  crear_saldo_plot()$
  panel_conocimiento(
    variables = c("conoce_pm_astiazaran", "conoce_pm_delrio"),
    valor = "Sí",
    titulo_size = 14
  )$
  ensamblar_graficas()



