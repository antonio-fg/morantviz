# usethis::use_mit_license( "Emilio Morones" )  # You can set another license here
usethis::use_readme_rmd( open = FALSE )
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

library(morantviz)

dicc <- tibble::tribble(~codigo, ~nombre, ~pregunta,
                        "conoce_pm_astiazaran", "Astiazarán", "Conoce o ha escuchado de (...)",
                        "conoce_pm_delrio", "Del Río", "Conoce o ha escuchado de (...)",
                        "conoce_pm_lia", "Lía Limón", "Conoce o ha escuchado de (...)",
                        "conoce_pm_javier", "Javier López Casarín", "Conoce o ha escuchado de (...)",
                        "opinion_pm_astiazaran", "Astiazarán","¿Cuál es su opinión sobre (...)?",
                        "opinion_pm_delrio", "Del Río","¿Cuál es su opinión sobre (...)?"
)


colores <- tibble::tribble(~respuesta, ~color,
                           "Sí", "#0c4c8a",
                           "No", "#ecf0f1",
                           "Buena", "#27ae60",
                           "Mala", "#c0392b",
                           "Muy buena", "#2ecc71",
                           "Muy mala", "#e74c3c",
                           "Regular", "#f1c40f",
                           "Ns/Nc", "#95a5a6")

g <- Graficar$new(bd = diseno_demo$variables,
                  diccionario = dicc,
                  colores = colores,
                  color_principal = "pink",
                  tema = tema_morant())

# conocimiento barras horizontal ------------------------------------------

g$
  contar_variables(variables = c("conoce_pm_astiazaran", "conoce_pm_delrio", "conoce_pm_lia", "conoce_pm_javier"))$
  calcular_pct()$
  filtrar_respuesta(valor = "Sí")$
  pegar_diccionario()$
  pegar_color()$
  envolver_etiquetas(columna = "nombre", ancho = 13)$
  # reordenar_columna(columna = "nombre", tipo = "manual", "Astiazarán", after = 1)
  reordenar_columna(columna = "nombre", tipo = "asc", freq = "pct")

# g$tbl$nombre

g$graficar_barras_h(x = "nombre", y = "pct")


# opinión barras facet ----------------------------------------------------

g$
  contar_variables(variables = c("opinion_pm_astiazaran", "opinion_pm_delrio"))$
  filtrar_respuesta(valor = c("Buena", "Muy buena", "Regular","Muy mala", "Mala", "Ns/Nc"))$
  calcular_pct(grupo = "codigo")$
  pegar_diccionario()$
  pegar_color()$
  reordenar_columna(columna = "respuesta",
                    tipo = "manual", c("Muy buena", "Buena", "Regular", "Mala", "Muy mala", "Ns/Nc"))$
  reordenar_columna(columna = "nombre", tipo = "manual", c("Del Río", "Astiazarán"))

g$graficar_barras_h(x = "respuesta", y = "pct") +
  facet_wrap(~nombre)


# opinión barras divergente ------------------------------------------------

g$
  contar_variables(variables = c("opinion_pm_astiazaran", "opinion_pm_delrio"))$
  filtrar_respuesta(valor = c("Muy buena", "Buena", "Regular", "Mala", "Muy mala"))$
  calcular_pct()$
  pegar_diccionario()$
  pegar_color()$
  reordenar_columna(columna = "respuesta", tipo = "manual", c("Muy buena", "Buena", "Regular","Mala","Muy mala"))$
  partir_regular(opcion = "Regular", freq = "pct")$
  cambiarSigno_freq(negativo = c("Mala", "Muy mala"), freq = "pct")$
  reordenar_columna(columna = "nombre", tipo = "suma", freq = "pct")$
  etiquetar_regular(regular = "Regular", freq = "pct")

g$tbl
op <- g$graficar_barras_divergente(regular = "Regular",
                                   positivas = c("Buena", "Muy buena"),
                                   negativas = c("Mala", "Muy mala"),
                                   y = "pct")

op
orden <- g$tbl$nombre |> levels()



# opinión barras divergente sin regular -----------------------------------

g$
  contar_variables(variables = c("opinion_pm_astiazaran", "opinion_pm_delrio"), confint = F)$
  filtrar_respuesta(valor = c("Muy buena", "Buena", "Mala", "Muy mala"))$
  calcular_pct()$
  pegar_diccionario()$
  pegar_color()$
  reordenar_columna(columna = "respuesta", tipo = "manual", c("Muy buena", "Buena","Mala","Muy mala"))$
  partir_regular(opcion = "Regular", freq = "pct")$
  cambiarSigno_freq(negativo = c("Mala", "Muy mala"), freq = "pct")$
  reordenar_columna(columna = "nombre", tipo = "suma", freq = "pct")$
  etiquetar_regular(regular = "", freq = "pct")

g$tbl
op <- g$graficar_barras_divergente(regular = NULL,
                                   positivas = c("Buena", "Muy buena"),
                                   negativas = c("Mala", "Muy mala"),
                                   y = "pct")
op
# conocimiento con porcentaje ----------------------------------------------

g$
  contar_variables(variables = c("conoce_pm_astiazaran", "conoce_pm_delrio"), confint = F)$
  calcular_pct()$
  filtrar_respuesta(valor = "Sí")$
  pegar_diccionario()$
  pegar_color()$
  envolver_etiquetas(columna = "nombre", ancho = 13)$
  # reordenar_columna(columna = "nombre", tipo = "manual", "Astiazarán", after = 1)
  reordenar_columna(columna = "nombre", tipo = "manual", orden)

conoc <- g$tbl |>
  ggplot(aes(x = nombre, y = 1)) +
  geom_tile(aes(fill = pct), color = "white", show.legend = F) +
  ggfittext::geom_fit_text(aes(label = scales::percent(pct, 1)), contrast = T) +
  coord_flip() +
  labs(x = NULL, y = NULL, title = "Conocimiento") +
  # tema_morant() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme_void() +
  theme(text = element_text(family = "Poppins"))

conoc

# ns/nc barras horizontal -------------------------------------------------

g$
  contar_variables(variables = c("opinion_pm_astiazaran", "opinion_pm_delrio"), confint = F)$
  filtrar_respuesta(valor = c("Muy buena", "Buena", "Mala", "Muy mala", "Ns/Nc"))$
  calcular_pct()$
  filtrar_respuesta(valor = c("Ns/Nc"))$
  pegar_diccionario()$
  pegar_color()$
  reordenar_columna(columna = "nombre", tipo = "manual", orden)


ns_nc <- g$graficar_barras_h(x = "nombre", y = "pct") +
  theme_void() +
  labs(caption = NULL, title = "No sabe / No contesta") +
  theme(text = element_text(family = "Poppins"))


ns_nc
# p + conoc + ns_nc -------------------------------------------------------

op + conoc + ns_nc + plot_layout(ncol = 3, widths = c(3, 1, 1))

