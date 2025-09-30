Graficar <- R6::R6Class("Graficar",
                        public = list(
                          tbl = NULL,
                          grafica = NULL,
                          diseno = NULL,
                          diccionario = NULL,
                          colores = NULL,
                          color_principal = NULL,
                          tema = NULL,
                          initialize = function(diseno, diccionario, colores, color_principal, tema) {
                            self$diseno <- diseno
                            self$diccionario <- diccionario
                            self$colores <- colores
                            self$tema <- tema
                            self$color_principal <- color_principal
                          },
                          contar_variables = function(variables, confint){
                            self$tbl <- contar_vars_pesos(variables = variables, confint = confint,
                                                          diccionario = self$deccionario, diseno = self$diseno)

                            invisible(self)
                          },
                          filtrar_respuesta = function(variable, valor){
                            self$tbl <- self$tbl |>
                              filter(respuesta %in% !!valor)
                            invisible(self)
                          },
                          reordenar_columna = function(columna, tipo, ...){
                            match.arg(tipo, choices = c("manual", "asc", "desc", "suma"))

                            if(tipo == "manual"){
                              self$tbl <- self$tbl |>
                                mutate(!!rlang::sym(columna) := forcats::fct_relevel(!!rlang::sym(columna), ...))
                            }

                            if(tipo %in% "desc"){

                              self$tbl <- self$tbl |>
                                arrange(desc(media)) |>
                                mutate(!!rlang::sym(columna) := forcats::fct_inorder(!!rlang::sym(columna)))
                            }

                            if(tipo == "asc"){

                              self$tbl <- self$tbl |>
                                arrange(media) |>
                                mutate(!!rlang::sym(columna) := forcats::fct_inorder(!!rlang::sym(columna)))

                            }

                            if(tipo == "suma"){
                              self$tbl <- self$tbl |>
                                mutate(!!rlang::sym(columna) := forcats::fct_reorder(!!rlang::sym(columna), media, .fun = sum))
                            }

                            invisible(self)
                          },
                          envolver_etiquetas = function(columna, ancho){
                            self$tbl <- self$tbl |>
                              mutate(!!rlang::sym(columna) := stringr::str_wrap(!!rlang::sym(columna), width = ancho))

                            invisible(self)
                          },
                          pegar_diccionario = function(){
                            self$tbl <- self$tbl |>
                              left_join(self$diccionario, join_by(codigo))

                            invisible(self)
                          },
                          partir_regular = function(opcion){

                            self$tbl <- self$tbl |>
                              filter(respuesta != !!opcion) |>
                              bind_rows(
                                c(1, -1) |>
                                  purrr::map_dfr(~{
                                    self$tbl |>
                                      filter(respuesta == !!opcion) |>
                                      mutate(media = .x*media/2)

                                  })
                              ) |>
                              mutate(respuesta2 = if_else(media < 0 & respuesta  == "Regular", "Regular2", respuesta))

                            invisible(self)
                          },
                          cambiarSigno_media = function(negativo){
                            self$tbl <- self$tbl |>
                              mutate(media = if_else(respuesta %in% !!negativo, -media, media))

                            invisible(self)
                          },
                          etiquetar_regular = function(regular){
                            self$tbl <- self$tbl |>
                              mutate(etiqueta = case_when(media < 0 & respuesta  == !!regular ~ "",
                                                          respuesta  == !!regular ~ scales::percent(media*2, accuracy = 1),
                                                          T ~ scales::percent(abs(media), accuracy = 1)
                              ))

                            invisible(self)
                          },
                          pegar_color = function(){
                            self$tbl <- self$tbl |>
                              left_join(self$colores, join_by(respuesta)) |>
                              mutate(color = if_else(is.na(color), self$color_principal, color))

                            invisible(self)
                          },
                          agregar_saldo = function(por){
                            self$tbl <- self$tbl |>
                              mutate(saldo = sum(media), .by = !!rlang::sym(por))

                            invisible(self)
                          },
                          graficar_barras_h = function(x){
                            self$grafica <- ggplot(self$tbl, aes(x = !!rlang::sym(x), y = media)) +
                              ggchicklet::geom_chicklet(aes(fill = color)) +
                              geom_text(aes(label = scales::percent(media)), size = 5, hjust = -.1, family = self$tema$text$family) +
                              coord_flip() +
                              labs(caption = self$tbl$pregunta[1]) +
                              scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                                 limits = c(0,1)) +
                              scale_fill_identity() +
                              self$tema

                            return(self$grafica)
                          },
                          graficar_barras_divergente = function(regular, positivas, negativas){
                            self$grafica <- self$tbl |>
                              ggplot(aes(x = nombre, y = media, group = factor(respuesta2, c(regular, paste0(regular,"2"), positivas, negativas)))) +
                              ggchicklet::geom_chicklet(aes(fill = respuesta, color = respuesta)) +
                              ggfittext::geom_fit_text(aes(label = etiqueta),
                                                       size = 25,
                                                       position = position_stack(.5, reverse = T),
                                                       vjust = .5,
                                                       contrast = T,
                                                       show.legend = F,
                                                       family = self$tema$text$family) +
                              coord_flip() +
                              scale_fill_manual(values = self$tbl |> distinct(respuesta, color) |> pull(color) |> purrr::set_names(self$tbl |> distinct(respuesta, color) |> pull(respuesta)))+
                              scale_color_manual(values = self$tbl |> distinct(respuesta, color) |> pull(color) |> purrr::set_names(self$tbl |> distinct(respuesta, color) |> pull(respuesta))) +
                              self$tema +
                              theme(legend.position = "bottom") +
                              lemon::scale_y_symmetric(labels = function(x) scales::percent(abs(x), accuracy = 1)) +
                              labs(fill = NULL, color = NULL)

                            return(self$grafica)
                          }
                        )
)

Encuesta <- R6::R6Class("Encuesta",
                        inherit = Graficar,
                        public = list(
                          tbl = NULL,
                          grafica = NULL,
                          diseno = NULL,
                          diccionario = NULL,
                          colores = NULL,
                          color_principal = NULL,
                          tema = NULL,
                          initialize = function(diseno, diccionario, colores, color_principal, tema){
                            super$initialize(diseno, diccionario, colores, color_principal, tema)
                          },
                          saldos_opinion = function(sufijo_opinion, cat_ns_nc, sufijo_conoce, cat_conoce, actores, positivas, negativas, regular){
                            ### graficar opinion

                            opinion <- paste(sufijo_opinion, actores, sep = "_")

                            super$
                              contar_variables(variables = opinion, confint = F)$
                              filtrar_respuesta(valor = c(positivas, negativas, regular))$
                              pegar_diccionario()$
                              pegar_color()$
                              reordenar_columna(columna = "respuesta", tipo = "manual", c(positivas, regular, negativas))$
                              partir_regular(opcion = regular)$
                              cambiarSigno_media(negativo = negativas)$
                              reordenar_columna(columna = "nombre", tipo = "suma")$
                              etiquetar_regular(regular = regular)

                            op <- super$graficar_barras_divergente(regular = regular,
                                                                   positivas = rev(positivas),
                                                                   negativas = negativas)

                            orden <- self$tbl$nombre |>
                              levels() # para mantener el orden del eje de las y en las 3 gráficas

                            ### graficar conocimiento

                            conoce <- paste(sufijo_conoce, actores, sep = "_")

                            super$
                              contar_variables(variables = conoce, confint = F)$
                              filtrar_respuesta(valor = cat_conoce)$
                              pegar_diccionario()$
                              pegar_color()$
                              reordenar_columna(columna = "nombre", tipo = "manual", orden)

                            conoc <- self$tbl |>
                              ggplot(aes(x = nombre, y = 1)) +
                              geom_tile(aes(fill = media), color = "white", show.legend = F) +
                              ggfittext::geom_fit_text(aes(label = scales::percent(media, 1)), contrast = T) +
                              coord_flip() +
                              labs(x = NULL, y = NULL, title = "Conocimiento") +
                              # tema_morant() +
                              theme(axis.text = element_blank(),
                                    axis.ticks = element_blank()) +
                              theme_void() +
                              theme(text = element_text(family = "Poppins"))

                            ### graficar barras de Ns/Nc

                            super$
                              contar_variables(variables = opinion, confint = F)$
                              filtrar_respuesta(valor = cat_ns_nc)$
                              pegar_diccionario()$
                              pegar_color()$
                              reordenar_columna(columna = "nombre", tipo = "manual", orden)

                            ns_nc <- super$graficar_barras_h(x = "nombre") +
                              theme_void() +
                              labs(caption = NULL, title = "No sabe / No contesta") +
                              theme(text = element_text(family = "Poppins"))

                            todo <- op + conoc + ns_nc + plot_layout(ncol = 3, widths = c(3, 1, 1))
                            return(todo)
                          }
                        )
)
