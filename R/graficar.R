#' Clase R6 Graficar
#'
#' Clase base para construir tablas, manipular respuestas y generar
#' gráficas a partir de encuestas con diseño muestral y diccionario.
#'
#' @docType class
#' @format R6Class object
#' @export
#' @import survey dplyr ggplot2 patchwork
#'
#' @examples
#' \dontrun{
#' g <- Graficar$new(
#'   diseno = encuesta_demo$muestra$diseno,
#'   diccionario = dicc,
#'   colores = colores,
#'   color_principal = "pink",
#'   tema = tema_morant()
#' )
#'
#' # Conocimiento en barras horizontales
#' g$
#'   contar_variables(
#'     variables = c("conoce_pm_astiazaran", "conoce_pm_delrio"),
#'     confint = FALSE
#'   )$
#'   filtrar_respuesta(valor = "Sí")$
#'   pegar_diccionario()$
#'   pegar_color()$
#'   envolver_etiquetas(columna = "nombre", ancho = 13)$
#'   reordenar_columna(columna = "nombre", tipo = "asc")
#'
#' g$graficar_barras_h(x = "nombre")
#' }

Graficar <- R6::R6Class(
  "Graficar",
  private = list(
    pesos = NULL
  ),
  public = list(
    #' @field tbl Tibble con los resultados procesados.
    tbl = NULL,
    #' @field grafica Objeto `ggplot` generado.
    grafica = NULL,
    #' @field diseno Objeto survey design (de `survey`).
    diseno = NULL,
    #' @field bd base de datos a analizar.
    bd = NULL,
    #' @field diccionario Diccionario de variables y etiquetas.
    diccionario = NULL,
    #' @field colores Tabla de colores por respuesta.
    colores = NULL,
    #' @field color_principal Color por defecto.
    color_principal = NULL,
    #' @field tema Tema de `ggplot` a aplicar en las gráficas.
    tema = NULL,

    #' Inicializar objeto Graficar
    #'
    #' @param diseno Diseño muestral de la encuesta (`survey::svydesign`).
    #' @param diccionario Diccionario con nombres y etiquetas de variables.
    #' @param colores Tabla con correspondencia respuesta-color.
    #' @param color_principal Color por defecto para respuestas sin color asignado.
    #' @param tema Tema de `ggplot` a aplicar en las gráficas.
    #' @return Un objeto de clase `Graficar`.
    #' @examples
    #' g <- Graficar$new(diseno, diccionario, colores, "pink", tema_morant())
    #'
    initialize = function(diseno = NULL, bd = NULL, diccionario, colores, color_principal, tema) {
      if(!is.null(diseno)) {
        self$diseno <- diseno
        private$pesos <- T
      }
      if(!is.null(bd)) {
        self$bd <- bd |> tibble::as_tibble()
        private$pesos <- F
      }
      self$diccionario <- diccionario
      self$colores <- colores
      self$tema <- tema
      self$color_principal <- color_principal
    },

    #' Contar variables ponderadas
    #'
    #' Llama a `contar_vars_pesos` para obtener proporciones y medias.
    #'
    #' @param variables Vector de nombres de variables.
    #' @param confint Lógico; si calcular intervalos de confianza.
    #' @return La tabla interna (`self$tbl`) se actualiza.
    #' @examples
    #' g$contar_variables(c("conoce_pm_astiazaran", "conoce_pm_delrio"), confint = FALSE)

    contar_variables = function(variables, confint = F, pct = T){
      if(private$pesos){
        self$tbl <- contar_vars_pesos(
          variables = variables,
          confint = confint,
          diseno = self$diseno
        )
      }

      if(!private$pesos){
        self$tbl <- contar_vars(
          bd = self$bd,
          variables = variables,
          pct = pct
        )
      }

      invisible(self)
    },

    #' Contar variables ponderadas por grupos
    #'
    #' Llama a `contar_vars_porGrupos_pesos` para obtener proporciones y medias.
    #'
    #' @param variables Vector de nombres de variables.
    #' @param grupos Vector de grupos.
    #' @param confint Lógico; si calcular intervalos de confianza.
    #' @return La tabla interna (`self$tbl`) se actualiza.
    #' @examples
    #' g$contar_variables_porGruos(c("conoce_pm_astiazaran", "conoce_pm_delrio"), grupos = "region", confint = FALSE)
    #'
    contar_variables_porGrupos = function(variables, grupos, confint){
      if(private$pesos){
        self$tbl <- contar_vars_porGrupos_pesos(
          variables = variables,
          grupos = grupos,
          confint = confint,
          diseno = self$diseno
        )
      }

      invisible(self)
    },
    #' Contar variable multirespuesta
    #'
    #' Llama a `contar_variable_multirespuesta` para obtener proporciones de una variable multirespuesta con algún separador.
    #'
    #' @param variable Nombre de la variable multirespuesta
    #' @param sep Separador de las respuestas elegidas.
    #' @param confint Lógico; si calcular intervalos de confianza.
    #' @return La tabla interna (`self$tbl`) se actualiza.
    #' @examples
    #' g$contar_variable_multirespuesta(variable = "problema_inseguridad", sep = "|", confint = F)
    #'
    contar_variable_multirespuesta = function(variable, sep, confint){
      self$tbl <- contar_multirespuesta_pesos(diseno = self$diseno,
                                  variable = variable,
                                  sep = sep, confint = confint)

      invisible(self)
    },
    calcular_pct = function(var = "n", grupo = "codigo"){
      self$tbl <- self$tbl |>
          mutate(pct = !!rlang::sym(var)/sum(!!rlang::sym(var)), .by = !!rlang::sym(grupo))

      invisible(self)
    },
    #' Filtrar respuestas específicas
    #'
    #' @param variable Nombre de la variable a filtrar.
    #' @param valor Valores a conservar.
    #' @return La tabla interna (`self$tbl`) se filtra.
    #' @examples
    #' g$filtrar_respuesta(variable = "respuesta", valor = "Sí")
    filtrar_respuesta = function(variable, valor){
      self$tbl <- self$tbl |>
        dplyr::filter(respuesta %in% !!valor)
      invisible(self)
    },

    #' Reordenar una columna
    #'
    #' Permite reordenar factores de forma manual, ascendente, descendente o por suma.
    #'
    #' @param columna Columna a reordenar.
    #' @param tipo Tipo de orden: `"manual"`, `"asc"`, `"desc"`, `"suma"`.
    #' @param freq Con respecto a qué columna es el criterio para ordenar
    #' @param ... Niveles en orden manual si `tipo = "manual"`.
    #' @return La tabla interna (`self$tbl`) se actualiza.
    #' @examples
    #' g$reordenar_columna("nombre", "asc")
    #' g$reordenar_columna("respuesta", "manual", c("Sí", "No"))
    reordenar_columna = function(columna, tipo, freq = "media",...){
      match.arg(tipo, choices = c("manual", "asc", "desc", "suma"))

      if(tipo == "manual"){
        self$tbl <- self$tbl |>
          dplyr::mutate(!!rlang::sym(columna) :=
                          forcats::fct_relevel(!!rlang::sym(columna), ...))
      }

      if(tipo %in% "desc"){
        self$tbl <- self$tbl |>
          dplyr::arrange(dplyr::desc(!!rlang::sym(freq))) |>
          dplyr::mutate(!!rlang::sym(columna) :=
                          forcats::fct_inorder(!!rlang::sym(columna)))
      }

      if(tipo == "asc"){
        self$tbl <- self$tbl |>
          dplyr::arrange(!!rlang::sym(freq)) |>
          dplyr::mutate(!!rlang::sym(columna) :=
                          forcats::fct_inorder(!!rlang::sym(columna)))
      }

      if(tipo == "suma"){
        self$tbl <- self$tbl |>
          dplyr::mutate(!!rlang::sym(columna) :=
                          forcats::fct_reorder(!!rlang::sym(columna), !!rlang::sym(freq), .fun = sum))
      }

      invisible(self)
    },

    #' Envolver etiquetas de texto
    #'
    #' @param columna Columna de etiquetas.
    #' @param ancho Número máximo de caracteres por línea.
    #' @examples
    #' g$envolver_etiquetas("nombre", ancho = 12)
    envolver_etiquetas = function(columna, ancho){
      self$tbl <- self$tbl |>
        dplyr::mutate(!!rlang::sym(columna) :=
                        stringr::str_wrap(!!rlang::sym(columna), width = ancho))
      invisible(self)
    },

    #' Pegar diccionario de variables
    #'
    #' Hace un `left_join` entre la tabla de resultados y el diccionario.
    #' @examples
    #' g$pegar_diccionario()
    pegar_diccionario = function(){
      self$tbl <- self$tbl |>
        dplyr::left_join(self$diccionario, dplyr::join_by(codigo))
      invisible(self)
    },

    #' Partir la categoría "Regular" en dos mitades
    #'
    #' Divide la proporción de "Regular" en dos (positivo/negativo).
    #'
    #' @param opcion Valor de la categoría Regular.
    #' @examples
    #' g$partir_regular("Regular")
    partir_regular = function(opcion, freq = "media"){
      self$tbl <- self$tbl |>
        dplyr::filter(respuesta != !!opcion) |>
        dplyr::bind_rows(
          c(1, -1) |>
            purrr::map_dfr(~{
              self$tbl |>
                dplyr::filter(respuesta == !!opcion) |>
                dplyr::mutate(!!rlang::sym(freq) := .x*!!rlang::sym(freq)/2)
            })
        ) |>
        dplyr::mutate(respuesta2 = dplyr::if_else(!!rlang::sym(freq) < 0 & respuesta == "Regular", "Regular2", respuesta))

      invisible(self)
    },

    #' Cambiar signo a medias de categorías negativas
    #'
    #' @param negativo Vector de respuestas negativas.
    #' @examples
    #' g$cambiarSigno_freq(c("Mala", "Muy mala"))
    cambiarSigno_freq = function(negativo, freq = "media"){
      self$tbl <- self$tbl |>
        dplyr::mutate(!!rlang::sym(freq) := dplyr::if_else(respuesta %in% !!negativo, -!!rlang::sym(freq), !!rlang::sym(freq)))
      invisible(self)
    },

    #' Etiquetar categoría Regular y porcentajes
    #'
    #' @param regular Valor de la categoría Regular.
    #' @examples
    #' g$etiquetar_regular("Regular").
    etiquetar_regular = function(regular, freq){
      self$tbl <- self$tbl |>
        dplyr::mutate(
          etiqueta = dplyr::case_when(
            !!rlang::sym(freq) < 0 & respuesta == !!regular ~ "",
            respuesta == !!regular ~ scales::percent(!!rlang::sym(freq)*2, accuracy = 1),
            TRUE ~ scales::percent(abs(!!rlang::sym(freq)), accuracy = 1)
          )
        )
      invisible(self)
    },

    #' Pegar colores
    #'
    #' Asigna colores a cada respuesta. Usa `color_principal` si falta.
    #' Volví a utilizar la función inicial de pegar_color()
    #' @examples
   # pegar_color = function(columna="respuesta"){
   #   self$tbl <- self$tbl |>
   #     columna_sym <- rlang::sym(columna)
   #     dplyr::left_join(self$colores, dplyr::join_by(!!columna_sym)) |>
   #     dplyr::mutate(color = dplyr::if_else(is.na(color), self$color_principal, color))
   #   invisible(self)
   # },

    pegar_color = function(columna = "respuesta") { 
      columna_sym <- rlang::sym(columna)
    
      self$tbl <- self$tbl |>
        dplyr::left_join(self$colores, dplyr::join_by(!!columna_sym)) |>
        dplyr::mutate(
          color = dplyr::if_else(
            is.na(.data$color),
            self$color_principal,
            .data$color
          )
        )
      
      invisible(self)
    },


    #' Agregar saldo por grupo
    #'
    #' @param por Variable de agrupación.
    #' @examples
    #' g$agregar_saldo("nombre")
    agregar_saldo = function(por, freq = "media"){ 
      self$tbl <- self$tbl |>
        dplyr::mutate(saldo = sum(!!rlang::sym(freq)), .by = !!rlang::sym(por))
      invisible(self)
    },

    #' Graficar barras horizontales
    #'
    #' @param x Variable en el eje X.
    #' @return Objeto `ggplot`.
    #' @examples
    #' g$graficar_barras_h("nombre")
    graficar_barras_h = function(x, y = "media"){
      self$grafica <- ggplot2::ggplot(self$tbl, ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))) +
        ggchicklet::geom_chicklet(ggplot2::aes(fill = color)) +
        ggplot2::geom_text(ggplot2::aes(label = scales::percent(!!rlang::sym(y))),
                           size = 5, hjust = -.1,
                           family = self$tema$text$family) +
        ggplot2::coord_flip() +
        ggplot2::labs(caption = self$tbl$pregunta[1]) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                    limits = c(0,1)) +
        ggplot2::scale_fill_identity() +
        self$tema
      return(self$grafica)
    },


################################### Graficar Líneas  ###################################

#' Genera una gráfica de líneas para una variable
#'
#' Esta función toma la tabla `self$tbl` y construye una gráfica de líneas
#' donde el eje X corresponde a la variable `x`, el eje Y corresponde a la 
#' métrica definida en `freq`, y las líneas se agrupan por la columna `codigo`.
#'
#' Además, se añaden puntos, etiquetas de porcentaje sobre los valores,
#' y se aplica el tema corporativo definido en la clase.
#'
#'  `x` Nombre de la columna que se usará en el eje X (ej. "respuesta").
#' `freq` Nombre de la columna numérica que define el eje Y 
#'        (por defecto "media").
#'
#' @return La gráfica de líneas
#'

  graficar_lineas = function(
      x,
      freq = "media",
      color = "color"
      ){
        group = "codigo"  

        aes_args <- aes(
          x = !!sym(x), 
          y = !!sym(freq), 
          group = !!sym(group), 
          color = color   # 🔑 Usar columna self$tbl$color
        )

        self$grafica <- self$tbl |> 
          ggplot(aes_args) +
          geom_line(linewidth = 1) +
          geom_point(size = 3) +
          geom_text(aes(label = scales::percent(media, accuracy = 1)),
                    size = 5, hjust = -.1,
                    family = self$tema$text$family,
                    vjust = -1, color = "black") +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                             limits = c(0,1)) +
          labs(caption = ifelse(is.na(self$tbl$pregunta[1]), 
                                "Sin pregunta definida", 
                                self$tbl$pregunta[1])) +
          self$tema

        return(self$grafica)
    },



################################### Grafica Sankey  ###################################
  
#' Genera un diagrama de Sankey a partir de la tabla de la clase
#'
#' Esta función toma la tabla `self$tbl` y construye un diagrama de Sankey
#' que muestra los flujos desde una variable de agrupación (`grupo`) hacia 
#' las respuestas (`respuesta`), con pesos definidos por una métrica (`freq`).
#'
#' `grupo` Nombre de la columna que se usará como primer nodo (ej. "sexo").
#' `freq` Nombre de la columna numérica que define el grosor de los flujos 
#'        (por defecto "media").
#'
#' @return La gráfica Sankey.
    
  graficar_sankey = function(grupo,freq = "media"){
  
    sankey_df <- self$tbl %>%
    select(grupo, respuesta, !!sym(freq))
  
    sankey_long <- sankey_df %>%
    make_long(grupo, respuesta, value = !!sym(freq))
  
    paleta <- setNames(self$colores$color, self$colores$respuesta)

    self$grafica <- ggplot(sankey_long,
       aes(x = x, next_x = next_x,
           node = node, next_node = next_node,
           value = value,
           fill = node)) +   # los flujos toman color del nodo
    geom_sankey(flow.alpha = 0.9, color = NA) +   
    geom_sankey_label(aes(label = node), size = 3.5, color = "black") +
    scale_fill_manual(values = paleta, na.value = "grey90") +  # usa paleta, gris claro para extras
    scale_y_continuous(breaks = NULL) +
    labs(caption = ifelse(is.na(self$tbl$pregunta[1]), 
                   "Sin pregunta definida", 
                   self$tbl$pregunta[1]))+  
    theme_void() +
    self$tema 
    
    return(self$grafica)
  },

#############################

  
  
    #' Graficar barras divergentes
    #'
    #' Genera un gráfico divergente de opinión (positivas vs negativas).
    #'
    #' @param regular Valor Regular.
    #' @param positivas Vector de categorías positivas.
    #' @param negativas Vector de categorías negativas.
    #' @return Objeto `ggplot`.
    #' @examples
    #' g$graficar_barras_divergente("Regular",
    #'                              positivas = c("Buena", "Muy buena"),
    #'                              negativas = c("Mala", "Muy mala"))
    graficar_barras_divergente = function(regular, positivas, negativas, y = "media"){
      self$grafica <- self$tbl |>
        ggplot2::ggplot(ggplot2::aes(x = nombre, y = !!rlang::sym(y),
                                     group = factor(respuesta2, c(regular, paste0(regular,"2"), positivas, negativas)))) +
        ggchicklet::geom_chicklet(ggplot2::aes(fill = respuesta, color = respuesta)) +
        ggfittext::geom_fit_text(
          ggplot2::aes(label = etiqueta),
          size = 25,
          position = ggplot2::position_stack(.5, reverse = TRUE),
          vjust = .5, contrast = TRUE, show.legend = FALSE,
          family = self$tema$text$family
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(
          values = self$tbl |> dplyr::distinct(respuesta, color) |> dplyr::pull(color) |>
            purrr::set_names(self$tbl |> dplyr::distinct(respuesta, color) |> dplyr::pull(respuesta))
        ) +
        ggplot2::scale_color_manual(
          values = self$tbl |> dplyr::distinct(respuesta, color) |> dplyr::pull(color) |>
            purrr::set_names(self$tbl |> dplyr::distinct(respuesta, color) |> dplyr::pull(respuesta))
        ) +
        self$tema +
        ggplot2::theme(legend.position = "bottom") +
        lemon::scale_y_symmetric(labels = function(x) scales::percent(abs(x), accuracy = 1)) +
        ggplot2::labs(fill = NULL, color = NULL)
      return(self$grafica)
    }
  )
)

#' Clase R6 Encuesta
#'
#' Hereda de `Graficar` y permite generar saldos de opinión y gráficos
#' combinados (opinión, conocimiento, Ns/Nc).
#'
#' @docType class
#' @format R6Class object
#' @export
#'
#' @examples
#' \dontrun{
#' g <- Encuesta$new(
#'   diseno = encuesta_demo$muestra$diseno,
#'   diccionario = dicc,
#'   colores = colores,
#'   color_principal = "pink",
#'   tema = tema_morant()
#' )
#' 
#
#' g$saldos_opinion(
#'   sufijo_opinion = "opinion_pm",
#'   cat_ns_nc = "Ns/Nc",
#'   sufijo_conoce = "conoce_pm",
#'   cat_conoce = "Sí",
#'   actores = c("astiazaran", "delrio"),
#'   positivas = c("Muy buena", "Buena"),
#'   negativas = c("Mala", "Muy mala"),
#'   regular = "Regular"
#' )
#' }

Encuesta <- R6::R6Class(
  "Encuesta",
  inherit = Graficar,
  public = list(
    #' Inicializa la clase Encuesta
    #'
    #' @inheritParams Graficar$initialize
    initialize = function(diseno = NULL, bd = NULL, diccionario, colores, color_principal, tema){
      super$initialize(diseno, bd, diccionario, colores, color_principal, tema)
    },

    ################################### Función máximo  ###################################
 
    #' Resalta el valor máximo de una métrica 
    #'
    #' Esta función modifica la columna `color` de `self$tbl`, asignando 
    #' un color especial (`col_max`) a la fila que contiene el valor máximo
    #' de la variable indicada en `freq`.

      color_maximo = function(col_max,freq="media") {

      self$tbl <- self$tbl |> mutate(color = dplyr::if_else(!!rlang::sym(freq) == max(!!rlang::sym(freq)), !!col_max, color))  

      invisible(self)
      },

    ##############

    ################################### función de degradado continuo ###################################

    #' Asigna un degradado de colores continuo a una métrica
    #'
    #' Esta función aplica una escala de color continua a la columna indicada 
    #' en `freq` (por defecto "media"). Cada valor recibe un color interpolado 
    #' entre los colores definidos en `colores_base`. 
    #' 
    #' Opcionalmente, si se pasa un color en `col_max`, también se resalta el 
    #' valor máximo con ese color (utilizando `self$color_maximo`).
    #' 

    degradado_continuo = function(colores_base,col_max = "",freq='media') {
    
    
      escala_color <- scales::col_numeric(
      palette = colores_base,
      domain = range(self$tbl[[freq]], na.rm = TRUE))
      
      #  Asignar color continuo a cada valor de 'media'
      self$tbl <- self$tbl |>
        dplyr::mutate(color = escala_color(!!rlang::sym(freq)))
      
     # Color max
      if (col_max != "") {
        self$color_maximo(col_max, freq = freq)
      }
    
      invisible(self)
    },
    
    #################

    #' Graficar saldos de opinión y conocimiento
    #'
    #' @param sufijo_opinion Sufijo de variables de opinión.
    #' @param cat_ns_nc Categorías de no sabe/no contesta.
    #' @param sufijo_conoce Sufijo de variables de conocimiento.
    #' @param cat_conoce Categorías de conocimiento.
    #' @param actores Vector de actores a graficar.
    #' @param positivas Categorías positivas.
    #' @param negativas Categorías negativas.
    #' @param regular Categoría regular.
    #' @return Un objeto `patchwork` con tres gráficos.
    saldos_opinion = function(sufijo_opinion, cat_ns_nc, sufijo_conoce, cat_conoce,
                              actores, positivas, negativas, regular){

      # --- Opinión ---
      opinion <- paste(sufijo_opinion, actores, sep = "_")
      super$
        contar_variables(variables = opinion, confint = FALSE)$
        filtrar_respuesta(valor = c(positivas, negativas, regular))$
        pegar_diccionario()$
        pegar_color()$
        reordenar_columna(columna = "respuesta", tipo = "manual", c(positivas, regular, negativas))$
        partir_regular(opcion = regular)$
        cambiarSigno_freq(negativo = negativas)$
        reordenar_columna(columna = "nombre", tipo = "suma")$
        etiquetar_regular(regular = regular)

      op <- super$graficar_barras_divergente(
        regular = regular,
        positivas = rev(positivas),
        negativas = negativas
      )

      orden <- self$tbl$nombre |> levels()

      # --- Conocimiento ---
      conoce <- paste(sufijo_conoce, actores, sep = "_")
      super$
        contar_variables(variables = conoce, confint = FALSE)$
        filtrar_respuesta(valor = cat_conoce)$
        pegar_diccionario()$
        pegar_color()$
        reordenar_columna(columna = "nombre", tipo = "manual", orden)

      conoc <- self$tbl |>
        ggplot2::ggplot(ggplot2::aes(x = nombre, y = 1)) +
        ggplot2::geom_tile(ggplot2::aes(fill = media), color = "white", show.legend = FALSE) +
        ggfittext::geom_fit_text(ggplot2::aes(label = scales::percent(media, 1)), contrast = TRUE) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = NULL, title = "Conocimiento") +
        ggplot2::theme(axis.text = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank()) +
        ggplot2::theme_void() +
        ggplot2::theme(text = ggplot2::element_text(family = "Poppins"))

      # --- Ns/Nc ---
      super$
        contar_variables(variables = opinion, confint = FALSE)$
        filtrar_respuesta(valor = cat_ns_nc)$
        pegar_diccionario()$
        pegar_color()$
        reordenar_columna(columna = "nombre", tipo = "manual", orden)

      ns_nc <- super$graficar_barras_h(x = "nombre") +
        ggplot2::theme_void() +
        ggplot2::labs(caption = NULL, title = "No sabe / No contesta") +
        ggplot2::theme(text = ggplot2::element_text(family = "Poppins"))

      # Combinar los tres gráficos en un patchwork
      todo <- op + conoc + ns_nc + patchwork::plot_layout(ncol = 3, widths = c(3, 1, 1))
      return(todo)
    }
  )
)
