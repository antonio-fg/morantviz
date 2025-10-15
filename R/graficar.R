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
  public = list(
    #' @field tbl Tibble con los resultados procesados.
    tbl = NULL,
    #' @field grafica Objeto `ggplot` generado.
    grafica = NULL,
    #' @field diseno Objeto survey design (de `survey`).
    diseno = NULL,
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
    initialize = function(diseno, diccionario, colores, color_principal, tema) {
      self$diseno <- diseno
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

    contar_variables = function(variables, confint){
      self$tbl <- contar_vars_pesos(
        variables = variables,
        confint = confint,
        diccionario = self$diccionario,
        diseno = self$diseno
      )
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
    #' @param ... Niveles en orden manual si `tipo = "manual"`.
    #' @return La tabla interna (`self$tbl`) se actualiza.
    #' @examples
    #' g$reordenar_columna("nombre", "asc")
    #' g$reordenar_columna("respuesta", "manual", c("Sí", "No"))
    reordenar_columna = function(columna, tipo, ...){
      match.arg(tipo, choices = c("manual", "asc", "desc", "suma"))

      if(tipo == "manual"){
        self$tbl <- self$tbl |>
          dplyr::mutate(!!rlang::sym(columna) :=
                          forcats::fct_relevel(!!rlang::sym(columna), ...))
      }

      if(tipo %in% "desc"){
        self$tbl <- self$tbl |>
          dplyr::arrange(dplyr::desc(media)) |>
          dplyr::mutate(!!rlang::sym(columna) :=
                          forcats::fct_inorder(!!rlang::sym(columna)))
      }

      if(tipo == "asc"){
        self$tbl <- self$tbl |>
          dplyr::arrange(media) |>
          dplyr::mutate(!!rlang::sym(columna) :=
                          forcats::fct_inorder(!!rlang::sym(columna)))
      }

      if(tipo == "suma"){
        self$tbl <- self$tbl |>
          dplyr::mutate(!!rlang::sym(columna) :=
                          forcats::fct_reorder(!!rlang::sym(columna), media, .fun = sum))
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
    partir_regular = function(opcion){
      self$tbl <- self$tbl |>
        dplyr::filter(respuesta != !!opcion) |>
        dplyr::bind_rows(
          c(1, -1) |>
            purrr::map_dfr(~{
              self$tbl |>
                dplyr::filter(respuesta == !!opcion) |>
                dplyr::mutate(media = .x*media/2)
            })
        ) |>
        dplyr::mutate(respuesta2 = dplyr::if_else(media < 0 & respuesta == "Regular", "Regular2", respuesta))
      invisible(self)
    },

    #' Cambiar signo a medias de categorías negativas
    #'
    #' @param negativo Vector de respuestas negativas.
    #' @examples
    #' g$cambiarSigno_media(c("Mala", "Muy mala"))
    cambiarSigno_media = function(negativo){
      self$tbl <- self$tbl |>
        dplyr::mutate(media = dplyr::if_else(respuesta %in% !!negativo, -media, media))
      invisible(self)
    },

    #' Etiquetar categoría Regular y porcentajes
    #'
    #' @param regular Valor de la categoría Regular.
    #' @examples
    #' g$etiquetar_regular("Regular").
    etiquetar_regular = function(regular){
      self$tbl <- self$tbl |>
        dplyr::mutate(
          etiqueta = dplyr::case_when(
            media < 0 & respuesta == !!regular ~ "",
            respuesta == !!regular ~ scales::percent(media*2, accuracy = 1),
            TRUE ~ scales::percent(abs(media), accuracy = 1)
          )
        )
      invisible(self)
    },

    #' Pegar colores
    #'
    #' Asigna colores a cada respuesta. Usa `color_principal` si falta.
    #' @examples
    #' g$pegar_color()
    #pegar_color = function(){
    #  self$tbl <- self$tbl |>
    #    dplyr::left_join(self$colores, dplyr::join_by(respuesta)) |>
    #    dplyr::mutate(color = dplyr::if_else(is.na(color), self$color_principal, color))
    #  invisible(self)
    #},
    
  pegar_color = function() {

  if (is.null(self$colores)) {
    # Caso 1: sin paleta definida → usa color principal para todas
    self$tbl <- self$tbl |>
      dplyr::mutate(color = self$color_principal)

  } else if (is.vector(self$colores) && !is.null(names(self$colores))) {
    #  Caso 2: vector nombrado → convertir a tibble
    paleta_extendida <- tibble::tibble(
      respuesta = names(self$colores),
      color = unname(self$colores)
    )

    self$tbl <- self$tbl |>
      dplyr::mutate(respuesta = as.character(respuesta)) |>
      dplyr::left_join(
        paleta_extendida |> dplyr::mutate(respuesta = as.character(respuesta)),
        by = "respuesta"
      ) |>
      dplyr::mutate(
        color = dplyr::if_else(is.na(.data$color), self$color_principal, .data$color)
      )

  } else if (tibble::is_tibble(self$colores)) {
    #  Caso 3: tibble → unir directamente
    if (!all(c("respuesta", "color") %in% names(self$colores))) {
      stop(" El tibble `colores` debe tener columnas `respuesta` y `color`.")
    }

    paleta_extendida <- self$colores |> 
      dplyr::mutate(respuesta = as.character(respuesta))

    self$tbl <- self$tbl |>
      dplyr::mutate(respuesta = as.character(respuesta)) |>
      dplyr::left_join(paleta_extendida, by = "respuesta") |>
      # 🔧 aquí el cambio clave: aseguramos la existencia de color
      dplyr::mutate(
        color = dplyr::if_else(
          is.na(.data$color),
          self$color_principal,
          .data$color
        )
      )
  } else {
    stop(" `colores` debe ser NULL, vector nombrado o tibble con columnas `respuesta` y `color`.")
  }

  invisible(self)
  },


    #' Agregar saldo por grupo
    #'
    #' @param por Variable de agrupación.
    #' @examples
    #' g$agregar_saldo("nombre")
    agregar_saldo = function(por){ 
      self$tbl <- self$tbl |>
        dplyr::mutate(saldo = sum(media), .by = !!rlang::sym(por))
      invisible(self)
    },

    #' Graficar barras horizontales
    #'
    #' @param x Variable en el eje X.
    #' @return Objeto `ggplot`.
    #' @examples
    #' g$graficar_barras_h("nombre")
    graficar_barras_h = function(x){
      self$grafica <- ggplot2::ggplot(self$tbl, ggplot2::aes(x = !!rlang::sym(x), y = media)) +
        ggchicklet::geom_chicklet(ggplot2::aes(fill = color)) +
        ggplot2::geom_text(ggplot2::aes(label = scales::percent(media)),
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

    # Lineas 
    graficar_lineas_clasificacion = function(
      x_var = "respuesta",
      y_var = "media",
      titulo = "",
      subtitulo = "",
      eje_x = "",
      caption="",
      colores = self$color_principal
      ){
        group_var = "codigo"
        aes_args <- aes_string(x = x_var, y = y_var, group = group_var, color = group_var)
        self$grafica <- self$tbl  |> ggplot(aes_args) +
          geom_line(linewidth = 1,color = self$color_principal) +
          geom_point(size = 3,color = self$color_principal) +
          geom_text(aes(label = scales::percent(media,accuracy = 1)),
                      size = 5, hjust = -.1,
                      family = self$tema$text$family,
              vjust = -1, size = 4.2,color = "black") +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                    limits = c(0,1)) +
          theme_minimal(base_size = 14) +
          labs(
            title = titulo,
            subtitle = subtitulo,
            x = eje_x,
            y = NULL,
            caption = caption) +
          theme(
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = 14, color = "gray40", margin = margin(b = 10)),  
            plot.caption = element_text(size = 12, color = "gray40", hjust = 1),
            strip.text = element_text(size = 16, face = "bold")  
          ) + self$tema
          
      return(self$grafica)
    },


    

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
    graficar_barras_divergente = function(regular, positivas, negativas){
      self$grafica <- self$tbl |>
        ggplot2::ggplot(ggplot2::aes(x = nombre, y = media,
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
    initialize = function(diseno, diccionario, colores, color_principal, tema){
      super$initialize(diseno, diccionario, colores, color_principal, tema)
    },


  
  
    #' @param columna_valor Nombre de la columna de la que se calculará el máximo (por defecto es 'media').
    #' @return Modifica el objeto self$tbl, agregando la columna 'Maximo_Global'.
    #' # La lógica de mutate calcula el máximo de la columna especificada
    # y lo asigna a una nueva columna en el data frame de la clase (self$tbl).
  

    ################################### Función máximo  ###################################
    
  color_maximo = function(col_max) {
    
  #  Calcula el valor máximo
  valor_maximo <- max(self$tbl$media, na.rm = TRUE)

  #Esta especificado por el usuario ahora, pero podria estar definido en
  #el script de colores 

  # Modifica directamente el color de la fila con ese máximo
  self$tbl <- self$tbl |>
    dplyr::mutate(
      color = dplyr::if_else(
        media == valor_maximo,
        col_max,
        color
      )
    )

  invisible(self)
  },

degradado_continuo = function(colores_base,resaltar_maximo = TRUE) {


  #  Crear función continua de color según el rango de 'media'
  escala_color <- scales::col_numeric(
    palette = colores_base,
    domain = range(self$tbl$media, na.rm = TRUE)
  )

  #  Asignar color continuo a cada valor de 'media'
  self$tbl <- self$tbl |>
    dplyr::mutate(color = escala_color(media))

  #  Si se desea, resaltar el máximo en un color especial

  col_max <- "#7ad29d" #color temporal
  if (isTRUE(resaltar_maximo)) {
    self$color_maximo(col_max)
  }

  invisible(self)
},


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
        cambiarSigno_media(negativo = negativas)$
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
