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
    niveles_ordenados = NULL,

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
    initialize = function(
      diseno = NULL,
      bd = NULL,
      diccionario,
      colores,
      color_principal,
      tema,
      niveles_ordenados
    ) {
      if (!is.null(diseno)) {
        self$diseno <- diseno
        private$pesos <- T
      }
      if (!is.null(bd)) {
        self$bd <- bd |> tibble::as_tibble()
        private$pesos <- F
      }
      self$diccionario <- diccionario
      self$colores <- colores
      self$tema <- tema
      self$color_principal <- color_principal
    },

    ################################### Contar variables ponderadas ###################################

    #'
    #' Llama a `contar_vars_pesos` para obtener proporciones y medias.
    #'
    #' @param variables Vector de nombres de variables.
    #' @param confint Lógico; si calcular intervalos de confianza.
    #' @return La tabla interna (`self$tbl`) se actualiza.
    #' @examples
    #' g$contar_variables(c("conoce_pm_astiazaran", "conoce_pm_delrio"), confint = FALSE)

    contar_variables = function(variables, confint = F, pct = T) {
      if (private$pesos) {
        self$tbl <- contar_vars_pesos(
          variables = variables,
          confint = confint,
          diseno = self$diseno
        )
      }

      if (!private$pesos) {
        self$tbl <- contar_vars(
          bd = self$bd,
          variables = variables,
          pct = pct
        )
      }

      invisible(self)
    },

    ################################### Contar variables ponderadas por grupos ###################################

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
    contar_variables_porGrupos = function(variables, grupos, confint) {
      if (private$pesos) {
        self$tbl <- contar_vars_porGrupos_pesos(
          variables = variables,
          grupos = grupos,
          confint = confint,
          diseno = self$diseno
        )
      } else {
        self$tbl <- contar_vars_porGrupos(
          bd = self$bd,
          variables = variables,
          grupos = grupos
        )
      }

      invisible(self)
    },

    ################################### Contar variable multirespuesta ###################################

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
    contar_variable_multirespuesta = function(variable, sep, confint) {
      if (private$pesos) {
        self$tbl <- contar_multirespuesta_pesos(
          diseno = self$diseno,
          variable = variable,
          sep = sep,
          confint = confint
        )
      } else {
        self$tbl <- contar_multirespuesta(
          bd = self$bd,
          variable = variable,
          sep = sep
        )
      }

      invisible(self)
    },
    calcular_pct = function(var = "n", grupo = "codigo") {
      self$tbl <- self$tbl |>
        mutate(
          pct = !!rlang::sym(var) / sum(!!rlang::sym(var)),
          .by = !!rlang::sym(grupo)
        )

      invisible(self)
    },

    ################################### Filtrar respuestas específicas ###################################

    #'
    #' @param variable Nombre de la variable a filtrar.
    #' @param valor Valores a conservar.
    #' @return La tabla interna (`self$tbl`) se filtra.
    #' @examples
    #' g$filtrar_respuesta(variable = "respuesta", valor = "Sí")
    filtrar_respuesta = function(variable = "respuesta", valor) {
      self$tbl <- self$tbl |>
        dplyr::filter(!!rlang::sym(variable) %in% !!valor)
      invisible(self)
    },

    ################################### Reordenar una columna ###################################

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
    reordenar_columna = function(columna, tipo, freq = "media", ...) {
      match.arg(tipo, choices = c("manual", "asc", "desc", "suma"))

      if (tipo == "manual") {
        self$tbl <- self$tbl |>
          dplyr::mutate(
            !!rlang::sym(columna) := forcats::fct_relevel(
              !!rlang::sym(columna),
              ...
            )
          )
      }

      if (tipo %in% "desc") {
        self$tbl <- self$tbl |>
          dplyr::arrange(dplyr::desc(!!rlang::sym(freq))) |>
          dplyr::mutate(
            !!rlang::sym(columna) := forcats::fct_inorder(!!rlang::sym(columna))
          )
      }

      if (tipo == "asc") {
        self$tbl <- self$tbl |>
          dplyr::arrange(!!rlang::sym(freq)) |>
          dplyr::mutate(
            !!rlang::sym(columna) := forcats::fct_inorder(!!rlang::sym(columna))
          )
      }

      if (tipo == "suma") {
        self$tbl <- self$tbl |>
          dplyr::mutate(
            !!rlang::sym(columna) := forcats::fct_reorder(
              !!rlang::sym(columna),
              !!rlang::sym(freq),
              .fun = sum
            )
          )
      }

      invisible(self)
    },

    ################################### Envolver etiquetas de texto ###################################

    #'
    #' @param columna Columna de etiquetas.
    #' @param ancho Número máximo de caracteres por línea.
    #' @examples
    #' g$envolver_etiquetas("nombre", ancho = 12)
    envolver_etiquetas = function(columna, ancho) {
      self$tbl <- self$tbl |>
        dplyr::mutate(
          !!rlang::sym(columna) := stringr::str_wrap(
            !!rlang::sym(columna),
            width = ancho
          )
        )
      invisible(self)
    },

    ################################### Pegar diccionario de variables ###################################

    #'
    #' Hace un `left_join` entre la tabla de resultados y el diccionario.
    #' @examples
    #' g$pegar_diccionario()
    pegar_diccionario = function() {
      self$tbl <- self$tbl |>
        dplyr::left_join(self$diccionario, dplyr::join_by(codigo))
      invisible(self)
    },

    ################################### Partir la categoría "Regular" en dos mitades ###################################

    #'
    #' Divide la proporción de "Regular" en dos (positivo/negativo).
    #'
    #' @param opcion Valor de la categoría Regular.
    #' @examples
    #' g$partir_regular("Regular")
    partir_regular = function(opcion, freq = "media") {
      self$tbl <- self$tbl |>
        dplyr::filter(respuesta != !!opcion) |>
        dplyr::bind_rows(
          c(1, -1) |>
            purrr::map_dfr(
              ~ {
                self$tbl |>
                  dplyr::filter(respuesta == !!opcion) |>
                  dplyr::mutate(
                    !!rlang::sym(freq) := .x * !!rlang::sym(freq) / 2
                  )
              }
            )
        ) |>
        dplyr::mutate(
          respuesta2 = dplyr::if_else(
            !!rlang::sym(freq) < 0 & respuesta == "Regular",
            "Regular2",
            respuesta
          )
        )

      invisible(self)
    },

    ################################### Cambiar signo a medias de categorías negativas ###################################

    #'
    #' @param negativo Vector de respuestas negativas.
    #' @examples
    #' g$cambiarSigno_freq(c("Mala", "Muy mala"))
    cambiarSigno_freq = function(negativo, freq = "media") {
      self$tbl <- self$tbl |>
        dplyr::mutate(
          !!rlang::sym(freq) := dplyr::if_else(
            respuesta %in% !!negativo,
            -!!rlang::sym(freq),
            !!rlang::sym(freq)
          )
        )
      invisible(self)
    },

    ################################### Etiquetar categoría Regular y porcentajes ###################################

    #'
    #' @param regular Valor de la categoría Regular.
    #' @examples
    #' g$etiquetar_regular("Regular").
    etiquetar_regular = function(regular, freq) {
      self$tbl <- self$tbl |>
        dplyr::mutate(
          etiqueta = dplyr::case_when(
            !!rlang::sym(freq) < 0 & respuesta == !!regular ~ "",
            respuesta == !!regular ~
              scales::percent(!!rlang::sym(freq) * 2, accuracy = 1),
            TRUE ~ scales::percent(abs(!!rlang::sym(freq)), accuracy = 1)
          )
        )
      invisible(self)
    },

    #
    ################################### Pegar colores ###################################

    #'
    #' Asigna colores a cada respuesta. Usa `color_principal` si falta.
    #' Volví a utilizar la función inicial de pegar_color()
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

    ################################### Pegar color por grupo ###################################

    #' La función asigna a self$tbl, los colores definidos en self$colores.
    #' Por defecto usa la columna "codigo" (las llaves) de self$tbl, ya que cada "llave" puede representar un grupo diferente,
    #' pero permite usar cualquier otra columna, por ejemplo "nombre"
    #' Si algún grupo no tiene color asignado, se le asigna automáticamente self$color_principal.
    #' Este método es comunmente usado en "graficar lineas" (ver el ejemplo ej_graficar_lineas.R)
    #'
    #' @param col. Nombre de la columna de self$tbl que determina los grupos.
    #' @examples
    #' g$pegar_colorPorGrupo(col = "nombre")
    #'

    pegar_colorPorGrupo = function(col = "codigo") {
      color_grupo <- self$colores |>
        rename(!!sym(col) := respuesta)

      self$tbl <- self$tbl |>
        left_join(color_grupo, by = col) |>
        mutate(
          color = if_else(
            is.na(.data$color),
            self$color_principal,
            .data$color
          )
        )
      invisible(self)
    },

    ################################### Agregar saldo por grupo ###################################

    #' @param por Variable de agrupación.
    #' @examples
    #' g$agregar_saldo("nombre")
    agregar_saldo = function(por, freq = "media") {
      self$tbl <- self$tbl |>
        dplyr::mutate(saldo = sum(!!rlang::sym(freq)), .by = !!rlang::sym(por))
      invisible(self)
    },

    ################################### Método de extraer respuestas ###################################

    #' Asegura que todas las respuestas definidas en el diccionario aparezcan en self$tbl,
    #' completa niveles faltantes "codigo", "nombre", "pregunta", "respuestas",
    #' esto con el fin de poder graficar las captions, las etiquetas correctamente.
    #' Extrae los niveles ordenados del diccionario para un código y los usa para
    #' completar filas faltantes en `self$tbl` mediante `tidyr::complete()`.
    #' Rellena columnas de contexto y normaliza columnas numéricas (`n`, `pct`).
    #'
    #' @param codigo Llave usada para recuperar los niveles del diccionario.

    extraer_respuestas = function(codigo) {
      # Extraer niveles ordenados desde el diccionario
      niveles_ordenados <- self$diccionario |>
        dplyr::filter(.data$codigo %in% .env$codigo) |>
        dplyr::pull(respuestas) |>
        stringr::str_split("_") |>
        purrr::pluck(1)

      self$niveles_ordenados <- niveles_ordenados

      # Identifica las principales columnas de contexto disponibles en self$tbl, ya que puede que no existan todas
      columnas_contexto <- intersect(
        c("codigo", "nombre", "pregunta", "respuestas"),
        colnames(self$tbl)
      )

      df <- self$tbl # por simplicidad

      # Completa las categorías faltantes según niveles_ordenados.
      # Si existe "codigo", completa por cada llave; de lo contrario, completa globalmente.

      if ("codigo" %in% names(df)) {
        df <- df |>
          dplyr::group_by(codigo) |>
          tidyr::complete(
            respuesta = niveles_ordenados,
            fill = list(n = 0)
          ) |>
          dplyr::ungroup()
      }

      # Rellena los NA en las columnas de contexto: "codigo", "nombre", "pregunta", "respuestas",
      # hacia abajo y arriba, garantizando que la columnas estén bien definidas para graficar las captions y etiquetas

      if (length(columnas_contexto) > 0) {
        df <- df |>
          tidyr::fill(dplyr::all_of(columnas_contexto), .direction = "downup")
      }

      # Asegura que n y pct en 0 donde haya NA
      df <- df |>
        dplyr::mutate(
          dplyr::across(
            dplyr::any_of(c("n", "pct")),
            ~ tidyr::replace_na(.x, 0)
          )
        )

      self$tbl <- df
      invisible(self)
    },

    ################################### Función máximo  ###################################

    #' Resalta el valor máximo de una métrica
    #'
    #' Esta función modifica la columna `color` de `self$tbl`, asignando
    #' un color especial (`col_max`) a la fila que contiene el valor máximo
    #' de la variable indicada en `freq`.

    color_maximo = function(col_max, freq = "media") {
      self$tbl <- self$tbl |>
        mutate(
          color = dplyr::if_else(
            !!rlang::sym(freq) == max(!!rlang::sym(freq)),
            !!col_max,
            color
          )
        )

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

    degradado_continuo = function(colores_base, col_max = "", freq = 'media') {
      escala_color <- scales::col_numeric(
        palette = colores_base,
        domain = range(self$tbl[[freq]], na.rm = TRUE)
      )

      #  Asignar color continuo a cada valor de 'media'
      self$tbl <- self$tbl |>
        dplyr::mutate(color = escala_color(!!rlang::sym(freq)))

      # Color max
      if (col_max != "") {
        self$color_maximo(col_max, freq = freq)
      }

      invisible(self)
    },

    ################################### Graficar barras horizontales ##################################
    #'
    #' @param x Variable en el eje X.
    #' @return Objeto `ggplot`.
    #' @examples
    #' g$graficar_barras_h("nombre")
    graficar_barras_h = function(
      x,
      y = "media",
      letra_tam = 5,
      hjust = -.1,
      ancho_cap = 80,
      ancho_etiquetas = 25
    ) {
      envoltura_cap <- stringr::str_wrap(
        self$tbl$pregunta[1],
        width = ancho_cap
      )

      self$grafica <- ggplot2::ggplot(
        self$tbl,
        ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))
      ) +
        ggchicklet::geom_chicklet(ggplot2::aes(fill = color)) +
        ggplot2::geom_text(
          ggplot2::aes(label = scales::percent(!!rlang::sym(y))),
          size = letra_tam,
          hjust = hjust,
          family = self$tema$text$family
        ) +
        ggplot2::coord_flip() +
        ggplot2::labs(caption = envoltura_cap) +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format(accuracy = 1),
          limits = c(0, 1)
        ) +
        ggplot2::scale_fill_identity() +
        ggplot2::scale_x_discrete(
          labels = ~ stringr::str_wrap(.x, width = ancho_etiquetas)
        ) +
        self$tema
      return(self$grafica)
    },

    ################################### Graficar barras verticales ###################################

    #'
    #' @param y Variable en el eje y.
    #' @return Objeto `ggplot`.
    #' @examples
    #' g$graficar_barras_v("nombre")
    graficar_barras_v = function(x, y = "media", 
                                    letra_tam = 5, 
                                    vjust = -.1,
                                    ancho_cap = 80,
                                    ancho_etiquetas = 25) {
      
      envoltura_cap <- stringr::str_wrap(self$tbl$pregunta[1], width = ancho_cap)
      
      self$grafica <- ggplot2::ggplot(
        self$tbl,
        ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))
      ) +
        ggchicklet::geom_chicklet(ggplot2::aes(fill = color), width = 0.8) +
        ggplot2::geom_text(
          ggplot2::aes(label = scales::percent(!!rlang::sym(y), accuracy = 1)),
          size = letra_tam,
          vjust = vjust,
          family = self$tema$text$family
        ) +
        ggplot2::labs(caption = envoltura_cap) +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format(accuracy = 1),
          limits = c(0, 1)
        ) +
        ggplot2::scale_fill_identity() +
        ggplot2::scale_x_discrete(labels = ~stringr::str_wrap(.x, width = ancho_etiquetas)) +
        self$tema
      return(self$grafica)
    },

    ################################### Graficar dona o gauge ###################################
    #'
    #' @param x Variable en el eje x.
    #' @return Objeto `ggplot`.
    #' @
    #' g$graficar_gauge("nombre")

    graficar_gauge = function(letra_tam = 12, freq = "media") {
      valor <- self$tbl |>
        dplyr::filter(respuesta %in% c("Sí", "Sí lo conoce")) |>
        dplyr::pull(freq)

      self$grafica <- self$tbl |>
        ggplot2::ggplot(ggplot2::aes(x = "", y = !!sym(freq), fill = color)) +
        ggplot2::geom_col(width = 0.4) +
        ggplot2::coord_polar(theta = "y", start = 0) +
        ggplot2::scale_fill_identity() +
        ggplot2::theme_void() +
        ggplot2::annotate(
          "text",
          x = 0,
          y = 0,
          label = base::paste0(scales::percent(valor, accuracy = 1)),
          size = letra_tam,
          fontface = "bold",
          color = "black"
        )
      return(self$grafica)
    },

    ################################### Graficar piramide ###################################

    #' Necesario hacer cruce de rango edad por sexo
    #' @param x Variable en el eje x.
    #' @return Objeto `ggplot`.
    #' @ examples
    #' g$contar_variables_porGrupos(variables = c("rango_edad"),grupos = c("sexo"), confint = F)
    #' g$graficar_piramide()
    graficar_piramide = function(
      cantidad_puntos = 30,
      tam_punto = 6,
      tam_texto_etiqueta_porcentaje = 6,
      separacion_texto = 1.5,
      espaciado = c(1, 1),
      tam_texto_rango_edad = 6
    ) {
      #Parámetros fijos
      grupo_izquierda <- "F"
      grupo_derecha <- "M"
      columna_categoria <- "respuesta"
      columna_grupo <- "sexo"
      columna_pct <- "media"
      fuente <- "Montserrat"
      titulo <- "Distribución poblacional por sexo"
      mostrar_etiquetas <- TRUE
      niveles_respuesta <- (base::unique(g$tbl$respuesta))
      colores_personalizados <- stats::setNames(
        c("#94D0CC", "#B49FCC"),
        c(grupo_izquierda, grupo_derecha)
      )

      # --- 3. Expandir puntos ---
      bd_expandido <- self$tbl %>%
        dplyr::filter(
          .data[[columna_grupo]] %in% c(grupo_izquierda, grupo_derecha)
        ) %>%
        dplyr::mutate(
          grupo = .data[[columna_grupo]],
          categoria = base::factor(
            .data[[columna_categoria]],
            levels = base::rev(
              niveles_respuesta %||% base::unique(.data[[columna_categoria]])
            )
          ),
          puntos = base::round(.data[[columna_pct]] * cantidad_puntos)
        ) %>%
        dplyr::filter(puntos > 0) %>%
        tidyr::uncount(puntos, .remove = FALSE) %>%
        dplyr::group_by(categoria, grupo) %>%
        dplyr::mutate(
          fila = dplyr::row_number(),
          x = dplyr::if_else(grupo == grupo_izquierda, -fila, fila),
          etiqueta = dplyr::if_else(
            fila == base::max(fila),
            base::paste0(
              base::round(100 * dplyr::first(.data[[columna_pct]]), 1),
              "%"
            ),
            NA_character_
          )
        ) %>%
        dplyr::ungroup()

      # --- 4. Etiquetas del centro ---
      etiquetas_centro <- bd_expandido %>%
        dplyr::distinct(categoria) %>%
        dplyr::mutate(x = 0, y = categoria)
      # --- 5. Colores --
      colores_usar <- colores_personalizados %||%
        stats::setNames(
          c("#94D0CC", "#B49FCC"),
          c(grupo_izquierda, grupo_derecha)
        )

      x_max <- base::max(base::abs(bd_expandido$x)) + 4 # margen de 2 puntos de ancho

      g_piramide <- ggplot2::ggplot(
        bd_expandido,
        ggplot2::aes(x = x, y = categoria, color = grupo)
      ) +
        ggplot2::geom_point(size = tam_punto, alpha = 0.8) +
        ggplot2::scale_color_manual(values = colores_usar, name = NULL) +
        ggplot2::scale_x_continuous(
          limits = c(-x_max, x_max),
          breaks = NULL,
          expand = c(0, 0)
        ) +
        ggplot2::scale_y_discrete(
          position = "right",
          expand = expansion(mult = espaciado)
        ) +
        ggplot2::geom_text(
          data = etiquetas_centro,
          aes(x = 0, y = y, label = y),
          inherit.aes = FALSE,
          size = tam_texto_rango_edad,
          hjust = 0.5,
          vjust = 0.5,
          family = fuente,
          color = "black"
        ) +
        ggplot2::geom_vline(
          xintercept = 0,
          color = "gray70",
          linetype = "dotted",
          linewidth = 0.3
        ) +
        ggplot2::theme_minimal(base_family = fuente) +
        ggplot2::labs(title = " ", x = NULL, y = NULL) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            face = "bold",
            hjust = 0.5,
            size = 10,
            family = fuente
          ),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          legend.position = "top",
          legend.text = ggplot2::element_text(size = 9, family = fuente)
        ) +
        ggplot2::geom_text(
          ggplot2::aes(
            label = etiqueta,
            hjust = dplyr::if_else(grupo == grupo_izquierda, 1.1, -0.1)
          ),
          size = tam_texto_etiqueta_porcentaje,
          na.rm = TRUE,
          family = fuente,
          color = "black",
          nudge_x = dplyr::if_else(
            bd_expandido$grupo == grupo_izquierda,
            -separacion_texto,
            separacion_texto
          )
        )
      return(g_piramide)
    },

    ################################### Graficar lollipops sin multirespuesta ###################################
    #'
    #' @param x Variable en el eje y.
    #' @return Objeto `ggplot`.
    #' @
    #' g$graficar_lollipops("respuesta")
    graficar_lollipops = function(
      x,
      y = "media",
      letra_tam = 6,
      hjust = -0.5,
      bola_tam = 5,
      ancho_cap = 80,
      ancho_etiquetas = 25
    ) {
      envoltura_cap <- stringr::str_wrap(
        self$tbl$pregunta[1],
        width = ancho_cap
      )

      self$grafica <- self$tbl |>
        ggplot2::ggplot(ggplot2::aes(
          x = stats::reorder(!!rlang::sym(x), !!rlang::sym(y)),
          y = !!rlang::sym(y),
          color = color
        )) +
        ggplot2::geom_segment(
          ggplot2::aes(xend = !!rlang::sym(x), y = 0, yend = !!rlang::sym(y)),
          linewidth = 1
        ) +
        ggplot2::geom_point(size = bola_tam) +
        ggplot2::geom_text(
          ggplot2::aes(label = scales::percent(!!rlang::sym(y), accuracy = 1.)),
          size = letra_tam,
          hjust = hjust,
          color = "black"
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_x_discrete(labels = function(x) {
          stringr::str_wrap(string = x, width = 60)
        }) +
        ggplot2::scale_y_continuous(
          limits = c(0, 1),
          labels = scales::percent_format()
        ) +
        ggplot2::scale_color_identity(guide = "none") +
        ggplot2::scale_x_discrete(
          labels = ~ stringr::str_wrap(.x, width = ancho_etiquetas)
        ) +
        #ggplot2::labs(title = " ") +
        ggplot2::labs(caption = envoltura_cap) +
        self$tema
      #ggplot2::theme(
      #  plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      #  axis.text.y = ggplot2::element_text(
      #    size = 15,
      #    family = self$tema$text$family
      #  ),
      #  panel.grid.major.y = ggplot2::element_blank(),
      #  panel.grid.minor = ggplot2::element_blank(),
      #  plot.background = ggplot2::element_rect(
      #    color = "transparent",
      #    fill = "transparent"
      #  ),
      #  panel.background = ggplot2::element_rect(
      #    color = "transparent",
      #    fill = "transparent"
      #  ),
      #  legend.background = ggplot2::element_rect(
      #    color = "transparent",
      #    fill = "transparent"
      #  )
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
      x = "respuesta",
      freq = "media",
      grupo = 'codigo',
      grupos_seleccion = NULL,
      vjust = -0.8,
      letra_tam = 4,
      rango_offset = 0.0,
      ancho_cap = 80,
      ancho_etiquetas = 25
    ) {
      envoltura_cap <- stringr::str_wrap(
        self$tbl$pregunta[1],
        width = ancho_cap
      )

      tbl_filtrada <- self$tbl

      if (!is.null(grupos_seleccion)) {
        tbl_filtrada <- tbl_filtrada |>
          filter(!!sym(grupo) %in% grupos_seleccion)
      }

      ## Parte de la superposición de los porcentajes
      #tbl_filtrada <- tbl_filtrada |>
      #  dplyr::group_by(!!rlang::sym(x)) |>
      #  # ordena por valor dentro de cada categoría del eje X
      #  dplyr::arrange(!!rlang::sym(freq), .by_group = TRUE) |>
      #  # asigna offsets simétricos alrededor de 0
      #  dplyr::mutate(
      #    offset_label = if (dplyr::n() == 1) {
      #      0
      #    } else {
      #      seq(
      #        from = -rango_offset,
      #        to = rango_offset,
      #        length.out = dplyr::n()
      #      )
      #    },
      #    y_label = !!rlang::sym(freq) + offset_label
      #  ) |>
      #  dplyr::ungroup()

      paleta <- tbl_filtrada |>
        distinct(!!sym(grupo), color)

      self$grafica <- tbl_filtrada |>
        ggplot(
          aes(
            x = !!sym(x),
            y = !!sym(freq),
            group = !!sym(grupo),
            color = color
          )
        ) +
        geom_line(linewidth = 1) +
        geom_point(size = 3) +
        geom_text(
          aes(y = !!sym(freq), label = scales::percent(!!sym(freq), accuracy = 1)),
          vjust = vjust,
          size = letra_tam,
          color = "black",
          show.legend = FALSE,
          max.overlaps = Inf
        ) +
        scale_color_identity(
          breaks = paleta$color,
          labels = paleta[[grupo]],
          guide = "legend"
        ) +
        scale_y_continuous(
          labels = scales::percent_format(accuracy = 1),
          # limits = c(0, 1)
        ) +
        # ggplot2::scale_x_discrete(
        #   labels = ~ stringr::str_wrap(.x, width = ancho_etiquetas)
        # ) +
        self$tema +
        ggplot2::theme(
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = ggplot2::element_blank()
        ) +
        ggplot2::guides(
          color = ggplot2::guide_legend(
            nrow = 1,
            byrow = TRUE
          )
        ) +
        labs(
          caption = envoltura_cap
        )
      return(self$grafica)
    },

    ################################### Graficar barras apiladas ###################################

    #' Graficar barras apiladas
    #'
    #' Genera un gráfico de barras apiladas, donde cada barra representa una categoría en el eje X y se descompone en
    #' segmentos definidos por la variable `fill`. Las etiquetas de porcentaje
    #' se muestran centradas dentro de cada segmento cuando su valor es al menos "5%"".
    #'
    #' Los colores de los segmentos se obtienen a partir de la columna `color` en
    #' `self$tbl`, construyendo un vector nombrado que se pasa a `scale_fill_manual()`.
    #' De manera opcional se puede agregar un caption siempre y cuando se espcifique @param caption `True`.
    #' Las etiquetas como la caption pueden envolverse usando un ancho configurable.
    #'
    #' @param x Nombre de la columna de `self$tbl` que se usa en el eje X
    #' @param freq Nombre de la columna numérica que contiene la frecuencia
    #'   o porcentaje a graficar. Por defecto `"pct"`.
    #' @param letra_tam Tamaño de las etiquetas de texto.
    #' @param fill Nombre de la columna categórica que define los segmentos
    #'   apilados y la leyenda. Por defecto `"respuesta"`.
    #' @param caption Valor lógico; si es `TRUE`, utiliza `self$tbl$pregunta[1]`
    #'   como caption, envuelto con `stringr::str_wrap()` a `ancho_cap`. Si es
    #'   `FALSE`, no se muestra caption.
    #' @param ancho_cap Ancho (en caracteres) usado para envolver el texto
    #'   del caption.
    #' @param ancho_etiquetas Ancho (en caracteres) usado para envolver las
    #'   etiquetas del eje X.
    #'
    #' @return Un objeto `ggplot` con el gráfico de barras apiladas. Además,
    #'   el gráfico se guarda en `self$grafica`.

    barras_apiladas = function(
      x,
      freq = "pct",
      letra_tam = 3.5,
      fill = "respuesta",
      caption = F,
      ancho_cap = 80,
      ancho_etiquetas = 25
    ) {
      if (isTRUE(caption)) {
        envoltura_cap <- stringr::str_wrap(
          self$tbl$pregunta[1],
          width = ancho_cap
        )
      } else {
        envoltura_cap <- NULL
      }
      # Vector nombrado: nombres = niveles de 'fill', valores = color hex
      colores_partidos <- self$tbl |>
        distinct(!!sym(fill), color) |>
        arrange(!!sym(fill)) |>
        deframe()

      self$grafica <- ggplot(
        self$tbl,
        aes(x = !!sym(x), y = !!sym(freq), fill = !!sym(fill))
      ) +
        geom_col(width = 0.7, color = NA) +
        coord_flip() +
        geom_text(
          aes(
            label = ifelse(
              !!sym(freq) >= 0.05,
              percent(!!sym(freq), accuracy = 1),
              NA
            )
          ),
          position = position_stack(vjust = 0.5),
          color = "white",
          size = letra_tam
        ) +
        scale_y_continuous(
          labels = percent_format(accuracy = 1),
          expand = c(0, 0)
        ) +
        scale_fill_manual(
          values = colores_partidos,
          drop = FALSE,
          breaks = names(colores_partidos),
          labels = names(colores_partidos)
        ) +
        labs(
          x = NULL,
          y = NULL,
          caption = envoltura_cap
        ) +
        ggplot2::scale_x_discrete(
          labels = ~ stringr::str_wrap(.x, width = ancho_etiquetas)
        ) +
        self$tema +
        theme(
          legend.position = "bottom",
          legend.title = element_blank()
        )

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

    graficar_sankey = function(grupo, freq = "media", letra_tam = 3.5) {
      sankey_df <- self$tbl %>%
        select(grupo, respuesta, !!sym(freq))

      sankey_long <- sankey_df %>%
        make_long(grupo, respuesta, value = !!sym(freq))

      paleta <- setNames(self$colores$color, self$colores$respuesta)

      self$grafica <- ggplot(
        sankey_long,
        aes(
          x = x,
          next_x = next_x,
          node = node,
          next_node = next_node,
          value = value,
          fill = node
        )
      ) + # los flujos toman color del nodo
        geom_sankey(flow.alpha = 0.9, color = NA) +
        geom_sankey_label(
          aes(label = node),
          fill = "white",
          alpha = 0.7,
          size = letra_tam,
          color = "black"
        ) +
        scale_fill_manual(values = paleta, na.value = "grey90") + # usa paleta, gris claro para extras
        scale_y_continuous(breaks = NULL) +
        labs(
          caption = ifelse(
            is.na(self$tbl$pregunta[1]),
            "Sin pregunta definida",
            self$tbl$pregunta[1]
          )
        ) +
        theme_void() +
        self$tema

      return(self$grafica)
    },

    ################################### Procesar nubes ###################################

    #' Aplica el método multirespuesta para poder calcular las frecuencias y porcentajes
    #' de cada categoría y los asignarlos en la `self$tbl`
    #'

    procesar_nubes = function(codigo, confint = F) {
      self$contar_variable_multirespuesta(
        variable = paste("categoria", codigo, sep = "_"),
        sep = ">>>",
        confint
      )
    },

    ################################### Graficar nube ###################################

    #' Grafica la nube de las categorias previemente procesadas
    #'
    #' @return Nube de palabras.

    graficar_nube = function(
      max_size = 15,
      n = 5,
      gradiente = c(bajo = "#ca4992", alto = "#5B1AA4")
    ) {
      self$grafica <- self$tbl |>
        filter(
          !respuesta %in% c("categoría inventada", "sin_categoria"),
          n >= !!n
        ) |>
        mutate(respuesta = str_to_sentence(respuesta)) |>
        ggplot(aes(label = respuesta, size = n, color = n)) +
        geom_text_wordcloud() +
        scale_size_area(max_size = max_size) +
        scale_color_gradient(
          low = gradiente["bajo"],
          high = gradiente["alto"]
        ) +
        tema_morant()
      return(self$grafica)
    },

    #############################

    #' Graficar Bloque
    #'
    #' Permite mostrar distintos tipos de métricas (media, n, porcentaje) dentro de cada bloque.
    #'
    #' @param freq Character. Indica la variable que se usará para determinar el tipo de los bloques.
    #'   Puede ser `"media"`, `"n"` o `"pct"`. Por defecto es `"media"`.
    #'
    #' @return
    #' Objeto `ggplot2::ggplot`. Se almacena en `self$grafica` y se retorna
    #' para permitir manipulaciones adicionales o guardado.
    graficar_bloque = function(freq = "media", letra_tam = 2) {
      # Crear la gráfica
      # Crear símbolo una sola vez

      self$grafica <- ggplot(
        self$tbl,
        aes(area = !!rlang::sym(freq), fill = color)
      ) +
        geom_treemap(color = "white", size = letra_tam) +
        geom_treemap_text(
          aes(
            label = paste0(
              respuesta,
              " ",
              scales::percent(!!rlang::sym(freq), accuracy = 1)
            )
          ),
          color = "white",
          place = "centre",
          grow = TRUE,
          reflow = TRUE,
          family = self$tema$text$family,
          fontface = "bold",
          size = 10
        ) +
        scale_fill_identity() +
        labs(caption = self$tbl$pregunta[1]) +
        self$tema
      return(self$grafica)
    },

    ###################################  Graficar barras divergentes ###################################

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
    graficar_barras_divergente = function(
      regular,
      positivas,
      negativas,
      letra_tam = 25,
      vjust = .5,
      x = "nombre",
      y = "media",
      caption = F,
      ancho_cap = 80,
      ancho_etiquetas = 25
    ) {
      if (isTRUE(caption)) {
        envoltura_cap <- stringr::str_wrap(
          self$tbl$pregunta[1],
          width = ancho_cap
        )
      } else {
        envoltura_cap <- NULL
      }

      self$grafica <- self$tbl |>
        ggplot2::ggplot(ggplot2::aes(
          x = !!rlang::sym(x),
          y = !!rlang::sym(y),
          group = factor(
            respuesta2,
            c(regular, paste0(regular, "2"), positivas, negativas)
          )
        )) +
        ggchicklet::geom_chicklet(ggplot2::aes(
          fill = respuesta,
          color = respuesta
        )) +
        ggfittext::geom_fit_text(
          ggplot2::aes(label = etiqueta),
          size = letra_tam,
          position = ggplot2::position_stack(.5, reverse = TRUE), #posición del porcentaje, centro
          vjust = vjust,
          contrast = TRUE,
          show.legend = FALSE,
          family = self$tema$text$family
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(
          values = self$tbl |>
            dplyr::distinct(respuesta, color) |>
            dplyr::pull(color) |>
            purrr::set_names(
              self$tbl |>
                dplyr::distinct(respuesta, color) |>
                dplyr::pull(respuesta)
            )
        ) +
        ggplot2::scale_color_manual(
          values = self$tbl |>
            dplyr::distinct(respuesta, color) |>
            dplyr::pull(color) |>
            purrr::set_names(
              self$tbl |>
                dplyr::distinct(respuesta, color) |>
                dplyr::pull(respuesta)
            )
        ) +
        ggplot2::scale_x_discrete(
          labels = ~ stringr::str_wrap(.x, width = ancho_etiquetas)
        ) +
        self$tema +
        ggplot2::theme(legend.position = "bottom") +
        lemon::scale_y_symmetric(labels = function(x) {
          scales::percent(abs(x), accuracy = 1)
        }) +
        ggplot2::labs(caption = envoltura_cap, fill = NULL, color = NULL)
      return(self$grafica)
    },

    #' Preparar datos para gráfico de waffle
    #' @param eje_y,            columna fija (eje Y)
    #' @param  valor           "valor",   valor resultante
    #' @param eje_x,            Eje X (ej. "region")

    #' @return Actualiza self$tbl con los datos preparados.
    generar_coordenadas = function(eje_y, eje_x, valor = "media") {
      stopifnot(!is.null(self$tbl))
      df <- self$tbl

      # Validación de columnas
      if (!all(c(eje_y, eje_x, valor) %in% names(df))) {
        stop("Algunas columnas indicadas no existen en self$tbl.")
      }

      # ----------------Niveles para factores --------------------

      #niveles_x: toma los valores únicos de la columna eje_x para mantener el orden de los niveles en el eje X.
      #niveles_y: agrupa por eje_y,  extrae los valores de la columna eje_y

      niveles_x <- df %>% dplyr::pull(!!rlang::sym(eje_x)) %>% unique() #levels(df[[eje_x]])
      niveles_y <- df %>%
        group_by(!!rlang::sym(eje_y)) %>%
        summarise(promedio = mean(!!rlang::sym(valor), na.rm = TRUE)) %>%
        pull(!!rlang::sym(eje_y))

      #----------------- Preparar tabla base con posiciones--------------------
      df <- df %>%
        mutate(
          !!rlang::sym(eje_x) := factor(
            !!rlang::sym(eje_x),
            levels = niveles_x
          ),
          !!rlang::sym(eje_y) := factor(
            !!rlang::sym(eje_y),
            levels = rev(niveles_y)
          ),
          col = as.numeric(!!rlang::sym(eje_x)),
          row = as.numeric(!!rlang::sym(eje_y)),
          fill_id = !!rlang::sym(valor),
          .id = row_number()
        )

      #---------------- Definir forma squircle ----------------------------
      # Genera los puntos de un squircle (una mezcla entre cuadrado y círculo) que será la forma de cada celda.
      n_puntos <- 100
      a <- 0.45
      b <- 0.45
      exp <- 9
      theta <- seq(0, 2 * pi, length.out = n_puntos)
      base_shape <- tibble(
        x_unit = a * sign(cos(theta)) * abs(cos(theta))^(2 / exp),
        y_unit = b * sign(sin(theta)) * abs(sin(theta))^(2 / exp),
        vertex_id = seq_along(theta)
      )

      #------------------ Combinar celda con vértices del squircle --------------
      # tidyr::crossing(df, base_shape): crea una fila por cada combinación de celda y vértice → así cada celda tendrá 100 puntos para dibujar el squircle.
      # x y y: coordenadas finales de cada vértice sumando la posición de la celda.
      df_squircles <- tidyr::crossing(df, base_shape) %>%
        mutate(
          x = col + x_unit,
          y = row + y_unit
        ) %>%
        arrange(.id, vertex_id)

      # --------------- Crear tabla para etiquetas centradas -----------
      #Extrae información única para colocar las etiquetas en el centro de cada celda.
      #row_pos será usado en geom_text para centrar verticalmente la etiqueta.

      df_etiquetas <- df %>%
        dplyr::distinct(
          .id,
          !!rlang::sym(eje_x),
          !!rlang::sym(eje_y),
          col,
          row,
          fill_id
        ) %>%
        dplyr::mutate(row_pos = row)

      # ----------------- Combinar ----------------------
      # Combina los datos de los polígonos con las etiquetas centradas.
      self$tbl <- df_squircles %>%
        left_join(
          df_etiquetas,
          by = c(".id", eje_x, eje_y, "col", "row", "fill_id")
        )

      invisible(self)
    },

    ################################### Graficar waffle ###################################

    #'
    #' Genera la visualización tipo squircle para mostrar porcentajes
    #'
    #' @param nombre_x        Etiqueta del eje X (horizontal)
    #' @param escala_color    Vector con colores para el gradiente. Debe tener dos elementos: low y high.
    #' @param eje_x           columna de self$tbl que será eje X
    #' @param eje_y           columna de self$tbl que será eje Y
    #' @param caption         agrega el caption a la grafica, la puede llevar o quedar vacio.
    #'
    #' @return Objeto ggplot.
    graficar_waffle = function(
      nombre_x = NULL,
      escala_color = c(low = "#9d7ad240", high = "#9d7ad2"),
      eje_x = "grupo",
      eje_y = "base_y",
      caption = F,
      ancho_cap = 80,
      ancho_etiquetas_x = 25,
      ancho_etiquetas_y = 25
    ) {
      stopifnot(!is.null(self$tbl))

      if (isTRUE(caption)) {
        envoltura_cap <- stringr::str_wrap(
          self$tbl$pregunta[1],
          width = ancho_cap
        )
      } else {
        envoltura_cap <- NULL
      }

      df <- self$tbl

      # --- Gráfico principal ---
      self$grafica <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = x, y = y, group = .id)
      ) +
        ggplot2::geom_polygon(ggplot2::aes(fill = fill_id), color = "white") +
        ggplot2::geom_text(
          ggplot2::aes(
            x = col,
            y = row_pos,
            label = scales::percent(fill_id, accuracy = 1)
          ),
          size = 10,
          color = "black",
          family = self$tema$text$family,
          vjust = 0.5,
          hjust = 0.5
        ) +
        ggplot2::scale_fill_gradient(
          low = escala_color["low"],
          high = escala_color["high"],
        ) +
        ggplot2::scale_x_continuous(
          breaks = seq_along(levels(df[[eje_x]])),
          labels = stringr::str_wrap(
            levels(df[[eje_x]]),
            width = ancho_etiquetas_x
          ),
          position = "top"
        ) +
        ggplot2::scale_y_continuous(
          breaks = seq_along(levels(df[[eje_y]])),
          labels = str_wrap(levels(df[[eje_y]]), width = ancho_etiquetas_y)
        ) +
        ggplot2::labs(
          caption = envoltura_cap,
          x = nombre_x,
        ) +
        self$tema +
        ggplot2::theme(
          axis.title.x = ggplot2::element_text(
            size = 14,
            face = "bold",
            hjust = 0.5,
            vjust = 2
          ),
          panel.grid = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(face = "bold", size = 12),
          axis.text.y = ggplot2::element_text(size = 14, face = "bold"),
          plot.title = ggplot2::element_text(
            face = "bold",
            size = 16,
            hjust = 0.5
          ),
          plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5)
        ) +
        theme(
          panel.ontop = FALSE,
          panel.grid.major.x = element_line(
            color = "#cccccc",
            linetype = "dashed",
            linewidth = 0.3
          ),
          panel.grid.minor.x = element_blank()
        )
      #+
      #theme(
      #panel.grid.major.x = element_blank(),
      #panel.grid.minor.x = element_blank()
      #)

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
#' \dontrun {
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
    initialize = function(
      diseno = NULL,
      bd = NULL,
      diccionario,
      colores,
      color_principal,
      tema
    ) {
      super$initialize(diseno, bd, diccionario, colores, color_principal, tema)
    },

    #################

    ################################### Graficar saldos de opinión y conocimiento ###################################

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
    saldos_opinion = function(
      sufijo_opinion,
      cat_ns_nc,
      sufijo_conoce,
      cat_conoce,
      actores,
      positivas,
      negativas,
      regular,
      letra_tam = 5,
      letra_tam_con = 20,
      hjust = -.1,
      vjust = .5,
      ancho_etiquetas = 25,
      ancho_ns_nc = 25,
      y = "media"
    ) {
      # --- Opinión ---
      opinion <- paste(sufijo_opinion, actores, sep = "_")
      super$contar_variables(
        variables = opinion,
        confint = FALSE
      )$filtrar_respuesta(
        valor = c(positivas, negativas, regular)
      )$pegar_diccionario()$pegar_color()$reordenar_columna(
        columna = "respuesta",
        tipo = "manual",
        c(positivas, regular, negativas)
      )$partir_regular(opcion = regular)$cambiarSigno_freq(
        negativo = negativas
      )$reordenar_columna(columna = "nombre", tipo = "suma")$etiquetar_regular(
        regular = regular,
        freq = y
      )

      op <- super$graficar_barras_divergente(
        regular = regular,
        positivas = rev(positivas),
        negativas = negativas,
        ancho_etiquetas = ancho_etiquetas
      )

      orden <- self$tbl$nombre |> levels()

      # --- Conocimiento ---
      conoce <- paste(sufijo_conoce, actores, sep = "_")
      super$contar_variables(
        variables = conoce,
        confint = FALSE
      )$filtrar_respuesta(
        valor = cat_conoce
      )$pegar_diccionario()$pegar_color()$reordenar_columna(
        columna = "nombre",
        tipo = "manual",
        orden
      )

      conoc <- self$tbl |>
        ggplot2::ggplot(ggplot2::aes(x = nombre, y = 1)) +
        ggplot2::geom_tile(
          ggplot2::aes(fill = !!rlang::sym(y)),
          color = "white",
          show.legend = F
        ) +
        ggfittext::geom_fit_text(
          ggplot2::aes(label = scales::percent(!!rlang::sym(y), 1)),
          contrast = TRUE,
          size = letra_tam_con,
          # deja crecer el texto si hay espacio
          min.size = 0
        ) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = NULL, title = "Conocimiento") +
        ggplot2::theme(
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank()
        ) +
        ggplot2::theme_void() +
        ggplot2::theme(text = ggplot2::element_text(family = "Poppins"))

      # --- Ns/Nc ---
      super$contar_variables(
        variables = opinion,
        confint = FALSE
      )$filtrar_respuesta(
        valor = cat_ns_nc
      )$pegar_diccionario()$pegar_color()$reordenar_columna(
        columna = "nombre",
        tipo = "manual",
        orden
      )
      titulo_ns_nc <- "No sabe / No contesta"
      ns_nc <- super$graficar_barras_h(
        x = "nombre",
        y = y,
        letra_tam = letra_tam,
        hjust = hjust
      ) +
        ggplot2::theme_void() +
        ggplot2::labs(
          caption = NULL,
          title = stringr::str_wrap(titulo_ns_nc, width = ancho_ns_nc)
        ) +
        ggplot2::theme(text = ggplot2::element_text(family = "Poppins"))

      # Combinar los tres gráficos en un patchwork
      todo <- op +
        conoc +
        ns_nc +
        patchwork::plot_layout(ncol = 3, widths = c(3, 1, 1))
      return(todo)
    }
  )
)
