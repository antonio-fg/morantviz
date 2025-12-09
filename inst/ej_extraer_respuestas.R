######################################################################
# Extaer respuestas
######################################################################

# Toma la llave de pregunta de la columna codigo:
# Obtiene del diccionario todas las respuestas posibles en el orden correcto.
# Asegura que self$tbl tenga una fila por cada respuesta posible, aunque en los datos originales no haya respuestas (poniendo n = 0).
# Rellena datos de contexto (pregunta, nombre, etc.) en las filas nuevas.
# Garantizar que las columnas n y pct no tengan NA, sino 0.
# Es decir: “completa” la tabla para que todas las categorías de respuesta existan y queden listas para graficar/analizar.

################################### Ejemplos con barras (horizontales y verticales)###################################

devtools::load_all(path = "../morantviz/")

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
  "Facebook",
  "Instagram",
  "TikTok"
)

## Base de datos
bd_redes_soc <- problemas(cat = categorias, llave = "redes_soc", n = 100)


### Diccionario de problemas
dicc <- tibble::tribble(
  ~codigo     , ~pregunta                                       , ~respuestas                              ,
  "redes_soc" , "¿Con qué red social relaciona al candidato?" , "Facebook_Instagram_TikTok_Ninguna_Otra"
)

### Inicialización del objeto g
g <- Graficar$new(
  bd = bd_redes_soc,
  diccionario = dicc,
  colores = colores,
  color_principal = "#340e63",
  tema = tema_morant()
)

g$contar_variables(
  variables = "redes_soc",
  confint = F
)$pegar_diccionario()$extraer_respuestas(
  codigo = "redes_soc"
)$pegar_color()$calcular_pct()$graficar_barras_h(x = "respuesta", y = "pct") #Se recomienda poner este método después de pegar diccionario

g$contar_variables(
  variables = "redes_soc",
  confint = F
)$pegar_diccionario()$extraer_respuestas(
  codigo = "redes_soc"
)$pegar_color()$calcular_pct()$graficar_barras_v(x = "respuesta", y = "pct")
