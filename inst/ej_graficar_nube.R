######################################################################
# Nube de palabras
######################################################################

# La actualización de la nube de palabras permite graficar las categorias generadas por el modelo de categorización (bot).
# Es importante aclarar que las categorias asociadas a las respuestas están previamente cargadas en la base de datos
# antes de inicializar el objeto "g".

library(tibble)
library(tidyr)
library(stringr)
library(ggwordcloud)

devtools::load_all(path = "../morantviz/")

colores <- tibble::tribble(
  ~respuesta    ,
  ~color        ,
  # Morant
  "algun_color" ,
  "#b5e24a"     ,
  "otro_color"  ,
  "#ca4992"
)

### Ejemplo de nubes:

bd <- tribble(
  ~categoria_j_problemaJefe                                        ,
  "carga administrativa"                                           ,
  "no hay trabajo en equipo"                                       ,
  "no atender necesidades"                                         ,
  "carga administrativa"                                           ,
  "no hay trabajo en equipo"                                       ,
  "problemas de convivencia"                                       ,
  "problemas de convivencia"                                       ,
  "problemas de gestión educativa"                                ,
  "dispersión de zonas escolares"                                 ,
  "problemas de convivencia"                                       ,
  "carga administrativa"                                           ,
  "carga administrativa"                                           ,
  "dispersión de zonas escolares"                                 ,
  "poca asesoría o retroalimentación"                            ,
  "atender conflictos o problemas"                                 ,
  "poca asesoría o retroalimentación"                            ,
  "problemas de convivencia"                                       ,
  "poca asesoría o retroalimentación>>>no hay trabajo en equipo" ,
  "atender conflictos o problemas"                                 ,
  "carga administrativa>>>no hay trabajo en equipo"                ,
  "atender conflictos o problemas>>>no atender necesidades"        ,
  "poca asesoría o retroalimentación"                            ,
  "atender conflictos o problemas>>>problemas de convivencia"      ,
  "apatía o actitudes negativas"                                  ,
  "categoría inventada"                                           ,
  "dispersión de zonas escolares"                                 ,
  "problemas de convivencia>>>problemas entre docentes"            ,
  "carga administrativa"                                           ,
  "carga administrativa"                                           ,
  "atender conflictos o problemas"                                 ,
  "categoría inventada"                                           ,
  "problemas entre docentes>>>problemas de convivencia"            ,
  "no atender necesidades"                                         ,
  "categoría inventada"                                           ,
  "atender conflictos o problemas"                                 ,
  "atender conflictos o problemas"                                 ,
  "carga administrativa"                                           ,
  "apatía o actitudes negativas"                                  ,
  "atender conflictos o problemas"                                 ,
  "poca asesoría o retroalimentación"                            ,
  "problemas de convivencia"                                       ,
  "categoría inventada"                                           ,
  "no hay trabajo en equipo"                                       ,
  "problemas de convivencia"                                       ,
  "problemas de convivencia"                                       ,
  "categoría inventada"                                           ,
  "problemas entre docentes>>>problemas de convivencia"            ,
  "dispersión de zonas escolares"                                 ,
  "apatía o actitudes negativas"                                  ,
  "carga administrativa"                                           ,
  "carga administrativa"                                           ,
  "categoría inventada"                                           ,
  "no atender necesidades"                                         ,
  "problemas entre docentes"
)

dicc <- tibble(
  codigo = rep("j_problemaJefe", 54),
  pregunta = rep(
    "¿Cuál es la problemática más frecuente que enfrenta usted en sus funciones?",
    54
  )
)


g <- Graficar$new(
  bd = bd,
  diccionario = dicc,
  colores = colores,
  color_principal = "pink",
  tema = tema_morant()
)


################################### Graficar nube ###################################

g$procesar_nubes("j_problemaJefe")$graficar_nube(
  n = 2,
  gradiente = c(bajo = "#ca4992", alto = "#5B1AA4")
)
