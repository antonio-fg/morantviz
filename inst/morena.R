library(readr)
library(readxl)
diseno_24 <- read_rds("../enc_chihuahua_nov2024/data/resultado_final_nov24.rda")
dicc <- read_xlsx("../enc_chihuahua_nov2024/Insumos/dicc_enc_chihuahua_nov2024.xlsx")

# diseno_24$variables |>
#   as_tibble() |>
#   select(contains("opinion"), contains("honesto"),
#          contains("cercano"), contains("conoce"),
#          contains("cumple"), contains("candidato"), contains("voto")) |>
#   select(contains(c("andrea", "cruz"))) |>
#   select(contains("estado"))

options(survey.lonely.psu="remove")
g <- Graficar$new(diseno = diseno_24, diccionario = dicc |> rename(codigo = llaves), colores = colores,color_principal = "pink",
             tema = tema_morant())

# opinión -----------------------------------------------------------------
op <- "opinion_per1"
personajes <- c("cruz", "andrea")

vars <- paste(op, personajes, sep = "_")
g$contar_variables(variables = vars,confint = F)

g$filtrar_respuesta(valor = "Buena")
g$pegar_diccionario()
op_morena <- g$tbl |>
  mutate(nombre = "Opinión")

# atributos ---------------------------------------------------------------


atributos <- tibble(atributo = c("caract_per_honesto", "caract_per_cercano", "caract_per_conocechi", "caract_per_cumple"),
                    puntos = c(1.25,.25,.25,.25),
                    nombre = c("Honestidad","Cercano\na la\ngente", "Conoce el\nEstado", "Cumple"))

vars <- expand.grid(atributos$atributo, personajes) |>
  mutate(var = paste(Var1, Var2, sep = "_")) |>
  pull(var)

g$contar_variables(variables = vars, confint = F)
g$filtrar_respuesta(valor = c("Mucho", "Algo"))

g$tbl <- g$tbl |>
  mutate(media = if_else(respuesta == "Algo", media *.5, media)) # agregar método dividir_respuesta



g$tbl <- g$tbl |>
  summarise(media = sum(media), .by = codigo)  #agregar método agrupar_variable y sumar

g$tbl
g$pegar_diccionario()
library(stringr)
frecuencia_atributos <- g$tbl |>
  mutate(atributo = str_match(codigo, atributos$atributo |>
                                paste(collapse = "|")) |>
           as.vector(), .before = 0) |>
  left_join(atributos, join_by(atributo)) |>
  mutate(puntos = if_else(media == max(media), puntos, 0), .by = atributo)

frecuencia_atributos



# buen candidato ----------------------------------------------------------
buen_candidato <- "tipo_candidato"
vars <- paste(buen_candidato, personajes, sep = "_")

g$contar_variables(vars, confint = F)
g$filtrar_respuesta(valor = "Sí")
g$pegar_diccionario()

buenC <- g$tbl |>
  mutate(puntos = if_else(media == max(media), 1, 0),
         nombre = "Buen candidato")


# votaría? ----------------------------------------------------------------

votaria <- "voto_fut"
vars <- paste(votaria, personajes, sep = "_")
g$contar_variables(vars, confint = F)

g$filtrar_respuesta(valor = "Sí votaría")
g$pegar_diccionario()

vot <- g$tbl |>
  mutate(puntos = if_else(media == max(media), 2, 0),
         nombre = "Votaría"
         )


# preferencia -------------------------------------------------------------


# todo --------------------------------------------------------------------

todo <- op_morena |>
  bind_rows(
    frecuencia_atributos
  ) |>
  bind_rows(
    buenC
  ) |>
  bind_rows(
    vot
  )

todo |>
  ggplot(aes(x = nombre, y = tema)) +
  geom_tile(aes(fill = media), show.legend = F) +
  ggfittext::geom_fit_text(aes(label = scales::percent(media,1)), contrast = T)

