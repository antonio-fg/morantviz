contar_1 <- function(bd, variable){
  aux <- bd |>
    count(respuesta := !!rlang::sym(variable)) |>
    mutate(codigo = variable)

  return(aux)
}

contar_vars <- function(bd, variables, pct){
  purrr::map_dfr(.x = variables,
                 .f = contar_1,
                 bd = bd)
}


