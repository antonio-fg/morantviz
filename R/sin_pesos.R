contar_1 <- function(bd, variable, pct){
  aux <- bd |>
    count(respuesta := !!rlang::sym(variable)) |>
    mutate(codigo = variable)

  if(pct){
    aux <- aux |>
      mutate(pct = n/sum(n))
  }

  return(aux)
}

contar_vars <- function(bd, variables, pct){
  purrr::map_dfr(.x = variables,
                 .f = contar_1,
                 bd = bd,
                 pct = pct)
}


