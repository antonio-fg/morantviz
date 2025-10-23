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


contar_multirespuesta <- function(bd, variable, sep = "\\s\\/\\s"){
  bd |>
    select(!!rlang::sym(variable)) |>
    separate_rows(!!rlang::sym(variable), sep = sep) |>
    mutate(!!rlang::sym(variable) := str_squish(!!rlang::sym(variable))) |>
    count(!!rlang::sym(variable), sort = T) |>
    mutate(pct = n/nrow(bd)) |>
    mutate(codigo = variable) |>
    rename(respuesta := !!rlang::sym(variable))
}

contar_vars_porGrupos <- function(bd, variables, grupos){
  aux <- bd |>
    group_by(across(all_of(grupos)))

  variables |>
    purrr::map_df(~{
      aux |>
        count(!!rlang::sym(.x)) |>
        rename(respuesta = !!rlang::sym(.x)) |>
        mutate(codigo = .x,
               pct = n/sum(n))
    }) |>
    ungroup()

}
