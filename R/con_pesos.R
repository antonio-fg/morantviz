contar_1_pesos <- function(variable, diseno, confint){
  surveySummary_mean <- survey::svymean(survey::make.formula(variable),
                                        design = diseno,
                                        na.rm = TRUE)


  aux <- surveySummary_mean |>
    tibble::as_tibble(rownames = "respuesta") |>
    rename(media = mean, ee = SE)

  if(confint){
    aux <- aux |>
      left_join(
        surveySummary_mean |>
          stats::confint() %>%
          tibble::as_tibble(rownames = "respuesta") |>
          rename(inf = 2, sup = 3),
        join_by(respuesta))
  }

  aux |>
    mutate(respuesta = stringr::str_replace(pattern = rlang::expr_text(ensym(variable)),
                                            replacement = "",
                                            string = respuesta),
           codigo = rlang::expr_text(ensym(variable)))

}

contar_vars_pesos <- function(variables, diseno, confint){

  purrr::map_dfr(.x = variables,
                 .f = contar_1_pesos,
                 diseno = diseno,
                 confint = confint)
}

contar_vars_porGrupos_pesos <- function(variables, grupos, diseno, confint){
  surveySummary_mean <- svyby(formula = make.formula(variables),
                              by = make.formula(grupos),
                              design = diseno, FUN = svymean, na.rm = T)


  aux <- surveySummary_mean |>
    as_tibble() |>
    select(-starts_with("se.")) |>
    pivot_longer(-all_of(grupos), values_to = "media")

  if(confint){
    aux <- aux |> left_join(
      surveySummary_mean |>
        confint() |>
        as_tibble(rownames = "id") |>
        separate(id, into = c("grupos", "name"), sep = ":") |>
        separate(grupos, into = grupos, sep = "\\.")
    )
  }

  aux |>
    mutate(codigo = str_match(name, variables |> paste(collapse = "|")) |> as.vector(),
           respuesta = str_replace(name, codigo, ""), .before = media) |>
    select(-name)

}



