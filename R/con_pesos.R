contar_1_pesos <- function(variable, diseno, confint = T){
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

contar_vars_pesos <- function(variables, diccionario, diseno, confint){

  purrr::map_dfr(.x = variables,
                 .f = contar_1_pesos,
                 diseno = diseno,
                 confint = confint)
}
