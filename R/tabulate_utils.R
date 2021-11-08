#' gather_longer
#'
#' @param data             [tibble] Some desc.
#' @param variables        [chr] Some desc.
#' @param .names_to        [chr] Some desc.
#' @param .values_to       [chr] Some desc.
#' @param .keep_variables  [chr] Some desc.
#' @param .values_drop_na  [chr] Some desc.
#'
#' @return [tibble]
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom dplyr rename_with matches across
#' @importFrom stringr str_remove_all
#'
gather_longer <- function(data, variables, .names_to, .values_to, .keep_variables = NULL, .values_drop_na = FALSE) {
  # This pivots a variable and keeps specified columns if any
  if(!is_empty(.keep_variables)) {
    reg_ex <- paste("^", .keep_variables, "$", collapse = "|", sep = "")
    data %<>% mutate(across(matches(reg_ex), .fns = list(tmpcolb4itturns = ~.)))
  }
  data %<>%
    pivot_longer(cols = all_of(variables), names_to = .names_to,
                 values_to = .values_to, values_drop_na = .values_drop_na)

  data %>% rename_with(.fn =~{str_remove_all(., "_tmpcolb4itturns$")},
                       .cols = matches("_tmpcolb4itturns$"))
}

#' check_and_ignore_wrong_type
#'
#' @param data        ["chr"] Some desc.
#' @param variables   ["chr"] Some desc.
#' @param warning_msg ["chr"] Some desc.
#' @param stop_msg    ["chr"] Some desc.
#' @return message
#'
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom purrr map_lgl
#' @importFrom rlang is_empty
#' @importFrom glue glue
#'
check_and_ignore_wrong_type <- function(data, variables, warning_msg, stop_msg) {
  # This ignores wrong data type arguments and stops if nothing left.
  variables_ignore <- data %>% select(all_of(variables)) %>% map_lgl(~{!is.numeric(.x)})
  if (any(variables_ignore)) {
    no_good <- names(variables_ignore)[variables_ignore]
    message(glue::glue(warning_msg))
    variables <- variables[!variables_ignore]
    if (is_empty(variables)) stop(stop_msg)
  }
  return(variables)
}
