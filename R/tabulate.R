#' @title  tabulate
#' @description
#' tji asd asd sda sad sa d tji asd asd sda sad sa  tji asd asd sda sad sa  tji asd asd sda sad sa
#' tji asd asd sda sad sa  tji asd asd sda sad sa tji asd asd sda sad sa  tji asd asd sda sad sa
#' #'
#' @param data A dataframe
#' @param groups    [str] string vector of variables or tidy supported selection ex. matches(...)
#' @param cols      [str] string vector of variables or tidy supported selection ex. matches(...)
#' @param weights   [str] string vector of variables or tidy supported selection ex. matches(...)
#' @param samples   [str] string vector of variables or tidy supported selection ex. matches(...)
#' @param return_mean [boolean] for nominal variables
#' @param values_drop_na [boolean] if the base and pct should include NAs
#' @param variable_sep [str] regex used to separate the variable column
#' @param variable_sep_suffix [str] name of the variable_sep right-hand side output column
#' @param keep_empty_levels [str] no levels imputations
#' @return frequency table, dataframe/tibble,   A standardized ET
#'
#' @import dplyr
#' @import tidyr
#' @importFrom Hmisc wtd.mean wtd.var
#' @importFrom haven is.labelled
#' @importFrom rlang is_empty
#' @importFrom magrittr `%<>%`
#' @importFrom glue glue
#' @import purrr
#' @import stringr
#'
#' @export
#'
#' @examples
#'
#' tabulate(iris, Species)
#'


tabulate <- function(data, cols, weights = NULL, groups = NULL, samples = NULL, 
                              return_mean = FALSE, values_drop_na = TRUE, 
                              variable_sep = NULL, variable_sep_suffix = NULL, keep_empty_levels = FALSE) {
  
  require(magrittr);require(tidyverse)
  
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
  
  msg_param_conflict   <- "Note: If data contains missing values (NA's) mean calculation will return missing values (NA's)"
  msg_fullbase         <- "Note: values_drop_na=FALSE increases calculation time by a factor of 3 (approximately)"
  msg_separate         <- "Note: Separating variable components, this may take some time..."
  msg_weight_overlap   <- "WARNING: weights overlap with other arguments"
  msg_ignore_weight    <- "Ignoring weights of type character: {paste(no_good, collapse = ',')}"
  msg_ignore_question  <- "Ignoring cols of type character [return_mean=TRUE]: {paste(no_good, collapse = ',')}"
  msg_no_question_left <- "No numeric questions left"
  msg_no_weight_left   <- "No numeric weights left"
  msg_inputet_rows_excisitn_levels   <- "Rrows added due to missing values on excisting levels: {nrow(extra_empty_cols)}"
  
  cols <- colnames(select(data,{{cols}}))
  if (!is_empty(expr(weights))) weights <- colnames(select(data,{{weights}} ) ); if(is_empty(weights)) weights <- NULL 
  if (!is_empty(expr(groups)))  groups  <- colnames(select(data,{{groups}}  ) ); if(is_empty(groups))  groups  <- NULL 
  if (!is_empty(expr(samples))) samples <- colnames(select(data,{{samples}} ) ); if(is_empty(samples)) samples <- NULL 
  
  # Check for varibles present
  if (!values_drop_na) message(msg_fullbase)
  in_cols <- c(cols, weights, groups, samples)
  c_missing <- in_cols[!(in_cols %in% colnames(data))] %>% paste(collapse = ",")
  if (!is_empty(c_missing) && c_missing != "") stop(glue::glue("Missing column(s): {c_missing}"))
  
  data %<>% select(any_of(c(cols, weights, groups, samples)))

  # Making a label and level dictionary to fill empty levels with at the end
  if(keep_empty_levels){
    levels_df <- data %>%
      select(all_of(cols)) %>% select(where(~{haven::is.labelled(.)|is.factor(.)})) %>% 
      distinct() %>% 
      mutate(across(everything(), ~{
        attr_var <- ifelse(!is.null(attr(.,"labels")),"labels","levels")
        factor(., levels = attr(., attr_var)) })) %>% 
      imap_dfr(~{tibble(variable = .y, levels = levels(.x)) })
  }
  
  # Check cols type to calculate mean
  if (return_mean) {
    cols %<>% check_and_ignore_wrong_type(data = data, variables = ., 
                                          warning_msg = msg_ignore_question,
                                          stop_msg = msg_no_question_left) 
  }
  # Check weights type to calculate, and ignore character only.
  if (!is.null(weights) ){
    weights %<>% check_and_ignore_wrong_type(data = data, variables = .,
                                             warning_msg = msg_ignore_weight,
                                             stop_msg = msg_no_weight_left) 
  }
  
  # Make everything class character to be able to turn/pivot everything
  data %<>%
    mutate(across(all_of(unique(c(cols, groups, samples, weights))),
                  .fns = ~as.character(.)))

  # This is to duplicate only the columns that overlap, not all
  overlapping_questions_sample <- intersect(cols, samples)
  overlapping_questions_group  <- intersect(cols, groups)
  overlapping_samples_group    <- intersect(groups, samples)
  overlapping_weight           <- intersect(unique(c(cols, samples, groups)), weights)
  
  if (!is_empty(overlapping_weight)) message(msg_weight_overlap)
  
  
  # Number #1 to #3 turns data. Number #4 renames the grouping vars with a group_ prefix.
  #1
  if (!is.null(weights)) {
    data %<>% gather_longer(variables = weights, .names_to = "weight", .values_to = "weight_value",
                            .keep_variables = unique(overlapping_weight)) 
  } else {
    data %<>% mutate(weight = "unweighted", weight_value = 1)
  }
  #2
  data %<>% gather_longer(variables = cols, .names_to = "variable_output", .values_to = "value_output",
                          .keep_variables = unique(c(overlapping_questions_sample, overlapping_questions_group)),
                          .values_drop_na = values_drop_na) 
  #3
  if (!is_empty(samples)) {
    data %<>% gather_longer(variables = samples, .names_to = "sample_class", .values_to = "sample",
                            .keep_variables = unique(overlapping_samples_group))
  }
  #4
  if (!is.null(groups)) {
    data %<>% rename_with(.fn = ~{paste0("group_",.)}, .cols = matches(paste0("^",groups,"$"))) 
    groups <- paste("group", groups, sep = "_")
  }
  # This renaming in the beginning allow for value/variable as input names in groups/samples args
  data %<>% rename(value = value_output, variable = variable_output)

  # Calculate percentages
  if (!return_mean) {
    res <- data %>%
      mutate(weight_value = as.double(weight_value)) %>%
      group_by(across(matches("^(sample|weight$|variable$)")), across(all_of(groups)), value, .drop = TRUE) %>%
      summarise(n = sum(weight_value, na.rm = TRUE), .groups = "drop_last") %>%
      mutate(base = sum(n), pct = n / base)
  } else if (return_mean) { 
    # Calculate means
    if (!values_drop_na) message(msg_param_conflict)
    res <- data %>%
      mutate(value = as.double(value), 
             weight_value = as.double(weight_value)) %>%
      group_by(across(matches("^(sample|weight$|variable$)")), across(all_of(groups)), .drop = TRUE) %>%
      mutate(weight_value_notna = ifelse(is.na(value), NA, weight_value)) %>%
      summarise(base = sum(weight_value_notna, na.rm = values_drop_na),
                mean = Hmisc::wtd.mean(weights = weight_value, x = value, na.rm = values_drop_na),
                stdev = sqrt(Hmisc::wtd.var(weights = weight_value, x = value, na.rm = values_drop_na)),
                .groups = "drop_last") %>%
      mutate(n = base, value = "numeric")
  }
  
  # Output column order
  column_order_char <- paste0("^", c("sample", "group", "weight", "variable", "value"))
  column_order_num <- paste0("^", c("n", "base", "pct", "mean", "stdev"), "$")
  
  res %<>% ungroup() %>%
    relocate(matches(c(column_order_char, column_order_num)), .after = last_col())
  
  # The empty levels are filled with 0 here. 
  if (keep_empty_levels && nrow(levels_df)>0){

    # make key to check levels within each group
    res %<>% unite("fake_res_key", matches(rev(column_order_char)[-1]), remove = FALSE)
    
    extra_empty_cols <- 
      unique(res$fake_res_key) %>%
      map_dfr(~{
        current_key_var <- filter(res, fake_res_key == .x) %>% pull(variable) %>% unique()
        levels_for_current_var  <- filter(levels_df, variable == current_key_var) %>% pull(levels)
        current_var_resulsts_df <- filter(res, fake_res_key == .x)
        
        fake_res <- current_var_resulsts_df %>% 
          mutate(value = "tmp") %>% 
          group_by(across(.cols = -matches(column_order_num) ) ) %>% 
          slice(1) %>% ungroup() %>% 
          mutate(n = 0 , pct = 0)
        
        value_for_current_var <- current_var_resulsts_df %>% pull(value) %>% unique()
        level_not_in_results <- setdiff(levels_for_current_var, value_for_current_var)

        map_dfr(level_not_in_results, ~{mutate(fake_res, value = .x) }) 
      })
    message(glue::glue(msg_inputet_rows_excisitn_levels))
    res <- bind_rows(res, extra_empty_cols) %>% select(-fake_res_key)
    res %<>% arrange(across(matches(column_order_char)))
  }
  
  # Separation of variable into variable and (brand)code
  if (is.null(variable_sep_suffix)) variable_sep_suffix <- "brandcode"
  if (!is.null(variable_sep)) {
    message(msg_separate)
    new_col <- glue::glue("group_{variable_sep_suffix}")
    res %<>% tidyr::separate(variable, into = c("variable", new_col), sep = variable_sep,
                             convert = TRUE, fill = "right", extra = "merge") %>%
      relocate(all_of(new_col), .before = variable)
  }
    return(res)
}
