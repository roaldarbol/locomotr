#' CoM lowering
#'
#' Displacement
#' @param data Data containing timings of touchdown and take-off
#' @param results A results file of correct number of columns
#' @param timing Specifies 'td' or 'to'
#' @export
#'

jump_disp_com <- function(data, results, timing = c('td', 'to')){
  df.temp <- data %>%
    filter(get(timing) == TRUE) %>%
    transmute(com.lower = lead(.data$pos.y.com) - .data$pos.y.com)

  r <- nrow(df.temp)
  p <- nrow(results)
  df.temp[(r-1):r,] <- NA

  return(df.temp)
}
