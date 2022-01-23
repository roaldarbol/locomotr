#' Horizontal and vertical velocity (and take-off angle)
#'
#' Get contact and flight times
#' @param data Data containing timings of touchdown and take-off
#' @param results A results file of correct number of columns
#' @param timing Specifies 'td' or 'to'
#'
#' @importFrom dplyr %>%
#' @export
#'
#'
jump_velocity <- function(data, results, timing = c('td', 'to')){
  df.temp <- data %>%
    filter(get(timing) == TRUE) %>%
    transmute(vel.hor = .data$vel.x.com,
              vel.vert = .data$vel.y.com,
              to.angle = rad2deg(atan2(.data$vel.y.com, .data$vel.x.com)))
  r <- nrow(df.temp)
  p <- nrow(results)
  if (r==p-1){
    df.temp[r+1,] <- NA
  }
  return(df.temp)
}
