#' times
#'
#' Get contact and flight times
#' @param df.phase Data containing timings of touchdown and take-off
#' @export
#'
jump_times <- function(df.phase){
  df.temp <- df.phase %>%
    transmute(support = df.phase$takeoff - df.phase$touchdown,
              flight = lead(df.phase$touchdown) - df.phase$takeoff)
  return(df.temp)
}
