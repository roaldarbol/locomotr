# pos2vel() ----
#' Positions to Velocity
#'
#' The basic linear velocity handle. Takes two positions and time points as input.
#' @param x1 The two positions.
#' @param x2 Bla
#' @param t1 The two time points.
#' @param t2 Bla
#'
#' @export
#'

pos2vel <- function(x1, x2, t1, t2){
  v = (x2 - x1) * (1/(abs(t2 - t1)))
  return(v)
}

# specify_decimal() ----
#' Specifying number of decimals
#'
#' Takes a number and returns it with k decimals.
#' @param x The input number.
#' @param k The number of decimals.
#' @export
#'
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
