# pos2vel() ----
#' Positions to Velocity
#'
#' The basic linear velocity handle. Takes two positions and time points as input.
#' @param x The two positions.
#' @param t The two time points.
#' @export
#'

pos2vel <- function(x1, x2, t1, t2){
  v = (x2 - x1) * (1/(abs(t2 - t1)))
  return(v)
}

# dist2vel() ----
#' Distance to Velocity
#'
#' The basic linear velocity handle. Takes two positions and time points as input.
#' @param d The distance covered
#' @param t The time taken.
#' @export
#'

dist2vel <- function(d, t){
  v = d * (1/t)
  return(v)
}

# pos2velXY() ----
#' Position to velocity, XY
#'
#' The basic linear velocity handle. Takes two positions and time points as input.
#' @param d The distance covered
#' @param t The time taken.
#' @export
#'

pos2velXY <- function(x1, x2, y1, y2, t1, t2){
    vel = sqrt((pos2vel(x1,x2, t1, t2)^2) + (pos2vel(y1,y2, t1, t2)^2))
    return(vel)
}

# pos2vel.all() ----
#' Velocity in data frame
#'
#' These functions computes velocities and acceleration for all points which have
#' an x and a y coordinate, and have a time measure.
#' @param df The data frame.
#' @param x The list of joints.
#' @param t The time parameter.
#' @export
#'

pos2vel.all <- function(df, x, t = "t"){
  vel <- tibble(.rows = nrow(df))
  for (i in 1:length(x)){
    for (j in 2:nrow(df)){
      vel[j, i] = pos2vel(df[j-1, x[i]], df[j, x[i]], df[j-1, t], df[j, t])
      colnames(vel)[i] <- paste0("vel.", x[i])
    }
  }
  return(vel)
}


# pos2velXY.all() ----
#' Velocity
#'
#' These functions computes velocities and acceleration for all points which have
#' an x and a y coordinate, and have a time measure.
#' @param df The data frame.
#' @param x The x coordinates.
#' @param y The y coordinates.
#' @param time The time parameter.
#' @export
#'
pos2velXY.all <- function(df, x, t = "t"){
  j.x <- c()
  j.y <- c()
  for (i in 1:length(x)){
    j.x[i] <- paste0("x.", x[i])
    j.y[i] <- paste0("y.", x[i])
  }
  vel <- tibble(.rows = nrow(df))
  for (i in 1:length(x)){
    for (j in 2:nrow(df)){
      vel[j, i] = pos2velXY(df[j-1, j.x[i]], df[j, j.x[i]],
                            df[j-1, j.y[i]], df[j, j.y[i]],
                            df[j-1, t], df[j, t])
      colnames(vel)[i] <- paste0("vel.", x[i])
    }
  }
  return(vel)
}

# vel2acc() ----
#' Velocity to acceleration
#'
#' The basic linear acceleration handle. Takes two velocities and time points as input.
#' @param v The velocities.
#' @param t The time parameter.
#' @export
#'

vel2acc <- function(v1, v2, t1, t2){
  a = (v2 - v1) * (1/(abs(t2 - t1)))
  return(a)
}

# vel2accXY() ----
#' Velocity to acceleration, XY
#'
#' The basic linear velocity handle. Takes two positions and time points as input.
#' @param t The time taken.
#' @export
#'

vel2accXY <- function(x1, x2, y1, y2, t1, t2){
  a = sqrt((vel2acc(x1,x2, t1, t2)^2) + (vel2acc(y1,y2, t1, t2)^2))
  return(a)
}

# vel2acc.all() ----
#' Acceleration in data frame
#'
#' These functions computes velocities and acceleration for all points which have
#' an x and a y coordinate, and have a time measure.
#' @param df The data frame.
#' @param x The list of joints.
#' @param t The time parameter.
#' @export
#'
vel2acc.all <- function(df, x, t = "t"){
  j.x <- c()
  for (i in 1:length(x)){
    j.x[i] <- paste0("vel.", x[i])
  }
  acc <- tibble(.rows = nrow(df))
  for (i in 1:length(x)){
    for (j in 2:nrow(df)){
      acc[j, i] = vel2acc(df[j-1, j.x[i]], df[j, j.x[i]], df[j-1, t], df[j, t])
      }
    colnames(acc)[i] <- paste0("acc.", x[i])
  }
  return(acc)
}
# vel2accXY.all() ----
#' Acceleration
#'
#' These functions computes velocities and acceleration for all points which have
#' an x and a y coordinate, and have a time measure.
#' @param df The data frame.
#' @param x The horisontal velocities.
#' @param y The vertical velocities.
#' @param time The time parameter.
#' @export
#'
vel2accXY.all <- function(df, x, t = "t"){
  j.x <- c()
  j.y <- c()
  for (i in 1:length(x)){
    j.x[i] <- paste0("x.", x[i])
    j.y[i] <- paste0("y.", x[i])
  }
  acc <- tibble(.rows = nrow(df))
  for (i in 1:length(x)){
    for (j in 2:nrow(df)){
      acc[j, i] = vel2accXY(df[j-1, j.x[i]], df[j, j.x[i]],
                            df[j-1, j.y[i]], df[j, j.y[i]],
                            df[j-1, t], df[j, t])
    }
    colnames(acc)[i] <- paste0("acc.", x[i])
  }
  return(acc)
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
