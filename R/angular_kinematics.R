#' Rad2Deg
#'
#' Changes between radians and degrees
#' @param ang Angle in radians
#' @export
#'

rad2deg <- function(ang){
  val <- (ang * 180) / pi
  return(val)
}

#' Deg2Rad
#'
#' Changes between radians and degrees
#' @param ang Angle in degrees
#' @export
#'
deg2rad <- function(ang){
  val = (ang * pi) / 180
  return(val)
  }

#' Joint angles
#'
#' Computes all joint angles for joints specified in setup$skeleton
#'
#' @param data Data
#' @param setup setup list containing a skeleton sublist
#' @export
#'

joint_angles <- function(data, setup){
  df.angles <- data.frame(data$t)
  colnames(df.angles) <- 't'
  for (i in 1:length(setup$skeleton$joints)){
    joint <- paste('theta', names(setup$skeleton$joints[i]), sep='.')
    x.vertex <- paste('x', setup$skeleton$joints[[i]][1], sep='.')
    x.arm <- paste('x', setup$skeleton$joints[[i]][2], sep='.')
    x.base <- paste('x', setup$skeleton$joints[[i]][3], sep='.')
    y.vertex <- paste('y', setup$skeleton$joints[[i]][1], sep='.')
    y.arm <- paste('y', setup$skeleton$joints[[i]][2], sep='.')
    y.base <- paste('y', setup$skeleton$joints[[i]][3], sep='.')

    min.vertex <- min(which(!is.na(data[,x.vertex])))
    min.arm <- min(which(!is.na(data[,x.arm])))
    min.base <- min(which(!is.na(data[,x.base])))
    min <- max(min.vertex, min.arm, min.base)

    max.vertex <- max(which(!is.na(data[,x.vertex])))
    max.arm <- max(which(!is.na(data[,x.arm])))
    max.base <- max(which(!is.na(data[,x.base])))
    max <- min(max.vertex, max.arm, max.base)

    for (j in min:max){
      angle.rad  <- atan2(data[[j,y.arm]] - data[[j,y.vertex]], data[[j,x.arm]] - data[[j,x.vertex]]) -
        atan2(data[[j,y.base]] - data[[j,y.vertex]], data[[j,x.base]] - data[[j,x.vertex]])
      angle.deg <- rad2deg(angle.rad)
      if (angle.deg<1){
        df.angles[j,joint]  <- angle.deg+360
      } else {
        df.angles[j,joint]  <- angle.deg
      }
    }
  }

  return(df.angles)
}
