#' Rad2Deg
#'
#' Changes between radians and degrees
#' @export
#'

rad2deg <- function(ang){
  val <- (ang * 180) / pi
  return(val)
}

#' Deg2Rad
#'
#' Changes between radians and degrees
#' @export
#'
deg2rad <- function(ang){
  val = (ang * pi) / 180
  return(val)
  }

#' deg360
#' Moves data from (-180:180) to (0:360).
#'
#' Changes between radians and degrees
#' @export
#'
deg360 <- function(ang, type = "rad", flip = TRUE){
  if (type == "rad"){
    if (flip == TRUE){
      val <- ifelse(ang <= 0, rad2deg(as.numeric(ang))+360, rad2deg(as.numeric(ang)))
    } else {
      val <- ifelse(ang <= 0, ang+2*pi, ang)
    }
  } else {
    if (flip == TRUE){
      val <- ifelse(ang <= 0, (deg2rad(ang))+2*pi, deg2rad(ang))
    } else {
      val <- ifelse(ang <= 0, ang+360, ang)
    }
  }
    return(val)
}


#' deg360.all
#'
#' Moves data from (-180:180) to (0:360).
#'
#' Changes between radians and degrees
#' @param x A list of joints
#' @export
#'
deg360.all <- function(df, x){
  vec <- tibble(.rows = nrow(df))
  for (i in 1:length(x)){
    for (j in 1:nrow(df)){
      vec[j, i] <- deg360(df[[j, x[i]]])
    }
    colnames(vec)[i] <- paste0(x[i])
  }
  return(vec)
}


# ankle_ang.vel = abs(thetaAnkle - lag(thetaAnkle)) * (1/(t - lag(t))),
# ankle_ang.acc = abs(ankle_ang.vel - lag(ankle_ang.vel)) * (1/(t - lag(t))),
#
# knee_ang.vel = abs(thetaKnee - lag(thetaKnee)) * (1/(t - lag(t))),
# knee_ang.acc = abs(knee_ang.vel - lag(knee_ang.vel)) * (1/(t - lag(t))),
#
# hip_ang.vel = abs(thetaHip - lag(thetaHip)) * (1/(t - lag(t))),
# hip_ang.acc = abs(hip_ang.vel - lag(hip_ang.vel)) * (1/(t - lag(t)))
