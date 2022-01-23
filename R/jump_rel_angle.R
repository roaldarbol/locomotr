#' Relative angles
#'
#' @param data Data containing timings of touchdown and take-off
#' @param df.phase Phase data
#' @param comparison Trunk, inclination, thigh
#' @param timing Touchdown or take-off
#' @export
#'

jump_rel_angle <- function(data,
                      df.phase,
                      comparison=c('trunk', 'inc', 'thigh'),
                      timing = c('td', 'to')){

  p <- nrow(df.phase)

  if (comparison == 'trunk'){
    angle <- data %>%
      filter(get(timing) == TRUE) %>%
      transmute(angle = -1*(rad2deg(atan2(.data$pos.y.head-.data$pos.y.hip, .data$pos.x.head-.data$pos.x.hip))-90))

  } else if(comparison == 'inc'){
    df.temp <- data %>%
      filter(get(timing) == TRUE) %>%
      transmute(angle.r = -1*(rad2deg(atan2(.data$pos.y.com-.data$pos.y.ankle.r, .data$pos.x.com-.data$pos.x.ankle.r))-90),
                angle.l = -1*(rad2deg(atan2(.data$pos.y.com-.data$pos.y.ankle.l, .data$pos.x.com-.data$pos.x.ankle.l))-90))

  } else if(comparison == 'thigh'){
    df.temp <- data %>%
      filter(get(timing) == TRUE) %>%
      transmute(angle.r = -1*(180-rad2deg(atan2(.data$pos.y.hip-.data$pos.y.knee.r, .data$pos.x.hip-.data$pos.x.knee.r))),
                angle.l = -1*(180-rad2deg(atan2(.data$pos.y.hip-.data$pos.y.knee.l, .data$pos.x.hip-.data$pos.x.knee.l))))
  }

  if (comparison == 'inc'){
    angle <- vector()
    for (i in 1:nrow(df.temp)){
      if(df.phase$foot[i] == 'r'){
        angle[i] <- df.temp$angle.r[i]
      } else if(df.phase$foot[i] == 'l'){
        angle[i] <- df.temp$angle.l[i]
      } else {
        angle[i] <- NA
      }
    }

    # Opposite for thigh angle as it is the free leg
  } else if (comparison == 'thigh'){
    angle <- vector()
    for (i in 1:nrow(df.temp)){
      if(df.phase$foot[i] == 'l'){
        angle[i] <- df.temp$angle.r[i]
      } else if(df.phase$foot[i] == 'r'){
        angle[i] <- df.temp$angle.l[i]
      } else {
        angle[i] <- NA
      }
    }
  }

  angle <- data.frame(angle)
  names(angle) <- paste(timing, comparison, 'angle', sep = '.')
  r <- nrow(angle)

  if (r==p){
    angle[r,] <- NA
  } else if (r==p-1){
    angle[(r+1),] <- NA
  } else if (r==p+1){
    angle <- angle[-r,]
    angle[r,] <- NA
  }

  return(angle)
}
