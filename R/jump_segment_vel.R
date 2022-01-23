#' Segment landing velocity
#'
#' @param data Data
#' @param df.phase Data containing timings of touchdown and take-off
#' @param segment Which segment
#' @param comparison Absolute velocity or relative to CoM
#' @param dir Either X or Y direction
#' @export
#'

jump_segment_vel <- function(data,
                        df.phase,
                        segment=c('ankle'),
                        comparison = c('rel', 'abs'),
                        dir = c('x', 'y')){

  name.r <- paste('vel', dir, segment, 'r', sep = '.')
  name.l <- paste('vel', dir, segment, 'l', sep = '.')

  if (comparison == 'abs'){
    df.temp <- data %>%
      filter(lead(.data$td) == TRUE) %>%
      transmute(vel.segment.r = get(name.r),
                vel.segment.l = get(name.l))

  } else if (comparison == 'rel'){
    df.temp <- data %>%
      filter(lead(.data$td) == TRUE) %>%
      transmute(vel.segment.r = get(name.r)-.data$vel.x.com,
                vel.segment.l = get(name.l)-.data$vel.x.com)
  }

  segment.vel <- vector()
  for (i in 1:nrow(df.temp)){
    if (is.na(df.temp$vel.segment.r[i]) || !df.phase$foot[i] %in% c('r','l')){
      segment.vel[i] <- NA
    } else if (df.phase$foot[i] == 'r'){
      segment.vel[i] <- df.temp$vel.segment.r[i]
    } else if(df.phase$foot[i] == 'l'){
      segment.vel[i] <- df.temp$vel.segment.l[i]
    }
  }

  segment.vel <- data.frame(segment.vel)
  names(segment.vel) <- paste(segment, 'vel', comparison, sep = '.')
  segment.vel[nrow(segment.vel),] <- NA

  return(segment.vel)
}
