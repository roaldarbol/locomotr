#' joint angle
#'
#' Gets the angle
#' @param data Filtered data
#' @param df.phase Data containing timings of touchdown and take-off
#' @param timing Touchdown, take-off or minimal/maximal angle
#' @param joint Which joint is being measured, either 'knee' or 'hip'
#' @export
#' @importFrom data.table %inrange%
#' @importFrom stringr str_sub
#'

jump_joint_angle <- function(data,
                        df.phase,
                        timing=c('td', 'to', 'min'),
                        joint=c('knee', 'hip')){

  # For angle at touchdown and take-off
  if (timing %in% c('td', 'to')){
    df.temp <- data %>%
      filter(get(timing) == TRUE) %>%
      transmute(joint.angle.r = get(paste0('pos.theta.', joint, '.r')),
                joint.angle.l = get(paste0('pos.theta.', joint, '.l')))
  } else if (timing == 'min'){

    # For minimal angle during contact
    df.temp <- tibble(.rows=1)
    for (i in 1:(nrow(df.phase))){
      df.temp2 <- data %>%
        subset(t %inrange% c(df.phase$touchdown[[i]], df.phase$takeoff[[i]])) %>%
        summarise(joint.angle.r = min(get(paste0('pos.theta.', joint, '.r'))),
                  joint.angle.l = min(get(paste0('pos.theta.', joint, '.l'))))
      df.temp <- rbind(df.temp, df.temp2)
    }
  }

  # Get the relevant angles
  joint.angle <- vector()
  for (i in 1:nrow(df.temp)){
    if(df.phase$foot[i] == 'r'){
      joint.angle[i] <- df.temp$joint.angle.r[i]
    } else if(df.phase$foot[i] == 'l'){
      joint.angle[i] <- df.temp$joint.angle.l[i]
    } else {
      joint.angle[i] <- NA
    }
  }

  joint.angle <- as.data.frame(joint.angle)
  colnames(joint.angle) <- paste(timing, joint, 'angle', sep = '.')

  r <- nrow(joint.angle)
  p <- nrow(df.phase)
  if (r==p-1){
    joint.angle[r+1,] <- NA
  }
  return(joint.angle)
}
