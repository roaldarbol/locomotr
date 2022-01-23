#' Step length
#'
#' Calculate step lengths
#' @param data Data containing timings of touchdown and take-off
#' @param df.phase Df.phase
#' @param dist Total distance of the jump, from setup file
#' @param comparison Absolute or relative step lengths
#' @export
#'

jump_step_length <- function(data, df.phase, dist, comparison=c('abs', 'rel')){

  # Determine whether LJ or TJ by seeing if same foot strikes twice in a row
  identical <- vector()
  for (i in 2:length(df.phase$foot)){
    if (df.phase$foot[i] == df.phase$foot[i-1]){
      identical[i] <- TRUE
    } else {
      identical[i] <- FALSE
    }
  }

  tj <- any(identical == TRUE, na.rm = TRUE)

  # Actual stuff...
  df.temp <- data %>%
    filter(.data$td == TRUE)
  step.length <- vector()
  for (i in 1:nrow(df.temp)){
    if(df.phase$foot[i] == 'both'){
      step.length[i] <- NA
    } else if(df.phase$foot[i] == 'r' & df.phase$foot[i+1] == 'l'){
      step.length[i] <- df.temp$pos.x.ankle.l[i+1]-df.temp$pos.x.ankle.r[i]
    } else if(df.phase$foot[i] == 'l' & df.phase$foot[i+1] == 'r'){
      step.length[i] <- df.temp$pos.x.ankle.r[i+1]-df.temp$pos.x.ankle.l[i]
    } else if(df.phase$foot[i] == 'r' & df.phase$foot[i+1] == 'r'){
      step.length[i] <- df.temp$pos.x.ankle.r[i+1]-df.temp$pos.x.ankle.r[i]
    } else if(df.phase$foot[i] == 'l' & df.phase$foot[i+1] == 'l'){
      step.length[i] <- df.temp$pos.x.ankle.l[i+1]-df.temp$pos.x.ankle.l[i]
    } else if(df.phase$foot[i+1] == 'both' & tj==TRUE){
      step.length[i] <- dist - sum(step.length[i-1], step.length[i-2])
    } else if(df.phase$foot[i+1] == 'both' & tj!=TRUE){
      step.length[i] <- dist
    }
  }

  step.length <- data.frame(step.length)
  step.length[nrow(step.length),] <- NA

  # For triple jump phase ratio
  if (comparison=='rel'){
    step.length2 <- step.length
    step.length2[1:(nrow(step.length2)-4),] <- NA
    step.length.sum <- sum(step.length2, na.rm=TRUE)
    step.length <- step.length2 %>%
      transmute(phase.ratio = step.length/step.length.sum)
  }

  return(step.length)
}
