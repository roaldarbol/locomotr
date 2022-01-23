#' Joint angular velocity
#'
#' @param data Data
#' @param df.phase Data containing timings of touchdown and take-off
#' @param joint Which segment
#' @export
#'

jump_joint_vel <- function(data,
                      df.phase,
                      joint=c('hip', 'knee')){

  name.r <- paste('vel.theta', joint, 'r', sep = '.')
  name.l <- paste('vel.theta', joint, 'l', sep = '.')

  # Average velocity of lead leg ----
  df <- tibble(.rows=1)
  for (i in 1:nrow(df.phase)){
    df.temp <- data %>%
      subset(t %inrange% c(df.phase$touchdown[i], df.phase$takeoff[i])) %>%
      summarise(mean.angvel.r = mean(get(name.r)),
                mean.angvel.l = mean(get(name.l)))
    df <- rbind(df, df.temp)
  }

  joint.vel <- vector()
  for (i in 1:nrow(df)){
    if(df.phase$foot[i] == 'r'){
      joint.vel[i] <- abs(df$mean.angvel.l[i])
    } else if(df.phase$foot[i] == 'l'){
      joint.vel[i] <- abs(df$mean.angvel.r[i])
    } else {
      joint.vel[i] <- NA
    }
  }
  joint.vel <- data.frame(joint.vel)
  names(joint.vel) <- paste(joint, 'angvel', sep = '.')
  joint.vel[nrow(joint.vel),] <- NA

  return(joint.vel)
}
