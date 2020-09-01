#' 2D Triple Jump Analysis
#'
#' This function outputs the desired results and graphs for a 2D analysis of triple jump
#'
#' @param path Path to input folder
#' @param joints List of joints to be analyzed (begin with capital letter).
#' @export
triplejump_2d <- function(filter='spline', input=NULL){

  # setwd("/Users/roaldarbol/Documents/r/report")
  # setwd("/Users/roaldarbol/Documents/r/biomechanics")
  # input = NULL
  # filter = 'spline'
  res <- kinematics_2d(filter=filter, input = input)

  df.filter <- as.data.frame(res[[1]])
  df.melt <- as.data.frame(res[[2]])
  df.phase <- as.data.frame(res[[3]]$phase)
  setup <- res[[3]]
  matrixNA <- as.data.frame(res[[4]])
  results <- tibble(.rows = nrow(df.phase))
  results$phase <- df.phase$names

  # Important measures - MAKE SURE TO FIND THE RIGHT TAKEOFF INSTANT!!!:
  # Duration on ground ----
  timing <- times(df.phase)

  # Velocity and take-off angle at take-off ----
  velocities <- velocity(df.filter, results, 'to')

  # CoM Lowering
  disp.com <- disp_com(df.filter, results, 'td')

  # Foot velocity ----
  foot.vel.abs <- foot_vel(df.filter, df.phase, 'abs')
  foot.vel.rel <- foot_vel(df.filter, df.phase, 'rel')

  # Horizontal velocity change - WAIT UNTIL 2LS and LS IS INCORPORATED ----
  # df.temp <- df.filter %>%
  #   filter(td == TRUE | to == TRUE) %>%
  #   transmute(vel.change.hor = vel.x.com-lag(vel.x.com)) %>%
  #   slice(2,4,6,1)
  # results <- cbind(results, df.temp)

  # Distribution of phase distances - NEEDS TO SUBT. FOOT LENGTH ----
  df.temp <- df.filter %>%
    filter(td == TRUE)
  distance <- vector()
  for (i in 1:nrow(df.temp)){
    if(df.phase$foot[i] == 'both'){
      distance[i] <- NA
    } else if(df.phase$foot[i] == 'r' & df.phase$foot[i+1] == 'r'){
      distance[i] <- df.temp$pos.x.ankle.r[i+1]-df.temp$pos.x.ankle.r[i]
    } else if(df.phase$foot[i] == 'l' & df.phase$foot[i+1] == 'l'){
      distance[i] <- df.temp$pos.x.ankle.l[i+1]-df.temp$pos.x.ankle.l[i]
    } else if(df.phase$foot[i] == 'r' & df.phase$foot[i+1] == 'l'){
      distance[i] <- df.temp$pos.x.ankle.l[i+1]-df.temp$pos.x.ankle.r[i]
    } else if(df.phase$foot[i] == 'l' & df.phase$foot[i+1] == 'r'){
      distance[i] <- df.temp$pos.x.ankle.r[i+1]-df.temp$pos.x.ankle.l[i]
    } else if(df.phase$foot[i+1] == 'both'){
      distance[i] <- res[[3]]$profile$distance - sum(distance[i-1], distance[i-2])
    }
    # } else if(df.phase$foot[i] == 'r' & df.phase$foot[i+1] == 'both'){
    #   distance[i] <- df.temp$pos.x.ankle.r[i+1]-df.temp$pos.x.ankle.r[i]-0.20 #20cm app. foot length - needs precision
    # } else if(df.phase$foot[i] == 'l' & df.phase$foot[i+1] == 'both'){
    #   distance[i] <- df.temp$pos.x.ankle.r[i+1]-df.temp$pos.x.ankle.l[i]-0.20 #20cm app. foot length - needs precision
    # }
  }
  distance <- as.data.frame(distance)
  distance[nrow(distance),] <- NA
  distance2 <- distance
  distance2[1:2,] <- NA
  dist.sum <- sum(distance2, na.rm=TRUE)
  rel.dist <- distance2 %>%
    transmute(rel.distance = distance/dist.sum)
  results <- cbind(results, distance, rel.dist)

  # Horizontal impulse - NEEDS WORK! ----
  df.temp <- df.filter %>%
    filter(phase == 'support')
  i <- 1
  for(j in 1:nrow(df.temp)){
    if(j-1 <= 0){
      df.temp$jump[j] <- i
    } else if (df.temp$t[j] <= (df.temp$t[j-1]+0.05)){
      df.temp$jump[j] <- i
    } else {
      i <- i+1
      df.temp$jump[j] <- i
    }
  }

  # Knee angles at TD, min and TO ----
  td.knee <- joint_angle(df.filter, df.phase, 'td', 'knee')
  min.knee <- joint_angle(df.filter, df.phase, 'min', 'knee')
  to.knee <- joint_angle(df.filter, df.phase, 'to', 'knee')

  # Relative angles - trunk, inclination, thigh at TD and TO
  td.trunk <- rel_angle(df.filter, df.phase, 'trunk', 'td')
  to.trunk <- rel_angle(df.filter, df.phase, 'trunk', 'to')
  td.inc <- rel_angle(df.filter, df.phase, 'inc', 'td')
  to.inc <- rel_angle(df.filter, df.phase, 'inc', 'to')
  td.thigh <- rel_angle(df.filter, df.phase, 'thigh', 'td')
  to.thigh <- rel_angle(df.filter, df.phase, 'thigh', 'to')

  results <- cbind(results,
                   timing,
                   velocities,
                   disp.com,
                   foot.vel.abs,
                   foot.vel.rel,
                   td.knee,
                   min.knee,
                   to.knee,
                   td.trunk,
                   to.trunk,
                   td.inc,
                   to.inc,
                   td.thigh,
                   to.thigh)

  # Average velocity of lead leg ----
  df.temp <- tibble(.rows=1)
  for (i in 1:nrow(df.phase)){
    df.temp2 <- df.filter %>%
      subset(t %inrange% c(df.phase$touchdown[i], df.phase$takeoff[i])) %>%
      summarise(mean.angvel.hip.r = mean(vel.theta.hip.r),
                mean.angvel.hip.l = mean(vel.theta.hip.l))
    df.temp <- rbind(df.temp, df.temp2)
  }

  hip.angvel <- vector()
  for (i in 1:nrow(df.temp)){
    if(df.phase$foot[i] == 'r'){
      hip.angvel[i] <- abs(df.temp$mean.angvel.hip.l[i])
    } else if(df.phase$foot[i] == 'l'){
      hip.angvel[i] <- abs(df.temp$mean.angvel.hip.r[i])
    } else {
      hip.angvel[i] <- NA
    }
  }
  hip.angvel <- as.data.frame(hip.angvel)
  hip.angvel[nrow(hip.angvel),] <- NA
  results <- cbind(results, hip.angvel)


  # Rounding off results ----
  results <- results  %>%
    mutate_if(is.numeric, round, 2)

  # Landing analysis
  landing.analysis <- landing_analysis(df.filter, df.phase)

  # Return ----
  report <- list(results, df.filter, df.melt, df.phase, setup, landing.analysis, matrixNA)
  return(report)
  setwd(act.path)
}
