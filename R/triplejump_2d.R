#' 2D Triple Jump Analysis
#'
#' This function outputs the desired results and graphs for a 2D analysis of triple jump
#'
#' @param path Path to input folder
#' @param joints List of joints to be analyzed (begin with capital letter).
#' @export
#' @import dplyr
triplejump_2d <- function(path = NULL, filter='butter'){

  if (!is.null(path)){
    act.path <- getwd()
    setwd(path)
  }

  res <- kinematics_2d(filter=filter)

  df.filter <- res[[1]]
  df.melt <- res[[2]]
  df.phase <- as.data.frame(res[[3]]$phase)
  setup <- res[[3]]
  matrixNA <- res[[4]]
  kpi.results <- tibble(.rows = nrow(df.phase))

  # Important measures - MAKE SURE TO FIND THE RIGHT TAKEOFF INSTANT!!!:
  # Duration on ground ----
  kpi.results$phase <- df.phase$names
  df.temp <- df.phase %>%
    transmute(support = df.phase$takeoff - df.phase$touchdown,
           flight = lead(df.phase$touchdown) - df.phase$takeoff)
  kpi.results <- cbind(kpi.results, df.temp)

  # Velocity and take-off angle at take-off ----
  df.temp <- df.filter %>%
    filter(to == TRUE) %>%
    transmute(vel.hor = vel.x.com,
              vel.vert = vel.y.com,
              to.angle = rad2deg(atan2(vel.y.com, vel.x.com)))
  df.temp[4,] <- NA
  kpi.results <- cbind(kpi.results, df.temp)

  # Foot velocity ----
  df.temp <- df.filter %>%
    filter(lead(td) == TRUE) %>%
    transmute(vel.x.ankle.r = vel.x.ankle.r,
              vel.x.ankle.l = vel.x.ankle.l)
  foot.hor.vel <- vector()
  for (i in 1:nrow(df.temp)){
    if(df.phase$foot[i] == 'r'){
      foot.hor.vel[i] <- df.temp$vel.x.ankle.r[i]
    } else if(df.phase$foot[i] == 'l'){
      foot.hor.vel[i] <- df.temp$vel.x.ankle.l[i]
    } else {
      foot.hor.vel[i] <- NA
    }
  }
  foot.hor.vel <- as.data.frame(foot.hor.vel)
  foot.hor.vel[4,] <- NA

  df.temp <- df.filter %>%
    filter(lead(td) == TRUE) %>%
    transmute(vel.x.ankle.r = vel.x.ankle.r-vel.x.com,
              vel.x.ankle.l = vel.x.ankle.l-vel.x.com)
  foot.hor.vel.rel <- vector()
  for (i in 1:nrow(df.temp)){
    if(df.phase$foot[i] == 'r'){
      foot.hor.vel.rel[i] <- df.temp$vel.x.ankle.r[i]
    } else if(df.phase$foot[i] == 'l'){
      foot.hor.vel.rel[i] <- df.temp$vel.x.ankle.l[i]
    } else {
      foot.hor.vel.rel[i] <- NA
    }
  }
  foot.hor.vel.rel <- as.data.frame(foot.hor.vel.rel)
  foot.hor.vel.rel[4,] <- NA
  kpi.results <- cbind(kpi.results, foot.hor.vel, foot.hor.vel.rel)

  # Horizontal velocity change - WAIT UNTIL 2LS and LS IS INCORPORATED ----
  # df.temp <- df.filter %>%
  #   filter(td == TRUE | to == TRUE) %>%
  #   transmute(vel.change.hor = vel.x.com-lag(vel.x.com)) %>%
  #   slice(2,4,6,1)
  # kpi.results <- cbind(kpi.results, df.temp)

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
    } else if(df.phase$foot[i] == 'r' & df.phase$foot[i+1] == 'both'){
      distance[i] <- df.temp$pos.x.ankle.r[i+1]-df.temp$pos.x.ankle.r[i]
    } else if(df.phase$foot[i] == 'l' & df.phase$foot[i+1] == 'both'){
      distance[i] <- df.temp$pos.x.ankle.r[i+1]-df.temp$pos.x.ankle.l[i]
    }
  }
  distance <- as.data.frame(distance)
  distance[4,] <- NA
  dist.sum <- sum(distance, na.rm=TRUE)
  rel.dist <- distance %>%
    transmute(rel.distance = distance/dist.sum)
  kpi.results <- cbind(kpi.results, distance, rel.dist)

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

  # Trunk lean angle ----
  df.temp <- df.filter %>%
    filter(td == TRUE) %>%
    transmute(td.trunk.angle = -1*(rad2deg(atan2(pos.y.head-pos.y.hip, pos.x.head-pos.x.hip))-90))
  if (nrow(df.temp == 4)){df.temp[4,] <- NA}
  kpi.results <- cbind(kpi.results, df.temp)
  df.temp <- df.filter %>%
    filter(to == TRUE) %>%
    transmute(to.trunk.angle = -1*(rad2deg(atan2(pos.y.head-pos.y.hip, pos.x.head-pos.x.hip))-90))
  df.temp[4,] <- NA
  kpi.results <- cbind(kpi.results, df.temp)

  # Inclination angle ----
  # Touchdown
  df.temp <- df.filter %>%
    filter(td == TRUE) %>%
    transmute(td.inc.angle.r = -1*(rad2deg(atan2(pos.y.com-pos.y.ankle.r, pos.x.com-pos.x.ankle.r))-90),
              td.inc.angle.l = -1*(rad2deg(atan2(pos.y.com-pos.y.ankle.l, pos.x.com-pos.x.ankle.l))-90)
    )
  td.inc.angle <- vector()
  for (i in 1:nrow(df.temp)){
    if(df.phase$foot[i] == 'r'){
      td.inc.angle[i] <- df.temp$td.inc.angle.r[i]
    } else if(df.phase$foot[i] == 'l'){
      td.inc.angle[i] <- df.temp$td.inc.angle.l[i]
    } else {
      td.inc.angle[i] <- NA
    }
  }
  td.inc.angle <- as.data.frame(td.inc.angle)
  td.inc.angle[4,] <- NA
  kpi.results <- cbind(kpi.results, td.inc.angle)


  # Take-off
  df.temp <- df.filter %>%
    filter(to == TRUE) %>%
    transmute(to.inc.angle.r = -1*(rad2deg(atan2(pos.y.com-pos.y.ankle.r, pos.x.com-pos.x.ankle.r))-90),
              to.inc.angle.l = -1*(rad2deg(atan2(pos.y.com-pos.y.ankle.l, pos.x.com-pos.x.ankle.l))-90)
    )
  to.inc.angle <- vector()
  for (i in 1:nrow(df.temp)){
    if(df.phase$foot[i] == 'r'){
      to.inc.angle[i] <- df.temp$to.inc.angle.r[i]
    } else if(df.phase$foot[i] == 'l'){
      to.inc.angle[i] <- df.temp$to.inc.angle.l[i]
    } else {
      to.inc.angle[i] <- NA
    }
  }
  to.inc.angle <- as.data.frame(to.inc.angle)
  to.inc.angle[4,] <- NA
  kpi.results <- cbind(kpi.results, to.inc.angle)

  # Minimal knee angle ----
  df.temp <- tibble(.rows=1)
  for (i in 1:length(df.phase)){
  df.temp2 <- df.filter %>%
    subset(t %inrange% c(df.phase$touchdown[i], df.phase$takeoff[i])) %>%
    summarise(min.knee.angle.r = min(pos.theta.knee.r),
              min.knee.angle.l = min(pos.theta.knee.l))
  df.temp <- rbind(df.temp, df.temp2)
  }

  min.knee.angle <- vector()
  for (i in 1:nrow(df.temp)){
    if(df.phase$foot[i] == 'r'){
      min.knee.angle[i] <- df.temp$min.knee.angle.r[i]
    } else if(df.phase$foot[i] == 'l'){
      min.knee.angle[i] <- df.temp$min.knee.angle.l[i]
    } else {
      min.knee.angle[i] <- NA
    }
  }
  min.knee.angle <- as.data.frame(min.knee.angle)
  min.knee.angle[4,] <- NA
  kpi.results <- cbind(kpi.results, min.knee.angle)

  # Thigh angle
  df.temp <- df.filter %>%
    filter(td == TRUE) %>%
    transmute(td.thigh.angle.r = -1*(180-rad2deg(atan2(pos.y.hip-pos.y.knee.r, pos.x.hip-pos.x.knee.r))),
              td.thigh.angle.l = -1*(180-rad2deg(atan2(pos.y.hip-pos.y.knee.l, pos.x.hip-pos.x.knee.l))))
  if (setup$phase$foot[1] == 'l'){
    a <- data.frame(df.temp[1:2,1])
    b <- data.frame(df.temp[3:4,2])
    colnames(a) <- 'td.thigh.angle'
    colnames(b) <- 'td.thigh.angle'
    df.temp <- rbind(a,b)
  }else{
    a <- data.frame(df.temp[1:2,2])
    b <- data.frame(df.temp[3:4,1])
    colnames(a) <- 'td.thigh.angle'
    colnames(b) <- 'td.thigh.angle'
    df.temp <- rbind(a,b)
  }
    kpi.results <- cbind(kpi.results, df.temp)


    df.temp <- df.filter %>%
      filter(to == TRUE) %>%
      transmute(to.thigh.angle.r = -1*(180-rad2deg(atan2(pos.y.hip-pos.y.knee.r, pos.x.hip-pos.x.knee.r))),
                to.thigh.angle.l = -1*(180-rad2deg(atan2(pos.y.hip-pos.y.knee.l, pos.x.hip-pos.x.knee.l))))
    if (setup$phase$foot[1] == 'l'){
      a <- data.frame(df.temp[1:2,1])
      b <- data.frame(df.temp[3:4,2])
      colnames(a) <- 'to.thigh.angle'
      colnames(b) <- 'to.thigh.angle'
      df.temp <- rbind(a,b)
    }else{
      a <- data.frame(df.temp[1:2,2])
      b <- data.frame(df.temp[3:4,1])
      colnames(a) <- 'to.thigh.angle'
      colnames(b) <- 'to.thigh.angle'
      df.temp <- rbind(a,b)
    }
    kpi.results <- cbind(kpi.results, df.temp)

  # Average velocity of lead leg ----
  df.temp <- tibble(.rows=1)
  for (i in 1:length(df.phase)){
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
  hip.angvel[4,] <- NA
  kpi.results <- cbind(kpi.results, hip.angvel)


  # Rounding off results ----
  kpi.results <- kpi.results  %>%
    mutate_if(is.numeric, round, 2)

  # Return ----
  report <- list(kpi.results, df.filter, df.melt, df.phase, setup, matrixNA)
  return(report)
  setwd(act.path)
}
