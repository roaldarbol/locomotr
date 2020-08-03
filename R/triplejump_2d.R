#' 2D Triple Jump Analysis
#'
#' This function outputs the desired results and graphs for a 2D analysis of triple jump
#'
#' @param path Path to input folder
#' @param joints List of joints to be analyzed (begin with capital letter).
#' @export
triplejump_2d <- function(filter='spline', input=NULL){

  res <- kinematics_2d(filter=filter, input = input)

  df.filter <- as.data.frame(res[[1]])
  df.melt <- as.data.frame(res[[2]])
  df.phase <- as.data.frame(res[[3]]$phase)
  setup <- res[[3]]
  matrixNA <- as.data.frame(res[[4]])
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
  if (filter=='butter'){
    df.temp[nrow(df.temp)+1,] <- NA
  } else {
    df.temp[nrow(df.temp),] <- NA
  }

  kpi.results <- cbind(kpi.results, df.temp)

  # CoM Lowering
  df.temp <- df.filter %>%
    filter(td == TRUE) %>%
    transmute(com.lower = lead(pos.y.com) - pos.y.com)
  if (filter=='butter'){
    df.temp[nrow(df.temp)+1,] <- NA
  } else {
    df.temp[nrow(df.temp),] <- NA
  }

  kpi.results <- cbind(kpi.results, df.temp)

  # Foot velocity ----
  df.temp <- df.filter %>%
    filter(lead(td) == TRUE) %>%
    transmute(vel.x.ankle.r = vel.x.ankle.r,
              vel.x.ankle.l = vel.x.ankle.l)
  foot.hor.vel <- vector()
  for (i in 1:nrow(df.temp)){
    if (is.na(df.temp$vel.x.ankle.r[i]) || !df.phase$foot[i] %in% c('r','l')){
      foot.hor.vel[i] <- NA
    } else if (df.phase$foot[i] == 'r'){
      foot.hor.vel[i] <- df.temp$vel.x.ankle.r[i]
    } else if(df.phase$foot[i] == 'l'){
      foot.hor.vel[i] <- df.temp$vel.x.ankle.l[i]
    }
  }
  foot.hor.vel <- as.data.frame(foot.hor.vel)
  foot.hor.vel[nrow(foot.hor.vel),] <- NA

  df.temp <- df.filter %>%
    filter(lead(td) == TRUE) %>%
    transmute(vel.x.ankle.r = vel.x.ankle.r-vel.x.com,
              vel.x.ankle.l = vel.x.ankle.l-vel.x.com)
  foot.hor.vel.rel <- vector()
  for (i in 1:nrow(df.temp)){
    if (is.na(df.temp$vel.x.ankle.r[i]) || !df.phase$foot[i] %in% c('r','l')){
      foot.hor.vel.rel[i] <- NA
    } else if(df.phase$foot[i] == 'r'){
      foot.hor.vel.rel[i] <- df.temp$vel.x.ankle.r[i]
    } else if(df.phase$foot[i] == 'l'){
      foot.hor.vel.rel[i] <- df.temp$vel.x.ankle.l[i]
    }
  }
  foot.hor.vel.rel <- as.data.frame(foot.hor.vel.rel)
  foot.hor.vel.rel[nrow(foot.hor.vel.rel),] <- NA
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
  df.temp[nrow(df.temp),] <- NA
  kpi.results <- cbind(kpi.results, df.temp)
  df.temp <- df.filter %>%
    filter(to == TRUE) %>%
    transmute(to.trunk.angle = -1*(rad2deg(atan2(pos.y.head-pos.y.hip, pos.x.head-pos.x.hip))-90))
  df.temp[nrow(df.temp),] <- NA
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
  td.inc.angle[nrow(td.inc.angle),] <- NA
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
  to.inc.angle[nrow(to.inc.angle),] <- NA
  kpi.results <- cbind(kpi.results, to.inc.angle)

  # Minimal knee angle ----
  df.temp <- tibble(.rows=1)
  for (i in 1:(nrow(df.phase))){
    df.temp2 <- df.filter %>%
      subset(t %inrange% c(df.phase$touchdown[i], df.phase$takeoff[i])) %>%
      summarise(min.knee.angle.r = min(pos.theta.knee.r),
                min.knee.angle.l = min(pos.theta.knee.l))
    df.temp <- rbind(df.temp, df.temp2)
  }

  min.knee.angle <- vector()
  for (i in 1:nrow(df.temp)){
    if (is.na(df.temp$min.knee.angle.r[i]) || !df.phase$foot[i] %in% c('r','l')){
      min.knee.angle[i] <- NA
    } else if(df.phase$foot[i] == 'r'){
      min.knee.angle[i] <- df.temp$min.knee.angle.r[i]
    } else if(df.phase$foot[i] == 'l'){
      min.knee.angle[i] <- df.temp$min.knee.angle.l[i]
    }
  }
  min.knee.angle <- as.data.frame(min.knee.angle)
  min.knee.angle[nrow(min.knee.angle),] <- NA
  kpi.results <- cbind(kpi.results, min.knee.angle)

  # Thigh angle
  df.temp <- df.filter %>%
    filter(td == TRUE) %>%
    transmute(td.thigh.angle.r = -1*(180-rad2deg(atan2(pos.y.hip-pos.y.knee.r, pos.x.hip-pos.x.knee.r))),
              td.thigh.angle.l = -1*(180-rad2deg(atan2(pos.y.hip-pos.y.knee.l, pos.x.hip-pos.x.knee.l))))
  if (setup$phase$foot[3] == 'l'){
    a <- data.frame(df.temp[1,1])
    b <- data.frame(df.temp[2,2])
    c <- data.frame(df.temp[3:4,1])
    d <- data.frame(df.temp[5:6,2])
    colnames(a) <- 'td.thigh.angle'
    colnames(b) <- 'td.thigh.angle'
    colnames(c) <- 'td.thigh.angle'
    colnames(d) <- 'td.thigh.angle'
    df.temp <- rbind(a,b,c,d)
  }else{
    a <- data.frame(df.temp[1,2])
    b <- data.frame(df.temp[2,1])
    c <- data.frame(df.temp[3:4,2])
    d <- data.frame(df.temp[5:6,1])
    colnames(a) <- 'td.thigh.angle'
    colnames(b) <- 'td.thigh.angle'
    colnames(c) <- 'td.thigh.angle'
    colnames(d) <- 'td.thigh.angle'
    df.temp <- rbind(a,b,c,d)
  }
  kpi.results <- cbind(kpi.results, df.temp)


  df.temp <- df.filter %>%
    filter(to == TRUE) %>%
    transmute(to.thigh.angle.r = -1*(180-rad2deg(atan2(pos.y.hip-pos.y.knee.r, pos.x.hip-pos.x.knee.r))),
              to.thigh.angle.l = -1*(180-rad2deg(atan2(pos.y.hip-pos.y.knee.l, pos.x.hip-pos.x.knee.l))))
  if (setup$phase$foot[3] == 'l'){
    a <- data.frame(df.temp[1,1])
    b <- data.frame(df.temp[2,2])
    c <- data.frame(df.temp[3:4,1])
    d <- data.frame(df.temp[5:6,2])
    colnames(a) <- 'to.thigh.angle'
    colnames(b) <- 'to.thigh.angle'
    colnames(c) <- 'to.thigh.angle'
    colnames(d) <- 'to.thigh.angle'
    df.temp <- rbind(a,b,c,d)
  }else{
    a <- data.frame(df.temp[1,2])
    b <- data.frame(df.temp[2,1])
    c <- data.frame(df.temp[3:4,2])
    d <- data.frame(df.temp[5:6,1])
    colnames(a) <- 'to.thigh.angle'
    colnames(b) <- 'to.thigh.angle'
    colnames(c) <- 'to.thigh.angle'
    colnames(d) <- 'to.thigh.angle'
    df.temp <- rbind(a,b,c,d)
  }
  kpi.results <- cbind(kpi.results, df.temp)

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
  kpi.results <- cbind(kpi.results, hip.angvel)


  # Rounding off results ----
  kpi.results <- kpi.results  %>%
    mutate_if(is.numeric, round, 2)

  # Landing analysis
  df.temp <- df.filter %>%
    filter((floor(t * 1000) / 1000) == setup$phase$touchdown[[length(setup$phase$touchdown)]])

  landing.distance <- df.temp$pos.x.ankle.r - df.temp$pos.x.com
  if(is.na(landing.distance)){
    landing.distance <- NA
  }


  df.temp <- df.filter %>%
    subset(t %inrange% c(df.phase[nrow(df.phase)-1,'touchdown'], df.phase[nrow(df.phase),'touchdown']))
  if (setup$phase$foot[length(setup$phase$foot)-1] == 'r'){
    level <- df.temp %>%
      filter(td == TRUE) %>%
      select(pos.y.ankle.r)
    level <- as.double(level[1,1])
  } else {
    level <- df.temp %>%
    filter(td == TRUE) %>%
    select(pos.y.ankle.l)
  level <- as.double(level[1,1])
  }

  df.temp <- df.temp %>%
    filter(!is.na(vel.x.com)) %>%
    select(vel.y.com, vel.x.com, pos.x.com, pos.y.com) %>%
    mutate(to.angle = rad2deg(atan2(vel.y.com, vel.x.com)),
           vel.com = sqrt(vel.x.com^2 + vel.y.com^2))
  df.temp <- df.temp[nrow(df.temp),]

  h <- df.temp$pos.y.com
  vel.h <- df.temp$vel.y.com
  g <- 9.815
  range <- df.temp$pos.x.com

  while (h > level){
    vel.h <- vel.h - (g*(1/120))
    h <- h + vel.h*(1/120)
    range <- range + df.temp$vel.x.com*(1/120)
  }

  actual.range <- df.filter %>%
    filter(td == TRUE) %>%
    select(pos.x.ankle.r, pos.x.ankle.l)
  foot.shortest <- colnames(actual.range[which.min(actual.range[nrow(actual.range),])])
  foot.shortest <- paste0('pos.theta.knee.', stringr::str_sub(foot.shortest, -1))
  actual.range <- min(actual.range[nrow(actual.range),])
  landing.distance.new <- actual.range - range

  landing.kin <- df.filter %>%
    filter(td == TRUE) %>%
    select(pos.theta.hip.r, all_of(foot.shortest), pos.y.head, pos.y.hip, pos.x.head, pos.x.hip)
  landing.kin <- landing.kin[nrow(landing.kin),]
  if (!is.na(landing.kin[,2])){
    landing.kin <- landing.kin %>%
      mutate(trunk.angle = -1*(rad2deg(atan2(pos.y.head-pos.y.hip, pos.x.head-pos.x.hip))-90)) %>%
      select(-c(pos.y.hip, pos.x.hip, pos.y.head, pos.x.head))
  } else {
    landing.kin <- df.filter %>%
      filter(td == TRUE) %>%
      select(pos.theta.hip.r, pos.theta.knee.r, pos.y.head, pos.y.hip, pos.x.head, pos.x.hip) %>%
      mutate(trunk.angle = -1*(rad2deg(atan2(pos.y.head-pos.y.hip, pos.x.head-pos.x.hip))-90)) %>%
      select(-c(pos.y.hip, pos.x.hip, pos.y.head, pos.x.head))
    landing.kin <- landing.kin[nrow(landing.kin),]
  }
  landing.distance <- round(landing.distance, digits = 2)
  landing.distance.new <- round(landing.distance.new, digits = 2)
  landing.analysis <- list()
  landing.analysis$landing.distance <- landing.distance
  landing.analysis$landing.distance.new <- landing.distance.new
  landing.analysis$landing.kin <- landing.kin

  # Return ----
  report <- list(kpi.results, df.filter, df.melt, df.phase, setup, landing.analysis, matrixNA)
  return(report)
  setwd(act.path)
}
