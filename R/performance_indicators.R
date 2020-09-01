#' joint angle
#'
#' Gets the angle
#' @param data Filtered data
#' @param df.phase Data containing timings of touchdown and take-off
#' @param timing Touchdown, take-off or minimal/maximal angle
#' @param joint Which joint is being measured, either 'knee' or 'hip'
#' @export
#'

joint_angle <- function(data,
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
  joint.angle[nrow(joint.angle),] <- NA
  return(joint.angle)
}



#' times
#'
#' Get contact and flight times
#' @param df.phase Data containing timings of touchdown and take-off
#' @export
#'
times <- function(df.phase){
  df.temp <- df.phase %>%
    transmute(support = df.phase$takeoff - df.phase$touchdown,
              flight = lead(df.phase$touchdown) - df.phase$takeoff)
  return(df.temp)
}

#' Horizontal and vertical velocity (and take-off angle)
#'
#' Get contact and flight times
#' @param data Data containing timings of touchdown and take-off
#' @param results
#' @param timing
#' @export
#'
velocity <- function(data, results, timing){
  df.temp <- data %>%
    filter(get(timing) == TRUE) %>%
    transmute(vel.hor = vel.x.com,
              vel.vert = vel.y.com,
              to.angle = rad2deg(atan2(vel.y.com, vel.x.com)))
  r <- nrow(data)
  p <- nrow(results)
  if (r==p-1){
    df.temp[r+1,] <- NA
  }
  return(df.temp)
}

#' CoM lowering
#'
#' Displacement
#' @param data Data containing timings of touchdown and take-off
#' @export
#'

disp_com <- function(data, results, timing = c('td', 'to')){
  df.temp <- data %>%
    filter(get(timing) == TRUE) %>%
    transmute(com.lower = lead(pos.y.com) - pos.y.com)

  r <- nrow(df.temp)
  p <- nrow(results)
  if (r==p-1){
    df.temp[r-1:r,] <- NA
  }
  return(df.temp)
}

#' Foot landing velocity
#'
#' @param data Data containing timings of touchdown and take-off
#' @param df.phase Phase data
#' @param comparison Absolute velocity or relative tom CoM
#' @export
#'

foot_vel <- function(data, df.phase, comparison = c('rel', 'abs')){

  if (comparison == 'abs'){
    df.temp <- data %>%
      filter(lead(td) == TRUE) %>%
      transmute(vel.x.ankle.r = vel.x.ankle.r,
                vel.x.ankle.l = vel.x.ankle.l)

  } else if (comparison == 'rel'){
    df.temp <- data %>%
      filter(lead(td) == TRUE) %>%
      transmute(vel.x.ankle.r = vel.x.ankle.r-vel.x.com,
                vel.x.ankle.l = vel.x.ankle.l-vel.x.com)
  }

  foot.vel <- vector()
  for (i in 1:nrow(df.temp)){
    if (is.na(df.temp$vel.x.ankle.r[i]) || !df.phase$foot[i] %in% c('r','l')){
      foot.vel[i] <- NA
    } else if (df.phase$foot[i] == 'r'){
      foot.vel[i] <- df.temp$vel.x.ankle.r[i]
    } else if(df.phase$foot[i] == 'l'){
      foot.vel[i] <- df.temp$vel.x.ankle.l[i]
    }
  }

  foot.vel <- data.frame(foot.vel)
  foot.vel[nrow(foot.vel),] <- NA

  return(foot.vel)
}

#' Relative angles
#'
#' @param data Data containing timings of touchdown and take-off
#' @param df.phase Phase data
#' @param comparison Trunk, inclination, thigh
#' @param timing Touchdown or take-off
#' @export
#'

rel_angle <- function(data,
                      df.phase,
                      comparison=c('trunk', 'inc', 'thigh'),
                      timing = c('td', 'to')){

  p <- nrow(df.phase)

  if (comparison == 'trunk'){
    angle <- data %>%
      filter(get(timing) == TRUE) %>%
      transmute(angle = -1*(rad2deg(atan2(pos.y.head-pos.y.hip, pos.x.head-pos.x.hip))-90))

  } else if(comparison == 'inc'){
    df.temp <- data %>%
      filter(get(timing) == TRUE) %>%
      transmute(angle.r = -1*(rad2deg(atan2(pos.y.com-pos.y.ankle.r, pos.x.com-pos.x.ankle.r))-90),
                angle.l = -1*(rad2deg(atan2(pos.y.com-pos.y.ankle.l, pos.x.com-pos.x.ankle.l))-90))

    } else if(comparison == 'thigh'){
    df.temp <- data %>%
      filter(get(timing) == TRUE) %>%
      transmute(angle.r = -1*(180-rad2deg(atan2(pos.y.hip-pos.y.knee.r, pos.x.hip-pos.x.knee.r))),
                angle.l = -1*(180-rad2deg(atan2(pos.y.hip-pos.y.knee.l, pos.x.hip-pos.x.knee.l))))
  }

  if (comparison %in% c('inc', 'thigh')){
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
  }

  angle <- data.frame(angle)
  names(angle) <- paste(timing, comparison, 'angle', sep = '.')
  r <- nrow(angle)

  if (r==p){
    angle[r,] <- NA
  } else if (r==p-1){
    angle[r-1:r,] <- NA
  } else if (r==p+1){
    angle <- angle[-r,]
    angle[r,] <- NA
  }

  return(angle)
}


#' landing analysis
#'
#' Get all important landing variables
#' @param data
#' @param df.phase Data containing timings of touchdown and take-off
#' @export
#'
landing_analysis <- function(data, df.phase){

  # Landing distance and height
  df.temp <- data %>%
    filter((floor(t * 1000) / 1000) == df.phase$touchdown[[length(df.phase$touchdown)]])
  landing.distance <- round(df.temp$pos.x.ankle.r - df.temp$pos.x.com, 2)
  landing.height <-  round(df.temp$pos.y.com - df.temp$pos.y.ankle.r, 2)


  # Adjusted landing distance
    # First, find the horizontal level from the take-off foot
  df.temp <- data %>%
    subset(t %inrange% c(df.phase[nrow(df.phase)-1,'touchdown'], df.phase[nrow(df.phase),'touchdown']))
  if (df.phase$foot[length(df.phase$foot)-1] == 'r'){
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

    # Continue CoM trajectory
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

    # Compute the shortest landing distance
  actual.range <- data %>%
    filter(td == TRUE) %>%
    select(pos.x.ankle.r, pos.x.ankle.l)
  foot.shortest <- colnames(actual.range[which.min(actual.range[nrow(actual.range),])])
  foot.shortest <- paste0('pos.theta.knee.', stringr::str_sub(foot.shortest, -1))
  actual.range <- min(actual.range[nrow(actual.range),])
  landing.distance.new <- round(actual.range - range, 2)

  # Landing kinematics, body positions
  landing.kin <- data %>%
    filter(td == TRUE) %>%
    select(pos.theta.hip.r, all_of(foot.shortest), pos.y.head, pos.y.hip, pos.x.head, pos.x.hip)
  landing.kin <- landing.kin[nrow(landing.kin),]
  if (!is.na(landing.kin[,2])){
    landing.kin <- landing.kin %>%
      mutate(trunk.angle = -1*(rad2deg(atan2(pos.y.head-pos.y.hip, pos.x.head-pos.x.hip))-90)) %>%
      select(-c(pos.y.hip, pos.x.hip, pos.y.head, pos.x.head))
  } else {
    landing.kin <- data %>%
      filter(td == TRUE) %>%
      select(pos.theta.hip.r, pos.theta.knee.r, pos.y.head, pos.y.hip, pos.x.head, pos.x.hip) %>%
      mutate(trunk.angle = -1*(rad2deg(atan2(pos.y.head-pos.y.hip, pos.x.head-pos.x.hip))-90)) %>%
      select(-c(pos.y.hip, pos.x.hip, pos.y.head, pos.x.head))
    landing.kin <- landing.kin[nrow(landing.kin),]
  }

  # Collect all measures into list
  landing.analysis <- list()
  landing.analysis$landing.distance <- landing.distance
  landing.analysis$landing.height <- landing.height
  landing.analysis$landing.distance.new <- landing.distance.new
  landing.analysis$landing.kin <- landing.kin

  return(landing.analysis)

}
