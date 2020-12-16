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

  r <- nrow(joint.angle)
  p <- nrow(df.phase)
  if (r==p-1){
    joint.angle[r+1,] <- NA
  }
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
#' @param results A results file of correct number of columns
#' @param timing Specifies 'td' or 'to'
#'
#' @importFrom dplyr %>%
#' @export
#'
#'
velocity <- function(data, results, timing = c('td', 'to')){
  df.temp <- data %>%
    filter(get(timing) == TRUE) %>%
    transmute(vel.hor = .data$vel.x.com,
              vel.vert = .data$vel.y.com,
              to.angle = rad2deg(atan2(.data$vel.y.com, .data$vel.x.com)))
  r <- nrow(df.temp)
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
#' @param results A results file of correct number of columns
#' @param timing Specifies 'td' or 'to'
#' @export
#'

disp_com <- function(data, results, timing = c('td', 'to')){
  df.temp <- data %>%
    filter(get(timing) == TRUE) %>%
    transmute(com.lower = lead(.data$pos.y.com) - .data$pos.y.com)

  r <- nrow(df.temp)
  p <- nrow(results)
  df.temp[(r-1):r,] <- NA

  return(df.temp)
}


#' Step length
#'
#' Calculate step lengths
#' @param data Data containing timings of touchdown and take-off
#' @param df.phase Df.phase
#' @param dist Total distance of the jump, from setup file
#' @param comparison Absolute or relative step lengths
#' @export
#'

step_length <- function(data, df.phase, dist, comparison=c('abs', 'rel')){

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



#' Segment landing velocity
#'
#' @param data Data
#' @param df.phase Data containing timings of touchdown and take-off
#' @param segment Which segment
#' @param comparison Absolute velocity or relative to CoM
#' @param dir Either X or Y direction
#' @export
#'

segment_vel <- function(data,
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

#' Joint angular velocity
#'
#' @param data Data
#' @param df.phase Data containing timings of touchdown and take-off
#' @param joint Which segment
#' @export
#'

joint_vel <- function(data,
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


#' landing analysis
#'
#' Get all important landing variables
#' @param data Data
#' @param df.phase Data containing timings of touchdown and take-off
#' @export
#'
landing_analysis <- function(data, df.phase){

  # Landing distance and height
  df.temp <- data %>%
    filter(specify_decimal(t, 3) == df.phase$touchdown[[length(df.phase$touchdown)]])
  landing.distance <- round(df.temp$pos.x.ankle.r - df.temp$pos.x.com, 2)
  landing.height <-  round(df.temp$pos.y.com - df.temp$pos.y.ankle.r, 2)


  # Adjusted landing distance
    # First, find the horizontal level from the take-off foot
  df.temp <- data %>%
    subset(t %inrange% c(df.phase[nrow(df.phase)-1,'touchdown'], df.phase[nrow(df.phase),'touchdown']))
  if (df.phase$foot[length(df.phase$foot)-1] == 'r'){
    level <- df.temp %>%
      filter(.data$td == TRUE) %>%
      select(.data$pos.y.ankle.r)
    level <- as.double(level[1,1])
  } else {
    level <- df.temp %>%
      filter(.data$td == TRUE) %>%
      select(.data$pos.y.ankle.l)
    level <- as.double(level[1,1])
  }

    # Continue CoM trajectory
  df.temp <- df.temp %>%
    filter(!is.na(.data$vel.x.com)) %>%
    select(.data$vel.y.com, .data$vel.x.com, .data$pos.x.com, .data$pos.y.com) %>%
    mutate(to.angle = rad2deg(atan2(.data$vel.y.com, .data$vel.x.com)),
           vel.com = sqrt(.data$vel.x.com^2 + .data$vel.y.com^2))
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
    filter(.data$td == TRUE) %>%
    select(.data$pos.x.ankle.r, .data$pos.x.ankle.l)
  foot.shortest <- colnames(actual.range[which.min(actual.range[nrow(actual.range),])])
  foot.shortest <- paste0('pos.theta.knee.', stringr::str_sub(foot.shortest, -1))
  actual.range <- min(actual.range[nrow(actual.range),])
  landing.distance.new <- round(actual.range - range, 2)

  # Landing kinematics, body positions
  landing.kin <- data %>%
    filter(.data$td == TRUE) %>%
    select(.data$pos.theta.hip.r, all_of(foot.shortest), .data$pos.y.head, .data$pos.y.hip, .data$pos.x.head, .data$pos.x.hip)
  landing.kin <- landing.kin[nrow(landing.kin),]
  if (!is.na(landing.kin[,2])){
    landing.kin <- landing.kin %>%
      mutate(trunk.angle = -1*(rad2deg(atan2(.data$pos.y.head-.data$pos.y.hip, .data$pos.x.head-.data$pos.x.hip))-90)) %>%
      select(-c(.data$pos.y.hip, .data$pos.x.hip, .data$pos.y.head, .data$pos.x.head))
  } else {
    landing.kin <- data %>%
      filter(.data$td == TRUE) %>%
      select(.data$pos.theta.hip.r, .data$pos.theta.knee.r, .data$pos.y.head, .data$pos.y.hip, .data$pos.x.head, .data$pos.x.hip) %>%
      mutate(trunk.angle = -1*(rad2deg(atan2(.data$pos.y.head-.data$pos.y.hip, .data$pos.x.head-.data$pos.x.hip))-90)) %>%
      select(-c(.data$pos.y.hip, .data$pos.x.hip, .data$pos.y.head, .data$pos.x.head))
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
