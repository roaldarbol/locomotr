#' landing analysis
#'
#' Get all important landing variables
#' @param data Data
#' @param df.phase Data containing timings of touchdown and take-off
#' @export
#'
jump_landing_analysis <- function(data, df.phase){

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
