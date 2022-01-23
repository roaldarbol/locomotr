#' Kinematics
#'
#' Computes velocities etc.
#'
#' This function filters data, computes angles, and velocities,
#' acceleration (both linear and angular), touchdown/takeoff,
#' and melts data for easy plotting.
#'
#' @param input List containing at least data and setup
#' @param filt Filter to be used
#' @export
#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom zoo 'na.locf' 'na.spline'
#' @importFrom stats 'na.exclude'
kinematics_2d <- function(input, filt='spline'){

  # Load data files
  if (!exists('input')) {
    load("/Users/roaldarbol/Documents/r/trackr/R/sysdata.rda")
    df.phase <- setup$phase
  } else if (is.null(input)){
    load("/Users/roaldarbol/Documents/r/trackr/R/sysdata.rda")
    df.phase <- setup$phase
  } else {
    data <- input[[1]]
    setup <- input[[2]]
    df.phase <- setup$phase
  }


  # Spline extrapolation for missing values
  for (i in 1:ncol(data)){
    if (!all(is.na(data[,i]))){
      min <- min(which(!is.na(data[,i])))
      max <- max(which(!is.na(data[,i])))
      data[min:max,i] <- zoo::na.spline(data[min:max,i], na.rm=TRUE)
    }
  }

  # Logical matrix & approximations ----
  matrixNA <- is.na(data)

  # Filter data
  df.filter <- data_filter(data, df.phase, filt=filt)

  # Creating lists of joints ----
  segments <- c()
  segments.x <- c()
  segments.y <- c()
  joints.theta <- c()

  for (i in 1:length(setup$skeleton$ref.points)){
    segments[i] <- setup$skeleton$ref.points[i]
    segments.x[i] <- paste('x', setup$skeleton$ref.points[i], sep = '.')
    segments.y[i] <- paste('y', setup$skeleton$ref.points[i], sep = '.')
  }

  for (i in 1:length(setup$skeleton$joints)){
    joints.theta[i] <- paste('theta', names(setup$skeleton$joints[i]), sep = '.')
  }

  joints.list <- list(segments.x, segments.y, joints.theta)

  # Compute angles ----
  df.angles <- joint_angles(df.filter, setup)

  # Compute linear velocities and accelerations ----
  nrows <- nrow(df.filter)
  for (i in 2:ncol(df.filter)){
    colnames(df.filter)[i] <- paste('pos', colnames(df.filter)[i], sep='.')
  }
  com <- centerofmass(df.filter, setup)
  df.filter <- cbind(df.filter, com)

  # Velocity
  pos.names <- grep('pos', names(df.filter), value=TRUE)
  new.names <- sub('pos.', '', pos.names)
  nrows <- nrow(df.filter)
  for (i in 1:length(pos.names)){
    varname <- paste('vel', new.names[i], sep='.')
    diff.var <- diff(df.filter[,pos.names[i]])
    diff.t <- diff(df.filter[,'t'])
    df.filter[2:nrows,varname] <- diff.var/diff.t
  }

  # Acceleration
  vel.names <- grep('vel', names(df.filter), value=TRUE)
  new.names <- sub('vel.', '', vel.names)
  nrows <- nrow(df.filter)
  for (i in 1:length(vel.names)){
    varname <- paste('acc', new.names[i], sep='.')
    diff.var <- diff(df.filter[,vel.names[i]])
    diff.t <- diff(df.filter[,'t'])
    df.filter[2:nrows,varname] <- diff.var/diff.t
  }

  # Compute angular speeds and accelerations ----
  nrows <- nrow(df.angles)
  for (i in 2:ncol(df.angles)){
    colnames(df.angles)[i] <- paste('pos', colnames(df.angles)[i], sep='.')
  }

  # Velocity
  pos.names <- grep("pos", names(df.angles), value=TRUE)
  for (i in 1:length(pos.names)){
    new.names <- sub('pos.', '', pos.names)
    varname <- paste('vel', new.names[i], sep='.')
    diff.angle <- diff(df.angles[,pos.names[i]])
    diff.t <- diff(df.angles[,'t'])
    df.angles[2:nrows,varname] <- diff.angle/diff.t
  }

  # Acceleration
  vel.names <- grep("vel", names(df.angles), value=TRUE)
  for (i in 1:length(vel.names)){
    new.names <- sub('vel.', '', pos.names)
    varname <- paste('acc', new.names[i], sep='.')
    diff.angle <- diff(df.angles[,vel.names[i]])
    diff.t <- diff(df.angles[,'t'])
    df.angles[2:nrows,varname] <- diff.angle/diff.t
  }


  # Compute touchdown and take-off ----
  df.filter <- df.filter %>%
    mutate(phase = "flight") %>%
    select(t, .data$phase, everything())
  df.filter <- cbind(df.filter, df.angles[2:ncol(df.angles)]) # Add df.angles to df.filter

  for(j in 1:length(setup$phase$touchdown)){
    for(k in 1:nrow(df.filter)){
      if(df.filter$t[k] >= setup$phase$touchdown[j] &&
         df.filter$t[k] <= setup$phase$takeoff[j]){
        df.filter$phase[k] = "support"
      }
    }
  }

  df.filter <- df.filter %>%
    mutate(td = if_else(.data$phase != lag(.data$phase) & .data$phase=="support", TRUE, FALSE),
           to = if_else(.data$phase != lag(.data$phase) & .data$phase=="flight", TRUE, FALSE))

  # Melting data ----
  df.melt <- reshape2::melt(df.filter, id=c('t', 'phase', 'td', 'to'), na.rm = FALSE)
  df.melt <- stats::na.exclude(df.melt)
  df.melt <- df.melt %>%
    mutate(motion = as.factor(if_else(grepl('theta', .data$variable), 'ang', 'lin')),
           vector = as.factor(case_when(grepl('pos', .data$variable) == TRUE ~ 'Position',
                                        grepl('vel', .data$variable) == TRUE ~ 'Velocity',
                                        grepl('acc', .data$variable) == TRUE ~ 'Acceleration')),
           direction = as.factor(case_when(grepl('x', .data$variable) == TRUE ~ 'Horizontal',
                                           grepl('y', .data$variable) == TRUE ~ 'Vertical')))

  df.melt$segment <- NA
  for (i in 1:length(segments)){
    df.melt$segment <- case_when(grepl(segments[i], df.melt$variable) == TRUE ~ segments[i],
                                 TRUE ~ as.character(df.melt$segment))
  }
  for (i in 1:length(joints.theta)){
    df.melt$segment <- case_when(grepl(joints.theta[i], df.melt$variable) == TRUE ~ sub('theta.', '', joints.theta[i]),
                                 TRUE ~ as.character(df.melt$segment))
  }
  df.melt$segment <- as.factor(df.melt$segment)

  data.list <- list(df.filter, df.melt, setup, matrixNA)
  return(data.list)
}
