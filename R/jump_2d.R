#' 2D Jump Analysis
#'
#' This function outputs the desired results and graphs for a 2D analysis
#' of either the long jump or the triple jump.
#'
#' @param event Path to input folder
#' @param filt Which filter to use. Defaults to 'spline'.
#' @param input Input list. Should contain a data file, a setup file and a commentary.
#' @export
jump_2d <- function(input, event=c('long', 'triple'), filt='spline'){

  if (!exists('filt')){filt <- 'spline'}
  if (!exists('input')){input <- NULL}

  # library(yaml)
  # library(readxl)
  # library(dplyr)
  # input <- list()
  # input[[1]] <- read_xlsx('/Users/roaldarbol/Downloads/23092020/P-TJ2-right.xlsx')
  # input[[2]] <- read_yaml('/Users/roaldarbol/Downloads/23092020/setup_P-TJ2-right.yaml')
  # input[[3]] <- read_yaml('/Users/roaldarbol/Downloads/23092020/comments_tj_template.yaml')
  res <- kinematics_2d(filt=filt, input = input)

  # Data and parameters
  df.filter <- as.data.frame(res[[1]])
  df.melt <- as.data.frame(res[[2]])
  df.phase <- as.data.frame(res[[3]]$phase)
  setup <- res[[3]]
  matrixNA <- as.data.frame(res[[4]])
  results <- tibble(.rows = nrow(df.phase))
  results$phase <- df.phase$names
  dist <- res[[3]]$profile$distance

  # Important measures - MAKE SURE TO FIND THE RIGHT TAKEOFF INSTANT!!!:
  timing <- times(df.phase) # Duration on ground and flight
  velocities <- velocity(df.filter, results, 'to') # Velocity and take-off angle
  disp.com <- disp_com(df.filter, results, 'td') # CoM Lowering
  foot.vel.abs <- segment_vel(df.filter, df.phase, 'ankle', 'abs', 'x') # Absolute foot velocity
  foot.vel.rel <- segment_vel(df.filter, df.phase, 'ankle', 'rel', 'x') # Relative foot velocity
  names(foot.vel.abs) <- 'foot.vel.abs'
  names(foot.vel.rel) <- 'foot.vel.rel'
  step.length <- step_length(df.filter, df.phase, dist, 'abs') # Absolute step length

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
  hip.angvel <- joint_vel(df.filter, df.phase, 'hip') # Hip angular velocity

  # Bind results ----
  results <- cbind(results,
                   timing,
                   velocities,
                   disp.com,
                   foot.vel.abs,
                   foot.vel.rel,
                   step.length,
                   td.knee,
                   min.knee,
                   to.knee,
                   td.trunk,
                   to.trunk,
                   td.inc,
                   to.inc,
                   td.thigh,
                   to.thigh,
                   hip.angvel)

  if (event == 'triple'){
    phase.ratio <- step_length(df.filter, df.phase, dist, 'rel') # Phase ratio
    results <- cbind(results, phase.ratio)
  }

  # Rounding off results ----
  results <- results  %>%
    mutate_if(is.numeric, round, 2)

  # Landing analysis
  landing.analysis <- landing_analysis(df.filter, df.phase)

  # Return ----
  report <- list(results, df.filter, df.melt, df.phase, setup, landing.analysis, matrixNA)
  return(report)
}
