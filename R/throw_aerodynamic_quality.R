aerodynamic_quality <- function(actual_distance,
                                release_height,
                                release_angle,
                                release_velocity){
  g <- 9.81
  release_angle <- deg2rad(release_angle)
  Vx = release_velocity * cos(release_angle)
  Vy = release_velocity * sin(release_angle)
  theoretical_distance <- Vx * (Vy + sqrt(Vy^2 + 2 * g * release_height)) / g
  diff_distance <- actual_distance - theoretical_distance
  # aerodynamic_quality <-
  return(diff_distance)
}
