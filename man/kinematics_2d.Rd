% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kinematics_2d.R
\name{kinematics_2d}
\alias{kinematics_2d}
\title{Kinematics}
\usage{
kinematics_2d(input, filt = "spline")
}
\arguments{
\item{input}{List containing at least data and setup}

\item{filt}{Filter to be used}
}
\description{
Computes velocities etc.
}
\details{
This function filters data, computes angles, and velocities,
acceleration (both linear and angular), touchdown/takeoff,
and melts data for easy plotting.
}
