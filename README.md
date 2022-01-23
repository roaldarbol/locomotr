
# locomotr

<!-- badges: start -->
<!-- badges: end -->

The goal of `locomotr`  is to perform fully automated biomechanical analyses for athletics events.

## Installation

You can install the development version of `locomotr` like so:

``` r
devtools::install_github("roaldarbol/locomotr")
```

## Example

`locomotr` is created to work in conjunction with `tidymocap` which ensures a common data format from a range of motion capture sources. This is a basic example which shows you how to solve a common problem:

``` r
library(tidymocap)
library(locomotr)
data_raw <- tidymocap::anipose_raw
data_tidy <- tidymocap::tidy_anipose(data_raw)
data_augmented <- tidymocap::augment_poses(data_tidy)
results <- locomotr::jump_2d(
                        data_augmented,
                        event = "long",
                        filt = "spline"
                        )
```

