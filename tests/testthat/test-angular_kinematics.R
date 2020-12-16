test_that("angular_kinematics", {
  # rad2deg
  expect_equal(rad2deg(2*pi), 360)
  #deg2rad
  expect_equal(deg2rad(360), 2*pi)
})
