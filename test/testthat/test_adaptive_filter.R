library(testthat)
library(Rfilter)

test_that("The adaptive filter remains stable on a white noise signal", {
  set.seed(123)
  signal <- rnorm(1000)  # Generates white noise

  filtered_signal <- adaptive_filter(signal, base_order = 2, adaptivity_factor = 0.5)

  # Checks that the variance remains within a reasonable limit after filtering
  expect_lt(var(filtered_signal), 1.5 * var(signal))
})

test_that("Adaptive filter handles invalid input correctly", {
  signal <- rnorm(1000)

  # Test for a negative 'base_order'
  expect_error(adaptive_filter(signal, base_order = -1, adaptivity_factor = 0.5),
               "Laguerre's orders must be positive or zero integers.")

  # Test for a negative 'adaptivity_factor'
  expect_error(adaptive_filter(signal, base_order = 2, adaptivity_factor = -0.5),
               "The adjustment factor must be positive.")
})

test_that("Adaptive filter reduces noise on a noisy ECG signal", {
  # Simulation of a noisy ECG signal
  time <- seq(0, 10, length.out = 1000)
  signal_ecg <- sin(2 * pi * 1:1000 / 100) + rnorm(1000, mean = 0, sd = 0.5)

  filtered_signal <- adaptive_filter(signal_ecg, base_order = 2, adaptivity_factor = 0.5)

  # Checks that the variance is reduced after filtering
  expect_lt(var(filtered_signal), var(signal_ecg))
})
