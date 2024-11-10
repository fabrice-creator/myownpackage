#' Perform adaptive filter
#'
#' This function applies  adaptive filter on a signal using Laguerre polynomials
#'
#' @param signal Incoming signal
#' @param base_order Base order of Laguerre's polynomial series
#' @param adaptivity_factor A parameter that controls the degree of variation in the order
#' of the Laguerre filter as a function of the local variance of the signal.
#' @return filtered_signal
#' @importFrom stats var
#' @export
#' @examples
#' signal <- c(1, 2, 3, 4, 5, 4, 3, 2, 1)
#' base_order <- 3
#' adaptivity_factor <- 5
#' filtered_signal <- adaptive_filter(signal, base_order, adaptivity_factor)

adaptive_filter <- function(signal, base_order, adaptivity_factor) {
  if (base_order < 0 ) {
    stop("Laguerre's orders must be positive or zero integers.")
  }
  if (adaptivity_factor < 0 ) {
    stop("The adjustment factor must be positive.")
  }

  n <- length(signal)
  filtered_signal <- numeric(n)

  for (i in 1:n) {
    # Calculating local variance with a wider window
    local_var <- var(signal[max(1, i - 20):min(n, i + 20)])  # Local variance

    # Set a threshold for constant signals
    if (local_var < 1e-06) {
      dynamic_order <- base_order  # Limit the order for constant signals
    } else {
      dynamic_order <- min(base_order + round(adaptivity_factor * local_var), base_order + 3)
    }

    # Filtering with Laguerre polynomials up to dynamic order
    x <- i / n
    for (k in 0:dynamic_order) {
      laguerre_value <- Laguerre_poly(k, x)
      filtered_signal[i] <- filtered_signal[i] + signal[i] * laguerre_value
    }
    filtered_signal[i] <- filtered_signal[i] / (dynamic_order + 1)
  }
  return(filtered_signal)
}
