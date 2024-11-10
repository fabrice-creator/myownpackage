#'Calculate Laguerre polynomial of order n
#'
#'
#'This function calculate Laguerre polynomial of order n
#'
#' @param order Order of Laguerre polynomial
#' @param x Argument of Laguerre polynomial
#' @return Laguerre polynomial of order n
#' @examples
#' Laguerre_poly(5,1)
#' @export

Laguerre_poly <- function(order,x){
  if (order < 0) {
    stop("Laguerre's order must be a positive or zero integer.")
  }
  polynom <- 0
  for (k in 0:order) {
    polynom <- polynom + choose(order,k)*((-x)^k/factorial(k))
  }
  return(polynom)
}
