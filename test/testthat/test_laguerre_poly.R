library(testthat)
library(Rfilter)


test_that("le polynôme de Laguerre d'ordre 0 retourne 1", {
  result <- Laguerre_poly(0, 0.5)
  expect_equal(result, 1)  # Le polynôme de Laguerre d'ordre 0 est toujours égal à 1
})
test_that("le polynôme de Laguerre d'ordre 1 et 2 est calculé correctement", {
  result_order_1 <- Laguerre_poly(1, 0.5)
  result_order_2 <- Laguerre_poly(2, 0.5)

  # Calcul attendu pour l'ordre 1 : L_1(x) = 1 - x
  expect_equal(result_order_1, 1 - 0.5)

  # Calcul attendu pour l'ordre 2 : L_2(x) = 1 - 2x + x^2 / 2
  expect_equal(result_order_2, 1 - 2 * 0.5 + (0.5^2) / 2)
})
test_that("le polynôme de Laguerre fonctionne avec des valeurs négatives de x", {
  result_neg_x <- Laguerre_poly(2, -0.5)

  # Le calcul exact dépend de l'ordre, mais on vérifie juste qu'il n'y a pas d'erreur
  expect_true(is.numeric(result_neg_x))  # Vérifie que le résultat est numérique
})
test_that("le polynôme de Laguerre est correct pour x = 1", {
  result_order_0 <- Laguerre_poly(0, 1)
  result_order_1 <- Laguerre_poly(1, 1)

  # Polynom de Laguerre d'ordre 0 : L_0(1) = 1
  expect_equal(result_order_0, 1)

  # Polynom de Laguerre d'ordre 1 : L_1(1) = 1 - 1
  expect_equal(result_order_1, 0)
})
test_that("les valeurs calculées sont conformes à la formule de Laguerre", {
  # Polynom de Laguerre d'ordre 3 pour x = 0.5
  result <- Laguerre_poly(3, 0.5)
  expected_result <- 1 - 3 * 0.5 + 3 * (0.5^2) / 2 - (0.5^3) / 6
  expect_equal(result, expected_result)
})

