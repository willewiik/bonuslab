data("iris")
library(MASS)
# 
# mod_ridge_RC <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, iris)
# mod_ridge <- lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, iris)
# diff <- mod_ridge_RC$coef() -  mod_ridge$coef
# all(abs(diff) < 0.05)


test_that("coefficients are similar between lm.ridge(from MASS) and ridgereg(from bonuslabpackage)", {
  mod_ridge_RC <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, iris)
  mod_ridge <- lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, iris)
  diff <- mod_ridge_RC$coef() -  mod_ridge$coef
  expect_true(all(abs(diff) < 0.05))
  
  mod_ridge_RC <- ridgereg(Petal.Length~Sepal.Length+Petal.Width, iris, lambda=5)
  mod_ridge <- lm.ridge(Petal.Length~Sepal.Length+Petal.Width, iris, lambda=5)
  diff <- mod_ridge_RC$coef() -  mod_ridge$coef
  expect_true(all(abs(diff) < 0.05))
  
  mod_ridge_RC <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, iris, lambda=50)
  mod_ridge <- lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, iris, lambda=50)
  diff <- mod_ridge_RC$coef() -  mod_ridge$coef
  expect_true(all(abs(diff) < 0.05))
})
