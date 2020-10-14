test_that("duplicates finds duplicate", {
  expect_equal(nrow(duplicates(
    tibble(x = c(1, 2, 1), y = rep(1, 3)), dplyr::everything())), 2)
})

test_that("duplicates finds multiple replicates", {
  expect_equal(nrow(duplicates(
    tibble(x = rep(1, 4), y = c(rep(1, 3), 0)), dplyr::everything())), 3)
})

test_that("duplicates finds multiple sets of duplicates", {
  expect_equal(nrow(duplicates(
    tibble(x = rep(1, 5), y = c(rep(1:2, 2), 0)), dplyr::everything())), 4)
})

test_that("duplicates handles unique input", {
  expect_equal(nrow(duplicates(
    tibble(x = 1:3, y = rep(1, 3)), dplyr::everything())), 0)
})

test_that("duplicates finds duplicate with NA", {
  expect_equal(nrow(duplicates(
    tibble(x = rep(1, 3), y = c(1, NA, NA)), dplyr::everything())), 2)
})

test_that("duplicates deems NA as unique value", {
  expect_equal(nrow(duplicates(
    tibble(x = rep(1, 3), y = c(1, 2, NA)), dplyr::everything())), 0)
})

test_that("duplicates works with cols by name", {
  expect_equal(nrow(duplicates(
    tibble(x = c(1, 2, 1), y = rep(1, 3), z = 1:3), "x", "y")), 2)
})

test_that("duplicates works with cols unquoted", {
  expect_equal(nrow(duplicates(
    tibble(x = c(1, 2, 1), y = rep(1, 3), z = 1:3), x, y)), 2)
})

test_that("duplicates works with cols as list", {
  expect_equal(nrow(duplicates(
    tibble(x = c(1, 2, 1), y = rep(1, 3), z = 1:3), c("x", "y"))), 2)
})

test_that("duplicates works with starts_with()", {
  expect_equal(nrow(duplicates(
    tibble(ax = c(1, 2, 1), ay = rep(1, 3), z = 1:3), dplyr::starts_with("a"))), 2)
})
