test_that("select_mtd works with valid BOIN inputs", {
  result <- select_mtd(
    method = "BOIN",
    pT = 0.25,
    EI = c(0.15, 0.35),
    n_obs = c(3, 6, 3, 0, 0),
    y_obs = c(0, 1, 2, 0, 0),
    DU.pp = 0.95,
    extrasafe = FALSE
  )

  expect_type(result, "list")
  expect_named(result, "d_selected")
  expect_true(is.numeric(result$d_selected))
})

test_that("select_mtd works with valid i3+3 inputs", {
  result <- select_mtd(
    method = "i3+3",
    pT = 0.25,
    EI = c(0.2, 0.3),
    n_obs = c(3, 6, 3, 0, 0),
    y_obs = c(0, 1, 2, 0, 0),
    DU.pp = 0.95,
    extrasafe = FALSE
  )

  expect_type(result, "list")
  expect_named(result, "d_selected")
})

test_that("select_mtd validates inputs correctly", {
  expect_error(
    select_mtd(
      method = "invalid",
      pT = 0.25,
      EI = c(0.15, 0.35),
      n_obs = c(3, 6, 3),
      y_obs = c(0, 1, 2)
    ),
    "Method must be one of"
  )

  expect_error(
    select_mtd(
      method = "BOIN",
      pT = 0.25,
      EI = c(0.15, 0.35),
      n_obs = c(3, 6, 3),
      y_obs = c(0, 1)
    ),
    "'n_obs' and 'y_obs' must have the same length"
  )

  expect_error(
    select_mtd(
      method = "BOIN",
      pT = 0.25,
      EI = c(0.15, 0.35),
      n_obs = c(3, 6, 3),
      y_obs = c(0, 1, 5)
    ),
    "Number of DLTs"
  )

  expect_error(
    select_mtd(
      method = "mTPI2",
      pT = 0.25,
      EI = c(0.2, 0.3),
      n_obs = c(3, 6, 3),
      y_obs = c(0, 1, 2),
      extrasafe = TRUE
    ),
    "Extra safety rule is only applicable"
  )
})
