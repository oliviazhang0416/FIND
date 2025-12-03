test_that("get_decision_boin works with valid inputs", {
  result <- get_decision_boin(pT = 0.25, EI = c(0.15, 0.35), npts = 12)

  expect_type(result, "list")
  expect_named(result, c("tab", "setup"))
  expect_s3_class(result$tab, "data.frame")
  expect_true(nrow(result$tab) > 0)
})

test_that("get_decision_boin validates inputs correctly", {
  expect_error(
    get_decision_boin(pT = NULL, EI = c(0.15, 0.35), npts = 12),
    "Target toxicity rate 'pT' must be specified"
  )

  expect_error(
    get_decision_boin(pT = 0.25, EI = NULL, npts = 12),
    "Equivalence interval 'EI' must be specified"
  )

  expect_error(
    get_decision_boin(pT = 0.02, EI = c(0.15, 0.35), npts = 12),
    "Target toxicity rate 'pT' is too low"
  )

  expect_error(
    get_decision_boin(pT = 0.65, EI = c(0.15, 0.35), npts = 12),
    "Target toxicity rate 'pT' is too high"
  )

  expect_error(
    get_decision_boin(pT = 0.25, EI = c(0.15, 0.35), npts = 2),
    "Number of participants 'npts' must be at least 3"
  )
})

test_that("get_decision_i3plus3 works with valid inputs", {
  result <- get_decision_i3plus3(pT = 0.25, EI = c(0.2, 0.3), npts = 12)

  expect_type(result, "list")
  expect_named(result, c("tab", "setup"))
  expect_s3_class(result$tab, "data.frame")
})

test_that("get_decision_3plus3 works with valid inputs", {
  result <- get_decision_3plus3(npts = 12)

  expect_type(result, "list")
  expect_named(result, c("tab", "setup"))
  expect_s3_class(result$tab, "data.frame")
})

test_that("get_decision_3plus3 validates inputs correctly", {
  expect_error(
    get_decision_3plus3(npts = 2),
    "Number of participants 'npts' must be at least 3"
  )
})

test_that("get_decision_g3plus3 works with valid inputs", {
  result <- get_decision_g3plus3(npts = 12)

  expect_type(result, "list")
  expect_named(result, c("tab", "setup"))
  expect_s3_class(result$tab, "data.frame")
})

test_that("get_decision_mtpi2 works with valid inputs", {
  result <- get_decision_mtpi2(pT = 0.25, EI = c(0.2, 0.3), npts = 12)

  expect_type(result, "list")
  expect_named(result, c("tab", "setup"))
  expect_s3_class(result$tab, "data.frame")
})
