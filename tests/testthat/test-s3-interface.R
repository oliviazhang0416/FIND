test_that("design constructors create proper S3 objects", {
  boin <- design_boin(pT = 0.25, EI = c(0.15, 0.35), npts = 12)
  i3 <- design_i3plus3(pT = 0.25, EI = c(0.2, 0.3), npts = 12)
  mtpi2 <- design_mtpi2(pT = 0.25, EI = c(0.2, 0.3), npts = 12)
  g3 <- design_g3plus3(npts = 12)
  three <- design_3plus3(npts = 12)

  # Check classes
  expect_s3_class(boin, "boin_design")
  expect_s3_class(boin, "phase1_design")
  expect_s3_class(i3, "i3plus3_design")
  expect_s3_class(mtpi2, "mtpi2_design")
  expect_s3_class(g3, "g3plus3_design")
  expect_s3_class(three, "threethree_design")

  # Check structure
  expect_type(boin, "list")
  expect_true("method" %in% names(boin))
  expect_equal(boin$method, "BOIN")
  expect_equal(boin$pT, 0.25)
  expect_equal(boin$EI, c(0.15, 0.35))
})

test_that("design constructors validate inputs", {
  expect_error(
    design_boin(EI = c(0.15, 0.35), npts = 12),
    "Target toxicity rate 'pT' must be specified"
  )

  expect_error(
    design_boin(pT = 0.25, npts = 12),
    "Equivalence interval 'EI' must be specified"
  )

  expect_error(
    design_boin(pT = 0.02, EI = c(0.15, 0.35), npts = 12),
    "too low"
  )

  expect_error(
    design_boin(pT = 0.7, EI = c(0.15, 0.35), npts = 12),
    "too high"
  )
})

test_that("get_decision S3 dispatch works", {
  boin <- design_boin(pT = 0.25, EI = c(0.15, 0.35), npts = 12)
  i3 <- design_i3plus3(pT = 0.25, EI = c(0.2, 0.3), npts = 12)
  three <- design_3plus3(npts = 12)

  result_boin <- get_decision(boin)
  result_i3 <- get_decision(i3)
  result_three <- get_decision(three)

  # Check structure
  expect_type(result_boin, "list")
  expect_named(result_boin, c("tab", "setup"))
  expect_s3_class(result_boin$tab, "data.frame")

  expect_type(result_i3, "list")
  expect_named(result_i3, c("tab", "setup"))

  expect_type(result_three, "list")
  expect_named(result_three, c("tab", "setup"))
})

test_that("get_decision fails with non-design objects", {
  expect_error(
    get_decision(list(a = 1, b = 2)),
    "requires a valid design object"
  )

  expect_error(
    get_decision("not a design"),
    "requires a valid design object"
  )
})

test_that("run_simulation S3 dispatch works", {
  skip_on_cran()

  boin <- design_boin(pT = 0.25, EI = c(0.15, 0.35), ncohort = 4, ntrial = 10)
  # Use vector format for single scenario
  # p.true: toxicity probabilities for each dose
  # mtd.true: binary indicator for MTD (1 = dose 3 is MTD)
  p.true <- c(0.05, 0.10, 0.20, 0.30, 0.45)
  mtd.true <- c(0, 0, 1, 0, 0)

  result <- run_simulation(boin, p.true = p.true, mtd.true = mtd.true)

  expect_type(result, "list")
  expect_true(all(c("selection", "allocation", "setup") %in% names(result)))
  expect_s3_class(result$selection, "data.frame")
  expect_s3_class(result$allocation, "data.frame")
})

test_that("run_simulation requires ncohort", {
  boin <- design_boin(pT = 0.25, EI = c(0.15, 0.35), npts = 12)
  p.true <- c(0.05, 0.10, 0.20, 0.30, 0.45)
  mtd.true <- 3

  expect_error(
    run_simulation(boin, p.true = p.true, mtd.true = mtd.true),
    "'ncohort' must be specified"
  )
})

test_that("print method for phase1_design works", {
  boin <- design_boin(pT = 0.25, EI = c(0.15, 0.35), npts = 12)

  expect_output(print(boin), "Phase I Dose-Finding Design: BOIN")
  expect_output(print(boin), "Target DLT rate")
  expect_output(print(boin), "Equivalence Interval")
})

test_that("decision_table wrapper works with S3 objects", {
  skip_on_cran()

  boin <- design_boin(pT = 0.25, EI = c(0.15, 0.35), npts = 12)
  i3 <- design_i3plus3(pT = 0.25, EI = c(0.2, 0.3), npts = 12)

  # Should not error (suppress ggplot2 warnings about aes)
  suppressWarnings(expect_error(decision_table(boin, i3), NA))
})

test_that("decision_table wrapper validates inputs", {
  expect_error(
    decision_table(list(a = 1)),
    "must be design objects"
  )

  expect_error(
    decision_table("not a design"),
    "must be design objects"
  )
})

test_that("backwards compatibility is maintained", {
  # Old interface should still work
  result_old <- get_decision_boin(pT = 0.25, EI = c(0.15, 0.35), npts = 12)

  # New interface
  boin <- design_boin(pT = 0.25, EI = c(0.15, 0.35), npts = 12)
  result_new <- get_decision(boin)

  # Results should be identical
  expect_equal(result_old$tab, result_new$tab)
})
