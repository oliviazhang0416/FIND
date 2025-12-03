#' FIND: Compare Objective Decision Tables for Phase I Dose-Finding Designs
#'
#' @description
#' The FIND package provides tools to compare decision tables and evaluate operating
#' characteristics for Phase I dose-finding trial designs. It implements five popular
#' designs: 3+3, i3+3 (Interval-based 3+3), BOIN (Bayesian Optimal Interval),
#' G3+3 (Generalized 3+3), and mTPI-2 (Modified Toxicity Probability Interval).
#'
#' @section Main Functions:
#'
#' Design Constructors: design_boin(), design_i3plus3(), design_mtpi2(),
#' design_g3plus3(), design_3plus3()
#'
#' Generic Functions: get_decision(), run_simulation()
#'
#' Wrapper Functions: decision_table(), oc_plot()
#'
#' Utility Functions: load_true(), select_mtd(), summarize_metric()
#'
#' @section Getting Started:
#'
#' The S3 interface provides the cleanest workflow:
#'
#' \preformatted{
#' # Create design specifications
#' boin <- design_boin(pT = 0.25, EI = c(0.15, 0.35), npts = 12)
#' i3 <- design_i3plus3(pT = 0.25, EI = c(0.2, 0.3), npts = 12)
#'
#' # Generate decision tables
#' decision_table(boin, i3)
#'
#' # Run simulations
#' p.true <- c(0.05, 0.10, 0.20, 0.30, 0.45)
#' mtd.true <- c(0, 0, 1, 0, 0)
#'
#' boin_sim <- design_boin(pT = 0.25, EI = c(0.15, 0.35), ncohort = 10)
#' results <- run_simulation(boin_sim, p.true = p.true, mtd.true = mtd.true)
#'
#' # Compare operating characteristics
#' oc_plot(results)
#' }
#'
#' @section Vignettes:
#'
#' See vignette("Generate_decision_tables"), vignette("Obtain_operating_characteristics"),
#' and vignette("S3_interface_guide") for detailed examples.
#'
#' @references
#' Storer, B. E. (1989). Design and analysis of phase I clinical trials.
#' Biometrics, 45, 925-937.
#'
#' Liu S. and Yuan, Y. (2015). Bayesian Optimal Interval Designs for Phase I
#' Clinical Trials. Journal of the Royal Statistical Society: Series C,
#' 64, 507-523.
#'
#' Guo, W., Wang, S. J., Yang, S., Lynn, H. S., and Ji, Y. (2017). A Bayesian
#' interval dose-finding design addressing Ockham's razor: mTPI-2.
#' Contemporary Clinical Trials, 58, 23-33.
#'
#' Liu, M., Wang, S. J., and Ji, Y. (2020). The i3+3 design for phase I clinical
#' trials. Journal of Biopharmaceutical Statistics, 30(2), 294-304.
#'
#' @keywords internal
"_PACKAGE"
