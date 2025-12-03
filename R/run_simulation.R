#' Run Simulations for Phase I Dose-Finding Designs
#'
#' Generic function to conduct computer simulations for phase I dose-finding designs.
#'
#' @param design A design object created by one of the design constructor functions
#'   (\code{\link{design_boin}}, \code{\link{design_i3plus3}}, \code{\link{design_mtpi2}},
#'   \code{\link{design_g3plus3}}, or \code{\link{design_3plus3}}).
#' @param p.true A vector or matrix containing the true toxicity probabilities of
#'   the investigational dose levels.
#' @param mtd.true A numeric value or vector specifying the true MTD.
#' @param ... Additional arguments passed to methods.
#'
#' @return A list containing 'selection' (a dataframe showing selection percentages and
#'   metrics POS, PCS, PUS), 'allocation' (a dataframe showing participant allocation across
#'   dose levels), and 'setup' (a list containing design parameters and simulation settings).
#'
#' @export
#'
#' @examples
#' # Create design specifications
#' boin <- design_boin(pT = 0.25, EI = c(0.15, 0.35), ncohort = 10)
#' i3 <- design_i3plus3(pT = 0.25, EI = c(0.2, 0.3), ncohort = 10)
#'
#' # Define toxicity scenarios
#' p.true <- matrix(c(0.05, 0.10, 0.20, 0.30, 0.45,
#'                    0.10, 0.15, 0.25, 0.35, 0.50),
#'                  nrow = 2, byrow = TRUE)
#' # Binary matrix indicating true MTD for each scenario (dose 3 for both)
#' mtd.true <- matrix(c(0, 0, 1, 0, 0,
#'                      0, 0, 1, 0, 0),
#'                    nrow = 2, byrow = TRUE)
#'
#' # Run simulations
#' \donttest{
#' sim_boin <- run_simulation(boin, p.true = p.true, mtd.true = mtd.true)
#' sim_i3 <- run_simulation(i3, p.true = p.true, mtd.true = mtd.true)
#' }
#'
run_simulation <- function(design, p.true, mtd.true, ...) {
  UseMethod("run_simulation")
}

#' @export
#' @rdname run_simulation
run_simulation.boin_design <- function(design, p.true, mtd.true, ...) {
  if (is.null(design$ncohort)) {
    stop("'ncohort' must be specified in the design object for simulation. ",
         "Use design_boin(..., ncohort = X) where X is the number of cohorts.")
  }

  run_sim_boin(
    p.true = p.true,
    mtd.true = mtd.true,
    pT = design$pT,
    EI = design$EI,
    ncohort = design$ncohort,
    cohortsize = design$cohortsize,
    startdose = design$startdose,
    DU.pp = design$DU.pp,
    n.earlystop = design$n.earlystop,
    extrasafe = design$extrasafe,
    ntrial = design$ntrial,
    seed = design$seed
  )
}

#' @export
#' @rdname run_simulation
run_simulation.i3plus3_design <- function(design, p.true, mtd.true, ...) {
  if (is.null(design$ncohort)) {
    stop("'ncohort' must be specified in the design object for simulation. ",
         "Use design_i3plus3(..., ncohort = X) where X is the number of cohorts.")
  }

  run_sim_i3plus3(
    p.true = p.true,
    mtd.true = mtd.true,
    pT = design$pT,
    EI = design$EI,
    ncohort = design$ncohort,
    cohortsize = design$cohortsize,
    startdose = design$startdose,
    DU.pp = design$DU.pp,
    n.earlystop = design$n.earlystop,
    extrasafe = design$extrasafe,
    ntrial = design$ntrial,
    seed = design$seed
  )
}

#' @export
#' @rdname run_simulation
run_simulation.mtpi2_design <- function(design, p.true, mtd.true, ...) {
  if (is.null(design$ncohort)) {
    stop("'ncohort' must be specified in the design object for simulation. ",
         "Use design_mtpi2(..., ncohort = X) where X is the number of cohorts.")
  }

  run_sim_mtpi2(
    p.true = p.true,
    mtd.true = mtd.true,
    pT = design$pT,
    EI = design$EI,
    ncohort = design$ncohort,
    cohortsize = design$cohortsize,
    startdose = design$startdose,
    DU.pp = design$DU.pp,
    n.earlystop = design$n.earlystop,
    ntrial = design$ntrial,
    seed = design$seed
  )
}

#' @export
#' @rdname run_simulation
run_simulation.g3plus3_design <- function(design, p.true, mtd.true, ...) {
  if (is.null(design$ncohort)) {
    stop("'ncohort' must be specified in the design object for simulation. ",
         "Use design_g3plus3(..., ncohort = X) where X is the number of cohorts.")
  }

  run_sim_g3plus3(
    p.true = p.true,
    mtd.true = mtd.true,
    ncohort = design$ncohort,
    cohortsize = design$cohortsize,
    startdose = design$startdose,
    n.earlystop = design$n.earlystop,
    ntrial = design$ntrial,
    seed = design$seed
  )
}

#' @export
#' @rdname run_simulation
run_simulation.threethree_design <- function(design, p.true, mtd.true, ...) {
  if (is.null(design$ncohort)) {
    # For 3+3, we can infer ncohort from npts if available
    if (!is.null(design$npts)) {
      design$ncohort <- ceiling(design$npts / design$cohortsize)
    } else {
      stop("'ncohort' must be specified in the design object for simulation. ",
           "Use design_3plus3(..., ncohort = X) where X is the number of cohorts.")
    }
  }

  run_sim_3plus3(
    p.true = p.true,
    mtd.true = mtd.true,
    startdose = design$startdose,
    ntrial = design$ntrial,
    seed = design$seed
  )
}

#' @export
#' @rdname run_simulation
run_simulation.default <- function(design, p.true, mtd.true, ...) {
  stop("run_simulation() requires a valid design object. ",
       "Use design_boin(), design_i3plus3(), design_mtpi2(), design_g3plus3(), or design_3plus3() ",
       "to create a design specification.")
}
