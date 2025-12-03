#' Design Constructor Functions
#'
#' Constructor functions to create design specification objects for Phase I dose-finding trials.
#'
#' @name design_constructors
#' @rdname design_constructors
NULL

#' @rdname design_constructors
#' @export
#'
#' @param pT a numeric value; the target DLT rate.
#' @param EI a vector of length 2; the equivalence interval (EI).
#' @param npts a numeric value; the number of participants for decision table generation.
#' @param ncohort a numeric value; the total number of cohorts for simulation.
#' @param cohortsize a numeric value; the cohort size (default: 3).
#' @param startdose a numeric value; the starting dose level (default: 1).
#' @param DU.pp a numeric value; the cutoff to remove an overly toxic dose (default: 0.95).
#' @param n.earlystop a numeric value; early stopping parameter (default: 100).
#' @param extrasafe a logical value; whether to implement extra safety rule (default: FALSE).
#' @param ntrial a numeric value; the total number of simulated trials (default: 1000).
#' @param seed a numeric value; random seed for simulation (default: 6).
#'
#' @return An S3 object of class \code{boin_design}, \code{i3plus3_design}, \code{mtpi2_design},
#'         \code{g3plus3_design}, or \code{threethree_design} containing the design specifications.
#'
#' @examples
#' # Create a BOIN design specification
#' boin <- design_boin(pT = 0.25, EI = c(0.15, 0.35), npts = 12)
#'
#' # Create an i3+3 design specification
#' i3 <- design_i3plus3(pT = 0.25, EI = c(0.2, 0.3), npts = 12)
#'
design_boin <- function(pT,
                        EI,
                        npts = 12,
                        ncohort = NULL,
                        cohortsize = 3,
                        startdose = 1,
                        DU.pp = 0.95,
                        n.earlystop = 100,
                        extrasafe = FALSE,
                        ntrial = 1000,
                        seed = 6) {

  # Input validation
  if (missing(pT)) {
    stop("Target toxicity rate 'pT' must be specified")
  }
  if (missing(EI)) {
    stop("Equivalence interval 'EI' must be specified")
  }
  if (!is.numeric(pT) || length(pT) != 1) {
    stop("'pT' must be a single numeric value")
  }
  if (!is.numeric(EI) || length(EI) != 2) {
    stop("'EI' must be a numeric vector of length 2")
  }
  if (pT <= 0.05) {
    stop("Target toxicity rate 'pT' is too low (must be > 0.05)")
  }
  if (pT >= 0.6) {
    stop("Target toxicity rate 'pT' is too high (must be < 0.6)")
  }
  if (EI[1] >= pT) {
    stop("Lower bound of EI must be less than pT")
  }
  if (EI[2] <= pT) {
    stop("Upper bound of EI must be greater than pT")
  }
  if ((pT - EI[1]) < (0.1 * pT)) {
    stop("Lower bound of EI is too close to pT (must be at least 10% below pT)")
  }
  if ((EI[2] - pT) < (0.1 * pT)) {
    stop("Upper bound of EI is too close to pT (must be at least 10% above pT)")
  }

  structure(
    list(
      method = "BOIN",
      pT = pT,
      EI = EI,
      npts = npts,
      ncohort = ncohort,
      cohortsize = cohortsize,
      startdose = startdose,
      DU.pp = DU.pp,
      n.earlystop = n.earlystop,
      extrasafe = extrasafe,
      ntrial = ntrial,
      seed = seed
    ),
    class = c("boin_design", "phase1_design")
  )
}

#' @rdname design_constructors
#' @export
design_i3plus3 <- function(pT,
                      EI,
                      npts = 12,
                      ncohort = NULL,
                      cohortsize = 3,
                      startdose = 1,
                      DU.pp = 0.95,
                      n.earlystop = 100,
                      extrasafe = FALSE,
                      ntrial = 1000,
                      seed = 6) {

  # Input validation
  if (missing(pT)) {
    stop("Target toxicity rate 'pT' must be specified")
  }
  if (missing(EI)) {
    stop("Equivalence interval 'EI' must be specified")
  }
  if (!is.numeric(pT) || length(pT) != 1) {
    stop("'pT' must be a single numeric value")
  }
  if (!is.numeric(EI) || length(EI) != 2) {
    stop("'EI' must be a numeric vector of length 2")
  }
  if (pT <= 0.05) {
    stop("Target toxicity rate 'pT' is too low (must be > 0.05)")
  }
  if (pT >= 0.6) {
    stop("Target toxicity rate 'pT' is too high (must be < 0.6)")
  }
  if (EI[1] >= pT) {
    stop("Lower bound of EI must be less than pT")
  }
  if (EI[2] <= pT) {
    stop("Upper bound of EI must be greater than pT")
  }
  if ((pT - EI[1]) < (0.1 * pT)) {
    stop("Lower bound of EI is too close to pT (must be at least 10% below pT)")
  }
  if ((EI[2] - pT) < (0.1 * pT)) {
    stop("Upper bound of EI is too close to pT (must be at least 10% above pT)")
  }

  structure(
    list(
      method = "i3+3",
      pT = pT,
      EI = EI,
      npts = npts,
      ncohort = ncohort,
      cohortsize = cohortsize,
      startdose = startdose,
      DU.pp = DU.pp,
      n.earlystop = n.earlystop,
      extrasafe = extrasafe,
      ntrial = ntrial,
      seed = seed
    ),
    class = c("i3plus3_design", "phase1_design")
  )
}

#' @rdname design_constructors
#' @export
design_mtpi2 <- function(pT,
                         EI,
                         npts = 12,
                         ncohort = NULL,
                         cohortsize = 3,
                         startdose = 1,
                         DU.pp = 0.95,
                         n.earlystop = 100,
                         extrasafe = FALSE,
                         ntrial = 1000,
                         seed = 6) {

  # Input validation
  if (missing(pT)) {
    stop("Target toxicity rate 'pT' must be specified")
  }
  if (missing(EI)) {
    stop("Equivalence interval 'EI' must be specified")
  }
  if (!is.numeric(pT) || length(pT) != 1) {
    stop("'pT' must be a single numeric value")
  }
  if (!is.numeric(EI) || length(EI) != 2) {
    stop("'EI' must be a numeric vector of length 2")
  }
  if (pT <= 0.05) {
    stop("Target toxicity rate 'pT' is too low (must be > 0.05)")
  }
  if (pT >= 0.6) {
    stop("Target toxicity rate 'pT' is too high (must be < 0.6)")
  }
  if (EI[1] >= pT) {
    stop("Lower bound of EI must be less than pT")
  }
  if (EI[2] <= pT) {
    stop("Upper bound of EI must be greater than pT")
  }
  if ((pT - EI[1]) < (0.1 * pT)) {
    stop("Lower bound of EI is too close to pT (must be at least 10% below pT)")
  }
  if ((EI[2] - pT) < (0.1 * pT)) {
    stop("Upper bound of EI is too close to pT (must be at least 10% above pT)")
  }

  structure(
    list(
      method = "mTPI2",
      pT = pT,
      EI = EI,
      npts = npts,
      ncohort = ncohort,
      cohortsize = cohortsize,
      startdose = startdose,
      DU.pp = DU.pp,
      n.earlystop = n.earlystop,
      extrasafe = extrasafe,
      ntrial = ntrial,
      seed = seed
    ),
    class = c("mtpi2_design", "phase1_design")
  )
}

#' @rdname design_constructors
#' @export
design_g3plus3 <- function(npts = 12,
                      ncohort = NULL,
                      cohortsize = 3,
                      startdose = 1,
                      DU.pp = 0.95,
                      n.earlystop = 100,
                      ntrial = 1000,
                      seed = 6) {

  # Input validation
  if (!is.numeric(npts) || length(npts) != 1) {
    stop("'npts' must be a single numeric value")
  }
  if (npts < 3){
    stop("Number of participants 'npts' must be at least 3")
  }

  structure(
    list(
      method = "G3",
      pT = 0.25,  # Fixed for G3
      EI = list(c(0.2, 1/3), c(0.2, 0.29)),  # Fixed for G3
      npts = npts,
      ncohort = ncohort,
      cohortsize = cohortsize,
      startdose = startdose,
      DU.pp = DU.pp,
      n.earlystop = n.earlystop,
      extrasafe = FALSE,  # Not applicable for G3
      ntrial = ntrial,
      seed = seed
    ),
    class = c("g3plus3_design", "phase1_design")
  )
}

#' @rdname design_constructors
#' @export
design_3plus3 <- function(npts = 12,
                               ncohort = NULL,
                               cohortsize = 3,
                               startdose = 1,
                               ntrial = 1000,
                               seed = 6) {

  # Input validation
  if (!is.numeric(npts) || length(npts) != 1) {
    stop("'npts' must be a single numeric value")
  }
  if (npts < 3){
    stop("Number of participants 'npts' must be at least 3")
  }

  structure(
    list(
      method = "3+3",
      pT = NULL,  # Not applicable for 3+3
      EI = NULL,  # Not applicable for 3+3
      npts = npts,
      ncohort = ncohort,
      cohortsize = cohortsize,
      startdose = startdose,
      DU.pp = NULL,  # Not applicable for 3+3
      n.earlystop = NULL,  # Not applicable for 3+3
      extrasafe = FALSE,  # Not applicable for 3+3
      ntrial = ntrial,
      seed = seed
    ),
    class = c("threethree_design", "phase1_design")
  )
}

#' @export
print.phase1_design <- function(x, ...) {
  cat("Phase I Dose-Finding Design:", x$method, "\n")
  cat("-----------------------------\n")
  if (!is.null(x$pT)) cat("Target DLT rate (pT):", x$pT, "\n")
  if (!is.null(x$EI) && !is.list(x$EI)) {
    cat("Equivalence Interval (EI): [", x$EI[1], ", ", x$EI[2], "]\n", sep = "")
  }
  cat("Number of participants:", x$npts, "\n")
  if (!is.null(x$ncohort)) {
    cat("Number of cohorts:", x$ncohort, "\n")
    cat("Cohort size:", x$cohortsize, "\n")
  }
  invisible(x)
}
