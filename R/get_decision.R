#' Generate Decision Tables for Phase I Dose-Finding Designs
#'
#' Generic function to generate dosing decisions (E, S, D or DU) for phase I dose-finding designs.
#'
#' @param design A design object created by one of the design constructor functions
#'   (\code{\link{design_boin}}, \code{\link{design_i3plus3}}, \code{\link{design_mtpi2}},
#'   \code{\link{design_g3plus3}}, or \code{\link{design_3plus3}}).
#' @param ... Additional arguments passed to methods.
#'
#' @return A list containing 'tab' (a dataframe with decisions E, S, D, or DU for each
#'   combination of y and n) and 'setup' (a list containing design parameters).
#'
#' @export
#'
#' @examples
#' # Create design specifications
#' boin <- design_boin(pT = 0.25, EI = c(0.15, 0.35), npts = 12)
#' i3 <- design_i3plus3(pT = 0.25, EI = c(0.2, 0.3), npts = 12)
#'
#' # Generate decision tables
#' decision_boin <- get_decision(boin)
#' decision_i3 <- get_decision(i3)
#'
get_decision <- function(design, ...) {
  UseMethod("get_decision")
}

#' @export
#' @rdname get_decision
get_decision.boin_design <- function(design, ...) {
  get_decision_boin(
    pT = design$pT,
    EI = design$EI,
    npts = design$npts
  )
}

#' @export
#' @rdname get_decision
get_decision.i3plus3_design <- function(design, ...) {
  get_decision_i3plus3(
    pT = design$pT,
    EI = design$EI,
    npts = design$npts
  )
}

#' @export
#' @rdname get_decision
get_decision.mtpi2_design <- function(design, ...) {
  get_decision_mtpi2(
    pT = design$pT,
    EI = design$EI,
    npts = design$npts
  )
}

#' @export
#' @rdname get_decision
get_decision.g3plus3_design <- function(design, ...) {
  get_decision_g3plus3(npts = design$npts)
}

#' @export
#' @rdname get_decision
get_decision.threethree_design <- function(design, ...) {
  get_decision_3plus3(npts = design$npts)
}

#' @export
#' @rdname get_decision
get_decision.default <- function(design, ...) {
  stop("get_decision() requires a valid design object. ",
       "Use design_boin(), design_i3(), design_mtpi2(), design_g3(), or design_threethree() ",
       "to create a design specification.")
}
