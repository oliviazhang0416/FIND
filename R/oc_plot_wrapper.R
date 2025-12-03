#' Generate Operating Characteristics Plots with S3 Support
#'
#' Enhanced wrapper to generate OC plots that supports both legacy and S3 interfaces.
#'
#' @param ... Either simulation result objects (from \code{run_simulation()}) or
#'   legacy arguments (\code{`3+3`}, \code{BOIN}, \code{mTPI2}, \code{i3+3}, \code{G3}).
#'
#' @return A list of plots showing operating characteristics for the specified design(s).
#'
#' @export
#'
#' @examples
#' # New S3 interface
#' boin <- design_boin(pT = 0.25, EI = c(0.15, 0.35), ncohort = 10)
#' i3 <- design_i3plus3(pT = 0.25, EI = c(0.2, 0.3), ncohort = 10)
#'
#' p.true <- c(0.05, 0.10, 0.20, 0.30, 0.45)
#' mtd.true <- c(0, 0, 1, 0, 0)  # Dose 3 is the true MTD
#'
#' \donttest{
#' # Run simulations
#' sim_boin <- run_simulation(boin, p.true = p.true, mtd.true = mtd.true)
#' sim_i3 <- run_simulation(i3, p.true = p.true, mtd.true = mtd.true)
#'
#' # Generate OC plots
#' oc_plot(sim_boin, sim_i3)
#' }
#'
oc_plot <- function(...) {
  args <- list(...)

  # Check if using unnamed arguments (could be simulation results)
  if (length(args) > 0 && is.null(names(args))) {
    # Check if all are lists with selection/allocation components
    has_components <- sapply(args, function(x) {
      is.list(x) && all(c("selection", "allocation", "setup") %in% names(x))
    })

    if (all(has_components)) {
      # These are simulation results
      # Extract method names and create named list
      method_names <- sapply(args, function(x) x$setup$method)
      names(args) <- method_names
      do.call(generate_oc_plot, args)
    } else {
      stop("Invalid arguments. Provide simulation results from run_simulation(), ",
           "or use named arguments for the legacy interface.")
    }

  } else if (!is.null(names(args))) {
    # Legacy interface with named arguments
    do.call(generate_oc_plot, args)

  } else {
    stop("Invalid arguments. Either provide simulation results or use named arguments ",
         "(e.g., BOIN = ..., `i3+3` = ...)")
  }
}
