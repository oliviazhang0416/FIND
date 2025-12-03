#' Generate Decision Tables with S3 Support
#'
#' Enhanced wrapper to generate decision tables that supports both legacy and S3 interfaces.
#'
#' @param ... Either design objects (created with \code{design_*} functions) or
#'   legacy arguments (\code{`3+3`}, \code{BOIN}, \code{mTPI2}, \code{i3+3}, \code{G3}).
#'
#' @return A plot showing decision table(s) for the specified design(s).
#'
#' @export
#'
#' @examples
#' # New S3 interface
#' boin <- design_boin(pT = 0.25, EI = c(0.15, 0.35), npts = 12)
#' i3 <- design_i3plus3(pT = 0.25, EI = c(0.2, 0.3), npts = 12)
#'
#' \donttest{
#' # Generate decision tables
#' decision_table(boin, i3)
#' }
#'
#' # Legacy interface still works
#' \donttest{
#' decision_table(
#'   BOIN = get_decision_boin(pT = 0.25, EI = c(0.15, 0.35), npts = 12),
#'   `i3+3` = get_decision_i3plus3(pT = 0.25, EI = c(0.2, 0.3), npts = 12)
#' )
#' }
#'
decision_table <- function(...) {
  args <- list(...)

  # Check if using S3 interface (unnamed design objects)
  if (length(args) > 0 && is.null(names(args))) {
    # New S3 interface
    designs <- args

    # Validate all are design objects
    is_design <- sapply(designs, inherits, "phase1_design")
    if (!all(is_design)) {
      stop("All arguments must be design objects created with design_*() functions, ",
           "or use named arguments for the legacy interface.")
    }

    # Generate decisions for each design
    decisions <- lapply(designs, get_decision)

    # Create named list for generate_decision_table
    method_names <- sapply(designs, function(x) x$method)
    names(decisions) <- method_names

    # Convert to legacy format and call original function
    legacy_args <- list()
    for (i in seq_along(decisions)) {
      method <- method_names[i]
      legacy_args[[method]] <- decisions[[i]]
    }

    do.call(generate_decision_table, legacy_args)

  } else if (!is.null(names(args))) {
    # Legacy interface with named arguments
    do.call(generate_decision_table, args)

  } else {
    stop("Invalid arguments. Either provide design objects or use named arguments ",
         "(e.g., BOIN = ..., `i3+3` = ...)")
  }
}
