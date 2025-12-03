#'
#' Dosing decision for the BOIN design
#'
#' Generate dosing decisions (E, S, D or DU) of the BOIN design for user-specified number of participants.
#'
#' @usage get_decision_boin(pT,
#'                          EI,
#'                          npts)
#'
#' @param pT a numeric value that specifies the target DLT rate (\eqn{p_T}).
#' @param EI a vector that specifies the equivalence interval (EI).
#' @param npts the number of participants within which dosing decisions are generated.
#'
#' @details Denote the current dose \eqn{d}. Let \eqn{n_d} and \eqn{y_d} represent the
#'           number of participants treated at dose \eqn{d} and the number of participants
#'           experienced DLT, respectively. Let \eqn{p_d} be the toxicity probability at
#'           dose \eqn{d}. Also, denote \eqn{\frac{y_d}{n_d}} the observed toxicity rate
#'           at the current dose.
#'
#'           The BOIN design uses the following decision rules: if
#'           \eqn{\frac{y_d}{n_d}} is lower than or equal to the escalation boundary,
#'           the decision is to escalate to the next higher dose; if \eqn{\frac{y_d}{n_d}}
#'           is higher than the de-escalation boundary, the decision is to the next lower
#'           dose; otherwise, the decision is to stay at the current dose.
#'
#'           Also, the BOIN design includes a dose exclusion rule. Let \eqn{p_T} represents
#'           the target DLT rate. If \eqn{Pr(p_d > p_T | y_d , n_d ) > 0.95}, dose \eqn{d}
#'           and those higher than \eqn{d} are removed from the trial since they are deemed
#'           excessively toxic.
#'
#' @return \code{get_decision_boin()} returns:
#'
#' (1) a dataframe containing the decisions (E, S, D or DU) for each combination of y and n (\code{$tab}),
#'
#' (2) a list (\code{$setup}) containing user input parameters, such as target, EI, npts, etc.
#'
#' @references Liu S. and Yuan, Y. (2015). Bayesian Optimal Interval Designs for Phase I Clinical Trials, \emph{Journal of the Royal Statistical Society: Series C}, 64, 507-523.
#'
#' @examples
#'
#' get_decision_boin(pT = 0.25,
#'                   EI = c(0.15,0.35),
#'                   npts = 12)
#' @export

get_decision_boin <- function(pT = NULL,
                              EI = NULL,
                              npts = 12){
  # Check inputs
  if (is.null(pT)) {
    stop("Target toxicity rate 'pT' must be specified")
  }
  if (is.null(EI)) {
    stop("Equivalence interval 'EI' must be specified")
  }
  if (!is.numeric(pT) || length(pT) != 1) {
    stop("'pT' must be a single numeric value")
  }
  if (!is.numeric(EI) || length(EI) != 2) {
    stop("'EI' must be a numeric vector of length 2")
  }
  if (npts < 3){
    stop("Number of participants 'npts' must be at least 3")
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

  lambda1 = log((1 - EI[1])/(1 - pT))/log(pT * (1 - EI[1])/(EI[1] * (1 - pT)))
  lambda2 = log((1 - pT)/(1 - EI[2]))/log(EI[2] * (1 - pT)/(pT * (1 - EI[2])))

  #Enumerate all possible y given n
  tab <- data.frame("n" = rep(3:npts, times = 4:(npts+1)),
                    "y" = NA,
                    "Decision" = NA)

  for (n in 3:npts){
    store <- c()

    for (y in 0:n){

      elim_p <- pbeta(pT, y + 1, n - y + 1, lower.tail = F)

      if (elim_p > 0.95){
        store <- c(store, "DU")
      }
      else{
        p_hat <- y/n

        if (p_hat <= lambda1){
          store <- c(store, "E")
        }
        else if (p_hat > lambda2){
          store <- c(store, "D")
        }
        else{
          store <- c(store, "S")
        }}}

    tab[tab$n == n,"y"] <- 0:n
    tab[tab$n == n,"Decision"] <- store
  }

  tab$condition <- " "
  tab$index <- 1
  tab$method <- "BOIN"

  ################# User's setup #################
  setup <- list(method = "BOIN",
                npts = npts,
                pT = pT,
                EI = paste0("[", EI[1], ", ", EI[2],")"),
                boundary = paste0(round(lambda1,3), ", ", round(lambda2,3)))

  return(list("tab" = tab,
              "setup" = setup))

}
