#'
#' Dosing decision for the mTPI2 design
#'
#' Generate dosing decisions (E, S, D or DU) of the mTPI2 design for user-specified number of participants.
#'
#' @usage get_decision_mtpi2(pT,
#'                           EI,
#'                           npts)
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
#'           The mTPI2 design divides the probability of DLT into equal-width intervals:
#'           underdosing, target dosing, and overdosing. Utilizing a  Bayesian model, mTPI-2
#'           updates the posterior probability estimates of DLTs. If the interval which maximizes
#'           the posterior probability is among the underdosing intervals, the decision is to
#'           escalate to the next higher dose; if the interval which maximizes the posterior
#'           probability is the target dosing interval, the decision is to stay at the current dose;
#'           if the interval which maximizes the posterior probability is among the overdosing intervals,
#'           the decision is to to de-escalate to the next lower dose.
#'
#'           Also, the mTPI2 design includes a dose exclusion rule. Let \eqn{p_T} represents
#'           the target DLT rate. If \eqn{Pr(p_d > p_T | y_d , n_d ) > 0.95}, dose \eqn{d}
#'           and those higher than \eqn{d} are removed from the trial since they are deemed
#'           excessively toxic.
#'
#' @return \code{get_decision_mtpi2()} returns:
#'
#' (1) a dataframe containing the decisions (E, S, D or DU) for each combination of y and n (\code{$tab}),
#'
#' (2) a list (\code{$setup}) containing user input parameters, such as target, EI, npts, etc.
#'
#' @references Guo, W., Liu, S., & Yin, G. (2017). A more efficient Bayesian model for oncology dose-finding
#' trials with toxicity probability interval. Clinical Trials, 14(1), 16-26.
#'
#' @examples
#'
#' get_decision_mtpi2(pT = 0.25,
#'                    EI = c(0.2,0.3),
#'                    npts = 12)
#' @export

get_decision_mtpi2 <- function(pT = NULL,
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

  calculate.pp <- function(y, n, start, end) {(pbeta(end, 1 + y, 1 + n - y) - pbeta(start, 1 + y, 1 + n - y))/(end-start);}

  epsilon = EI[2] - EI[1]

  if (abs(EI[1] %% epsilon) < .Machine$double.eps^0.5){
    # Intervals for [0, a) with length epsilon
    LI <- cbind(seq(0, EI[1]-epsilon, by = epsilon),
                seq(epsilon, EI[1], by = epsilon))
  }
  else{
    # Intervals for [0, a) with length epsilon
    LI <- cbind(seq(EI[1]-ceiling(EI[1]/epsilon)*epsilon,  EI[1]-epsilon, by = epsilon),
                seq(EI[1]-ceiling(EI[1]/epsilon)*epsilon+epsilon,  EI[1], by = epsilon))
  }

  if (abs((1-EI[2]) %% epsilon) < .Machine$double.eps^0.5){
    # Intervals for (b, 1] with length epsilon
    HI <- cbind(seq(EI[2],  1-epsilon, by = epsilon),
                seq(EI[2]+epsilon,  1, by = epsilon))
  }
  else{
    # Intervals for (b, 1] with length epsilon
    HI <- cbind(seq(EI[2],  EI[2]+ceiling((1-EI[2])/epsilon)*epsilon-epsilon, by = epsilon),
                seq(EI[2]+epsilon, EI[2]+ceiling((1-EI[2])/epsilon)*epsilon, by = epsilon))
  }

  intervals <- as.data.frame(rbind(LI,
                                   c(EI[1], EI[2]),
                                   HI))
  colnames(intervals) <- c("start", "end")

  # Modify endpoints
  intervals[1,"start"] <- 0
  intervals[nrow(intervals),"end"] <- 1
  intervals <- round(intervals,3)

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
        # Apply the pp function to each row and store the results in a vector
        pp.values <- mapply(function(start, end) calculate.pp(y, n, start, end),
                            intervals$start, intervals$end)

        # Find the midpoint of the maximum pp value
        midpoint <- (intervals[which.max(pp.values), "end"] + intervals[which.max(pp.values), "start"])/2

        if (midpoint < EI[1]){
          store <- c(store, "E")
        }
        else if (midpoint > EI[2]){
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
  tab$method <- "mTPI2"

  ################# User's setup #################
  setup <- list(method = "mTPI2",
                npts = npts,
                pT = pT,
                EI = paste0("[", EI[1], ", ", EI[2],"]"),
                boundary = "Not applicable")

  return(list("tab" = tab,
              "setup" = setup))

}
