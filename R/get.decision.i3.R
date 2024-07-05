#'
#' Dosing decision for the i3+3 design
#'
#' Generate dosing decisions (E, S, D or DU) of the i3+3 design for user-specified number of patients.
#'
#' @usage get.decision.i3(pT = NULL,
#'                        EI = NULL,
#'                        npatients = 12)
#'
#' @param pT a numeric value that specifies the target DLT rate (\eqn{p_T}).
#' @param EI a vector that specifies the equivalence interval (EI).
#' @param npatients the number of patients within which dosing decisions are generated.
#'
#' @details  Denote the current dose \eqn{d}. Let \eqn{n_d} and \eqn{y_d} represent the
#'           number of patients treated at dose \eqn{d} and the number of patients
#'           experienced DLT, respectively. Let \eqn{p_d} be the toxicity probability at
#'           dose \eqn{d}. Also, denote \eqn{\frac{y_d}{n_d}} the observed toxicity rate
#'           at the current dose.
#'
#'           The i3+3 design uses the following decision rules.
#'
#'           If \eqn{\frac{y_d}{n_d}} is lower than the escalation boundary (i.e. below
#'           the EI), the decision is to escalate to the next higher dose; if
#'           \eqn{\frac{y_d}{n_d}} is between the escalation and de-escalation boundaries
#'           (i.e. inside the EI), the decision is to stay at the current dose; if
#'           \eqn{\frac{y_d}{n_d}} is higher than the de-escalation boundary (i.e. above
#'           the EI), there are two options: option one, if \eqn{\frac{y_d-1}{n_d}} is
#'           lower than the escalation boundary (i.e., below the EI), the decision is
#'           to stay at the current dose; option two, else if \eqn{\frac{y_d-1}{n_d}}
#'           is equal to or higher than the escalation boundary, the decision is to
#'           de-escalate to the next lower dose.
#'
#'           Also, the i3+3 design includes a dose exclusion rule. Let \eqn{p_T} represents
#'           the target DLT rate. If \eqn{Pr(p_d > p_T | y_d , n_d ) > 0.95}, dose \eqn{d}
#'           and those higher than \eqn{d} are removed from the trial since they are deemed
#'           excessively toxic.
#'
#' @return \code{get.decision.i3()} returns:
#'
#' (1) a dataframe containing the decisions (E, S, D or DU) for each combination of y and n (\code{$tab}),
#'
#' (2) a list (\code{$setup}) containing user input parameters, such as target, EI, npatients, etc.
#'
#' @references Liu M., Wang, SJ. and Ji, Y. (2020). The i3+3 Design for Phase I Clinical Trials, \emph{Journal of
#' biopharmaceutical statistics}, 30(2):294–304.
#'
#' @examples
#'
#' get.decision.i3(pT = 0.25,
#'                 EI = c(0.2,0.3),
#'                 npatients = 12)
#' @export
#'
get.decision.i3 <- function(pT = NULL,
                            EI = NULL,
                            npatients = 12){

  #Check inputs
  if (npatients < 3){
    stop("Warnings: the number of patients should be greater or equal to 3.")
  }
  if (!is.null(pT)){
    if (pT < 0.05) {
      stop("Warnings: the pT is too low!")

    }
    if (pT > 0.6) {
      stop("Warnings: the pT is too high!")
    }
  }
  if (!is.null(EI)) {
    if ((pT - EI[1]) < (0.1 * pT)) {
      stop("Warnings: the upper bound for EI cannot be higher than or too close to the pT!")
    }
    if ((EI[2] - pT) < (0.1 * pT)) {
      stop("Warnings: the lower bound for EI cannot be lower than or too close to the pT!")
    }
  }

  #Enumerate all possible y given n
  tab <- data.frame("n" = rep(3:npatients,
                              times = 4:(npatients+1)),
                    "y" = NA,
                    "Decision" = NA)

  for (n in 3:npatients){
    store <- c()

    for (y in 0:n){

      elim_p <- pbeta(pT, y + 1, n - y + 1, lower.tail = F)

      if (elim_p > 0.95){
        store <- c(store, "DU")
      }
      else{
        p_hat <- y/n

        if (p_hat < EI[1]){

          store <- c(store, "E")
        }
        else {
          if (p_hat >= EI[1] & p_hat <= EI[2]){
          store <- c(store, "S")
          }
          else{

            p_hat_ <- (y-1)/n

            if (p_hat_ < EI[1]){
              store <- c(store, "S")
            }
            else{
              store <- c(store, "D")
            }
          }
        }
      }
    }
    tab[tab$n == n,"y"] <- 0:n
    tab[tab$n == n,"Decision"] <- store
  }

  tab$condition <- " "
  tab$index <- 1
  tab$method <- "i3+3"

  ################# User's setup #################
  setup <- list(method = "i3+3",
                npatients = npatients,
                pT = ifelse(!is.null(pT),
                            pT,
                            NA),
                EI = c(ifelse(!is.null(EI[1]),
                              EI[1],
                              NA),
                       ifelse(!is.null(EI[2]),
                              EI[2],
                              NA)),
                boundary = c(ifelse(!is.null(EI[1]),
                                    EI[1],
                                    NA),
                             ifelse(!is.null(EI[2]),
                                    EI[2],
                                    NA)))

  return(list("tab" = tab,
              "setup" = setup))

}
