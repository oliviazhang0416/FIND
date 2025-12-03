#'
#' Dosing decision for the G3 design
#'
#' Generate dosing decisions (E, S, D or DU) of the G3 design for user-specified number of participants.
#'
#' @usage get_decision_g3plus3(npts = 12)
#'
#' @param npts the number of participants within which dosing decisions are generated.
#'
#' @details  Denote the current dose \eqn{d}. Let \eqn{n_d} and \eqn{y_d} represent the
#'           number of participants treated at dose \eqn{d} and the number of participants
#'           experienced DLT, respectively. Let \eqn{p_d} be the toxicity probability at
#'           dose \eqn{d}. Also, denote \eqn{\frac{y_d}{n_d}} the observed toxicity rate
#'           at the current dose.
#'
#'           The G3 design uses the following decision rules. For \eqn{n} is three or six,
#'           we simply apply the 3+3 rules; otherwise, we use the following decision rules,
#'           with EI being [0.2, 0.29]. If \eqn{\frac{y_d}{n_d}} is lower than the escalation
#'           boundary (i.e. below the EI), the decision is to escalate to the next higher dose;
#'           if \eqn{\frac{y_d}{n_d}} is between the escalation and de-escalation boundaries
#'           (between the EI), the decision is to stay at the current dose; if
#'           \eqn{\frac{y_d}{n_d}} is higher than the de-escalation boundary (i.e. above
#'           the EI), the decision is to de-escalate to the next lower dose.
#'
#'           Also, the G3 design includes a dose exclusion rule. If \eqn{Pr(p_d > 0.25 | y_d , n_d ) > 0.95},
#'           dose \eqn{d} and those higher than \eqn{d} are removed from the trial since they
#'           are deemed excessively toxic.
#'
#' @return \code{get_decision_g3plus3()} returns:
#'
#' (1) a dataframe containing the decisions (E, S, D or DU) for each combination of y and n (\code{$tab}),
#'
#' (2) a list (\code{$setup}) containing user input parameters, such as npts.
#'
#' @references To be added.
#'
#' @examples
#'
#' get_decision_g3plus3(npts = 12)
#'
#' @export
#'
get_decision_g3plus3 <- function(npts = 12){

  # Check inputs
  if (!is.numeric(npts) || length(npts) != 1) {
    stop("'npts' must be a single numeric value")
  }
  if (npts < 3){
    stop("Number of participants 'npts' must be at least 3")
  }

  pT <- 0.25

  #Enumerate all possible y given n
  tab <- data.frame("n" = rep(3:npts,
                              times = 4:(npts+1)),
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
        if (n == 3){
          a <- 0.2
          b <- 1/3
        }
        else{
          a <- 0.2
          b <- 0.29
        }

        p_hat <- y/n

        if (p_hat < a){

          store <- c(store, "E")
        }
        else if (p_hat >= a & p_hat <= b){
          store <- c(store, "S")
        }
        else{
          store <- c(store, "D")
        }
      }
    }
    tab[tab$n == n,"y"] <- 0:n
    tab[tab$n == n,"Decision"] <- store
  }

  tab$condition <- " "
  tab$index <- 1
  tab$method <- "G3"

  ################# User's setup #################
  setup <- list(method = "G3",
                npts = npts,
                pT = pT,
                EI = c("[0.2, 1/3]\n[0.2, 0.29]"),
                boundary = c("0.2, 1/3\n0.2, 0.29"))

  return(list("tab" = tab,
              "setup" = setup))

}
