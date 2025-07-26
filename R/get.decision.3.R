#'
#' Dosing decision for the 3+3 design
#'
#' Generate dosing decisions (E, S, D or DU) of the 3+3 design for user-specified number of participants.
#'
#' @usage get.decision.3(npts = 12)
#'
#' @param npts the number of participants within which dosing decisions are generated.
#'
#' @details The 3+3 design uses the following decision rules.
#'
#' (1). Start trial by treating three participants at the initial dose.
#'
#' (2). Denote the dose level being used to treat participants as the current dose level.
#'      Treat three participants at the current dose level.
#'
#' 2a. If the maximum number of participants has been accrued, stop the trial. The MTD is inconclusive.
#'
#' (3). Check the number of participants at the current dose level.
#'
#' 3a. If there are three participants, go to (4).
#'
#' 3b. If there are six participants, go to (5).
#'
#' (4). Check the number of toxicities (among three participants) at the current dose level.
#'
#' 4a. If there are zero toxicities, escalate and go to (7).
#'
#' 4b. If there is one toxicity, stay at the current dose and go to (2).
#'
#' 4c. If there are two or three toxicities, declare that the MTD has been exceeded and go to (6).
#'
#' (5). Check the number of toxicities (among six participants) at the current dose level.
#'
#' 5a. If there are zero toxicities, stop the trial and declare that the MTD is the current dose.
#'
#' 5b. If there is one toxicity, and the MTD has been exceeded, stop the trial and declare that
#' the MTD is the current dose; otherwise, go to (7).
#'
#' 5c. If there are two or more than two toxicities, declare that the MTD has been exceeded and go to (6).
#'
#' (6). The MTD has been exceeded.
#'
#' 6a. If the current dose is the lowest dose, stop the trial and declare that the MTD is lower than
#' the lowest dose level.
#'
#' 6b. If then next-lower dose level has six participants, stop the trial and declare that the MTD is the
#' next lower dose level; otherwise, the next lower dose level has three participants; set the current dose
#' level to be the next-lower dose level and go to (2).
#'
#' (7). Escalate if possible.
#'
#' 7a. If the current dose level is the highest dose level, stop the trial and declare that the MTD is
#' the highest dose level.
#'
#' 7b. Otherwise, escalate to the next higher dose level and go to (2).
#'
#' @return \code{get.decision.3()} returns:
#'
#' (1) a dataframe containing the decisions (E, S, D or DU) for each combination of y and n (\code{$tab}),
#'
#' (2) a list (\code{$setup}) containing user input parameters, such as npts.
#'
#' @references Storer B. (1989). Design and analysis of phase i clinical trials, \emph{Biometrics}, 925–937.
#'
#' Yang, S., Wang, S.J. and Ji, Y., (2015). An integrated dose-finding tool for phase I trials in oncology. \emph{Contemporary clinical trials}, 45, pp.426-434.
#'
#' @examples
#'
#' get.decision.3(npts = 12)
#'
#' @export
#'

get.decision.3 <- function(npts = 12){

  #Check inputs
  if (npts < 3){
    stop("Warnings: the number of participants should be greater or equal to 3.")
  }
    tab <- data.frame("n" = rep(3:npts,
                                times = 4:(npts+1)),
                      "y" = NA,
                      "Decision" = NA)

    #Enumerate all possible y given n
    vec <- c()
    for (i in 3:npts){
      vec <- c(vec, 0:i)
      }
    tab$y <- vec

    tab$index <- 1

    #3+3 only has decision for the following y and n:
    A <- (tab$n == 3 | tab$n == 6) & (tab$y == 2 | tab$y == 3 | tab$y == 4)
    B <- (tab$n == 6) & (tab$y == 1)

    #Several y and n has more than one decision:
    tmp <- tab[A | B,]
    tmp$index <- 2
    tab <- rbind(tab, tmp)


    A <- (tab$n == 3 | tab$n == 6) & (tab$y == 2 | tab$y == 3 | tab$y == 4)
    B <- (tab$n == 6) & (tab$y == 1)
    C <- (tab$n == 6) & (tab$y == 0)
    D <- (tab$n == 3) & (tab$y == 1)
    E <- (tab$n == 3) & (tab$y == 0)
    G <- (tab$n == 6) & (tab$y == 5 | tab$y == 6)

    #Add conditions for those y and n with more than one decision
    tab$condition <- ifelse(A & tab$index == 2, expression(scriptstyle(paste("if ",n[d-1]==6))),
                            ifelse(A & tab$index == 1, expression(scriptstyle(paste("if ",n[d-1]==3))),
                                   ifelse(B & tab$index == 2, expression(scriptstyle(paste("if ",n[d+1]>0))),
                                          ifelse(B & tab$index == 1, expression(scriptstyle(paste("if ",n[d+1]==0))),
                                           " "))))

    tab$condition <- factor(tab$condition, levels = c(expression(scriptstyle(paste("if ",n[d-1]==6))),
                                            expression(scriptstyle(paste("if ",n[d-1]==3))),
                                            expression(scriptstyle(paste("if ",n[d+1]==0))),
                                            expression(scriptstyle(paste("if ",n[d+1]>0))),
                                            " "))
    #Add decisions
    tab$Decision <- ifelse(A & tab$index == 2, "d-1",
                           ifelse(A & tab$index == 1, "DU",
                                  ifelse(B & tab$index == 2, "d",
                                         ifelse(B & tab$index == 1, "E",
                                                ifelse(C, "d",
                                                       ifelse(D, "S",
                                                              ifelse(E, "E",
                                                                     ifelse(G, "na",
                                                                            "na"))))))))
    tab$method <- "3+3"

    ################# User's setup #################
    setup <- list(method = "3+3",
                  npts = npts,
                  pT = NA,
                  EI = NA,
                  boundary = NA)

    return(list("tab" = tab,
                "setup" = setup))
  }
