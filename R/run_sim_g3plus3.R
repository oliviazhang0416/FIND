#'
#' Simulations for the G3 design.
#'
#' Conduct computer simulations for the G3 design.
#'
#' @usage run_sim_g3plus3(p.true,
#'                   mtd.true,
#'                   ncohort,
#'                   cohortsize = 3,
#'                   startdose = 1,
#'                   n.earlystop = 100,
#'                   ntrial = 1000,
#'                   seed = 6)
#'
#' @param p.true a vector or matrix containing the true toxicity probabilities of the investigational dose levels.
#' @param mtd.true a numeric value or a vector which specifies the true MTD.
#' @param ncohort a numeric value; the total number of cohorts.
#' @param cohortsize a numeric value; the cohort size.
#' @param startdose a numeric value; the starting dose level for the trial.
#' @param n.earlystop a numeric value; the early stopping parameter. If the number of participants
#'                    treated at the current dose reaches \code{n.earlystop},
#'                    stop the trial and select the MTD based on the observed data.
#'                    The default value \code{n.earlystop=100} essentially turns
#'                    off this type of early stopping.
#' @param ntrial a numeric value; the total number of simulated trials.
#' @param seed a numeric value; the random seed for simulation.
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
#'           dose \eqn{d} and those higher than \eqn{d} are removed from the trial since
#'           they are deemed excessively toxic.
#'
#'           Three rules (1)-(3) with which G3 selects the final MTD after the trial is completed.
#'           (1) After removing doses based on the dose exclusion rule in (b), at the end of the
#'           trial, G3 selects the highest tested dose for which the decision is to de-escalate.
#'           (2) If no tested doses have a decision to de-escalate (meaning their decisions are
#'           either escalate or stay), G3 selects the highest tested dose as the MTD. (3) If the
#'           lowest dose has a decision to de-escalate, no dose is selected as the MTD.
#'
#'           The G3 design has two early stopping rules: (1) stop the trial if the lowest
#'          dose is eliminated due to toxicity, and no dose should be selected as the MTD; and
#'          (2) stop the trial and select the MTD if the number of participants treated at the current
#'          dose reaches \code{n.earlystop}.
#'
#'
#' @return \code{run_sim_g3plus3()} returns:
#'
#' (1) a dataframe (\code{$selection}) with each column showing:
#'        the numbered index for each scenarios specified,
#'        the name of the design,
#'        the selection percentage at each dose level,
#'        the percentage of early stopping without selecting the MTD,
#'        the percentage of overdosing selection (POS),
#'        the percentage of correct selection (PCS),
#'        the percentage of underdosing selection (PUS),
#'        the numbered index for the true MTD, respectively.
#'
#' (2) a dataframe (\code{$allocation}) with each column showing:
#'        the numbered index for each scenarios specified,
#'        the name of the design,
#'        the number of participants treated at each dose level,
#'        the average number of participants treated,
#'        the percentage of overdosing assignment (POA),
#'        the percentage of correct assignment (PCA),
#'        the percentage of underdosing assignment (PUA),
#'        the numbered index for the true MTD, respectively.
#'
#' (3) a list (\code{$setup}) containing user input parameters.
#'
#' @references To be added
#'
#' @examples
#' run_sim_g3plus3(p.true = c(0.25, 0.41, 0.45, 0.49, 0.53),
#'            mtd.true = c(1,0,0,0,0),
#'            ncohort = 9,
#'            cohortsize = 3,
#'            startdose = 1,
#'            n.earlystop = 100,
#'            ntrial = 1000,
#'            seed = 6)
#'
#' @export
#'
run_sim_g3plus3 = function (p.true,
                       mtd.true,
                       ncohort,
                       cohortsize = 3,
                       startdose = 1,
                       n.earlystop = 100,
                       ntrial = 1000,
                       seed = 6)
{
  ############################## Check inputs ##############################
  if (is.matrix(p.true) & is.matrix(mtd.true)){
    if (nrow(p.true) == nrow(mtd.true) & ncol(p.true) == ncol(mtd.true)){
      # number of scenarios
      nscene = nrow(p.true)
      ndose = ncol(p.true)
    }
    else{
      stop("Warnings: please make sure the p.true and mtd.true have the same dimension!")
    }
  }
  else if (is.vector(p.true) & is.vector(mtd.true)){
    if (length(p.true) == length(mtd.true)){
      nscene = 1
      ndose = length(p.true)
      p.true = matrix(p.true, nrow = 1)
      mtd.true = matrix(mtd.true, nrow = 1)
    }
    else{
      stop("Warnings: please make sure the p.true and mtd.true have the same dimension!")
    }
  }
  else{
    stop("Warnings: please make sure the p.true and mtd.true have the same dimension!")
  }
  if (n.earlystop <= 6) {
    warning("Warnings: the value of n.earlystop is too low to ensure good operating characteristics. Recommend n.earlystop = 9 to 18.")
  }

  ################################# Step 1 #################################
  ###################### Safety rule (dose exclusion) ######################
  pT = 0.25
  DU.pp = 0.95

  b.DU = c()
  for (n in 1:(ncohort * cohortsize)) {
    if (n < 3) {
      b.DU = c(b.DU, NA)
    }
    else {
      for (n in 1:n) {
        # Vectorized pbeta calculation for all y values within the current n
        y_ <- 0:n
        p_ <- 1 - pbeta(pT, y_ + 1, n - y_ + 1)

        # Find the first y that meets the condition, or NA if none do
        y_DU <- y_[which(p_ > DU.pp)[1]]
        b.DU[n] <- if (!is.na(y_DU)) {y_DU} else {NA}
      }
    }
  }

  ############################## Step 2 #################################
  ################### Enumerate through scenarios #######################

  # Initialize matrix for storage
  select.perc = matrix(rep(0, ndose * nscene), ncol = ndose)
  stop.perc = rep(0, nscene)
  nptsdose = matrix(rep(0, ndose * nscene), ncol = ndose)
  npts = rep(0, nscene)

  for (j in 1:nscene){

    set.seed(seed)

    p = p.true[j,]

    # store Y and N
    Y = matrix(rep(0, ndose * ntrial), ncol = ndose)
    N = matrix(rep(0, ndose * ntrial), ncol = ndose)

    # store MTD
    MTD = rep(0, ntrial)

    ################### Trial simulation starts #######################
    for (trial in 1:ntrial) {

      y = rep(0, ndose)
      n = rep(0, ndose)
      decision = rep(" ", ndose)

      d = startdose
      earlystop = 0
      elimi = rep(0, ndose)

      for (c in 1:ncohort) {

        ### Generate random toxicity response ###
        DLT = runif(cohortsize) < p[d];
        y[d] = y[d] + sum(DLT);
        n[d] = n[d] + cohortsize

        ### Set default EIs ###
        if (n[d] == 3){
          a <- 0.2
          b <- 1/3
        }
        else{
          a <- 0.2
          b <- 0.29
        }

        ###### DU current and higher doses ######
        if (!is.na(b.DU[n[d]])) {
          if (y[d] >= b.DU[n[d]]) {

            elimi[d:ndose] = 1

            if (d == 1) {
              earlystop = 1
              break;
            }
          }}

          ###### Early stop rule if n reaches xx ######
          # OLD LOGIC: stop when current decision is "S" and n[d] >= n.earlystop
          # if(n[d]>=n.earlystop &&
          #    (
          #      (y[d]/n[d] >= a && y[d]/n[d] <= b) ||
          #      (d==1 && y[d]/n[d] > b)||
          #      ((d==ndose || elimi[d+1]==1) && y[d]/n[d] < a)
          #    )  ) break;

          ########## if lowest dose ##########
          if (d == 1){
            if(y[d]/n[d] < a){
              if (elimi[d + 1] == 0){
                decision[d] = "E"
                d = d+1
                }
              else{
                decision[d] = "S"
                d = d
                }
              }
            else if( y[d]/n[d] >= a & y[d]/n[d] <= b){
              decision[d] = "S"
              d = d
              }
            else if (y[d]/n[d] > b){
              decision[d] = "S"
              d = d
              }
          }
          ########## if highest dose ##########
          else if (d == ndose) {
            if(y[d]/n[d] < a){
              decision[d] = "S"
              d = d
            }
            else if( y[d]/n[d] >= a & y[d]/n[d] <= b){
              decision[d] = "S"
              d = d
            }
            else if ( y[d]/n[d] > b){
              decision[d] = "D"
              d = d-1
            }
          }
          ########## if in middle dose ##########
          else if((d > 1) && (d < ndose)){
            if( y[d]/n[d] < a){
              if (elimi[d + 1] == 0){
                decision[d] = "E"
                d = d+1
                }
              else{
                d = d
                decision[d] = "S"
                }
              }
            else if(y[d]/n[d] >= a & y[d]/n[d] <= b){
              decision[d] = "S"
              d = d
              }
            else if( y[d]/n[d] > b){
              decision[d] = "D"
              d = d-1
            }
            }

          ###### NEW Early stop rule: after decision, if n at next dose reaches n.earlystop ######
          if(n[d] >= n.earlystop) break;
        }

      ################################# Step 3 #################################
      ############################### Select MTD ###############################
      if (earlystop == 1) {
        MTD[trial] = 99
      }
      else {
        DU_index <- ifelse(length(which(elimi == 1)) == 0,
                           99,
                           which(elimi == 1)[1])

        # If there is D among the tested doses
        if (length(which(decision=="D")) > 0){
          if (min(which(decision=="D")) == 1){
            MTD[trial] <- 99
          }
          else {
            MTD[trial] <- min(which(decision=="D"), DU_index)-1
          }
        }
        # If all doses are tested and no D
        else if ((length(which(decision=="D")) == 0) & (length(which(decision==" ")) == 0)){
          # Select the highest tested dose
          MTD[trial] <- min(ndose, DU_index-1)
        }
        # If not all are tested and no D
        else if ((length(which(decision=="D")) == 0) & (length(which(decision==" ")) > 0)){
          # Select the highest tested dose
          MTD[trial] <- min(which(decision==" ")-1, DU_index-1)
        }
      }

      Y[trial, ] = y
      N[trial, ] = n
    }

    ################################# Step 4 #################################
    ########################### Summarize metrics ############################
    nptsdose[j, ] = colMeans(N) #average number of participants
    #nptsdose.perc = colMeans(N)/sum(colMeans(N))
    npts[j] = mean(rowSums(N)) #average total number of participants

    for (i in 1:ndose) {
      select.perc[j, i] = sum(MTD == i)/ntrial
    }
    stop.perc[j] = sum(MTD == 99)/ntrial
    }

    setup = list(method = "G3",
                  startdose = startdose,
                  cohortsize =  cohortsize,
                  ncohort =  ncohort,
                  pT = "0.25",
                  EI = c("[0.2, 1/3]\n[0.2, 0.29]"),
                  boundary = c("0.2, 1/3\n0.2, 0.29"),
                  DU.pp =  "0.95",
                  n.earlystop =  n.earlystop)

    out = list(selection = summarize_metric(select.perc = select.perc,
                                            stop.perc = stop.perc,
                                            nptsdose = nptsdose,
                                            npts = npts,
                                            mtd.true = mtd.true)$sel,
               allocation = summarize_metric(select.perc = select.perc,
                                             stop.perc = stop.perc,
                                             nptsdose = nptsdose,
                                             npts = npts,
                                             mtd.true = mtd.true)$alo,
               setup = setup
    )

  return(out)
}
