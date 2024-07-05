#'
#' Simulations for the i3+3 design.
#'
#' Conduct computer simulations for the i3+3 design.
#'
#' @usage run.sim.i3(p.true,
#'                   mtd.true,
#'                   pT,
#'                   EI,
#'                   ncohort,
#'                   cohortsize = 3,
#'                   startdose = 1,
#'                   DU.pp = 0.95,
#'                   n.earlystop = 100,
#'                   extrasafe = FALSE,
#'                   ntrial = 1000,
#'                   seed = 6)
#'
#' @param p.true a vector or matrix containing the true toxicity probabilities of the investigational dose levels.
#' @param mtd.true a numeric value or a vector which specifies the true MTD.
#' @param pT a numeric value; the target DLT rate.
#' @param EI a vector which specifies the equivalence interval (EI).
#' @param ncohort a numeric value; the total number of cohorts.
#' @param cohortsize a numeric value; the cohort size.
#' @param startdose a numeric value; the starting dose level for the trial.
#' @param DU.pp a numeric value; the cutoff to remove an overly toxic dose for safety.
#'              We recommend the default value of (\code{DU.pp=0.95}) for general use.
#' @param n.earlystop a numeric value; the early stopping parameter. If the number of patients
#'                    treated at the current dose reaches \code{n.earlystop},
#'                    stop the trial and select the MTD based on the observed data.
#'                    The default value \code{n.earlystop=100} essentially turns
#'                    off this type of early stopping.
#' @param extrasafe a logical value which specifies whether to implement a more strict safety rule (see more in the Details).
#' @param ntrial a numeric value; the total number of simulated trials.
#' @param seed a numeric value; the random seed for simulation.
#'
#' @details  Denote the current dose \eqn{d}. Let \eqn{n_d} and \eqn{y_d} represent the
#'           number of patients treated at dose \eqn{d} and the number of patients
#'           experienced DLT, respectively. Let \eqn{p_d} be the toxicity probability at
#'           dose \eqn{d}. Also, denote \eqn{\frac{y_d}{n_d}} the observed toxicity rate
#'           at the current dose.
#'
#'           The i3+3 design uses the following decision rules. If
#'           \eqn{\frac{y_d}{n_d}} is lower than the escalation boundary (i.e. below
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
#'           The i3+3 design has two early stopping rules: (1) stop the trial if the lowest
#'          dose is eliminated due to toxicity, and no dose should be selected as the MTD; and
#'          (2) stop the trial and select the MTD if the number of patients treated at the current
#'          dose reaches \code{n.earlystop}.
#'
#'          For some applications, investigators may prefer a more strict safety rule for MTD selection
#'          (This can be achieved by setting \code{extrasafe} == T). If the isotonically-transformed
#'          posterior mean of the selected MTD is above the EI, select the next lower dose as the final MTD.
#'
#' @return \code{run.sim.i3()} returns:
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
#'        the number of patients treated at each dose level,
#'        the average number of patients treated,
#'        the percentage of overdosing assignment (POA),
#'        the percentage of correct assignment (PCA),
#'        the percentage of underdosing assignment (PUA),
#'        the numbered index for the true MTD, respectively.
#'
#' (3) a list (\code{$setup}) containing user input parameters, such as target, EI, npatients, etc.
#'
#' @references Liu M., Wang, SJ. and Ji, Y. (2020). The i3+3 Design for Phase I Clinical Trials, \emph{Journal of
#' biopharmaceutical statistics}, 30(2):294–304.
#'
#' @examples
#' run.sim.i3(p.true = c(0.25, 0.41, 0.45, 0.49, 0.53),
#'            mtd.true = c(1,0,0,0,0),
#'            pT = 0.25,
#'            EI = c(0.2,0.3),
#'            ncohort = 9,
#'            cohortsize = 3,
#'            startdose = 1,
#'            DU.pp = 0.95,
#'            n.earlystop = 100,
#'            extrasafe = FALSE,
#'            ntrial = 1000,
#'            seed = 6)
#'
#' @export
#'
run.sim.i3 = function (p.true,
                        mtd.true,
                        pT,
                        EI,
                        ncohort,
                        cohortsize = 3,
                        startdose = 1,
                        DU.pp = 0.95,
                        n.earlystop = 100,
                        extrasafe = FALSE,
                        ntrial = 1000,
                        seed = 6)
{
  # check inputs
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
  if (n.earlystop <= 6) {
    warning("Warnings: the value of n.earlystop is too low to ensure good operating characteristics. Recommend n.earlystop = 9 to 18.")
  }

  # Initialize matrix for storage
  select.perc = matrix(rep(0, ndose * nscene), ncol = ndose)
  stop.perc = rep(0, nscene)
  nptsdose = matrix(rep(0, ndose * nscene), ncol = ndose)
  npts = rep(0, nscene)

  ################################# Step 1 #################################
  for (j in 1:nscene){

    set.seed(seed)

    p = p.true[j,]

    # store Y and N
    Y = matrix(rep(0, ndose * ntrial), ncol = ndose)
    N = matrix(rep(0, ndose * ntrial), ncol = ndose)

    # store MTD
    MTD = rep(0, ntrial)

    ############################## Step 2 #################################
    for (trial in 1:ntrial) {

      y = rep(0, ndose)
      n = rep(0, ndose)

      d = startdose

      stop = 0
      earlystop = 0

      elimdose = ndose+1
      selected = 0

        while (stop == 0){

          ### Generate random toxicity response
          xx = 0
          for(i in 1:cohortsize){
            ttt = runif(1)
            if(ttt < p[d])
              xx = xx + 1
          }

          y[d] = y[d] + xx;
          n[d] = n[d] + cohortsize

          ### Determine if the next dose is too toxic
          if(d < ndose){
            if(1 - pbeta(pT, 1 + y[d+1], 1 + n[d+1] - y[d+1]) > DU.pp) {
              elimdose = d+1
            }
          }

          ###### Early stop rule if n reaches xx ######
          if(n[d]>=n.earlystop &&
             (
               (y[d]/n[d] >= EI[1] && y[d]/n[d] <= EI[2]) ||
               (y[d]/n[d] > EI[2] && (y[d]-1)/n[d] < EI[1]) ||
               (d==1 && y[d]/n[d] > EI[2] && (y[d]-1)/n[d] >= EI[1]) ||
               (d==ndose && y[d]/n[d] < EI[1]) ||
               (d+1==elimdose && y[d]/n[d] < EI[1])
             )  ) {
            stop = 1
          }

          ########## if lowest dose ##########
          if (d == 1){
            ## if the first dose is too toxic, the trial will be terminated
            if(( 1 - pbeta(pT, 1 + y[d], 1 + n[d] - y[d])) > DU.pp){
              earlystop = 1
              stop = 1
            }
            else{
              if(y[d]/n[d] < EI[1]){
                decision = "E"
                d = min(d+1, elimdose-1)
              }

              else if( y[d]/n[d] >= EI[1] & y[d]/n[d] <= EI[2]){
                decision = "S"
                d = d
              }

              else if((y[d]-1)/n[d]< EI[1]){
                decision = "S"
                d = d
              }

              else{
                decision = "S"
                d = d
              }
            }
          }

          ########## if highest dose ##########
          else if (d == ndose) {

            if(y[d]/n[d] < EI[1]){
              decision = "S"
              d = d
            }
            else if( y[d]/n[d] >= EI[1] & y[d]/n[d] <= EI[2]){
                decision = "S"
                d = d
            }
            else if( (y[d]-1)/n[d] < EI[1]){
                decision = "S"
                d = d
              }
            else{
              decision = "D"
              d = d-1
            }
          }
          ########## if in middle dose ##########
          else {
            if((d > 1) && (d < ndose)){
              if( y[d]/n[d] < EI[1]){
                decision = "E"
                d = min(d+1, elimdose-1)
              }
              else if(y[d]/n[d] >= EI[1] & y[d]/n[d] <= EI[2]){
                  decision = "S"
                  d = d
              }
              else if( (y[d]-1)/n[d] < EI[1]){
                  decision = "S"
                  d = d
              }
              else{
                decision = "D"
                d = d-1
              }

            }
          }

          total = sum(n)
          if(total >= ncohort*cohortsize){
            stop = 1
          }
        }

      ################################# Step 3 #################################
      ## Select MTD
      if (earlystop == 1) {
        MTD[trial] = 99
      }
      else {
        MTD[trial] = select.mtd(method = "i3+3",
                                pT = pT,
                                EI = EI,
                                n_obs = n,
                                y_obs = y,
                                DU.pp = DU.pp,
                                extrasafe = extrasafe)$d_selected
      }

      Y[trial, ] = y
      N[trial, ] = n
    ##########################################################################################
  }

  ################################# Step 4 #################################

  nptsdose[j, ] = colMeans(N) #average number of patients
  #nptsdose.perc = colMeans(N)/sum(colMeans(N))
  npts[j] = mean(rowSums(N)) #average total number of patients

  for (i in 1:ndose) {
    select.perc[j, i] = sum(MTD == i)/ntrial
  }
  stop.perc[j] = sum(MTD == 99)/ntrial

  }

  setup = list(method = "i3+3",
                startdose = startdose,
                cohortsize =  cohortsize,
                ncohort =  ncohort,
                pT = ifelse(!is.null( pT),
                             pT,
                            NA),
                EI = c(ifelse(!is.null( EI[1]),
                               EI[1],
                              NA),
                       ifelse(!is.null( EI[2]),
                               EI[2],
                              NA)),
                boundary = c(ifelse(!is.null( EI[1]),
                                     EI[1],
                                    NA),
                             ifelse(!is.null( EI[2]),
                                     EI[2],
                                    NA)),
                DU.pp =  DU.pp,
                extrasafe =  extrasafe,
                n.earlystop =  n.earlystop)

  out = list(selection = summarize.metric(select.perc = select.perc,
                                       stop.perc = stop.perc,
                                       nptsdose = nptsdose,
                                       npts = npts,
                                       mtd.true = mtd.true)$sel,
             allocation = summarize.metric(select.perc = select.perc,
                                     stop.perc = stop.perc,
                                     nptsdose = nptsdose,
                                     npts = npts,
                                     mtd.true = mtd.true)$alo,
             setup = setup
  )

  return(out)
}
