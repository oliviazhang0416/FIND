#'
#' Simulations for the 3+3 design.
#'
#' Conduct computer simulations for the 3+3 design.
#'
#' @usage run.sim.3(p.true,
#'                  mtd.true,
#'                 startdose = 1,
#'                 ntrial = 1000,
#'                 seed = 6)
#'
#' @param p.true a vector or matrix containing the true toxicity probabilities of the investigational dose levels.
#' @param mtd.true a numeric value or a vector which specifies the true MTD.
#' @param startdose a numeric value; the starting dose level for the trial.
#' @param ntrial a numeric value; the total number of simulated trials.
#' @param seed a numeric value; the random seed for simulation.
#'
#' @details The 3+3 design uses the following decision rules.
#'
#' (1). Start trial by treating three patients at the initial dose.
#'
#' (2). Denote the dose level being used to treat patients as the current dose level.
#'      Treat three patients at the current dose level.
#'
#' (3). Check the number of patients at the current dose level.
#'
#' 3a. If there are three patients, go to (4).
#'
#' 3b. If there are six patients, go to (5).
#'
#' (4). Check the number of toxicities (among three patients) at the current dose level.
#'
#' 4a. If there are zero toxicities, escalate and go to (7).
#'
#' 4b. If there is one toxicity, stay at the current dose and go to (2).
#'
#' 4c. If there are two or three toxicities, declare that the MTD has been exceeded and go to (6).
#'
#' (5). Check the number of toxicities (among six patients) at the current dose level.
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
#' 6b. If then next-lower dose level has six patients, stop the trial and declare that the MTD is the
#' next lower dose level; otherwise, the next lower dose level has three patients; set the current dose
#' level to be the next-lower dose level and go to (2).
#'
#' (7). Escalate if possible.
#'
#' 7a. If the current dose level is the highest dose level, stop the trial and declare that the MTD is
#' the highest dose level.
#'
#' 7b. Otherwise, escalate to the next higher dose level and go to (2).
#'
#' @return \code{run.sim.3()} returns:
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
#' @references Storer B. (1989). Design and analysis of phase i clinical trials, \emph{Biometrics}, 925–937.
#'
#' Yang, S., Wang, S.J. and Ji, Y., (2015). An integrated dose-finding tool for phase I trials in oncology. \emph{Contemporary clinical trials}, 45, pp.426-434.
#'
#'
#' @examples
#' run.sim.3(p.true = c(0.25, 0.41, 0.45, 0.49, 0.53),
#'           mtd.true = c(1,0,0,0,0),
#'           startdose = 1,
#'           ntrial = 1000,
#'           seed = 6)
#'
#' @export
run.sim.3 <- function (p.true,
                       mtd.true,
                       startdose = 1,
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


  # Initialize matrix for storage
  select.perc = matrix(rep(0, ndose * nscene), ncol = ndose)
  stop.perc = rep(0, nscene)
  nptsdose = matrix(rep(0, ndose * nscene), ncol = ndose)
  npts = rep(0, nscene)

  ################################# Step 1 #################################
  for (j in 1:nscene){
    set.seed(seed)

    p <- p.true[j,]

    # number of doses
    ndose = length(p)

    # store Y and N
    Y = matrix(rep(0, ndose * ntrial), ncol = ndose)
    N = matrix(rep(0, ndose * ntrial), ncol = ndose)

    # store MTD
    MTD = rep(0, ntrial)

    ############################## Step 2 #################################

    for (trial in 1:ntrial) {

      y <- rep(0, ndose)
      n <- rep(0, ndose)

      d = startdose
      earlystop = 0
      exceed = 0

      mtd_d_1 = 0
      mtd_d = 0

      stop = 0

    while (stop == 0) {
      #############################################################
      ## 2) Accrue and treat three patients at the current dose: ##
      #############################################################

      ### Generate random toxicity response
      xx <- 0
      for(i in 1:3){
        ttt <- runif(1)
        if(ttt < p[d])
          xx <- xx + 1
      }

      y[d] <- y[d] + xx;
      n[d] <- n[d] + 3

      ######################################################
      ## 3a) If there are 3 patients at the current dose: ##
      ######################################################
      if (n[d] == 3){

        ##############
        ## If 0 DLT ##
        ##############
        if (y[d] == 0){

          if (d != ndose){
            #### E ####
            d = d + 1
            }
          else if (d == ndose){
            #### Stop if highest dose ####
            #### Declare MTD is the highest dose
            mtd_d <- 1
            break
            }
        }
        ##############
        ## If 1 DLT ##
        ##############
        else if (y[d] == 1){
          #### S ####
          d = d
        }
        ################
        ## If > 1 DLT ##
        ################
        else if (y[d] > 1){

          #### The MTD has been exceeded ####
          exceed <- 1

          if (d == 1){
            #### Stop if lowest dose ####
            earlystop = 1
            break
          }
          #### Stop if the next-lower dose has 6 patients ####
          #### Declare MTD is the next lower dose
          else if (d > 1 & n[d-1] == 6){
            mtd_d_1 <- 1
            break
          }
          #### If the next-lower dose has 3 patients ####
          else if (d > 1 & n[d-1] == 3){
            #### D ####
            d = d - 1
          }

        }
      }

      ######################################################
      ## 3b) If there are 6 patients at the current dose: ##
      ######################################################

      else if (n[d] == 6){

        ##############
        ## If 0 DLT ##
        ##############
        #### Declare MTD is the current dose
        if (y[d] == 0){
          mtd_d <- 1
          break
        }

        ##############
        ## If 1 DLT ##
        ##############
        else if (y[d] == 1 & exceed == 1){
          mtd_d <- 1
          break
        }
        else if (y[d] == 1 & exceed == 0) {

          if (d != ndose){
            #### E ####
            d = d + 1
          }
          else if (d == ndose){
            #### Stop if highest dose ####
            #### Declare MTD is the highest dose
            mtd_d <- 1
            break
          }
        }

        ################
        ## If > 1 DLT ##
        ################
        else if (y[d] > 1) {
          #### The MTD has been exceeded ####
          exceed <- 1

          if (d == 1){
            #### Stop if lowest dose ####
            earlystop = 1
            break
          }
          #### Stop if the next-lower dose has 6 patients ####
          #### Declare MTD is the next lower dose
          else if (d > 1 & n[d-1] == 6){
            mtd_d_1 <- 1
            break
          }
          #### If the next-lower dose has 3 patients ####
          else if (d > 1 & n[d-1] == 3){
            #### D ####
            d = d - 1
          }

        }
      }

    #   total <- sum(n)
    #   if(total >= ncohort*3){
    #     stop <- 1
    #   }
    }

    ################################# Step 3 #################################
    ## Select MTD
    if (earlystop == 1) {
      MTD[trial] = 99
    }
    if (mtd_d_1 == 1){
      MTD[trial] = d-1
    }
    if (mtd_d == 1){
      MTD[trial] = d
    }
    # if (stop == 1 & earlystop == 0 & mtd_d_1 == 0 & mtd_d == 0){
    #   MTD[trial] = -1
    # }

      Y[trial, ] = y
      N[trial, ] = n

    ##########################################################################################
  }

    ################################# Step 4 #################################

    nptsdose[j, ] = colMeans(N) #average number of patients
    #nptsdose.perc = colMeans(N)/sum(colMeans(N))
    npts[j] <- mean(rowSums(N)) #average total number of patients

    for (i in 1:ndose) {
      select.perc[j, i] = sum(MTD == i)/ntrial
    }

    stop.perc[j] <- sum(MTD == 99)/ntrial
  }

  setup <- list(method = "3+3",
                startdose = startdose,
                cohortsize = 3,
                ncohort = "not pre-specified")

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
