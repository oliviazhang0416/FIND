#'
#' Select the maximum tolerated dose (MTD)
#'
#' Select the maximum tolerated dose (MTD) when the trial is completed
#'
#' @usage select.mtd(method,
#'                   pT,
#'                   EI,
#'                   n_obs,
#'                   y_obs,
#'                   DU.pp=0.95,
#'                   extrasafe = F)
#'
#' @param method the design name.
#' @param pT a numeric value; the target DLT rate.
#' @param EI a vector which specifies the equivalence interval (EI).
#' @param n_obs a vector containing the number of patients treated at each dose level.
#' @param y_obs a vector containing the number of patients who experienced dose-limiting toxicity at each dose level.
#' @param DU.pp a numeric value; the cutoff to remove an overly toxic dose for safety.
#'              We recommend the default value of (\code{DU.pp=0.95}) for general use.
#' @param extrasafe a logical value which specifies whether to implement a more strict safety rule.

#' @return  \code{select.mtd()} returns the selected MTD dose (\code{$d_selected})
#'
#' @details \code{select.mtd()} selects the MTD based on isotonic estimates of toxicity
#'          probabilities. If there are ties, we select from the ties the highest dose
#'          level when the estimate of the DLT rate is smaller than the target, or the
#'          lowest dose level when the estimate of the DLT rate is greater than the target.
#'          The isotonic estimates are obtained by the pooled-adjacent-violators algorithm
#'          (PAVA) (Barlow, 1972).
#'
#' @export
select.mtd <- function (method,
                        pT,
                        EI,
                        n_obs,
                        y_obs,
                        DU.pp = 0.95,
                        extrasafe = F)
{

  pava <- function (x, wt = rep(1, length(x)))
  {
    n <- length(x)
    if (n <= 1)
      return(x)
    if (any(is.na(x)) || any(is.na(wt))) {
      stop("Missing values in 'x' or 'wt' not allowed")
    }
    lvlsets <- (1:n)
    repeat {
      viol <- (as.vector(diff(x)) < 0)
      if (!(any(viol)))
        break
      i <- min((1:(n - 1))[viol])
      lvl1 <- lvlsets[i]
      lvl2 <- lvlsets[i + 1]
      ilvl <- (lvlsets == lvl1 | lvlsets == lvl2)
      x[ilvl] <- sum(x[ilvl] * wt[ilvl])/sum(wt[ilvl])
      lvlsets[ilvl] <- lvl1
    }
    x
  }

  y = y_obs
  n = n_obs

  ndose = length(n)
  doses = rep(0, ndose)

  ################# Safety rule (dose exclusion) ###################
  for (i in 1:ndose) {
    if (n[i] >= 3) {
      # Terminate
      if (1 - pbeta(pT, y[i] + 1, n[i] - y[i] + 1) > DU.pp) {
        doses[i:ndose] = 1
        break
      }
    }
  }

  # If dose 1 is too toxic, no selection
  if (doses[1] == 1 || sum(n[doses == 0]) == 0) {
    selectdose = 99
  }
  else {
    #### Find the non-DU doses ####
    adm.set = (n != 0) & (doses == 0)
    adm.index = which(adm.set == T)

    y.adm = y[adm.set]
    n.adm = n[adm.set]

    #### Posterior mean ####
    phat = (y.adm + 0.05)/(n.adm + 0.1)
    phat.var = (y.adm + 0.05) * (n.adm - y.adm + 0.05)/((n.adm + 0.1)^2 * (n.adm + 0.1 + 1))

    #### Isotonic-transformed posterior means ####
    phat = pava(phat, wt = 1/phat.var)
    phat = phat + (1:length(phat)) * 1e-10
    ## by adding an increasingly small number to tox prob at higher doses,
    ## it will break the ties and make the lower dose level the MTD if the ties were larger than pT
    ## or make the higher dose level the MTD if the ties are smaller than pT

    #### Find the index of the smallest diff(posterior means, pT)
    selectd = sort(abs(phat - pT), index.return = T)$ix[1]
    selectdose = adm.index[selectd]

    if (method == "BOIN"){
      thres <- log((1 - pT)/(1 - EI[2]))/log(EI[2] * (1 - pT)/(pT * (1 - EI[2])))
    }
    else if (method == "i3+3"){
      thres <- EI[2]
    }
    ##******************* Edit Jan. 27, 2023  ************###############
    if(phat[selectd] > thres & extrasafe == T){
      if (selectdose > 1){
        selectdose = selectdose-1
      }
      else{
        selectdose = 99
      }
    }
    ##*******************##*******************##*******************####
  }


    out = list(d_selected = selectdose)

  return(out)
}
