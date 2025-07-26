#'
#' Summarize after conducting simulations
#'
#' Summarize after conducting simulations
#'
#' @usage summarize.metric(select.perc,
#'                         stop.perc,
#'                         nptsdose,
#'                         npts,
#'                         mtd.true)
#'
#' @param select.perc the selection percentage for each dose.
#' @param stop.perc the percentage of early stopping without selecting the MTD.
#' @param nptsdose the number of participants treated for each dose.
#' @param npts the average number of participants treated.
#' @param mtd.true a vector (or a matrix) with the same dimension as
#' the vector (or matrix) containing the true toxicity probabilities (\code{$p.true}).
#' It should takes value of 0 or 1, and 1 specifies the true MTD.
#'
#' @return \code{summarize.metric()} returns:
#'
#' (1) a dataframe (\code{$selection}) with each column showing:
#'        the numbered index for each scenarios specified,
#'        the selection percentage at each dose level,
#'        the percentage of early stopping without selecting the MTD,
#'        the percentage of overdosing selection (POS),
#'        the percentage of correct selection (PCS),
#'        the percentage of underdosing selection (PUS), respectively.
#'
#' (2) a dataframe (\code{$allocation}) with each column showing:
#'        the numbered index for each scenarios specified,
#'        the number of participants treated at each dose level,
#'        the average number of participants treated,
#'        the percentage of overdosing allocation (POA),
#'        the percentage of correct allocation (PCA),
#'        the percentage of underdosing allocation (PUA), respectively.
#'
#' @examples summarize.metric(select.perc = c(0.588, 0.218, 0.025, 0.0, 0.0, 0.0),
#'                            stop.perc = 0.168,
#'                            nptsdose = c(20.53, 9.03, 2.48, 0.29, 0.01, 0.00),
#'                            npts = 32.4,
#'                            mtd.true = c(1,0,0,0,0,0))
#' @export
#'
summarize.metric <- function(select.perc,
                          stop.perc,
                          nptsdose,
                          npts,
                          mtd.true){

  # check inputs
  if (is.matrix(mtd.true) & is.matrix(select.perc) & is.matrix(nptsdose) & is.vector(stop.perc) & is.vector(npts)){
    # number of scenarios
      nscene = nrow(mtd.true)
      ndose = ncol(mtd.true)
  }
  else if (is.vector(mtd.true) & is.vector(select.perc) & is.vector(nptsdose) & is.double(stop.perc) & is.double(npts)){
      nscene = 1
      ndose = length(mtd.true)
      mtd.true = matrix(mtd.true, nrow = 1)
      select.perc = matrix(select.perc, nrow = 1)
      nptsdose = matrix(nptsdose, nrow = 1)
  }
  else{
    stop("Warnings: please make sure the inputs have proper dimensions!")
  }


  #Create an empty dataframe for storage
  sel <- as.data.frame(matrix(data = NA, nrow = nscene, ncol = ndose+4))
  alo <- as.data.frame(matrix(data = NA, nrow = nscene, ncol = ndose+4))

  nptsdose.perc <- sweep(nptsdose, 1, npts, "/")

  for (j in 1:nscene){

    ######## If no MTD ########
    if (sum(mtd.true[j, ]) == 0){
      ######## MTD selection ########
      sel[j, ] <- c(#-- Raw --#
                    select.perc[j, ] * 100, stop.perc[j] * 100,
                    #-- Summarized --#
                    sum(select.perc[j, 1:ndose]) * 100, #Over
                    stop.perc[j] * 100, #Correct
                    0) #Under
      ######## Patient allocation ########
      alo[j, ] <- c(#-- Raw --#
                    nptsdose.perc[j, ] * 100, npts[j],
                    #-- Summarized --#
                    sum(nptsdose.perc[j, 1:ndose]) * 100, #Over
                    0, #Correct
                    0) #Under
    }

    ######## If lowest dose is MTD ########
    else if (sum(mtd.true[j, ]) == 1 & which(mtd.true[j, ] == 1) == 1){
      ######## MTD selection ########
      sel[j, ] <- c(#-- Raw --#
                    select.perc[j, ] * 100, stop.perc[j] * 100,
                    #-- Summarized --#
                    sum(select.perc[j, 2:ndose]) * 100, # Over
                    select.perc[j, 1] * 100, # Correct
                    stop.perc[j] * 100) # Under
      ######## Patient allocation ########
      alo[j, ] <- c(#-- Raw --#
                    nptsdose.perc[j, ] * 100, npts[j],
                    #-- Summarized --#
                    sum(nptsdose.perc[j, 2:ndose]) * 100, # Over
                    nptsdose.perc[j, 1] * 100, # Correct
                    0) # Under
    }

    ######## If highest dose is MTD ########
    else if (sum(mtd.true[j, ]) == 1 & which(mtd.true[j, ] == 1) == ndose){
      ######## MTD selection ########
      sel[j, ] <- c(#-- Raw --#
                    select.perc[j, ] * 100, stop.perc[j] * 100,
                    #-- Summarized --#
                    0, #Over
                    select.perc[j, ndose] * 100, #Correct
                    (sum(select.perc[j, 1:(ndose-1)]) + stop.perc[j]) * 100) #Under
      ######## Patient allocation ########
      alo[j, ] <- c(#-- Raw --#
                    nptsdose.perc[j, ] * 100, npts[j],
                    #-- Summarized --#
                    0, #Over
                    nptsdose.perc[j, ndose] * 100, #Correct
                    sum(nptsdose.perc[j, 1:(ndose-1)]) * 100) #Under
    }

    ######## If middle dose is MTD ########
    else if (sum(mtd.true[j, ]) == 1 & which(mtd.true[j, ] == 1) > 1 & which(mtd.true[j, ] == 1) < ndose){
      b <- max(which(mtd.true[j, ] == 1))
      ######## MTD selection ########
      sel[j, ] <- c(#-- Raw --#
                    select.perc[j, ] * 100, stop.perc[j] * 100,
                    #-- Summarized --#
                    sum(select.perc[j, (b+1):ndose]) * 100, #Over
                    select.perc[j, b] * 100, #Correct
                    (sum(select.perc[j, 1:(b-1)]) + stop.perc[j]) * 100) #Under
      ######## Patient allocation ########
      alo[j, ] <- c(#-- Raw --#
                    nptsdose.perc[j, ] * 100, npts[j],
                    #-- Summarized --#
                    sum(nptsdose.perc[j, (b+1):ndose]) * 100, #Over
                    nptsdose.perc[j, b] * 100, #Correct
                    sum(nptsdose.perc[j, 1:(b-1)]) * 100) #Under
    }
  }

  sel <- round(sel, 1)
  alo <- round(alo, 1)

    sel$scenario <- 1:nscene
    colnames(sel) <- c(paste0("Dose.",1:ndose),
                       "No.selection", "Overdose.Perc", "Correctdose.Perc", "Underdose.Perc",
                       "Scenario")

    sel <- sel[,c("Scenario",
                  paste0("Dose.",1:ndose),
                  "No.selection", "Overdose.Perc", "Correctdose.Perc", "Underdose.Perc")]

    alo$scenario <- 1:nscene
    colnames(alo) <- c(paste0("Dose.",1:ndose),
                       "Total.participants", "Overdose.Perc", "Correctdose.Perc", "Underdose.Perc",
                       "Scenario")

    alo <- alo[,c("Scenario",
                  paste0("Dose.",1:ndose),
                  "Total.participants", "Overdose.Perc", "Correctdose.Perc", "Underdose.Perc")]

    out = list(sel = sel,
               alo = alo
    )

    return(out)
}
