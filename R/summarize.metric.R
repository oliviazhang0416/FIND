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
#' @param nptsdose the number of patients treated for each dose.
#' @param npts the average number of patients treated.
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
#'        the number of patients treated at each dose level,
#'        the average number of patients treated,
#'        the percentage of overdosing assignment (POA),
#'        the percentage of correct assignment (PCA),
#'        the percentage of underdosing assignment (PUA), respectively.
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
    if (sum(mtd.true[j, ]) == 0){
      sel[j, ] <- c(select.perc[j, ] * 100, stop.perc[j] * 100,
                    sum(select.perc[j, 1:ndose]) * 100, stop.perc[j] * 100, 0)
      alo[j, ] <- c(nptsdose.perc[j, ] * 100, npts[j],
                    sum(nptsdose.perc[j, 1:ndose]) * 100, 0, 0)
    }
    else if (sum(mtd.true[j, ]) == 1 & which(mtd.true[j, ] == 1) == 1){
      sel[j, ] <- c(select.perc[j, ] * 100, stop.perc[j] * 100,
                    sum(select.perc[j, 2:ndose]) * 100, select.perc[j, 1] * 100, stop.perc[j] * 100)
      alo[j, ] <- c(nptsdose.perc[j, ] * 100, npts[j],
                    sum(nptsdose.perc[j, 2:ndose]) * 100, nptsdose.perc[j, 1] * 100, 0)
    }
    else if (sum(mtd.true[j, ]) == 1 & which(mtd.true[j, ] == 1) == ndose){
      sel[j, ] <- c(select.perc[j, ] * 100, stop.perc[j] * 100,
                    0, select.perc[j, ndose] * 100, (sum(select.perc[j, 1:(ndose-1)]) + stop.perc[j]) * 100)
      alo[j, ] <- c(nptsdose.perc[j, ] * 100, npts[j],
                    0, nptsdose.perc[j, ndose] * 100, sum(nptsdose.perc[j, 1:(ndose-1)]) * 100)
    }
    else if (sum(mtd.true[j, ]) == 1 & which(mtd.true[j, ] == 1) > 1 & which(mtd.true[j, ] == 1) < ndose){
      b <- max(which(mtd.true[j, ] == 1))
      sel[j, ] <- c(select.perc[j, ] * 100, stop.perc[j] * 100,
                    sum(select.perc[j, (b+1):ndose]) * 100,
                    select.perc[j, b] * 100,
                    (sum(select.perc[j, 1:(b-1)]) + stop.perc[j]) * 100)
      alo[j, ] <- c(nptsdose.perc[j, ] * 100, npts[j],
                    sum(nptsdose.perc[j, (b+1):ndose]) * 100,
                    nptsdose.perc[j, b] * 100,
                    sum(nptsdose.perc[j, 1:(b-1)]) * 100)
    }
    else if (sum(mtd.true[j, ]) > 1 & min(which(mtd.true[j, ] == 1)) == 1){
      b <- max(which(mtd.true[j, ] == 1))
      sel[j, ] <- c(select.perc[j, ] * 100, stop.perc[j] * 100,
                    sum(select.perc[j, (b+1):ndose]) * 100,
                    sum(select.perc[j, 1:b]) * 100,
                    stop.perc[mtd.true[j, ]] * 100)
      alo[j, ] <- c(nptsdose.perc[j, ] * 100, npts[j],
                    sum(nptsdose.perc[j, (b+1):ndose]) * 100,
                    sum(nptsdose.perc[j, 1:b]) * 100,
                    0)
    }
    else if (sum(mtd.true[j, ]) > 1 & max(which(mtd.true[j, ] == 1)) == ndose){
      a <- min(which(mtd.true[j, ] == 1))
      sel[j, ] <- c(select.perc[j, ] * 100, stop.perc[j] * 100,
                    0,
                    sum(select.perc[j, a:ndose]) * 100,
                    (sum(select.perc[j, 1:(a-1)]) + stop.perc[j]) * 100)
      alo[j, ] <- c(nptsdose.perc[j, ] * 100, npts[j],
                    0,
                    sum(nptsdose.perc[j, a:ndose]) * 100,
                    sum(nptsdose.perc[j, 1:(a-1)]) * 100)
    }
    else{
      a <- min(which(mtd.true[j, ] == 1))
      b <- max(which(mtd.true[j, ] == 1))
      sel[j, ] <- c(select.perc[j, ] * 100, stop.perc[j] * 100,
                    sum(select.perc[j, (b+1):ndose]) * 100,
                    sum(select.perc[j, a:b]) * 100,
                    (sum(select.perc[j, 1:(a-1)]) + stop.perc[j]) * 100)
      alo[j, ] <- c(nptsdose.perc[j, ] * 100, npts[j],
                    sum(nptsdose.perc[j, (b+1):ndose]) * 100,
                    nptsdose.perc[j, a:b] * 100,
                    sum(nptsdose.perc[j, 1:(a-1)]) * 100)
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
                       "Total.patients", "Overdose.Perc", "Correctdose.Perc", "Underdose.Perc",
                       "Scenario")

    alo <- alo[,c("Scenario",
                  paste0("Dose.",1:ndose),
                  "Total.patients", "Overdose.Perc", "Correctdose.Perc", "Underdose.Perc")]

    out = list(sel = sel,
               alo = alo
    )

    return(out)
}
