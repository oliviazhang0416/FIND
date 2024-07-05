#'
#' Load several commonly used scenarios
#'
#' Load several commonly used scenarios
#'
#' @usage load.true(pT = 0.25)
#'
#' @param pT the target DLT rate.
#'
#' @return \code{load.true()} returns:
#'
#' (1) a matrix (\code{$p.true}) containing the true toxicity probabilities of the investigational dose levels.
#'
#' (2) a vector (\code{$mtd.true}) which specifies the true MTD.
#'
#' @export

load.true <- function(pT = 0.25){

    if (pT == 0.2){
      p.true <- rbind(c(0.2, 0.27, 0.30, 0.35, 0.5),
                      c(0.05, 0.2, 0.27, 0.35, 0.46),
                      c(0.03, 0.1, 0.2, 0.3, 0.45),
                      c(0.02, 0.06, 0.11, 0.2, 0.28),
                      c(0.06, 0.08, 0.1, 0.12, 0.2))

      mtd.true <- 1:5

      ndose <- ncol(p.true)
      nscene <- nrow(p.true)

      names(mtd.true) = paste0("Scenario.",1:nscene)

      colnames(p.true) = c(paste0("Dose.",1:ndose))
      rownames(p.true) = paste0("Scenario.",1:nscene)

      return(list("p.true" = p.true,
                  "mtd.true" = mtd.true))
    }
    if (pT == 0.25){

      p.true <- rbind(c(0.25, 0.41, 0.45, 0.49, 0.53),
                      c(0.12, 0.25, 0.42, 0.49, 0.55),
                      c(0.04, 0.12, 0.25, 0.43, 0.63),
                      c(0.02, 0.06, 0.1, 0.25, 0.4),
                      c(0.02, 0.05, 0.08, 0.11, 0.25))
      mtd.true <- c(1:5)

      ndose <- ncol(p.true)
      nscene <- nrow(p.true)

      names(mtd.true) = paste0("Scenario.",1:nscene)

      colnames(p.true) = c(paste0("Dose.",1:ndose))
      rownames(p.true) = paste0("Scenario.",1:nscene)

      a <- list("p.true" = p.true,
                "mtd.true" = mtd.true)

      p.true <- rbind(c(0.26, 0.34, 0.47, 0.64, 0.66, 0.77),
                      c(0.18, 0.25, 0.32, 0.36, 0.60, 0.69),
                      c(0.09, 0.16, 0.23, 0.34, 0.51, 0.74),
                      c(0.07, 0.12, 0.17, 0.27, 0.34, 0.55),
                      c(0.03, 0.13, 0.17, 0.19, 0.26, 0.31),
                      c(0.04, 0.05, 0.09, 0.14, 0.15, 0.24),
                      c(0.34, 0.42, 0.46, 0.49, 0.58, 0.62),
                      c(0.13, 0.41, 0.45, 0.58, 0.75, 0.76))

      mtd.true <- c(1:6, NA, 1)

      ndose <- ncol(p.true)
      nscene <- nrow(p.true)

      names(mtd.true) = paste0("Scenario.",1:nscene)

      colnames(p.true) = c(paste0("Dose.",1:ndose))
      rownames(p.true) = paste0("Scenario.",1:nscene)

      b <- list("p.true" = p.true,
                "mtd.true" = mtd.true)

      return(list("BOIN provided" = a,
                  "FFP-BOIN provided" = b))
    }
    if (pT == 0.3){
      p.true <- rbind(c(0.3, 0.37, 0.4, 0.45, 0.6),
                      c(0.15, 0.3, 0.37, 0.45, 0.56),
                      c(0.08, 0.2, 0.3, 0.4, 0.45),
                      c(0.07, 0.12, 0.18, 0.3, 0.4),
                      c(0.05, 0.07, 0.1, 0.15, 0.3))

      mtd.true <- 1:5

      ndose <- ncol(p.true)
      nscene <- nrow(p.true)

      names(mtd.true) = paste0("Scenario.",1:nscene)

      colnames(p.true) = c(paste0("Dose.",1:ndose))
      rownames(p.true) = paste0("Scenario.",1:nscene)

      return(list("p.true" = p.true,
                  "mtd.true" = mtd.true))
    }
}
