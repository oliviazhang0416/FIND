create.legend <- function(method){

  ################# Create grob for legend #################
  E <- data.frame("Escalate to d+1")
  colnames(E) <- NULL
  row.names(E) <- c("E\n ")
  Ei <- data.frame("If cdn is true, escalate to d+1")
  colnames(Ei) <- NULL
  row.names(Ei) <- c("E\nif cdn")
  S <- data.frame("Stay and enroll the next cohort")
  colnames(S) <- NULL
  row.names(S) <- c("S\n ")
  D <- data.frame("De-escalate to d-1")
  colnames(D) <- NULL
  row.names(D) <- c("D\n ")
  DU <- data.frame("De-escalate to d-1 and eliminate doses \u2265 d")
  colnames(DU) <- NULL
  row.names(DU) <- c("DU\n ")
  DUi <- data.frame("If cdn is true, de-escalate to d-1 and eliminate doses \u2265 d")
  colnames(DUi) <- NULL
  row.names(DUi) <- c("DU\nif cdn")
  i <- data.frame("If cdn is true, stop the trial and MTD is x")
  colnames(i) <- NULL
  row.names(i) <- c("x\nif cdn")
  na <- data.frame("The decision is not applicable")
  colnames(na) <- NULL
  row.names(na) <- c("na\n ")

  E. <- data.frame("Escalate to d+1")
  colnames(E.) <- NULL
  row.names(E.) <- c("E")
  S. <- data.frame("Stay and enroll the next cohort")
  colnames(S.) <- NULL
  row.names(S.) <- c("S")
  D. <- data.frame("De-escalate to d-1")
  colnames(D.) <- NULL
  row.names(D.) <- c("D")
  DU. <- data.frame("De-escalate to d-1 and eliminate doses \u2265 d")
  colnames(DU.) <- NULL
  row.names(DU.) <- c("DU")

  E <- tableGrob(E, theme=ttheme_minimal(base_size = 8,
                                         core = list(bg_params=list(fill=c("white"))),
                                         rowhead = list(fg_params=list(fontface="bold", hjust=0.5, vjust=0.8, x=0.5, col="black"),
                                                        bg_params=list(fill="#81C784"))))
  Ei <- tableGrob(Ei, theme=ttheme_minimal(base_size = 8,
                                           core = list(bg_params=list(fill=c("white"))),
                                           rowhead = list(fg_params=list(fontface="bold", hjust=0.5, vjust=0.5, x=0.5, col="black"),
                                                          bg_params=list(fill="#81C784"))))
  S <- tableGrob(S, theme=ttheme_minimal(base_size = 8,
                                         core = list(bg_params=list(fill=c("white"))),
                                         rowhead = list(fg_params=list(fontface="bold", hjust=0.5, vjust=0.8, x=0.5, col="black"),
                                                        bg_params=list(fill="#64B5F6"))))
  D <- tableGrob(D, theme=ttheme_minimal(base_size = 8,
                                         core = list(bg_params=list(fill=c("white"))),
                                         rowhead = list(fg_params=list(fontface="bold", hjust=0.5, vjust=0.8, x=0.5, col="black"),
                                                        bg_params=list(fill="#E57373"))))
  DU <- tableGrob(DU, theme=ttheme_minimal(base_size = 8,
                                           core = list(fg_params=list(parse=TRUE),
                                                       bg_params=list(fill=c("white"))),
                                           rowhead = list(fg_params=list(fontface="bold", hjust=0.5, vjust=0.8, x=0.5, col="black"),
                                                          bg_params=list(fill="#BA68C8"))))
  DUi <- tableGrob(DUi, theme=ttheme_minimal(base_size = 8,
                                             core = list(fg_params=list(parse=TRUE),
                                                         bg_params=list(fill=c("white"))),
                                             rowhead = list(fg_params=list(fontface="bold", hjust=0.5, vjust=0.5, x=0.5, col="black"),
                                                            bg_params=list(fill="#BA68C8"))))
  i <- tableGrob(i, theme=ttheme_minimal(base_size = 8,
                                         core = list(bg_params=list(fill=c("white"))),
                                         rowhead = list(fg_params=list(fontface="bold", hjust=0.5, x=0.5, col="black"),
                                                        bg_params=list(fill="grey"))))
  na <- tableGrob(na, theme=ttheme_minimal(base_size = 8,
                                           core = list(bg_params=list(fill=c("white"))),
                                           rowhead = list(fg_params=list(fontface="bold", hjust=0.5, vjust=0.8, x=0.5, col="black"),
                                                          bg_params=list(fill="gray97"))))

  E. <- tableGrob(E., theme=ttheme_minimal(base_size = 8,
                                           core = list(bg_params=list(fill=c("white"))),
                                           rowhead = list(fg_params=list(fontface="bold", hjust=0.5, vjust=0.8, x=0.5, col="black"),
                                                          bg_params=list(fill="#81C784"))))
  S. <- tableGrob(S., theme=ttheme_minimal(base_size = 8,
                                           core = list(bg_params=list(fill=c("white"))),
                                           rowhead = list(fg_params=list(fontface="bold", hjust=0.5, vjust=0.8, x=0.5, col="black"),
                                                          bg_params=list(fill="#64B5F6"))))
  D. <- tableGrob(D., theme=ttheme_minimal(base_size = 8,
                                           core = list(bg_params=list(fill=c("white"))),
                                           rowhead = list(fg_params=list(fontface="bold", hjust=0.5, vjust=0.8, x=0.5, col="black"),
                                                          bg_params=list(fill="#E57373"))))
  DU. <- tableGrob(DU., theme=ttheme_minimal(base_size = 8,
                                             core = list(fg_params=list(parse=TRUE),
                                                         bg_params=list(fill=c("white"))),
                                             rowhead = list(fg_params=list(fontface="bold", hjust=0.5, vjust=0.8, x=0.5, col="black"),
                                                            bg_params=list(fill="#BA68C8"))))

  if (length(method) == 1){
    if (method == "3+3"){
      valigned <- gtable_combine(E,Ei,S,DU,DUi,i,na,along=2)
    }
    if (method == "i3+3"){
      valigned <- gtable_combine(E.,S.,D.,DU., along=2)
    }
    if (method == "BOIN"){
      valigned <- gtable_combine(E.,S.,D.,DU., along=2)
    }
  }
  else{
    if ("3+3" %in% method){
      valigned <- gtable_combine(E,Ei,S,D,DU,DUi,i,na, along=2)
    }
    else{
      valigned <- gtable_combine(E.,S.,D.,DU., along=2)
    }
  }

  return(valigned)
}


add_features <- function(param,
                         method){


  info <- data.frame(" " = rep(1,9))
  if ("3+3" %in% method){
    info <- cbind(info, data.frame(`3+3` = c("not appliable",
                                 "not appliable",
                                 "not appliable",
                                 param$`3+3`$startdose,
                                 param$`3+3`$cohortsize,
                                 param$`3+3`$ncohort,
                                 "not appliable",
                                 "not appliable",
                                 "not appliable"),
                                 check.names = F))
  }
  if ("BOIN" %in% method){
  info <- cbind(info, data.frame(`BOIN` = c(param$`BOIN`$pT,
                                paste0("[", param$`BOIN`$EI[1], ", ", param$`BOIN`$EI[2],")"),
                                paste0(round(param$`BOIN`$boundary[1],3), ", ", round(param$`BOIN`$boundary[2],3)),
                                param$`BOIN`$startdose,
                                param$`BOIN`$cohortsize,
                                param$`BOIN`$ncohort,
                                param$`BOIN`$DU.pp,
                                ifelse(param$`BOIN`$extrasafe == T,
                                       "Yes, if the isotonically-transformed posterior\nmean of the selected MTD\nis above the EI,\nselect the next lower dose as the final MTD",
                                       "No"),
                                ifelse(param$`BOIN`$n.earlystop < param$`BOIN`$cohortsize * param$`BOIN`$ncohort,
                                       paste0("Yes, if the number of patients\ntreated at the current dose\nreaches ", param$`BOIN`$n.earlystop, ",\nstop the trial and select the MTD\nbased on the observed data"),
                                       "No")
                     ),
                     check.names = F))
  }
  if ("i3+3" %in% method){
    info <- cbind(info, data.frame(`i3+3` = c(param$`i3+3`$pT,
                                paste0("[", param$`i3+3`$EI[1], ", ", param$`i3+3`$EI[2],"]"),
                                paste0(param$`i3+3`$EI[1], ", ", param$`i3+3`$EI[2]),
                                param$`i3+3`$startdose,
                                param$`i3+3`$cohortsize,
                                param$`i3+3`$ncohort,
                                param$`i3+3`$DU.pp,
                                ifelse(param$`i3+3`$extrasafe == T,
                                       "Yes, if the isotonically-transformed posterior\nmean of the selected MTD\nis above the EI,\nselect the next lower dose as the final MTD",
                                       "No"),
                                ifelse(param$`i3+3`$n.earlystop < param$`i3+3`$cohortsize * param$`i3+3`$ncohort,
                                       paste0("Yes, if the number of patients\ntreated at the current dose\nreaches ", param$`BOIN`$n.earlystop, ",\nstop the trial and select the MTD\nbased on the observed data"),
                                       "No")),
                     check.names = F))
  }

  info <- data.frame(info[, method],
                     check.names = F)
  colnames(info) <- method

  row.names(info) <- c(paste0("p","[","T","]"),
                       "EI", "Boundary", "Start Dose", "Cohort Size", "Number of Cohorts",
                       "The Cutoff\nto Eliminate\nAn Overly Toxic Dose:",
                       "Whether to Implement the\nExtra Safety Rule",
                       "Whether to Implement the\nExtra Stopping Rule")

  ################# Create grob for info table #################
  info.grob <- tableGrob(info, theme=ttheme_default(base_size = 6,
                                                    core = list(fg_params=list(fontface="bold",
                                                                               hjust = 0.5, x=0.5, vjust = 0.5, col="black")),
                                                    rowhead = list(fg_params=list(parse=TRUE,
                                                                                  fontface="bold",
                                                                                  hjust = 0.5, x=0.5, col="black"))))

  #add out border
  info.grob <- gtable_add_grob(info.grob,
                               grobs = rectGrob(gp = gpar(fill = NA,
                                                          lwd = 0.5)),
                               t = 2,
                               r = 2, l = ncol(info.grob),
                               b = nrow(info.grob))

  return(info.grob)
}
