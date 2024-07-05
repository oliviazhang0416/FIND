#'
#' Generate the decision table(s) for a single or multiple designs
#'
#' Generate the decision table(s) for a single or multiple designs
#'
#' @usage generate_decision_table(`3+3` = NULL,
#'                                `BOIN` = NULL,
#'                                `i3+3` = NULL)
#'
#' @param 3+3 the object returned by get.decision.3()
#' @param BOIN the object returned by get.decision.b()
#' @param i3+3 the object returned by get.decision.i3()
#'
#' @return \code{generate_decision_table()} returns a figure displaying the decision table(s) for the user-specified design(s).
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_text facet_grid vars labs theme element_text unit element_blank element_rect element_line scale_y_discrete scale_fill_manual ggsave scale_x_discrete coord_fixed as_labeller guides guide_legend facet_wrap position_stack geom_bar
#' @importFrom grid rectGrob gpar segmentsGrob
#' @importFrom gridExtra grid.arrange tableGrob ttheme_default ttheme_minimal gtable_combine
#' @importFrom gtable gtable_add_grob
#' @importFrom dplyr %>% group_by mutate
#' @importFrom rlang .data
#' @importFrom stats pbeta runif
#'
#' @export

generate_decision_table <- function(`3+3` = NULL,
                                    `BOIN` = NULL,
                                    `i3+3` = NULL){

  setup.store <- list(`3+3` = `3+3`$setup,
                      `BOIN` = `BOIN`$setup,
                      `i3+3` = `i3+3`$setup)

  tab.store <- rbind(`3+3`$tab,
                     `BOIN`$tab,
                     `i3+3`$tab)

  method <- unique(tab.store$method)[order(match(unique(tab.store$method),c("3+3","BOIN","i3+3")))]

  # Check
  if(length(method) != sum(!is.null(`3+3`),!is.null(`BOIN`), !is.null(`i3+3`))){
    stop("Warnings: Please double check the input(s)!")
  }
  if("3+3" %in% method & (is.null(`3+3`))){
    stop("Warnings: Please double check the input(s)!")
  }
  if("BOIN" %in% method & (is.null(`BOIN`))){
    stop("Warnings: Please double check the input(s)!")
  }
  if("i3+3" %in% method & (is.null(`i3+3`))){
    stop("Warnings: Please double check the input(s)!")
  }
  if(is.null(`3+3`) & !is.null(`i3+3`) & !is.null(`BOIN`)){
    if ((`i3+3`$setup$npatients != `BOIN`$setup$npatients)){
      stop("Please make sure to input the same number of patients for the designs specified.")
    }
  }
  if(is.null(`i3+3`) & !is.null(`3+3`) & !is.null(`BOIN`)){
    if ((`3+3`$setup$npatients != `BOIN`$setup$npatients)){
      stop("Please make sure to input the same number of patients for the designs specified.")
    }
  }
  if(is.null(`BOIN`) & !is.null(`3+3`) & !is.null(`i3+3`)){
    if ((`3+3`$setup$npatients != `i3+3`$setup$npatients)){
      stop("Please make sure to input the same number of patients for the designs specified.")
    }
  }
  if(!is.null(`BOIN`) & !is.null(`3+3`) & !is.null(`i3+3`)){
    if ((`3+3`$setup$npatients != `i3+3`$setup$npatients) || (`i3+3`$setup$npatients != `BOIN`$setup$npatients) || (`3+3`$setup$npatients != `BOIN`$setup$npatients)){
      stop("Please make sure to input the same number of patients for the designs specified.")
    }
  }


  npatients <- max(tab.store$n)
  tab.store$Decision <- factor(tab.store$Decision, levels = c("E",
                                                              "S",
                                                              "D",
                                                              "DU",
                                                              "d-1",
                                                              "d",
                                                              "na"))
  # Define the colors
  c <- match(unique(tab.store$Decision),
             c("E","S","D","DU","d-1","d","na"))

  colors <- c("#81C784", "#64B5F6", "#E57373", "#BA68C8", "grey", "grey", "gray97")[sort(c)]

  ############################ Main plot ############################

  if (length(method) == 1 & "3+3" %in% method){

    # colors <- c("#81C784",
    #             "#64B5F6",
    #             "#BA68C8",
    #             "grey",
    #             "grey",
    #             "gray97")

    plot <- ggplot(tab.store, aes(factor(tab.store$method), factor(tab.store$index))) +
      geom_tile(aes(fill = tab.store$Decision),
                color="white",
                width=0.5, height=1) +
      geom_text(aes(label = tab.store$Decision),
                size=2)+
      geom_text(aes(label = tab.store$condition),
                size=2,
                vjust=2.5,
                parse=T) +
      facet_grid(rows = vars(factor(tab.store$y, levels=c(npatients:0))), cols = vars(tab.store$n),
                 scales = "free",
                 space = "free",
                 switch = "both") +
      labs(x = expression(paste("Number of patients treated at the current dose (",n[d],")")),
           y = expression(paste("Number of patients with DLT (",y[d],")")),
           title = paste0("Decision table for the ", method, " design"),
           subtitle = expression(paste("Dose ",italic(d)," is the current dose"))) +
      theme(plot.title = element_text(size=10),
            plot.subtitle = element_text(size=8),
            plot.title.position = "plot",
            plot.margin = unit(c(3,0,0,2),"mm"),
            panel.background = element_blank(),
            panel.spacing.x=unit(0, "lines"),
            panel.spacing.y=unit(0, "lines"),
            axis.ticks = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.title = element_text(size=6),
            legend.position="none",
            strip.placement = "bottom",
            strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF"),
            strip.text.y.left = element_text(angle = 0))+
      scale_y_discrete(labels = NULL)+
      scale_x_discrete(labels = NULL)+
      scale_fill_manual(values = colors)


    ################# Create grob for legend #################
    legend <- create.legend(method = method)

    ################# Final output #################
    pp <- grid.arrange(plot,
                      legend,
                      widths = c(3/5, 2/5),
                      ncol = 2)
    return(pp)

  }

  else if (length(method) == 1 & "BOIN" %in% method){

    # colors <- c("#81C784",
    #             "#64B5F6",
    #             "#E57373",
    #             "#BA68C8")

    plot <- ggplot(tab.store, aes(factor(tab.store$method), factor(tab.store$index))) +
      geom_tile(aes(fill = tab.store$Decision), color = "white") +
      coord_fixed() +
      geom_text(aes(label = tab.store$Decision), size=2) +
      geom_text(aes(label = tab.store$condition),
                size=5,
                hjust=-0.7) +
      facet_grid(rows = vars(factor(tab.store$y, levels=c(npatients:0))), cols = vars(tab.store$n),
                 switch = "both") +
      labs(x = expression(paste("Number of patients treated at the current dose (",n[d],")")),
           y = expression(paste("Number of patients with DLT (",y[d],")")),
           title = paste0("Decision table for the ", method, " design"),
           subtitle = expression(paste("Dose ",italic(d)," is the current dose"))) +
      theme(plot.title = element_text(size=10),
            plot.subtitle = element_text(size=8),
            plot.title.position = "plot",
            plot.margin = unit(c(3,0,0,2),"mm"),
            panel.background = element_blank(),
            panel.spacing.x=unit(0, "lines"),
            panel.spacing.y=unit(0, "lines"),
            axis.ticks = element_blank(),
            axis.line = element_line(colour = "black", linewidth = 0.3),
            axis.title=element_text(size=6),
            legend.position="none",
            strip.placement = "bottom",
            strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF"),
            strip.text.y.left = element_text(angle = 0))+
      scale_y_discrete(labels = NULL)+
      scale_x_discrete(labels = NULL)+
      scale_fill_manual(values = colors)


    sidebox <- data.frame(`BOIN` = c(setup.store$`BOIN`$pT,
                          paste0("[", setup.store$`BOIN`$EI[1], ", ", setup.store$`BOIN`$EI[2],")"),
                          paste0(round(setup.store$`BOIN`$boundary[1],3), ", ", round(setup.store$`BOIN`$boundary[2],3))),
                          check.names = F)

    ################# Create grob for info table #################
    sidebox.grob <- tableGrob(sidebox, theme=ttheme_default(base_size = 6,
                                                      core = list(fg_params=list(fontface="bold",
                                                                                 hjust = 0.5, x=0.5, col="black")),
                                                      rowhead = list(fg_params=list(parse=TRUE,
                                                                                    fontface="bold",
                                                                                    hjust = 0.5, x=0.5, col="black"))))

    #add out border
    sidebox.grob <- gtable_add_grob(sidebox.grob,
                                 grobs = rectGrob(gp = gpar(fill = NA,
                                                            lwd = 0.5)),
                                 t = 2,
                                 r = 2, l = ncol(sidebox.grob),
                                 b = nrow(sidebox.grob))


    ################# Create grob for legend #################
    legend <- create.legend(method = method)

    ################# Final output #################
    pp <- grid.arrange(plot,
                       legend,
                       sidebox.grob,
                       heights = c(6/10, 4/10),
                       widths = c(4/5, 1/5),
                       layout_matrix = rbind(c(1, 2),
                                             c(1, 3)))
    return(pp)
  }


  else if (length(method) == 1 & "i3+3" %in% method){

    # colors <- c("#81C784",
    #             "#64B5F6",
    #             "#E57373",
    #             "#BA68C8")

    plot <- ggplot(tab.store, aes(factor(tab.store$method), factor(tab.store$index))) +
      geom_tile(aes(fill = tab.store$Decision), color = "white") +
      coord_fixed() +
      geom_text(aes(label = tab.store$Decision), size=2) +
      facet_grid(rows = vars(factor(tab.store$y, levels=c(npatients:0))), cols = vars(tab.store$n),
                 switch = "both") +
      labs(x = expression(paste("Number of patients treated at the current dose (",n[d],")")),
           y = expression(paste("Number of patients with DLT (",y[d],")")),
           title = paste0("Decision table for the ", method, " design"),
           subtitle = expression(paste("Dose ",italic(d)," is the current dose"))) +
      theme(plot.title = element_text(size=10),
            plot.subtitle = element_text(size=8),
            plot.title.position = "plot",
            plot.margin = unit(c(3,0,0,2),"mm"),
            panel.background = element_blank(),
            panel.spacing.x=unit(0, "lines"),
            panel.spacing.y=unit(0, "lines"),
            axis.ticks = element_blank(),
            axis.line = element_line(colour = "black", linewidth = 0.3),
            axis.title=element_text(size=6),
            legend.position="none",
            strip.placement = "bottom",
            strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF"),
            strip.text.y.left = element_text(angle = 0))+
      scale_y_discrete(labels = NULL)+
      scale_x_discrete(labels = NULL)+
      scale_fill_manual(values = colors)


    sidebox <- data.frame(`i3+3` = c(setup.store$`i3+3`$pT,
                                     paste0("[", setup.store$`i3+3`$EI[1], ", ", setup.store$`i3+3`$EI[2],"]"),
                                     paste0(setup.store$`i3+3`$EI[1], ", ", setup.store$`i3+3`$EI[2])),
                          check.names = F)

    ################# Create grob for info table #################
    sidebox.grob <- tableGrob(sidebox, theme=ttheme_default(base_size = 6,
                                                      core = list(fg_params=list(fontface="bold",
                                                                                 hjust = 0.5, x=0.5, col="black")),
                                                      rowhead = list(fg_params=list(parse=TRUE,
                                                                                    fontface="bold",
                                                                                    hjust = 0.5, x=0.5, col="black"))))

    #add out border
    sidebox.grob <- gtable_add_grob(sidebox.grob,
                                 grobs = rectGrob(gp = gpar(fill = NA,
                                                            lwd = 0.5)),
                                 t = 2,
                                 r = 2, l = ncol(sidebox.grob),
                                 b = nrow(sidebox.grob))

    ################# Create grob for legend #################
    legend <- create.legend(method = method)

    ################# Final output #################
    pp <- grid.arrange(plot,
                       legend,
                       sidebox.grob,
                       heights = c(6/10, 4/10),
                       widths = c(4/5, 1/5),
                       layout_matrix = rbind(c(1, 2),
                                             c(1, 3)))
    return(pp)
  }


  else{

    # colors <- c("#81C784",
    #             "#64B5F6",
    #             "#E57373",
    #             "#BA68C8",
    #             "grey",
    #             "grey",
    #             "gray97")

    plot <- ggplot(tab.store, aes(factor(tab.store$method, levels = c("3+3","BOIN","i3+3")),
                                  factor(tab.store$index))) +
      geom_tile(aes(fill = tab.store$Decision),
                color="white",
                width=1, height=1) +
      geom_text(aes(label = tab.store$Decision),
                size=2) +
      geom_text(aes(label = tab.store$condition),
                size=2,
                vjust=2.5,
                parse=T) +
      facet_grid(rows = vars(factor(tab.store$y, levels=c(npatients:0))), cols = vars(tab.store$n),
                 scales = "free",
                 space = "free",
                 switch = "both") +
      labs(x = expression(paste("Number of patients treated at the current dose (",n[d],")")),
           y = expression(paste("Number of patients with DLT (",y[d],")")),
           title = paste0("Decision table for the ", paste(method, collapse = ", "), " designs"),
           subtitle = expression(paste("Dose ",italic(d)," is the current dose"))) +
      theme(plot.title = element_text(size=10),
            plot.subtitle = element_text(size=8),
            plot.title.position = "plot",
            plot.margin = unit(c(3,0,0,2),"mm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = 'white', colour = 'grey', linewidth = 0.1),
            panel.spacing.x=unit(0, "lines"),
            panel.spacing.y=unit(0, "lines"),
            axis.ticks = element_blank(),
            axis.text.x = element_text(size=6),
            axis.line = element_line(colour = "black", linewidth = 0.3),
            axis.title=element_text(size=6),
            legend.position="none",
            strip.placement = "bottom",
            strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF"),
            strip.text.y.left = element_text(angle = 0))+
      scale_y_discrete(labels = NULL)+
      scale_fill_manual(values = colors)

    ############################ Add features ############################

    if ("BOIN" %in% method){

      sidebox <- data.frame(`BOIN` = c(setup.store$`BOIN`$pT,
                                       paste0("[", setup.store$`BOIN`$EI[1], ", ", setup.store$`BOIN`$EI[2],")"),
                                       paste0(round(setup.store$`BOIN`$boundary[1],3), ", ", round(setup.store$`BOIN`$boundary[2],3))),
                            check.names = F)


      if ("i3+3" %in% method){
        sidebox <- cbind(sidebox,
                      data.frame(`i3+3` = c(setup.store$`i3+3`$pT,
                                            paste0("[", setup.store$`i3+3`$EI[1], ", ", setup.store$`i3+3`$EI[2],"]"),
                                            paste0(setup.store$`i3+3`$EI[1], ", ", setup.store$`i3+3`$EI[2])),
                                 check.names = F))
      }
    }
    else{
      sidebox <- data.frame(`i3+3` = c(setup.store$`i3+3`$pT,
                                       paste0("[", setup.store$`i3+3`$EI[1], ", ", setup.store$`i3+3`$EI[2],"]"),
                                       paste0(setup.store$`i3+3`$EI[1], ", ", setup.store$`i3+3`$EI[2])),
                            check.names = F)
    }

    ################# Create grob for info table #################
    sidebox.grob <- tableGrob(sidebox, theme=ttheme_default(base_size = 6,
                                                      core = list(fg_params=list(fontface="bold",
                                                                                 hjust = 0.5, x=0.5, col="black")),
                                                      rowhead = list(fg_params=list(parse=TRUE,
                                                                                    fontface="bold",
                                                                                    hjust = 0.5, x=0.5, col="black"))))

    #add out border
    sidebox.grob <- gtable_add_grob(sidebox.grob,
                                 grobs = rectGrob(gp = gpar(fill = NA,
                                                            lwd = 0.5)),
                                 t = 2,
                                 r = 2, l = ncol(sidebox.grob),
                                 b = nrow(sidebox.grob))



      ################# Create grob for legend #################
      legend <- create.legend(method = method)

    pp <- grid.arrange(plot,
                       legend,
                       sidebox.grob,
                       heights = c(6/10, 4/10),
                       widths = c(4/5, 1/5),
                       layout_matrix = rbind(c(1, 2),
                                             c(1, 3)))


    return(pp)

  }

}
