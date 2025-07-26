#'
#' Generate the plot(s) displaying operating characteristics for a single or multiple designs
#'
#' Generate the plot(s) displaying operating characteristics for a single or multiple designs
#'
#' @usage generate_oc_plot(`3+3` = NULL,
#'                         `BOIN` = NULL,
#'                         `mTPI2` = NULL,
#'                         `i3+3` = NULL,
#'                         `G3` = NULL)
#'
#' @param 3+3 the object returned by run.sim.3()
#' @param BOIN the object returned by run.sim.boin()
#' @param mTPI2 the object returned by run.sim.mtpi2()
#' @param i3+3 the object returned by run.sim.i3()
#' @param G3 the object returned by run.sim.g3()
#'
#' @return \code{generate_oc_plot()} returns figures displaying the operating characteristics for the user-specified design(s).
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


generate_oc_plot <- function(`3+3` = NULL,
                             `BOIN` = NULL,
                             `mTPI2` = NULL,
                             `i3+3` = NULL,
                             `G3` = NULL){

  method <- c("3+3","BOIN","mTPI2","i3+3","G3")[c(!is.null(`3+3`),
                                          !is.null(`BOIN`),
                                          !is.null(`mTPI2`),
                                          !is.null(`i3+3`),
                                          !is.null(`G3`))]

  # Check
  if(length(method) != sum(!is.null(`3+3`),!is.null(`BOIN`),!is.null(`mTPI2`),!is.null(`i3+3`), !is.null(`G3`))){
    stop("Warnings: Please double check the input(s)!")
  }
  if("3+3" %in% method & (is.null(`3+3`))){
    stop("Warnings: Please double check the input(s)!")
  }
  if("BOIN" %in% method & (is.null(`BOIN`))){
    stop("Warnings: Please double check the input(s)!")
  }
  if("mTPI2" %in% method & (is.null(`mTPI2`))){
    stop("Warnings: Please double check the input(s)!")
  }
  if("i3+3" %in% method & (is.null(`i3+3`))){
    stop("Warnings: Please double check the input(s)!")
  }
  if("G3" %in% method & (is.null(`G3`))){
    stop("Warnings: Please double check the input(s)!")
  }


  result.store <- list("selection" = rbind(`3+3`$selection,
                                           `BOIN`$selection,
                                           `mTPI2`$selection,
                                           `i3+3`$selection,
                                           `G3`$selection),
                       "allocation" = rbind(`3+3`$allocation,
                                         `BOIN`$allocation,
                                         `mTPI2`$allocation,
                                         `i3+3`$allocation,
                                         `G3`$allocation))

  result.store[[1]]$Method <- rep(method, each = nrow(result.store[[1]])/length(method))
  result.store[[2]]$Method <- rep(method, each = nrow(result.store[[2]])/length(method))

  setup.store <- list(`3+3` = `3+3`$setup,
                      `BOIN` = `BOIN`$setup,
                      `mTPI2` = `mTPI2`$setup,
                      `i3+3` = `i3+3`$setup,
                      `G3` = `G3`$setup)

  plot.store <- list("selection" = NULL,
                     "allocation" = NULL)

  for (j in 1:2){

    result <- as.data.frame(result.store[[j]])

    result <- result[,c("Scenario", "Method", "Overdose.Perc", "Correctdose.Perc", "Underdose.Perc")]

    ################# Add title #################
    purals <- ifelse(length(method) == 1, " design", " designs")
    titleName <- c("Operating characteristics (dose selection)",
                   "Operating characteristics (participant allocation)")

    ################# Reformat data #################
    #Melt data
    result_ <- stats::reshape(result,
                       varying = list(c("Overdose.Perc", "Correctdose.Perc", "Underdose.Perc")),
                       v.names = "Percent",
                       timevar = "Metric",
                       times = c("Overdose.Perc", "Correctdose.Perc", "Underdose.Perc"),
                       direction = "long")

    #Add % to percentages
    result_$PercentLabel <- ifelse(result_$Percent > 0, paste0("",result_$Percent, "%"), " ")

    #Add labels to indicate PUA, PCA, etc.
    if (j == 1){
      result_$MetricLabel <- ifelse(result_$Metric == "Overdose.Perc", "Percentage of overdosing selection (POS)",
                                   ifelse(result_$Metric == "Correctdose.Perc", "Percentage of correct selection (PCS)",
                                          ifelse(result_$Metric == "Underdose.Perc", "Percentage of underdosing selection (PUS)", NA)))
      result_$MetricLabel <- factor(result_$MetricLabel, levels = c("Percentage of overdosing selection (POS)",
                                                                  "Percentage of correct selection (PCS)",
                                                                  "Percentage of underdosing selection (PUS)"))
    }
    else if (j == 2){
      result_$MetricLabel <- ifelse(result_$Metric == "Overdose.Perc", "Percentage of overdosing allocation (POA)",
                                   ifelse(result_$Metric == "Correctdose.Perc", "Percentage of correct allocation (PCA)",
                                          ifelse(result_$Metric == "Underdose.Perc", "Percentage of underdosing allocation (PUA)", NA)))
      result_$MetricLabel <- factor(result_$MetricLabel, levels = c("Percentage of overdosing allocation (POA)",
                                                                  "Percentage of correct allocation (PCA)",
                                                                  "Percentage of underdosing allocation (PUA)"))
    }

    ################# Add facet labels #################
    nscene <- max(result$Scenario)
    facetLabel <- paste0("Scenario ",1:nscene)
    names(facetLabel) <- 1:nscene
    facetLabel <- as_labeller(facetLabel)

    ################# Positions #################
    result_ <- result_ %>% group_by(.data$Method, .data$Scenario) %>%
      mutate(PercentPos = (100-cumsum(.data$Percent) + 0.5*.data$Percent))

    ################# Order methods: 3+3, i3+3, BOIN, mTPI2, G3 #################
    result_$Method <- factor(result_$Method,
                             levels = c("3+3","BOIN","mTPI2","i3+3","G3"))

    ############################# Main plot #############################

    plot <- ggplot(data=result_, aes(x=result_$Method, y=result_$Percent, fill=result_$MetricLabel)) +
      geom_bar(stat="identity", width=0.3, position = position_stack(reverse = FALSE))+
      geom_text(aes(y = result_$PercentPos,
                    label = result_$PercentLabel),
                color="black", size=3)+
      facet_wrap(~ result_$Scenario, labeller = facetLabel, ncol = 2, scales = "free") +
      labs(x = " ",
           y = "Percent",
           title = paste0(titleName[j]," for the ", paste(method, collapse = ", "), purals))+
      theme(plot.title = element_text(size=10),
            plot.title.position = "plot",
            plot.caption = element_text(color = "blue", face = "italic"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "grey80"),
            strip.background =element_rect(fill="grey70"),
            axis.ticks = element_line(colour = "black", size = 0.1),
            axis.line = element_line(colour = "black", linewidth = 0.1),
            axis.title = element_text(size=8),
            text=element_text(size=8),
            legend.title= element_blank(),
            legend.text = element_text(size=8),
            legend.key.size = unit(3, 'mm'),
            legend.position="top")+
      guides(fill=guide_legend(nrow=1, byrow=TRUE)) +
      scale_fill_manual(values = c("#FF8A65", "#81C784", "#64B5F6"))


      ################# Create grob for sidebox #################
      sidebox.grob <- add_features(param = setup.store,
                                   method = method)


    ################# Final output #################
    plot.store[[j]] <- grid.arrange(plot,
                                    sidebox.grob,
                                    widths = c(6/10, 4/10),
                                    ncol = 2)
  }


  return(plot.store)
}

