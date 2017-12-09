#' @title Plot a graph of observed vs predicted values across a feature.
#' @description This function takes a dataset with associated predictions.
#' For a specified feature, a plot of the observed vs the predicted values is produced.
#'
#' @param data name of the data frame to evaluate
#' @param prediction prediction per unit weight
#' @param actual name of the column containing the weights
#' @param weight name of the column containing the weights
#' @param feature name of the column containing the feature of interest
#' @return a (ggplot2) plot is returned
#' @details Some details here
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname plot_actual_vs_predicted
#' @export
#'
#=============================================================================
# Change History :
#   [2017/12/06 [Tom Snowdon] [Created]
#=============================================================================

plot_actual_vs_predicted <- function(data,actual,prediction,weight,feature){

  # Error Handling
  #===============================================
    # Force data into a data frame
    #------------------------------------
      data <- as.data.frame(data)

  # Calculate key stats by each level of feature
  #===============================================
    #---Copy each featureiable to save complicated dplyr references--# ###Should be removed in future.###
      data[,"weight"] <- data[,weight]
      data[,"prediction"] <- data[,prediction]
      data[,"actual"] <- data[,actual]
      data[,"feature"] <- data[,feature]

    #--Preparatory calculations--#
      data[,"Weighted_Pred"] <- data[,prediction]*data[,weight]
      TotalWeight <- sum(data[,weight])

    #--Summaries--# ###Is this nececcary, or can we just use sufficient stats when calling ggplot?
      data_summarised <- data %>%
        group_by(feature) %>%
          summarise(
            weight_sum = sum(weight),
            actuals_mean = sum(actual)/weight_sum,
            preds_mean = sum(Weighted_Pred)/weight_sum,
            weight_perc = sum(weight)/TotalWeight
          )
      data_summarised <- as.data.frame(data_summarised) ###Convert to dataframe to work with other functions below###

  # Plot Obs vs Predicted
  #=======================
    # Cals to help with axes on the plot
    #------------------------------------
      MinPreds <- min(subset(data_summarised[,"preds_mean"],!is.na(data_summarised[,"preds_mean"])))
      MinActuals <- min(subset(data_summarised[,"actuals_mean"],!is.na(data_summarised[,"actuals_mean"])))
      MinWeight <- min(subset(data_summarised[,"weight_perc"],!is.na(data_summarised[,"weight_perc"])))

      MaxPreds <- max(subset(data_summarised[,"preds_mean"],!is.na(data_summarised[,"preds_mean"])))
      MaxActuals <- max(subset(data_summarised[,"actuals_mean"],!is.na(data_summarised[,"actuals_mean"])))
      MaxWeight <- max(subset(data_summarised[,"weight_perc"],!is.na(data_summarised[,"weight_perc"])))

      MeanPreds <- mean(subset(data_summarised[,"preds_mean"],!is.na(data_summarised[,"preds_mean"])))
      MeanActuals <- mean(subset(data_summarised[,"actuals_mean"],!is.na(data_summarised[,"actuals_mean"])))
      MeanWeight <- mean(subset(data_summarised[,"weight_perc"],!is.na(data_summarised[,"weight_perc"])))

    # Plot
    #------------------------------------
      barplot(data_summarised[,"weight_perc"],names.arg=data_summarised[,"feature"],ylim=c(0,MaxWeight*3),col="light yellow",main=paste("Obs vs Fitted - ",feature,sep=""),xlab=feature,ylab="Exposure")
      par(new=TRUE)
      plot(data_summarised[,"preds_mean"],type="l",axes=FALSE,xlab=feature,ylab="",pch="18",col="dark green",ylim=c(min(MinActuals,MinPreds)-0.2*MeanActuals,max(MaxActuals,MaxPreds)+0.2*MeanActuals))
      axis(side=4)
      points(data_summarised[,"preds_mean"],col="dark green",pch=18)
      points(data_summarised[,"actuals_mean"],type="l",col="violet")
      points(data_summarised[,"actuals_mean"],col="violet",pch=20)
}


# test <- cbind.data.frame(
#   Obs = c(0,3,2,2)
#   ,Pred = c(0.5,1.5,1.8,2.2)
#   ,Weight = c(0.5,0.5,1,1)
#   ,Feat = c(1,1,2,3)
# )
#
# plot_actual_vs_predicted(
#   data = test
#   ,actual = "Obs"
#   ,prediction = "Pred"
#   ,weight = "Weight"
#   ,feature = "Feat"
# )





