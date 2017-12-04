#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' This is the top bit of text
#'
#'
#' @param df Dataframe
#' @param Var Variable
#' @param weight Weight
#' @param unitpred Per Unit Weight Prediction
#' @param response Response field
#' @param LUTable Lookup table, Default: NA
#' @param LUType Lookup type, Default: 'Direct'
#' @param LUOrdered IS LU Ordered?, Default: 'N'
#' @return Plot in plot
#' @details Some details here
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname Plot_Obs_v_Fitted
#' @export
#'

###############################################################################
#------------------------------------------------------------------------------
# Script : Plot_Obs_v_Fitted.R
#------------------------------------------------------------------------------
# Description : Takes a data frame, and creates a plot of the observed vs
#               fitted values of a specified metric, for a specified feature.
#               A format can be applied to the feature if desired.
#------------------------------------------------------------------------------
# Change History :
#   [2015/12/27 [Tom Snowdon] [Created]
#   [2016/06/03 [Tom Snowdon] [Replaced tapplys with dplyr summary]
#------------------------------------------------------------------------------
###############################################################################

Plot_Obs_v_Fitted<- function(df,Var,weight,unitpred,response,LUTable=NA,LUType="Direct",LUOrdered="N"){
  return(1)
}




