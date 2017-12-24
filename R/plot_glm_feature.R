###############################################################################
#------------------------------------------------------------------------------
# Script : Plot_GLM_Factor.R
#------------------------------------------------------------------------------
# Description : Plot GLM Factor Parameters & SEs. Rescale only works for 
#------------------------------------------------------------------------------
# Change History :
#   [2016/03/10] [Tom Snowdon] [Created]                            
#------------------------------------------------------------------------------
###############################################################################

  plot_glm_feature <- function(glm,feature){
    #=========================================================================#
    # Gather the data we need
    #=========================================================================#
      #-----------------------------------------------------------------------#
      # Extract what's needed from the summary(GLM)$coefficients table
      #-----------------------------------------------------------------------#
        x <- subset(summary(GLM)$coefficients,rownames(summary(GLM)$coefficients) %like% Field)
        #Intercept <- subset(summary(GLM)$coefficients,rownames(summary(GLM)$coefficients) == "(Intercept)")[,"Estimate"]

      #-----------------------------------------------------------------------#
      # Derive what we need for for the plots
      #-----------------------------------------------------------------------#
        #--Betas--#
          Betas <- c(0,x[,"Estimate"])

        #--Standard Errors--#
          TopSEs <- c(NA,x[,"Estimate"] + 2*x[,"Std. Error"])
          BottomSEs <- c(NA,x[,"Estimate"] - 2*x[,"Std. Error"])

          #--For Graphing Limits--#
            LowestSE <- min(tail(BottomSEs,length(BottomSEs)-1))
            HighestSE <- max(tail(TopSEs,length(TopSEs)-1))

        #--Labels--#
          Labels <- c("Intercept",rownames(x))

        #--Weights--#
          Weights <- rep(1,length(Betas))

    #=========================================================================#
    # Make the plot #
    #=========================================================================#
      barplot(Weights,names.arg=Labels,ylim=c(0,max(Weights)*3),col="light yellow",main=paste("Parameters & SEs - ",Field,sep=""),xlab=Field,ylab="Exposure")
      par(new=TRUE)
      plot(Betas,type="l",axes=FALSE,xlab=Field,ylab="",pch="18",col="light green",ylim=c(min(Betas,LowestSE)*1.33,max(Betas,HighestSE)*1.33))
      axis(side=4)
      points(Betas,col="light green",pch=18)
      points(BottomSEs,type="l",col="dark red")
      points(BottomSEs,col="dark red",pch=18)
      points(TopSEs,type="l",col="red")
      points(TopSEs,col="red",pch=18)
      abline(h=0,col="light grey")

  }
