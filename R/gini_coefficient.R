
gini_coefficient <- function(data, actual, prediction, weight){
  df = as.data.frame(data)
  df$actual = df[,actual]
  df$prediction = df[,prediction]
  df$weight = df[,weight]


  df <- df[order(df$predicted, decreasing = TRUE),]
  df$random = cumsum((df$weight/sum(df$weight)))
  totalPositive <- sum(df$actual * df$weight)
  df$cumPosFound <- cumsum(df$actual * df$weight)
  df$Lorentz <- df$cumPosFound / totalPositive
  n <- nrow(df)
  gini <- sum(df$Lorentz[-1]*df$random[-n]) - sum(df$Lorentz[-n]*df$random[-1])
  return(gini)
}

var11 <- c(1, 2, 5, 4, 3)
pred <- c(0.1, 0.4, 0.3, 1.2, 0.0)
target <- c(0, 0, 1, 0, 1)

data=as.data.frame(var11,pred,target)

gini_coefficient(data=data,actual="target",prediction="pred",weight="var11")

