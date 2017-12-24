
decode_feature <- function(data,feature,mapping){

  # Data prep
  #================================================================
    # Ensure mapping is a dataframe
    #----------------------------------
      mapping <- as.data.frame(mapping)
      
  # Data processing
  #================================================================    
    # Get the decoded value
    #---------------------------------- 
      decoded_feature <- sapply(data[,feature],function(i) mapping[i,2])
      
  # Output
  #================================================================  
    return(decoded_feature)
}

    
# feature = "feat"
# 
# data<-cbind.data.frame(
#   feat = c(1,2,3,4,5,8,99)
# )
# 
# mapping <- cbind.data.frame(
#   encoded_level = c(1,2,3,4,5,6,7)
#   ,decoded_value = c(3,4,5,6,7,8,9)
# )      
      