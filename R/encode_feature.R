
encode_feature <- function(data,feature,encoding,is_range_lookup,is_ordered){

  # Error handling
  #================================================================
    # Output warning is is_ordered=TRUE and encoded values column is a character
    #----------------------------------

  # Data prep
  #================================================================
    # Ensure encoding is a dataframe
    #----------------------------------
      encoding <- as.data.frame(encoding)

    # If encoding is non-ordered then ensure values are character
    #----------------------------------
      if(!isTRUE(is_ordered)){
        encoding[,2] <- as.character(encoding[,2])
      }

  # Data processing
  #================================================================
    # Work out the index of the encoded value to apply
    #----------------------------------
      if (isTRUE(is_range_lookup)){
        encoding_index <- findInterval(data[,feature],encoding[,1],rightmost.closed = FALSE, all.inside = FALSE)
      }else{
        encoding_index <- match(data[,feature],encoding[,1],nomatch=NA)
      }

    # Get the encoded value
    #----------------------------------
      encoded_feature <- sapply(encoding_index,function(i) encoding[i,2])

  # Output
  #================================================================
    return(encoded_feature)

}


# encoding <- cbind.data.frame(
#   lookup_from = c(1,4,5,10)
#   ,encoding   = c(3,4,5,99)
# )
#
# data <- cbind.data.frame(
#   FEAT = c(1,2,3,4,5,6,7,8,12)
# )
#
# feature = "FEAT"
# is_range_lookup = FALSE
# is_ordered = FALSE
