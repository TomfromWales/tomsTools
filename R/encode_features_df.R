encode_features_df <- function(data,encoding_framework){

  for(i in 1:length(encoding_framework)){
    
    # Check if feature exists, throw a warning if not
    #-------------------------------------------------
    
    # Encode feature & add result to the DF
    #---------------------------------------
      feat_name = names(encoding_framework)[i]
      
      data[,paste0(feat_name,"_ENC")] <- encode_feature(
        data = data
        ,feature = feat_name
        ,encoding = encoding_framework[[i]]$encoding
        ,is_range_lookup = encoding_framework[[i]]$is_range_lookup
        ,is_ordered = encoding_framework[[i]]$is_ordered
      )
      
  } 
  return(data)
}
# 
# 
# encoding_framework = list(
#   FEAT = list(
#     encoding = encoding
#     ,is_range_lookup = FALSE
#     ,is_ordered = FALSE
#   )
#   # ,feat2 = list(
#   #   encoding = encoding
#   #   ,is_range_lookup = FALSE
#   #   ,is_ordered = FALSE
#   # ) 
# )
# 
# encode_features_df(data,encoding_framework)

  