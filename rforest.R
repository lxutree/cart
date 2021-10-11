# This is the function for random forrest
# The bulk is identical to the function 'bagg' for bagging. 
#  - The fundamental difference is that with random forest, a random subset of features are selected to split each node in a tree. 
#  - The random selection of features is done through the argument "type = 'rf'", which tells the function to randomly select a subset of features at each split

rforest <- function(data, resp, n.boot = 10, min.obs, ...){
  # start by drawing 'n.boot' bootstrap samples:
  feat = names(data)[names(data)!=resp]
  data_list = index_list = list()
  pred = c()
  
  for (i in 1:n.boot){
    # index of bootstrap sample:
    index_list[[i]] = sample(nrow(data),  replace = TRUE)
    
    # ith bootstrap sample:
    data_list[[i]] =data[index_list[[i]], ]
    
  }
  
  # list of trees for each bootstrap sample
  tree_list = lapply(data_list, function(data_i){
    if(class(data_i[, resp]) == "factor"){
      # !!! type = 'rf' for random forest
      classtree(data = data_i, resp = resp, min.obs = min.obs, type = "rf")
    } else {
      # !!! type = 'rf' for random forest
      regtree(data = data_i, resp = resp, min.obs = min.obs, type = "rf")
    }
  })
  
  # creating list of datasets with shuffled values for one feature at a time:
  data.temp = data[ , feat]
  newdata_list = lapply(1:length(feat), function(i){
    data.temp[ , feat[i]] = sample(data[ , feat[i]], replace = FALSE)
    data.temp[, resp] = data[, resp]
    data.temp
  })
  
  environment(ooberror) <- environment()
  
  # missclassification error or rmse based the original values of the features
  error_original = ooberror(newdata = data)
  error_shuffle = sapply(1:length(feat), function(i){
    ooberror(newdata = newdata_list[[i]])
  })
  
  # missclassification error or rmse based the randomly shuffled values of the features
  diff_error = error_shuffle - error_original
  names(diff_error) = feat
  diff_error = sort(diff_error, decreasing = TRUE)

  # Obtaining measures of varianble importance, then sorting from most important -> least important
  var_rank_mean = apply(sapply(tree_list, function(tree){ tree$var_rank$criterion}), MARGIN = 1, FUN = mean)
  names(var_rank_mean) = feat
  rank = sort(var_rank_mean, decreasing = TRUE)
  
  return(list(error = error_original, "Importance by difference" = rank, "Importance with shuffle" = diff_error))
}

