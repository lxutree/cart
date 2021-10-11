# This is the implementation of boosting

boost = function(data, resp, depth, iter, min.obs, rate, ...){
  # depth = maximum of terminal nodes allowed. This is equal to the number of splits + 1
  # iter = number of iterations
  
  feat = names(data)[names(data)!=resp]
  
  # for continuous response:
  if (!is.factor(data[,resp])){
    
    # empty lists:
    pred_list = tree_list = importance_list = resid_list = list()
    
    # set first predictions to be the mean of the response
    pred_list[[1]] = rep(mean(data[, resp]), nrow(data))
    
    # find the corresponding residuals
    resid_list[[1]] = data[, resp] - pred_list[[1]]
    
    # start loop:
    for (i in 1:iter){
      
      # build tree using current residuals as response:
      tree = regtree(data = data.frame(data[,feat], resid = resid_list[[i]]), min.obs = min.obs, resp = "resid", depth = depth)
      
      # store outputs
      tree_list[[i]] = tree$output
      
      # generate the predicted residuals
      pred_resid = predtree(newdata = data, resp = resp, res = tree$output, data = data)[,resp]
      
      # generate new predictions by adding the predicted residuals multiplied by a scare factor 'rate'
      pred_list[[i+1]] =  pred_list[[i]] + pred_resid*rate 
      
      # generate new residuals for the next iteration
      resid_list[[i + 1]] = data[, resp] - pred_list[[i+1]]
      
      # store measure of variable importance
      importance_list[[i]] = tree$var_rank
    }
    
    # average performance of each feature across all trees 
    importance = apply(do.call(cbind, importance_list), MARGIN = 1, mean)
    
    return(list(pred = pred_list[[iter+1]], importance = data.frame(var = names(importance), influence = sort(importance, decreasing = TRUE)), trees = tree_list, resp = resp, data = data, rate = rate))
  }
}


boost_pred = function(newdata, boost_object){
  trees = boost_object$trees
  resp = boost_object$resp
  rate = boost_object$rate
  iter = length(trees)
  data = boost_object$data
  pred_list = resid_list = list()

  pred_list[[1]] = rep(mean(data[, resp]), nrow(newdata))

  resid_list[[1]] = newdata[, resp] - pred_list[[1]]
  
  for (i in 1:iter){
    pred_resid = predtree(newdata = newdata, resp = resp, res = trees[[i]], data = data)[,resp]
    
    pred_list[[i+1]] =  pred_list[[i]] + pred_resid*rate 
    
    resid_list[[i + 1]] = newdata[, resp] - pred_list[[i+1]]
  }
  
  return(pred = pred_list[[iter+1]])
}

