# function to generate predictions based on given tree
predtree = function(newdata, resp, res, data){
  
  # splitting rules from tree:
  rules = data.frame(t(sapply(res$split.rule[-1 ], function(x){scan(text = x, what = "" , quiet = TRUE, )}, USE.NAMES = FALSE)), res$iter[-1], stringsAsFactors = FALSE)
  names(rules) = c("feat", "operator", "value", "iter")
  
  for(k in 1:nrow(newdata)){
    
    ii=1
    stoploop = FALSE
    while(!stoploop){
      if(compare(newdata[k, rules[ii, "feat"]], rules[ii, "value"],  (rules[ii, "operator"]))) {
        if(rules$iter[ii+1] == rules$iter[ii]) stoploop = TRUE else ii = ii + 1
      } else {
        ii = which(rules$feat == rules$feat[ii] & rules$iter == rules$iter[ii])[which(rules$feat == rules$feat[ii] & rules$iter == rules$iter[ii]) > ii][1]
      }

      if(rules$iter[ii+1] < rules$iter[ii] | ii == nrow(rules)) stoploop = TRUE
    }
    
    if(class(data[, resp]) == "factor"){
      newdata[,resp][k] = if(res$prob[ii+1] > 0.5) levels(data[, resp])[1] else { if(res$prob[ii+1] == 0.5) levels(data[, resp])[sample(x = 2, size = 1, prob = c(0.5, 0.5))] else levels(data[, resp])[2]}
    } else {
      newdata[,resp][k] = res$mean[ii+1]
    }
  }
  return(newdata)
}

# helper function:
compare = function(x1,value, operator){ 
  if(operator == "=") operator = "=="
  if(!is.na(suppressWarnings(as.numeric(value)))) value = as.numeric(value)
  res = getFunction(operator)(x1,value); res }

na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }



# this function calculates the out of bag error based on predictions using the vaues of features in 'newdata'

ooberror = function(newdata){
  
  # compute predictions for each obs in the dataset
  for (obs in 1:nrow(data)){
    # for obs i, gather list of trees that weren't built from this obs 
    # only those trees will be used to predict
    ifoob = sapply(index_list, function(index){
      !(obs %in% index)
    })
    
    if(class(data[, resp]) == "factor"){
      # for classification:
      pred_obs = lapply(1:length(tree_list), function(k){
        # generate prediction from tree k if it didn't contain current obs
        if(ifoob[k] == TRUE) data.frame(resp = predtree(newdata = newdata[obs, ], resp = resp, res = tree_list[[k]]$output, data = data)[,resp]) else NA
      })
      pred_obs = na.omit.list(pred_obs)
      pred_obs = do.call(rbind, pred_obs)
      
      # if no prediction was generated from any tree, set to NA
      if(is.null(pred_obs)) pred[obs] = NA else{
        # other wise, find the majority vote
        pred[obs] = if(table(pred_obs)[1] != table(pred_obs)[2]){
          levels(pred_obs$resp)[which.max(table(pred_obs))]
        } else {
          # If there's no majority, pick randomly with equal probabilities
          levels(pred_obs$resp)[sample(x = 2, size = 1, prob = c(0.5, 0.5))]
        } 
      }
    } else {
      # for regression:
      # generate prediction from tree k if it didn't contain current obs
      pred_obs = sapply(1:length(tree_list), function(k){
        if(ifoob[k] == TRUE) predtree(newdata = newdata[obs, ], resp = resp, res = tree_list[[k]]$output, data = data)[,resp] else NA
      })
      
      # if no prediction was generated from any tree, set to NA. Otherwise, compute the average of predictions
      if (all(is.na(pred_obs))) pred[obs] = NA else pred[obs]  = mean(pred_obs, na.rm = TRUE)
      
    }
    
  }
  if(class(data[, resp]) == "factor"){
    # dataframe containing predicted and observed values:
    comb = data.frame(pred, observed = data[,resp])
    comb = na.omit(comb)
    
    # find the rate of missclassification for classification trees:
    error = mean(comb$pred != comb$observed)
  } else {
    # compute rmse for regression trees:
    error = sqrt(mean( (pred - data[,resp])^2, na.rm = TRUE))
  }
  
  return(error)
}
