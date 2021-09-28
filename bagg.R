# this is the function for boostrap aggregation

bagg <- function(data, resp, n.boot = 10, min.obs, ...){
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
      classtree(data = data_i, resp = resp, min.obs = min.obs)
    } else {
      regtree(data = data_i, resp = resp, min.obs = min.obs)
    }
  })
  
  # compute the average value/majority vote for each obs
  for (obs in 1:nrow(data)){
    # list of trees that weren't built from this obs
    ifoob = sapply(index_list, function(index){
      !(obs %in% index)
    })
    
    if(class(data[, resp]) == "factor"){
      # for classification:
      pred_obs = lapply(1:length(tree_list), function(k){
        if(ifoob[k] == TRUE) data.frame(resp = predtree(newdata = data[obs, ], resp = resp, res = tree_list[[k]]$output, data = data)[,resp]) else NA
      })
      pred_obs = na.omit.list(pred_obs)
      pred_obs = do.call(rbind, pred_obs)
      
      if(is.null(pred_obs)) pred[obs] = NA else{
        # find the majority vote
        pred[obs] = if(table(pred_obs)[1] != table(pred_obs)[2]){
          levels(pred_obs$resp)[which.max(table(pred_obs))]
        } else {
        # If there's no majority, pick randomly
          levels(pred_obs$resp)[sample(x = 2, size = 1, prob = c(0.5, 0.5))]
        } 
      }
    } else {
      # for regression:
      pred_obs = sapply(1:length(tree_list), function(k){
        if(ifoob[k] == TRUE) predtree(newdata = data[obs, ], resp = resp, res = tree_list[[k]]$output, data = data)[,resp] else NA
      })
      
      # compute the average of predictions
      if (all(is.na(pred_obs))) pred[obs] = NA else pred[obs]  = mean(pred_obs, na.rm = TRUE)
      
    }
    
  }
  if(class(data[, resp]) == "factor"){
    comb = data.frame(pred, observed= data[,resp])
    comb = na.omit(comb)
    
    # find the rate of missclassification
    error = mean(comb$pred != comb$observed)
  } else {
    # compute rmse
    error = sqrt(mean( (pred - data[,resp])^2, na.rm = TRUE))
  }
  
  var_rank_mean = apply(sapply(tree_list, function(tree){ tree$var_rank$criterion}), MARGIN = 1, FUN = mean)
  names(var_rank_mean) = feat
  rank = sort(var_rank_mean, decreasing = TRUE)
   return(list(error = error, "Importance Rank" = rank ))
}



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
