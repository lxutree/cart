regtree <- function(data, resp, min.obs, feat = NULL, nfeat =NULL, type = NULL, depth = NULL){
  # nfeat = number of features selected at random at each split
  # feat = list of features 
  # depth = maximum of terminal nodes allowed. This is equal to the number of splits + 1
  
  
  # minimum size of leaves?
  leafsize = min.obs/3 # same as the default in rpart
  
  # data.frame to store results:
  output = data.frame(status = "split", count = nrow(data), "split rule" = "root", iter = 0, mean = mean(data[, resp]), stringsAsFactors = FALSE)
  # - status: 
  #   - "split" to be split in the next iteration 
  #   - "parent" nodes lead to further splits
  #   - "leaf" no more splitting to be done
  # - count: number of observations in the current branch
  # - split rule: how the splits were done
  # - iter: how many iterations so far?
  
  # list of observations in each node/leaf:
  data.list = list()
  
  # data at the root:
  data.list[[1]] = data
  
  # indicator for whether the tree keeps growing:
  stopsplit = FALSE
  
  iter = 1
  
  # list of features:
  if(is.null(feat)) feat = allfeat = names(data)[names(data)!=resp]
  if(is.null(nfeat)) nfeat = round(sqrt(length(feat)))
  
  # vector of features/gini index reduction for each split:
  feat_vec = c()
  diff_vec = c()
  
  # iterative process:
  while(!stopsplit) {
    
    # list of splits to be done:
    split.queue = which(output$status == "split")
    
    for (j in split.queue[1]) {
      # empty vector 
      weighted.sd = c()
      error = c()
      split_val = c()
      
      data.temp = data.list[[j]]
      
      # for random forrest
      if(!is.null(type)){if(type == "rf") feat = sample(allfeat, size = nfeat, replace = FALSE)}
      
      
      for (i in 1:length(feat)){
        data_sub = data.frame(var = data.temp[, feat[i]], resp = data.temp[, resp])
        
        # gini index of parent node
        rss_parent = sum((data_sub$resp - mean(data_sub$resp))^2)
        
        # calculating sse for categorical feature:
        if( is.factor(data_sub$var) ) {
          data.split = split(data_sub, data_sub$var)
          error[i] = sum(sapply(data.split, function(x){
            sum( (x$resp - mean(x$resp)) ^ 2 )
          })) 
          
          # checking size of children nodes; if less than specified, the split is not considered
          count_min = min(sapply(data.split, nrow))
          if( count_min < leafsize) error[i] = NA

        } else {
        # calculating gini index for continuous feature:
          splits_sort = sort(unique(data_sub$var)) # all possible splits
          sse <- c() 
          
          # calculating sse for all possible splits
          for( k in 1:length(splits_sort)){
            sse[k] = sum( (data_sub$resp[data_sub$var < splits_sort[k]] - mean(data_sub$resp[data_sub$var < splits_sort[k]]) )^2 ) + sum( (data_sub$resp[data_sub$var >= splits_sort[k]] - mean(data_sub$resp[data_sub$var >= splits_sort[k]]) )^2 ) 
            
            # checking size of children nodes; if less than specified, the split is not considered
            count_min = min(length(data_sub$resp[data_sub$var < splits_sort[k]]), length(data_sub$resp[data_sub$var >= splits_sort[k]]))
            if(count_min < round(leafsize)) sse[k] = NA
          }
          
          # clean up for when none of the splits is valid:
          if(all(is.na(sse))) {error[i] = NA; split_val[i] = NA} else { error[i] = min(sse, na.rm = TRUE); split_val[i] = splits_sort[which.min(sse)]}
        }
      }
      
      # characteristics of the current split
      splitvar = feat[which.min(error)] # feature leading to the lowest sse
      rss_diff = rss_parent - min(error, na.rm = TRUE) # difference in rss
      feat_vec = c(feat_vec, splitvar) # recorded in vector
      diff_vec = c(diff_vec, rss_diff) # recorded in vector
      
      # creating children nodes:
      if( is.factor(data.temp[[splitvar]])) {
        # for categorical feature:
        data.next = split(data.temp, data.temp[ , splitvar])
      } else {
        # for continuous feature:
          value = split_val[which.min(error)]
          index = which(sort(unique(data.temp[[splitvar]])) == value)
          # taking the middle point of unique values as the splitting point to be consistent with 'rpart':
          value = (sort(unique(data.temp[[splitvar]]))[index] + sort(unique(data.temp[[splitvar]]))[index-1])/2
          data.next = list()
          data.next[[1]] = data.temp[which(data.temp[[splitvar]] < value), ]
          data.next[[2]] = data.temp[which(data.temp[[splitvar]] > value), ]
      }
      

      # Stopping criteria: 
      # - less than 3 observations
      # - all observations have the same label
      status = sapply(data.next, function(x){
        if (ncol(x) > 2) {
          if (nrow(x) < min.obs | nrow( unique(x[, -which(names(x) %in% resp)]) ) == 1) status = "leaf" else status = "split" 
        } else status = "leaf"
        
        status
      })
      
      # change current status from 'split' to 'parent' so it won't be split further:
      output$status[j] = "parent"
      
      # record how the split was done:
      if( is.factor(data.temp[[splitvar]]) ) {
        splitrule = sapply(names(data.next), function(x){paste(splitvar, "=" , x)})
      } else {
        splitrule = c(paste(splitvar, "<", value),paste(splitvar, ">", value) )
      }

      # creating outputs
      temp.output = data.frame(status = status, count = sapply(data.next, nrow), "split rule" = splitrule, iter = iter, row.names = NULL, mean = sapply(data.next, function(x){mean(x[[resp]])}))
      
      # attach new outputs to existing dataframe
      output = rbind(output[1:j,], temp.output, output[-c(1:j), ])
      names(data.next) = NULL; data.list = c(data.list[1:j], data.next, data.list[-c(1:j)])
    }
    
    # check if there are remaining splits to be done:
    if(all(output$status != "split")) stopsplit = TRUE
    
    # to control the depth of the tree
    if(!is.null(depth)) {if (length(feat_vec) + 1 >= depth) stopsplit = TRUE  }
    
    iter = iter+1
  }
  
  # summing up RSS difference for each feature 
  rss_sum = c()
  for (i in 1:length(allfeat)){
    rss_sum[i] = sum(diff_vec[which(feat_vec == allfeat[i])])
  }
  
  return(list(output = output, var_rank = data.frame(criterion = rss_sum, row.names = allfeat)))
}

