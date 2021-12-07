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
      
      # empty objects for later use 
      weighted.sd = c()
      error = c()
      split_val = list()
      
      # load data corresponding to current node
      data.temp = data.list[[j]]
      
      ### for random forrest
      if(!is.null(type)){if(type == "rf") feat = sample(allfeat, size = nfeat, replace = FALSE)}
      
      for (i in 1:length(feat)){ # loop through all features
        data_sub = data.frame(var = data.temp[, feat[i]], resp = data.temp[, resp])
        
        # sse of parent node
        rss_parent = sum((data_sub$resp - mean(data_sub$resp))^2)
        
        # calculating sse for categorical feature:
        if( is.factor(data_sub$var) ) {
          
          if( length(levels(data_sub$var)) > 2 ){
            # find all possible binary splits
            kk = 1
            varcomb = list()
            for (ii in 1:(length(levels(data_sub$var))-1)) {
              comb = combn(length(levels(data_sub$var)), ii)
              for (jj in 1:ncol(comb)){
                varcomb[[kk]] = levels(data_sub$var)[comb[, jj]]
                kk = kk +1
              }
            }
            
            # calculate sse for all possible splits
            sse = sapply(varcomb, function(varcomb_i){ 
              sum((data_sub$resp[data_sub$var %in% varcomb_i] - mean(data_sub$resp[data_sub$var %in% varcomb_i]))^2) + sum((data_sub$resp[!(data_sub$var %in% varcomb_i)] - mean(data_sub$resp[!(data_sub$var %in% varcomb_i)]))^2) })
            
            # checking size of children nodes; if less than specified, the split is not considered
            count_min = sapply(varcomb, function(varcomb_i){min(length(data_sub$resp[data_sub$var %in% varcomb_i]), length(data_sub$resp[!(data_sub$var %in% varcomb_i)]))})
            for(ii in 1:length(varcomb)) {if(count_min[ii] < round(leafsize)) sse[ii] = NA}
            
            # clean up:
            if(all(is.na(sse))) {error[i] = NA; split_val[[i]] = NA} else { error[i] = min(sse, na.rm = TRUE); split_val[[i]] = varcomb[[which.min(sse)]]}
            
          } else {
            data.split = split(data_sub, data_sub$var)
            error[i] = sum(sapply(data.split, function(x){
              sum( (x$resp - mean(x$resp)) ^ 2 )
            })) 
            
            # checking size of children nodes; if less than specified, the split is not considered
            count_min = min(sapply(data.split, nrow))
            if( count_min < leafsize) error[i] = NA
          }
        } else {
          # calculating sse for continuous feature:
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
          if(all(is.na(sse))) {error[i] = NA; split_val[[i]] = NA} else { error[i] = min(sse, na.rm = TRUE); split_val[[i]] = splits_sort[which.min(sse)]}
        }
      }
      
      if(all(is.na(error))){
        # if none of the splits is good, consider the current node to 'leaf'
        output$status[j] = "leaf"
      } else {
        # characteristics of the current split
        splitvar = feat[which.min(error)] # feature leading to the lowest sse
        rss_diff = rss_parent - min(error, na.rm = TRUE) # difference in sse
        feat_vec = c(feat_vec, splitvar) # record feature used to split
        diff_vec = c(diff_vec, rss_diff) # record different in sse
        
        # creating children nodes:
        if( is.factor(data.temp[[splitvar]])) {
          # for categorical feature:
          
          if( length(levels(data.temp[[splitvar]])) > 2 ){
            yeslevels = split_val[[which.min(error)]]
            nolevels = levels(data.temp[[splitvar]])[!(levels(data.temp[[splitvar]]) %in% yeslevels)]
            data.next = list()
            data.next[[1]] = data.temp[data.temp[[splitvar]] %in% nolevels, ]
            data.next[[2]] = data.temp[data.temp[[splitvar]] %in% yeslevels, ]
          } else data.next = split(data.temp, data.temp[ , splitvar])
        } else {
          # for continuous feature:
          value = split_val[[which.min(error)]]
          index = which(sort(unique(data.temp[[splitvar]])) == value)
          # taking the middle point of unique values as the splitting point to be consistent with 'rpart':
          value = (sort(unique(data.temp[[splitvar]]))[index] + sort(unique(data.temp[[splitvar]]))[index-1])/2
          data.next = list()
          data.next[[1]] = data.temp[which(data.temp[[splitvar]] <= value), ]
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
          if( length(levels(data.temp[[splitvar]])) > 2 ){
            splitrule = c(paste(splitvar, "=", paste(nolevels, collapse = ",")), paste(splitvar, "=", paste(yeslevels, collapse = ",")) )
          } else splitrule = sapply(names(data.next), function(x){paste(splitvar, "=" , x)})
        } else {
          splitrule = c(paste(splitvar, "<=", value),paste(splitvar, ">", value) )
        }
        
        # creating outputs
        temp.output = data.frame(status = status, count = sapply(data.next, nrow), "split rule" = splitrule, iter = output$iter[j] + 1, row.names = NULL, mean = sapply(data.next, function(x){mean(x[[resp]])}))
        
        # attach new outputs to existing dataframe
        output = rbind(output[1:j,], temp.output, output[-c(1:j), ])
        names(data.next) = NULL; data.list = c(data.list[1:j], data.next, data.list[-c(1:j)])
        
        # check if there are remaining splits to be done:
        if(all(output$status != "split")) stopsplit = TRUE
      }
    }
    
    # summing up RSS difference for each feature 
    allfeat = names(data)[names(data)!=resp]
    rss_sum = c()
    for (i in 1:length(allfeat)){
      rss_sum[i] = sum(diff_vec[which(feat_vec == allfeat[i])])
    }
  
  
}
  return(list(output = output, var_rank = data.frame(criterion = rss_sum, row.names = allfeat)))
}
