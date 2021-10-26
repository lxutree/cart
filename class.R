classtree <- function(data, resp, min.obs, feat = NULL,  nfeat = NULL, type = NULL, depth = NULL){
  # nfeat = number of features selected at random at each split
  # feat = list of features 
  # depth = maximum of terminal nodes allowed. This is equal to the number of splits + 1
  
  # minimum size of leaves?
  leafsize = min.obs/3 # same as the default in rpart
  
  # data.frame to store results:
  output = data.frame(status = "split", count = nrow(data), "split rule" = "root", "response" = paste(levels(data[[resp]])[1], ":", table(data[[resp]])[1] , "/",nrow(data)), iter = 0, prob =  table(data[[resp]])[1]/nrow(data), stringsAsFactors = FALSE)
  
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
  
  # list of features:
  if(is.null(feat)) feat = allfeat = names(data)[names(data)!=resp]
  if(is.null(nfeat)) nfeat = round(sqrt(length(feat)))
  
  # vector of features/gini index reduction for each split:
  feat_vec = c()
  gini_vec = c()
  
  # iterative process:
  while(!stopsplit) {
    
    # list of splits to be done:
    split.queue = which(output$status == "split")
    
    for (j in split.queue[1]) {
      
      # empty vector for gini index:
      gini=c()
      min_split = c()
      
      data.temp = data.list[[j]]
      
      # for random forrest
      if(!is.null(type)){if(type == "rf") feat = sample(allfeat, size = nfeat, replace = FALSE)}
      
      # calculating gini index:
      for (i in 1:length(feat)){
        data.gini = data.frame(var = data.temp[, feat[i]], resp = data.temp[, resp])
        
        # gini index of parent node
        count = table(data.gini)
        gini_parent = 1 - sum((count / sum(count)) ^ 2)
        
        # calculating sse for categorical feature:
        if( is.factor(data.gini$var) ) {
          data.gini.list = split(data.gini, data.gini$var)
          gini[i] = sum(sapply(data.gini.list, function(x){
            count = table(x)
            gini = 1 - sum((count / sum(count)) ^ 2)
            gini * sum(count)
          }) / nrow(data.gini))
          
          # checking size of children nodes; if less than specified, the split is not considered
          count_min = min(sapply(data.gini.list, nrow))
          if( count_min < leafsize) gini[i] = NA
          } else {
            # calculating gini index for continuous feature:
            gini_splits = c()
            splits_sort = sort(unique(data.gini$var))

             if (length(unique(splits_sort)) < 2) {gini[i] = NA; min_split[i] = NA}
            else {
               for( k in 2:length(splits_sort)){
                 
                 data.gini = data.frame(var = data.temp[, feat[i]], data.temp[, resp])
                 
                 data.gini$var = cut(data.gini$var, breaks =c(splits_sort[1],mean(c(splits_sort[k-1], splits_sort[k])), max(splits_sort)+1 ), right=FALSE)
                 
                 data.gini.list = split(data.gini, data.gini$var)
                 gini_splits[k-1] = sum(sapply(data.gini.list, function(x){
                   count = table(x)
                   gini = 1 - sum((count / sum(count)) ^ 2)
                   gini * sum(count)
                 }) / nrow(data.gini))
                 
                 # checking size of children nodes; if less than specified, the split is not considered
                 count_min = min(sapply(data.gini.list, nrow))
                 if(count_min < round(leafsize)) gini_splits[k-1] = NA
               }      
             }
            # calculating sse for all possible splits
            
            
            # clean up for when none of the splits is valid:
            if(all(is.na(gini_splits))) {
              gini[i] = NA
              min_split[i] = NA
          } else {
            gini[i] = min(gini_splits, na.rm = TRUE)
            min_split[i] = splits_sort[which.min(gini_splits)]}
          }
        }
      
      if(all(is.na(gini))){
        # if none of the splits is good, consider the current node to 'leaf'
        output$status[j] = "leaf"
      }
      
      {
        # characteristics of the current split
        split.var = feat[which.min(gini)] # feature leading to the lowest gini index
        gini_diff = gini_parent - min(gini, na.rm = TRUE) # difference in gini index
        feat_vec = c(feat_vec, split.var) # recorded in vector
        gini_vec = c(gini_vec, gini_diff) # recorded in vector
        
        # creating children nodes by the selected feature:
        if( is.factor(data.temp[[split.var]]) ) {
          # for categorical feature:
          data.next = split(data.temp, data.temp[ , split.var])
  
        } else {
          # for continuous feature:
            split.val = min_split[which.min(gini)]
            index = which(sort(unique(data.temp[[split.var]])) == split.val)
            # taking the middle point of unique values as the splitting point to be consistent with 'rpart':
            split.val = (sort(unique(data.temp[[split.var]]))[index] + sort(unique(data.temp[[split.var]]))[index+1])/2
            data.next = list()
            data.next[[1]] = data.temp[which(data.temp[[split.var]] <= split.val), ]
            data.next[[2]] = data.temp[which(data.temp[[split.var]] > split.val), ]
        }
        
        # Stopping criteria: 
        # - less than 3 observations
        # - all observations have the same label
        status = sapply(data.next, function(x){
            if (ncol(data.frame(x)) == 1 | length(unique(x[[resp]])) == 1 ) status = "leaf" else {
              if (nrow(x) < min.obs | nrow( unique(data.frame(x[, -which(names(x) %in% resp)])) ) == 1) status = "leaf" else status = "split"
            }
          status
        })
        
        
        # change current status from 'split' to 'parent' so it won't be split further:
        output$status[j] = "parent"
        
        # record how the split was done:
        split_rule =  if( is.factor(data.temp[[split.var]]) ) {
          sapply(names(data.next), function(x){paste(split.var, "=" , x)})
        } else {
          c(paste(split.var, " <= ", split.val),  paste(split.var, " > ", split.val))
        }
        
        # attach new outputs to existing dataframe
        temp.output = data.frame(status = status, count = sapply(data.next, function(x) nrow(data.frame(x))), "split rule" = split_rule, "response" = paste(levels(data[[resp]])[1], ":", sapply(data.next, function(x){ table(x[[resp]])[1]}), "/", sapply(data.next,nrow)), prob = sapply(data.next, function(x){ table(x[[resp]])[1]}) / sapply(data.next,nrow), iter = output$iter[j] + 1, row.names = NULL)
        
        output = rbind(output[1:j,], temp.output, output[-c(1:j), ])
        
        names(data.next) = NULL; data.list = c(data.list[1:j], data.next, data.list[-c(1:j)])
      
        
        
        
        
        }
      }
    
    # check if there are remaining splits to be done:
    if(all(output$status != "split")) stopsplit = TRUE
    
    # to control the depth of the tree:
    if(!is.null(depth)) {if (length(feat_vec) + 1 >= depth) stopsplit = TRUE  }
  }
  
  # summing up gini index differences for each feature 
  gini_sum = c()
  for (i in 1:length(allfeat)){
    gini_sum[i] = sum(gini_vec[which(feat_vec == allfeat[i])])
  }
  
  return(list(output = output, var_rank = data.frame(criterion = gini_sum, row.names = allfeat)))
}
