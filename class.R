classtree <- function(data, resp, min.obs = 20){
  
  # minimum size of leaves?
  leafsize = min.obs/3 # same as the default in rpart
  
  # data.frame to store results:
  output = data.frame(status = "split", count = nrow(data), "split rule" = "root", "response" = paste(levels(data[[resp]])[1], ":", table(data[[resp]])[1] , "/",nrow(data)), iter = 0, stringsAsFactors = FALSE)
  
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
  feat = names(data)[names(data)!=resp]
  
  # iterative process:
  while(!stopsplit) {
    
    # list of splits to be done:
    split.queue = which(output$status == "split")
    
    for (j in split.queue) {
      
      # empty vector for gini index:
      gini=c()
      min_split = c()
      
      data.temp = data.list[[j]]
      

      
      # calculating gini index:
      for (i in 1:length(feat)){
        data.gini = data.frame(var = data.temp[, feat[i]], data.temp[, resp])
        
        if( is.factor(data.gini$var) ) {
          data.gini.list = split(data.gini, data.gini$var)
          gini[i] = sum(sapply(data.gini.list, function(x){
            count = table(x)
            gini = 1 - sum((count / sum(count)) ^ 2)
            gini * sum(count)
          }) / nrow(data.gini))
          } else {
            gini_splits = c()
            splits_sort = sort(unique(data.gini$var))
            for( k in 2:length(splits_sort)){

              data.gini = data.frame(var = data.temp[, feat[i]], data.temp[, resp])

              data.gini$var = cut(data.gini$var, breaks =c(splits_sort[1],splits_sort[k], max(splits_sort)+1 ), right=FALSE)
              
              data.gini.list = split(data.gini, data.gini$var)
              gini_splits[k-1] = sum(sapply(data.gini.list, function(x){
                count = table(x)
                gini = 1 - sum((count / sum(count)) ^ 2)
                gini * sum(count)
              }) / nrow(data.gini))
              
              count_min = min(sapply(data.gini.list, nrow))
              
              if(count_min < round(leafsize)) gini_splits[k-1] = NA
            }
            gini[i] = min(gini_splits, na.rm = TRUE)
            min_split[i] = splits_sort[which.min(gini_splits)]
          }
        }
      
      # the feature with the lowest gini index is selected:
      split.var = feat[which.min(gini)]
      
      # split data by the selected feature:
      if( is.factor(data.temp[[split.var]]) ) {
        data.next = split(data.temp, data.temp[ , split.var])

      } else {
          split.val = min_split[which.min(gini)]
          index = which(sort(unique(data.temp[[split.var]])) == split.val)
          split.val = (sort(unique(data.temp[[split.var]]))[index] + sort(unique(data.temp[[split.var]]))[index+1])/2
          data.next = list()
          data.next[[1]] = data.temp[which(data.temp[[split.var]] < split.val), ]
          data.next[[2]] = data.temp[which(data.temp[[split.var]] > split.val), ]
      }
      
      
      status = sapply(data.next, function(x){
          if (ncol(data.frame(x)) == 1 | length(unique(x[[resp]])) == 1 ) status = "leaf" else {
            if (nrow(x) < min.obs | nrow( unique(data.frame(x[, -which(names(x) %in% resp)])) ) == 1) status = "leaf" else status = "split"
          }
        status
      })
      
      
      # change current status:
      output$status[j] = "parent"
      
      # creating outputs
      split_rule =  if( is.factor(data.temp[[split.var]]) ) {
        sapply(names(data.next), function(x){paste(split.var, "=" , x)})
      } else {
        c(paste(split.var, " < ", split.val),  paste(split.var, " > ", split.val))
      }
      
      temp.output = data.frame(status = status, count = sapply(data.next, function(x) nrow(data.frame(x))), "split rule" = split_rule, "response" = paste(levels(data[[resp]])[1], ":", sapply(data.next, function(x){ table(x[[resp]])[1]}), "/", sapply(data.next,nrow)), iter = iter, row.names = NULL)
      
      output = rbind(output, temp.output)
      
      names(data.next) = NULL; data.list = c(data.list, data.next)
    }
    
    # check if there are remaining splits to be done:
    if(all(output$status != "split")) stopsplit = TRUE
    
    iter = iter+1
  }
  
  return(list(output = output, data.list = data.list))
}
