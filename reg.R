setwd("~/../LX_Project/CART/")

data = read.csv("Datasets/data_cart_rerg_step.csv", row.names = 1)
regtree(data = data, resp = "time")[[1]]



data = read.csv("Datasets/ML_reg 2.csv", row.names = 1)[1:100,]
regtree(data = data, resp = "time")[[1]]

# # step by step break down of CART:
# 1. Start with full dataset as the 'root' of the tree
# 2. Identify the feature(s) by which the splitting(s) should be done 
#    - for a classification tree, we compute the gini index for each feature, then select the one with the lowest gini index
#    - for a regression tree, we ???
# 3. Split the dataset at the current node
# 4. Examine the children nodes - whether they are deemed leaves or need to be split further
# 5. repeat 2-4 until all all nodes are either parent or leaves.

regtree <- function(data, resp){

  
  # data.frame to store results:
  output = data.frame(status = "split", count = nrow(data), "split rule" = "root", iter = 0, stringsAsFactors = FALSE, sd = sd(data[, resp]), mean = mean(data[, resp]) )
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
  
  # iterative process:
  while(!stopsplit) {
    
    # list of splits to be done:
    split.queue = which(output$status == "split")
    
    for (j in split.queue) {
      # empty vector 
      weighted.sd = c()
      error = c()
      split_val = c()
      
      data.temp = data.list[[j]]
      
      # list of features:
      feat = names(data.temp)[names(data.temp)!=resp]
      
      # calculating gini index:
      for (i in 1:length(feat)){
        data_sub = data.frame(var = data.temp[, feat[i]], resp = data.temp[, resp])
        
        if( is.factor(data_sub$var) ) {
          data.split = split(data_sub, data_sub$var)
          error[i] = sum(sapply(data.split, function(x){
            sum( (x$resp - mean(x$resp)) ^ 2 )
          })) 
          # weighted.sd[i] = sum(sapply(data.split, function(x){
          #   if(nrow(x) == 1) {sd = 0} else {sd = sd(x$resp) * nrow(x)}
          # }) / nrow(data_sub))
        } else {
          splits_sort = sort(unique(data_sub$var))
          sse <- c() # vector of sses for each possible split
          for( k in 1:length(splits_sort)){
            sse[k] = sum( (data_sub$resp[data_sub$var < splits_sort[k]] - mean(data_sub$resp[data_sub$var < splits_sort[k]]) )^2 ) + sum( (data_sub$resp[data_sub$var >= splits_sort[k]] - mean(data_sub$resp[data_sub$var >= splits_sort[k]]) )^2 ) 
          }
          error[i] = min(sse)
          split_val[i] = splits_sort[which.min(sse)]
        }
      }
      

      # if( is.factor(data_sub$var) ) {
      #   splitvar = feat[which.max(sd(data.temp[[resp]]) - weighted.sd)]
      #   
      #   # split data by the selected feature:
      #   data.next = split(data.temp, data.temp[ , splitvar])
      #   
      # } else {
      #   splitvar = feat[which.min(error)]
      #   value = split_val[which.min(error)]
      #   
      #   data.next = list()
      #   data.next[[1]] = data.temp[which(data.temp[[splitvar]] < value), ]
      #   data.next[[2]] = data.temp[which(data.temp[[splitvar]] >= value), ]
      # }
      
      splitvar = feat[which.min(error)]
      
      if( is.factor(data.temp[[splitvar]])) {
        data.next = split(data.temp, data.temp[ , splitvar])
      } else {
          splitvar = feat[which.min(error)]
          value = split_val[which.min(error)]

          data.next = list()
          data.next[[1]] = data.temp[which(data.temp[[splitvar]] < value), ]
          data.next[[2]] = data.temp[which(data.temp[[splitvar]] >= value), ]
      }
      
      data.next = lapply(data.next, function(x){x[, -which(names(x) %in% splitvar)]})

      # Stopping criteria: 
      # - less than 3 observations
      # - all observations have the same label
      status = sapply(data.next, function(x){
        if (ncol(x) > 2) {
          if (nrow(x) < 3 | nrow( unique(x[, -which(names(x) %in% resp)]) ) == 1) status = "leaf" else status = "split" 
        } else status = "leaf"
        
        status
      })
      
      # change current status:
      output$status[j] = "parent"
      
      if( is.factor(data.temp[[splitvar]]) ) {
        splitrule = sapply(names(data.next), function(x){paste(splitvar, "=" , x)})
      } else {
        splitrule = c(paste(splitvar, "< ", value),paste(splitvar, ">= ", value) )
      }

      # creating outputs
      temp.output = data.frame(status = status, count = sapply(data.next, nrow), "split rule" = splitrule, iter = iter, row.names = NULL, sd = sapply(data.next, function(x){sd(x[[resp]])}), mean = sapply(data.next, function(x){mean(x[[resp]])}))
      
      output = rbind(output, temp.output)
      
      names(data.next) = NULL; data.list = c(data.list, data.next)
    }
    
    # check if there are remaining splits to be done:
    if(all(output$status != "split")) stopsplit = TRUE
    
    iter = iter+1
  }
  
  return(list(output, data.list))
}

# marks the steps inside function
# write down step by step 
# separate function for reg tree
# check agaisnt other datasets and cart package
# rmd on github

# checking against datasets in package 'rpart'
library(rpart)
regtree(data = data, resp = "time")[[1]]
rpart(time ~ age + num, data = data, cp=-1, minsplit=2, method = "anova")
rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, method = "anova")
rpart(Kyphosis~., data = kyphosis, cp=-1, minsplit=2)
classtree(data = kyphosis, resp = "Kyphosis")[[1]]

rpart(readm~., data = data, cp=-1, minsplit=0)
data
