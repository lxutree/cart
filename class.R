setwd("~/../LX_Project/CART/")

data = read.csv("Datasets/data_cart_classif_step.csv", row.names = 1)

classtree(data = data, resp = "readm",min.obs = 1)[[1]]

# # step by step break down of CART:
# 1. Start with full dataset as the 'root' of the tree
# 2. Identify the feature(s) by which the splitting(s) should be done 
#    - for a classification tree, we compute the gini index for each feature, then select the one with the lowest gini index
#    - for a regression tree, we ???
# 3. Split the dataset at the current node
# 4. Examine the children nodes - whether they are deemed leaves or need to be split further
# 5. repeat 2-4 until all all nodes are either parent or leaves.

classtree <- function(data, resp, min.obs = 3){
  
  # data.frame to store results:
  output = data.frame(status = "split", count = nrow(data), "split rule" = "root", iter = 0, stringsAsFactors = FALSE)
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
      
      # empty vector for gini index:
      gini=c()
      
      data.temp = data.list[[j]]
      
      # list of features:
      feat = names(data.temp)[names(data.temp)!=resp]
      
      # calculating gini index:
      for (i in 1:length(feat)){
        data.gini = data.frame(var = data.temp[, feat[i]], data.temp[, resp])
        data.gini.list = split(data.gini, data.gini$var)
        gini[i] = sum(sapply(data.gini.list, function(x){
          count = table(x)
          gini = 1 - sum((count / sum(count)) ^ 2)
          gini * sum(count)
        }) / nrow(data.gini))
      }
      
      # the feature with the lowest gini index is selected:
      split.var = feat[which.min(gini)]
      
      # split data by the selected feature:
      data.next = split(data.temp, data.temp[ , split.var])
      data.next = lapply(data.next, function(x){x[, -which(names(x) %in% split.var)]})
      
      # Stopping criteria: 
      # - less than 3 observations
      # - all observations have the same label
      
      
      # status = sapply(data.next, function(x){
      #   if (ncol(x) > 2) {
      #     if (nrow(x) < min.obs | nrow( unique(x[, -which(names(x) %in% resp)]) ) == 1) status = "leaf" else status = "split" 
      #   } else status = "leaf"
      #   
      #   status
      # })
      # 
      
      status = sapply(data.next, function(x){
          if (ncol(data.frame(x)) == 1) status = "leaf" else {
            if (nrow(x) < min.obs | nrow( unique(data.frame(x[, -which(names(x) %in% resp)])) ) == 1) status = "leaf" else status = "split"
          }
        status
      })
      
      
      # change current status:
      output$status[j] = "parent"
      
      # creating outputs
      temp.output = data.frame(status = status, count = sapply(data.next, function(x) nrow(data.frame(x))), "split rule" = sapply(names(data.next), function(x){paste(split.var, "=" , x)}), iter = iter, row.names = NULL)
      
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

rpart(Kyphosis~., data = kyphosis, cp=-1, minsplit=2)
classtree(data = kyphosis, resp = "Kyphosis")[[1]]

