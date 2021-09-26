---
`title: "CART example"
author: "Liang"
date: "14/09/2021"
output: html_document
---

Set working directory:
  
```{r}
setwd("~/../LX_Project/cart/")

# setwd("")
```
source("bagg.R")

library(rpart)
library(ipred)
set dataset:


data = read.csv("Datasets/ML_reg 2.csv", row.names = 1)[777:888,]
tree = regtree(data=data, resp = "num", min.obs = 50)$output
tree2 = rpart(num ~., data = data, minsplit = 50, cp=-1)

newdata1 = data[11, ]
predtree(newdata = newdata1, "num", tree, data=data)
predict(tree2, newdata = newdata1)

newdata2 = data[111, ]
predtree(newdata = newdata2, "num", tree, data=data)
predict(tree2, newdata = newdata2)

bagg(data = data, resp = "num", n.boot = 100, min.obs=50)

bagging(
  formula = num ~ .,
  data = data,
  nbagg = 100,  
  coob = TRUE,
  control = rpart.control(minsplit = 50, cp = -1)
)$err

bagg(data = data, resp = "readm", n.boot = 10, min.obs=3,  seed =123)

bagging(
  formula = readm ~ .,
  data = data,
  nbagg = 10,  
  coob = TRUE,
  control = rpart.control(minsplit = 3, cp = -1)
)$err

bagg(data = kyphosis, resp = "Kyphosis", n.boot = 2, min.obs=20)

bagging(
  formula = Kyphosis ~ .,
  data = kyphosis,
  nbagg = 100,  
  coob = TRUE,
  control = rpart.control(minsplit = 20, cp = -1)
)$err

data = read.csv("Datasets/data_cart_rerg_step.csv", row.names = 1)
tree = regtree(data=data, resp = "time", min.obs = 3)$output
tree2 = rpart(time ~., data = data, minsplit = 3, cp=-1)

newdata1 = data[2, ]
predtree(newdata = newdata1, "time", tree)
predict(tree2, newdata = newdata1)

bagg(data = data, resp = "time", n.boot = 100, min.obs=3)
bagging(
  formula = time ~ .,
  data = data,
  nbagg = 10,  
  coob = TRUE,
  control = rpart.control(minsplit = 3, cp = -1)
)$err



rforrest = function(data, resp, n.boot, n.pred, ...){
  # start by drawing 'n.boot' bootstrap samples:
  feat = names(data)[names(data)!=resp]
  data_list = list()
  
  for (i in 1:n.boot){
    index = sample(nrow(data),  replace = TRUE)
    
    # the ith bootstrap sample:
    data_list[[i]] =data[index, ]
    
  }
  
  tree_list = lapply(data_list, function(data){
    if(class(data[, resp]) == "factor"){
      
    } else {
      feat = names(data)[names(data)!=resp]
      regtree(data = data, resp = resp, min.obs = min.obs)$output
      
    }
  })
  
  
  
  return(rmse = mean(rmse))
}



variable importance plot - how the variables are ranked 