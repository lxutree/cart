data = read.csv("Datasets/data_cart_rerg_step.csv", row.names = 1)

# this is the function for boostrap aggregation

# start by drawing 

function(data, n.boot = 10, ...){
  n.boot=10
  # start by drawing 'n.boot' bootstrap samples:
  
  data_list = list()
  outtabag = list()
  
  for (i in 1:n.boot){
    index = sample(nrow(data),  replace = TRUE)
    
    # the ith bootstrap sample:
    data_list[[i]] =data[index, ]
    
    # the ith 'out of bag' sample
    outtabag[[i]] = data[!(1:nrow(data) %in% unique(index)), ]
  }

}

library(rpart)
bag_test <- bagging(
  formula = time ~ .,
  data = data,
  nbagg = 10,  
  coob = TRUE,
  control = rpart.control(minsplit = 2, cp = -1)
)

bag_test


predtree = function(newdata, res){
  newdata$resp = c()
  rules = data.frame(t(sapply(res$split.rule[-1 ], function(x){scan(text = x, what = "")}, USE.NAMES = FALSE)), res$iter[-1], stringsAsFactors = FALSE)
  names(rules) = c("feat", "operator", "value", "iter")
  
  for(k in 1:nrow(newdata)){

    ii=1
    stoploop = FALSE
    while(!stoploop){
      if(compare(newdata[k, rules[ii, "feat"]], rules[ii, "value"],  (rules[ii, "operator"]))) {
        if(rules$iter[ii+1] == rules$iter[ii]) stoploop = TRUE else ii = ii + 1
      } else {
        ii = which(rules$feat == rules$feat[ii])[which(rules$feat == rules$feat[ii]) > ii][1]
      }
      
      if(rules$iter[ii+1] < rules$iter[ii] | ii == nrow(rules)) stoploop = TRUE
    }
    
    newdata$resp[k] = res$mean[ii+1]
  }
  return(newdata)
}

predtree(newdata = unique(data[,-4]), res = res)

res = regtree(data=data, resp = "num", min.obs = 50)$output
predtree(newdata = unique(data[20,]), res = res)

source("reg.R")
data = read.csv("Datasets/data_cart_rerg_step.csv", row.names = 1)
res = regtree(data = data, resp = "time", min.obs = 3)$output


compare = function(x1,x2, operator){ 
  if(operator == "=") operator = "=="
  res = getFunction(operator)(x1,x2); res }
