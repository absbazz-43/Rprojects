names(mtcars)[1:3]
c <- NULL
for(i in 2:ncol(mtcars)){
   cat("model=" = i,"coeff=" = coef(lm(mpg ~., data = mtcars[,c("mpg",names(mtcars)[2:i])])),"\n")
}
coef(lm(mpg~.,data = mtcars))

dat <- mtcars[,-1]
for(i in 1:ncol(dat)){
 c[i] <- AIC(lm(mpg ~., data = mtcars[,c("mpg",names(dat)[1:i])]))
}
c




add_var <- function(data,va){
  dat <- data[,-1]
  for(i in 1:ncol(dat)){
   s <- coef(lm(va~. ,data = data[,c("va",names(dat)[1:i])]))
  }
  return(s)
}
add_var(mtcars,mpg)












add_var <- function(data,var){
  dat <- data[,-1]
  s <- NULL
  for(i in 1:ncol(dat)){
    s[i] <- AIC(lm(var ~ . ,data = data[,c(var,names(dat)[1:i])]))
  }
  return(s)
}
add_var(mtcars,"mpg")
