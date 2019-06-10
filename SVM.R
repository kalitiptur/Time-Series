library(e1071)
set.seed(1)
x = matrix(rnorm(20*2), ncol = 2)
x
y = c(rep(-1,10), rep(1, 10))
y

x[y==1,]=x[y==1,] + 1
plot(x, col = (3-y))

dat = data.frame(x=x, y = as.factor(y))

head(dat)

svmfit = svm(y~.,data = dat, kernel = "linear" , cost = 10,scale = FALSE )
plot(svmfit, dat)
