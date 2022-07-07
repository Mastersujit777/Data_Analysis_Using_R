#No. of units repaired
x = c(1,2,3,4,4,5,6,6,7,8,9,9,10,10); x
#time in minutes
y = c(23,29,49,64,74,87,96,97,109,119,149,145,154,166); y

#covariance of x & y
cov_x_y = sum((x-mean(x))*(y-mean(y))); cov_x_y
#variance of x
var_x = sum((x-mean(x))^2); var_x

y_mean = mean(y); y_mean
x_mean = mean(x); x_mean

beta = cov_x_y/var_x; beta
alpha = y_mean-(B*x_mean); alpha

predict_y  = alpha+beta*x;predict_y
error=y-predict_y; error

data=data.frame(x,y,round(predict_y),error); data
hist(error)
mean(error)
plot(x,y)
fit = lm(y~x)
fit
plot(fit)
