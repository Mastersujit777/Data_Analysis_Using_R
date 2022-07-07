#Temprature (Independent/Predictor Variable)
x = c(1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0); x
#Converted Sugar (Dependent Variable)
y = c(8.1, 7.8, 8.5, 9.8, 9.5, 8.9, 8.6, 10.2, 9.3, 9.2, 10.5); y

y_mean = mean(y); y_mean
x_mean = mean(x); x_mean

#covariance of x & y
cov_x_y = sum((x-x_mean)*(y-y_mean)); cov_x_y
#variance of x
var_x = sum((x-x_mean)^2); var_x


beta = cov_x_y/var_x; beta
alpha = y_mean-(beta*x_mean); alpha

predict_y  = alpha+beta*x;predict_y
predict_y_r = round(predict_y); predict_y_r

error=y-predict_y; error
error_r = round(error); error_r

#by function
lr = lm(y~x); lr
plot(lr)

#Estimate the converted sugar at 1.75 temp
converted_sugar = alpha+(beta*1.75); converted_sugar

data=data.frame(x,y,predict_y_r,error_r); data
hist(error)
plot(x,y)
