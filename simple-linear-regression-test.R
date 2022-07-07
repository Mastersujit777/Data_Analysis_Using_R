#No. of units repaired
x = c(1,2,3,4,4,5,6,6,7,8,9,9,10,10); x
#time in minutes
y = c(23,29,49,64,74,87,96,97,109,119,149,145,154,166); y

length(x)
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
sigma= sqrt(sum((error)^2)/(length(x)-2)); sigma

SE_of_Beta = sigma/sqrt(var_x); SE_of_Beta 

#95% CI
CI_upper = beta+1.96*(SE_of_Beta); CI_upper
CI_lower = beta-1.96*(SE_of_Beta); CI_lower

#Testing linearity assumption 

beta_given = 12; beta_given
t = (beta-beta_given)/SE_of_Beta; t

#tabulated value 2.18 for 12 DF alpha=5%
#cal.t>tab.t 
#so we reject

#R square
sum_sqr_error=sum((error)^2); sum_sqr_error

R_square = 1-(sum_sqr_error/sum((y-y_mean)^2)); R_square

data=data.frame(x,y,round(predict_y),error); data
hist(error)
mean(error)
plot(x,y)

#using R inbuilt function

fit = lm(y~x)
fit
summary(fit)
plot(fit)
plot(y~x)
abline(fit)
