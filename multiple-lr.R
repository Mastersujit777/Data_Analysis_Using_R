#Data Entry
x1=c(2,-1, 1, 2,1); x1
x2=c(1,2,-3,1,4); x2
x3=c(4,1,4,2,6); x3
y1=c(8,10,9,6,12); y1

#Matrix formation
x=cbind(x1,x2,x3);x
y=matrix(y1, ncol=1); y

#Transpose of x
x_transpose=t(x);x_transpose
first_term = x_transpose %*% x; first_term
first_term_inv = solve(first_term); first_term_inv
second_term = x_transpose %*% y; second_term

beta1 = first_term_inv %*% second_term ; beta1

#by using function
fit=lm(y1~x1+x2+x3); fit
summary(fit)
