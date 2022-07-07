x = c(1,2,3,4,4,5,6,6,7,8,9,9,10,10); x
y = c(23,29,49,64,74,87,96,97,109,119,149,145,154,166); y
length(y)
nu = sum((x-mean(x))*(y-mean(y))); nu
var_x = sum((x-mean(x))^2); var_x
var_y = sum((y-mean(y))^2)
var_x

r=nu/(sqrt(var_x)*sqrt(var_y))
r
t=r*sqrt(12)/sqrt(1-r^2)
t
r=cor(x,y);r
t=cor.test(x,y); t
