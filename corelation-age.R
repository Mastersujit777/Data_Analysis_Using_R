husbands_age = c(23,27,28,29,30,31,33,35,36,39); husbands_age
wives_age = c(18,22,23,24,25,26,28,29,30,32); wives_age
length(wives_age)
nu = sum((husbands_age-mean(husbands_age))*(wives_age-mean(wives_age))); nu
var_x = sum((husbands_age-mean(husbands_age))^2); var_x
var_y = sum((wives_age-mean(wives_age))^2); var_y

r=nu/(sqrt(var_x)*sqrt(var_y));r
r
t=r*sqrt(length(husbands_age)-2)/sqrt(1-r^2)
t
r=cor(x,y);r
t=cor.test(husbands_age,wives_age); t
