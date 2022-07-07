#Create a 4X4 matrix having elements(6,5,4,3,7,8,9,2,1,0,1,3,7,3,2,9)
A = matrix(c(6,5,4,3,7,8,9,2,1,0,1,3,7,3,2,9), nrow = 4, ncol = 4, byrow=TRUE);

cat("The 4X4 matrix:\n")
print(A)

cat("The eigen value & vector of 'A' matrix:\n")
print(eigen(A))

cat("The Determinant of 'A' matrix:\n")
print(det(A))

cat("The Inverse of 'A' matrix:\n")
print(solve(A))
