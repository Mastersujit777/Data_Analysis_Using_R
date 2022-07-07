#Create a 4X4 matrix having elements(1,2,3,4,5,6,7,8,2,6,4,8,3,1,1,2)
A = matrix(c(1,2,3,4,5,6,7,8,2,6,4,8,3,1,1,2), nrow = 4, ncol = 4, byrow=TRUE);

cat("The 4X4 matrix:\n")
print(A)

cat("The Inverse of matrix 'A':\n")
print(solve(A))

cat("The Transpose of matrix 'A':\n")
print(t(A))

cat("The Rank of matrix 'A':\n")
library(Matrix)
print(rankMatrix(A))

cat("The column sum of matrix 'A':\n")
print(colSums(A))

cat("The row sum of matrix 'A':\n")
print(rowSums(A))


cat("The Determinant of matrix 'A':\n")
print(det(A))

cat("The Dimension of matrix 'A':\n")
print(dim(A))

cat("The eigen value & vector of matrix 'A':\n")
print(eigen(A))

cat("The Trace of matrix 'A':\n")
tr=sum(diag(A))
print(tr)



