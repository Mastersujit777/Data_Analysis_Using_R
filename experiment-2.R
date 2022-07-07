#Solve the given system of linear equations in 5 unknowns

A = rbind(c(1,2,3,4,5),
          c(2,1,2,3,4),
          c(3,2,1,2,3),
          c(4,3,2,1,2),
          c(5,4,3,2,1)
           ); 
print(A)

B = rbind(7,-1,-3,5,17);
print(B)

C=solve(A, B);
print(C)
