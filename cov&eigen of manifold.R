library(scatterplot3d)
### sphere manifold
manifold_random_num = function(len,R){
  r = runif(len,-1,1)
  s = r / sqrt(sum(r^2)) * R
  return(s)
}
### experiment
n1=10000
dim = 3
my_ma = matrix(data = NA,nrow = n1, ncol = dim)
for (i in 1:n1) {
  my_ma[i,] = manifold_random_num(dim,1)
}


my_ma_2 = matrix(data = NA,nrow = n1, ncol = dim)
my_ma_2[,1] = my_ma[,1]
my_ma_2[,2] = my_ma[,2]
my_ma_2[,3] = abs(my_ma[,3])

plot(my_ma_2[,1],my_ma_2[,2])
plot(my_ma_2[,1],my_ma_2[,3])
scatterplot3d(my_ma_2)
cov_matrix = t(my_ma_2) %*% (my_ma_2)/n1
cov_matrix
eigen(cov_matrix)

