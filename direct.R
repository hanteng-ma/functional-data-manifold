#x <- seq(0, 1, length.out = 11)
x = runif(11,0,1)
basis_01 <- CreateBasis(30, pts = x, type = c("fourier")) 
par(mfrow=c(3,3))
for(i in 1:9){
  plot(x,basis_01[,i])
}



rank = rankMatrix(basis_01[,5:20])