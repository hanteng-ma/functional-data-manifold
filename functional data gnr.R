library(splines2)
library(sm)
library(splines)
library(scatterplot3d)
library(fpca)
packageVersion("splines2")


n1=500
knots <- c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
x <- seq(0, 1, length.out = 21)
bsMat <- bSpline(x, knots = knots, degree = 3, intercept = TRUE)
basis_01 <- CreateBasis(6, pts = x, type = c("fourier")) 
plot(bsMat, mark_knots = "all",type = 'p')
plot(bsMat[,3])

my_tj = matrix(, ncol = 3)

manifold_random_num = function(len,R){
  r = runif(len,0,1)
  s = r / sqrt(sum(r^2)) * R
  return(s)
}

dim = 3
my_ma = matrix(data = NA,nrow = n1, ncol = dim)
for (i in 1:n1) {
  my_ma[i,] = manifold_random_num(dim,5)
}

scatterplot3d(my_ma)

for (i in 1:n1){
  sample_tj = basis_01[,1:3] %*% manifold_random_num(dim,5)
  N = length(sample_tj)

  data_m = matrix(data = NA, nrow = N, ncol = 3)
  data_m[1:N,1] = rep(i, N)            #标号
  data_m[1:N,2] = sample_tj #轨迹值
  data_m[1:N,3] = x         #格点位置
  
  my_tj = rbind(my_tj, data_m)
}
my_tj = my_tj[-1,]
plot(my_tj[my_tj[,1] == 51,2])
par(mfrow=c(3,3))
for(i in 1:9){
  plot(x,my_tj[my_tj[,1] == i,2])
}

## candidate models for fitting
M.set<-c(15)
r.set<-c(7,8)

##parameters for fpca.mle
ini.method="EM"
basis.method="bs"
sl.v=rep(0.5,10)
max.step=50
grid.l=seq(0,1,0.01)
grids=seq(0,1,0.001) #for ini.method = EM

##fit candidate models by fpca.mle
result<-fpca.mle(my_tj, M.set,r.set,ini.method, basis.method,sl.v,max.step,grid.l,grids)
summary(result)

eigenfest<-result$eigenfunctions
evalest<-result$eigenvalues
M<-result$selected_model[1]
r<-result$selected_model[2]
grids.new<-result$grid
sig2est<-result$error_var
muest<-result$fitted_mean

par(mfrow=c(1,1))
plot(grids.new,muest)

par(mfrow=c(2,2))
for(i in 1:4){
  plot(grids.new,eigenfest[i,],ylim=range(eigenfest),xlab="time",ylab=paste("eigenfunction",i))
  
}
fpcs<-fpca.score(my_tj,grids.new,muest,evalest,eigenfest,sig2est,r)
pred<-fpca.pred(fpcs, muest,eigenfest)

par(mfrow=c(3,3))
for (i in 81:89){
  id<-i ##for curve i
  t.c<-my_tj[my_tj[,1]==id,3] ##measurement points
  t.proj<-ceiling(N*t.c) ##measurement points projected on the grid
  y.c<-my_tj[my_tj[,1]==id,2] ##obs
  y.pred.proj<-pred[t.proj,id] ##predicted obs on the measurement points 投影到新格点上的第几个点
  #plots
  plot(t.c,y.c,ylim=range(pred[,id]),xlab="time",ylab="obs", main=paste("predicted trajectory of curve", id), type = 'l')
  points(grids.new,pred[,id],col=3,type='l')
  ##points(t.c,y.pred.proj,col=2, pch=2) ##predicted measurements at observed measurement times
}

####### n2  #######
n2= 5000


my_tj_2 = matrix(, ncol = 3)


for (i in 1:n2){
  sample_tj_2 = bsMat[,2:5] %*% manifold_random_num(length(bsMat[1,])-2,5)
  N = length(sample_tj_2)
  
  data_m = matrix(data = NA, nrow = N, ncol = 3)
  data_m[1:N,1] = rep(i, N)            #标号
  data_m[1:N,2] = sample_tj_2 #轨迹值
  data_m[1:N,3] = x         #格点位置
  
  my_tj_2 = rbind(my_tj_2, data_m)
}
my_tj_2 = my_tj_2[-1,]

fpcs_2<-fpca.score(my_tj_2,grids.new,muest,evalest,eigenfest,sig2est,r)
pred_2<-fpca.pred(fpcs_2, muest,eigenfest)

par(mfrow=c(3,3))
for (i in 81:89){
  id<-i ##for curve i
  t.c<-my_tj_2[my_tj_2[,1]==id,3] ##measurement points
  t.proj<-ceiling(N*t.c) ##measurement points projected on the grid
  y.c<-my_tj_2[my_tj_2[,1]==id,2] ##obs
  y.pred.proj<-pred_2[t.proj,id] ##predicted obs on the measurement points 投影到新格点上的第几个点
  #plots
  plot(t.c,y.c,ylim=range(pred_2[,id]),xlab="time",ylab="obs", main=paste("predicted trajectory of curve", id), type = 'l')
  points(grids.new,pred_2[,id],col=3,type='l')
  ##points(t.c,y.pred.proj,col=2, pch=2) ##predicted measurements at observed measurement times
}

write.csv(x = fpcs_2, file = 'D:/研究生文件/科研任务/mainfold vae/pca_data_0407_d7r4.csv')