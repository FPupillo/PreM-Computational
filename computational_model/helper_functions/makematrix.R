# test cluster script

data<-matrix(NA, 6, 6)

for (r in 1: nrow(data)){
for ( c in 1:ncol(data)){
data[r,c]<-runif(1)
}
}

  data.frame<-data
data
