getmu<-function(exp){
#------------------------------------------------------------------------------#
# function that creates the exact contingencies depending on the experiment
# 
# created: "Fri Nov  5 18:10:25 2021"
#   INPUT: exp = exp1 or exp2 
#   OUTPUT: "mu" a matrix with the probabilities for each scene
#------------------------------------------------------------------------------#

if (exp == "exp1"){
  
  strong1<-c(0.80, 0.10, 0.10)
  strong2<-c(0.10, 0.80, 0.10)
  strong3<-c(0.10, 0.10, 0.80)
  flat<-c(0.33, 0.33, 0.33)
  
  mu<-matrix(rbind(strong1, strong2, strong3, flat, flat, flat),
             nrow = 6, ncol =3)
  
} else {
  strong1<-c(0.90, 0.10)
  strong2<-c(0.10, 0.90)
  weak1<-c(0.70,0.30)
  weak2<-c(0.30, 0.70)
  flat<-c(0.50, 0.50)
  
  mu<-matrix(rbind(strong1, strong2, weak1, weak2, flat, flat),
             nrow = 6, ncol =2)
  
}
  
  return(mu)
}
  