#------------------------------------------------------------------------------#
# script that reads the output of the model recovery script
# and plots the confusion matrix
# "Mon Nov 15 18:01:34 2021"
# argument: setup
#------------------------------------------------------------------------------#

rm(list=ls())
library(here)
library(ggplot2)
library(viridis)#
#library(farver)
library(reshape2)
library(gplots)
library(hrbrthemes)

# create an empty matrix
mt<-as.data.frame(matrix(0, nrow = 3, ncol = 3))


# function that takes the arguments from the command line
Args<-commandArgs(trailingOnly = T)
# the only argument is setup
setup<-Args[1]
#setup<-"exp1"
# now add all the files
cd<-getwd()

setwd("computational_model/temp")
# list files
list<-list.files(pattern=paste0("^ModelRecovery.", setup, "."))
# get names for the matrix
names(mt)<-names(read.csv(list[1]))[2:4]
rownames(mt)<-names(mt)

# add the files
for (i in 1:length(list)){
  mat<-read.csv(list[i])
  mt<-mt+ (mat[,2:4])
}

setwd(cd)

# get confusion matrix:
#p(fit model | simulated model)

getCM<-function(matrix){
# initialise the matrix
Matrixmean<-matrix(NA, nrow = ncol(matrix), ncol=ncol(matrix))

# compute the percentages in each cell
for (r in 1: ncol(matrix)){
  sum<-sum(matrix[r,1:ncol(matrix)])
  for (c in 1:ncol(matrix)){
  Matrixmean[r, c]<-matrix[r,c]/sum
  }
}

Matrixmean<-data.frame(Matrixmean)
names(Matrixmean)<-names(matrix)
rownames(Matrixmean)<-names(matrix)

# convert to matrix
mat<-data.matrix(Matrixmean)
# melt it to create a long dataset
melted_matrix<-melt(mat)

# plot it 
# transpose matrix
matrTrans<-t(Matrixmean)[,nrow(mat):1]

melted_transmatr<-melt(matrTrans)

return(melted_transmatr)

}

longmatr<-getCM(mt)

p<-ggplot(longmatr, aes(x=(Var1), y=(Var2), fill=value))
p+geom_tile(aes(fill = value)) +#
geom_text(aes(label =round(value, 2)))+
  
  #theme_ipsum()+
  labs(y="", x = "")+
  theme_bw()+
  scale_x_discrete(position = "top") +
  theme(legend.title = element_blank())+
#scale_fill_gradient(low = "blue", high = "yellow")
scale_fill_viridis()

# save the picture
ggsave(file=paste0("computational_model/figures/ConfusionMatrix.", setup, ".jpg"),
       width = 4, height = 2.5, units = "in")

#------------------------------------------------------------------------------#
# now the betaplus 1
setwd("computational_model/temp")
# list files
list<-list.files(pattern=paste0("^ModelRecoveryBetaPlus1.", setup, "."))
# get names for the matrix
names(mt)<-names(read.csv(list[1]))[2:4]
rownames(mt)<-names(mt)

# add the files
for (i in 1:length(list)){
  mat<-read.csv(list[i])
  mt<-mt+ (mat[,2:4])
}

setwd(cd)

#
longmatr2<-getCM(mt)


p2<-ggplot(longmatr2, aes(x=(Var1), y=(Var2), fill=value))
p2+geom_tile(aes(fill = value)) +#
  geom_text(aes(label =round(value, 2)))+
  #theme_ipsum()+
  labs(y="", x = "")+
  theme_bw()+
  scale_x_discrete(position = "top") +
  theme(legend.title = element_blank())+
  #scale_fill_gradient(low = "blue", high = "yellow")
  scale_fill_viridis()

# save the picture
ggsave(file=paste0("computational_model/figures/ConfusionMatrixBetaPlus1.",
                   setup, ".jpg"))
