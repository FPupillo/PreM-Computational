# plot the preference for the category that is associated with that scene 80% of the time
# we want the cumulative plot
printplot<-1

for (s in 4:6){
  #subset only trials for scene 
  fileSub<-par[par$scene_cat==s,]
  a<-fileSub$response
if (s>3) {  
  
  if (s==4){c<-1
  #reorder variables to show the most likely one on top
  newCat<-a
  newCat[a==1]<-1
  newCat[a==2]<-0.5
  newCat[a==3]<-0
  # y represent the category. We need to plot the most common one as the top one in the graph
  y<-c(0,2,3,c)
  } else if (s==5){c<-2
  newCat<-a
  newCat[a==1]<-0
  newCat[a==2]<-1
  newCat[a==3]<-0.5
  y<-c(0,1,3,c)
  } else if (s==6){c<-3
  newCat<-a
  y<-c(0,1,2,c)}
  
} else { # for flat prior, plot the category chosen more frequently
  table<-table(a[a!=0]) # we need to exclude 0 from that table
  c<-as.numeric(names(table)[which.max(table)])}

choice<-NULL  # actual choice made by participants
preference<-NULL   #actual performance by participant, as the sum of the expected choice over the sum of the trials
# the variable choice assigns 1 when participants selected the most likely category, 0 if not
for (e in 1 :length(fileSub$scene_cat)){ 
  if(fileSub$response[e]==c){choice[e]<-1
  }else{choice[e]<-0}
  preference[e]<-sum(choice)/e
} 

# print the cumulative preference for the most likely variable
if (printplot==1){
plot(preference, ylim=c(-1,1), xlim=c(1, length(fileSub$scene_cat)),xaxt="n",xlab="",ylab="Preference for Category")
# get the Q  
file$Q<-par[[c+6]]
Q<-file$Q[file$scene_cat==s]
lines(Q, col="blue")
# get PE. Since the observational models have three deltas (trhree PEs), try to come up with a tryCatch syustem
tryCatch({
file$PrE<-par$Delta
PrE<-file$PrE[file$scene_cat==s]
lines(PrE, col="red")
})

if (all(is.na(file$PrE))) {# if all elemnts of prE are null
  if (!is.null(file$Delta1)){# if columns with "delta 1" existr
  for (i in 1 : nrow(file)){
    if (file$response[i]!=0){
  file$PrE[i]<- par[[paste("Delta",file$response[i] , sep ="")]][i]
    } else{ file$PrE[i]<-NA}
  }
    PrE<-file$PrE[file$scene_cat==s]
    lines(PrE, col="red")
  }
}

# get probability
file$Prob<-par[[paste("P", c, sep="")]]
Prob<-file$Prob[file$scene_cat==s]
lines(Prob, col="green")
# if it has the variable learning rate
if (!is.null(par$lr)){
 lr<- par$lr[file$scene_cat==s]
 lines(lr, col = "orange")
}
abline(h=c(-0.2,-0.4,-0.6,-0.80,0,0.2,0.4,0.6,0.8), lty=1, col="grey")
abline(v=seq(1:length(fileSub$scene_cat)), lty=1, col="grey")#
axis(1, at = c(1:length(fileSub$scene_cat)),  labels = fileSub$acc, cex.axis=0.5)
axis(3, at = 1:length(fileSub$scene_cat), labels = a,cex.axis=0.5)
mtext("Accuracy", 1, line=1,ad =-0.15, cex=0.4)
mtext("Choice", 3, line=1, adj=-0.15, cex=0.5)
title(main = paste("alpha = ", round(alpha,3), ",","beta = ",round(beta,3)), line=2)
title(main=paste("Part = ", substr(phase1Files[j],5,6),"scene = ", s), line=3)
} else{
# print the choice
plot(newCat, yaxt='n', xaxt="n",xlab="")
  lines(newCat)#
axis(1, at = c(1:length(fileSub$scene_cat)),  labels = fileSub$acc,cex.axis=0.5)
axis(2, at = c(0,0.5,1), labels= y[2:4])
axis(4, at = c(0,0.5,1))
mtext("Accuracy", 1, line=1,ad =-0.15, cex=0.4)
abline(h=1, lty=3, col="green")#
PrE<-file$PrE[file$scene_cat==s]
lines(PrE, col="red")
title(main = paste("alpha = ", round(alpha,3), ",","beta = ",round(beta,3)), line=0.3)
title(main=paste("Part = ", substr(phase1Files[j],5,6),"scene = ", s), line=1.2)
}
}
