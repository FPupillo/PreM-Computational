# merge recognition files at the trial level

cd<-getwd()

setwd("premup-pilot")

# set data directory
datadir<-("merged_phases")

# list all the files
fileNum<-list.files(datadir, pattern = "*.csv")

# how many participants?
partNum<-34

# create data file
DataPhase3Merged<-vector()
for (j in 1:partNum){
  tryCatch({
  #if (j<10){
  #currfile<-read.csv(paste(datadir,"/PEM_", "0", j, "_results.csv", sep=""))
  #} else{
  currfile<-read.csv(paste(datadir,"/PEM_", j, "_results.csv", sep=""))
  #}
  
  # select only old items
  currfile<-currfile[currfile$rec_trialType=="old",]
  # merge them
  DataPhase3Merged<-rbind(DataPhase3Merged, currfile)
}, error=function(e){message(paste("PROBLEM WITH PARTICIPANT", j))}
)
}

unique(DataPhase3Merged$particip_code)

  
# print them
write.csv(DataPhase3Merged,"outputs/group_level/share/DataPhase3Merged.csv", row.names = F)
setwd(cd)
