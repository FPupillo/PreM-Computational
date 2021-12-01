# merge recognition files at the trial level

cd<-getwd()

# set data directory
datadir<-("merged_phases")

# list all the files
fileNum<-list.files(datadir, pattern = "*.csv")

# create data file
DataPhase3Merged<-vector()
for (j in 1:length(fileNum)){
  tryCatch({
  if (j<10){
  currfile<-read.csv(paste(datadir,"/PEM_", "0", j, "_results.csv", sep=""))
  } else{
  currfile<-read.csv(paste(datadir,"/PEM_", j, "_results.csv", sep=""))
  }
  
  # select only old items
  currfile<-currfile[currfile$rec_trialType=="old",]
  # merge them
  DataPhase3Merged<-rbind(DataPhase3Merged, currfile)
}, error=function(e){message(paste("PROBLEM WITH PARTICIPANT", j))}
)
}
  
# print them
write.csv(DataPhase3Merged,"output_files/DataPhase3Merged.csv")
