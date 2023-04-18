selfiles<-function(setup){
  #------------------------------------------------------------------------------------------------#
  # function that select participants' files depending on the setup
  # INUPTS:
  #   setup: flat pilot priors three
  # OUTPUS: 
  #   a file with all the participants data merged
  #
  #------------------------------------------------------------------------------------------------#
  
  if (setup == "pilot"){
    phase1Files<-selPhase(1, "pilot")
    setwd(cd)
    
    phase2Files<-selPhase(2, "pilot")
    setwd(cd)
    
    Allfiles<-vector()
    for (j in 1: length(phase2Files)){
      print(paste("Working on participant", j))
      #counter for the column number of the Matrix that stores the variables
      
      # read the files
      file1<-read.csv(paste("premup-pilot/trial_sequences/",phase1Files[j], sep=""))
      # now read the data from phase 2
      file2<-read.csv(paste("premup-pilot/trial_sequences/",phase2Files[j], sep=""))
      
      # bind the two
      files<-rbind(file1, file2)
      
      # bind with the others
      Allfiles<-rbind(Allfiles, files)
    }
    
  }else if (setup=="priors"){
    # take only phase 2
    Allfiles<- read.csv2("premup-priors/outputs/group_level/share/group_task-rec.csv",sep=";",
                            header=T,stringsAsFactors=FALSE)
  
  } else if (setup=="flat"){
    Allfiles<- read.csv2("premup-flat/outputs/group_level/share/group_task-rec.csv",sep=";",
                         header=T,stringsAsFactors=FALSE)
  } else if (setup="three"){
    Allfiles<- read.csv2("premup-three/outputs/group_level/share/group_task-rec.csv",sep=";",
                         header=T,stringsAsFactors=FALSE)
  }

return(Allfiles)
}