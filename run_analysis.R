run_analysis<-function(directory){
  
    traindirectory<-paste(directory, "/UCI HAR Dataset", "/train", sep="" )
    testdirectory<-paste(directory, "/UCI HAR Dataset", "/test", sep="" )
    
    # Load features vector and select only the ones which end in 'mean()' and 'std()'
    varfile<-paste(directory, "/UCI HAR Dataset/features.txt", sep="")
    var<-read.table(varfile)
    varmean<-grep("mean()", var[,2], fixed = T)
    varstd<-grep("std()", var[,2], fixed = T)
    allvar<-c(varmean, varstd)
    
    #Creates a vector with the indices of the columns for reading into memory, and so handle
    # with smaller tables
    classvect<-c()
    i<-1
    while(i<=length(var[,2])){
      if(is.element(i, allvar)){
        classvect<-c(classvect, "numeric")
      }else{
        classvect<-c(classvect, "NULL")
      }
      i<-i+1
    }
    traindatafile<-paste(traindirectory, "/X_train.txt", sep="")
    traindata<-read.table(traindatafile, colClasses= classvect)
    testdatafile<-paste(testdirectory, "/X_test.txt", sep="")
    testdata<-read.table(testdatafile, colClasses= classvect)
    
    # Bind the rows of the test and train data
    ttdata<-rbind(traindata,testdata)
    names(ttdata)<-var[,2][allvar]
   
    # Loading necessary files into memory
    trainpersonfile<-paste(traindirectory, "/subject_train.txt", sep="")
    trainperson<-readLines(trainpersonfile)
    testpersonfile<-paste(testdirectory, "/subject_test.txt", sep="")
    testperson<-readLines(testpersonfile)
    ttperson<-c(trainperson, testperson)
    
    trainactivityfile<-paste(traindirectory, "/y_train.txt", sep="")
    trainactivity<-readLines(trainactivityfile)
    testactivityfile<-paste(testdirectory, "/y_test.txt", sep="")
    testactivity<-readLines(testactivityfile)
    ttactivity<-c(trainactivity, testactivity)
    
    # Label the levels of the activities
    ttactivity <- factor(ttactivity, levels=c(1:6),labels=c('WALKING',
    'WALKING_UPSTAIRS', 'WALKING_DOWNSTAIRS', 'SITTING', 'STANDING', 'LAYING'))
    
    # Replace any character with no-space and convert all letters in lower case
    names(ttdata)<-tolower(gsub("[[:punct:]]","",names(ttdata)))
    newdata<<-cbind(personid = ttperson, activity = ttactivity, ttdata)
    
}