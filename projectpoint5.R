project_part_5<-function(newdata){
        library(dplyr)
        newdata[,1] = factor(newdata[,1],levels(newdata[,1])[c(1,12,23,25:30,2:11,13:22,24)])
        neworderdata<<-newdata%>%group_by(personid,activity)%>%summarise_each(funs(mean))
        
       
}