#Update the below path with the location on the computer where the OrdonezB_ADLs.txt or other dataset can be found
setwd("/Users/johnabraham/Desktop/Graph_Related/UCI_ADL_Binary_Dataset")
library(dplyr)
library(lubridate)
library(RNeo4j)
library(sqldf)

#Update the name of the file if another dataset is being used
b.data <- read.csv('OrdonezB_ADLs.txt', sep="\t",stringsAsFactors = FALSE)

#Processing the data and creating relevant fields
b.data$Start.time <-  strptime(b.data$Start.time,format="%d/%m/%y %H:%M",tz="CET")
b.data$End.time <-  strptime(b.data$End.time,format="%d/%m/%y %H:%M",tz="CET")
b.data$Act.startime <- format(b.data$Start.time,"%H:%M")
b.data$Act.endtime <- format(b.data$End.time,"%H:%M")
b.data$Act.startime.mins <- as.numeric(substr(b.data[,"Act.startime"],1,2))*60+as.numeric(substr(b.data[,"Act.startime"],4,5))
b.data$Act.endtime.mins <- as.numeric(substr(b.data[,"Act.endtime"],1,2))*60+as.numeric(substr(b.data[,"Act.endtime"],4,5))
b.data$Act.duration.mins <- as.numeric(difftime(b.data$End.time,b.data$Start.time))/60

#To which segment of the day an activity belongs depends on which segment most part of the activity falls under
#This is determined by identifying the segment the central point(time) of the activity falls under
b.data$Act.center.datetime <- as.POSIXlt((as.numeric(as.POSIXct(b.data[,"Start.time"]))+as.numeric(as.POSIXct(b.data[,"End.time"])))/2,tz="CET",origin = '1970-01-01 00:00.00 UTC')
b.data$Act.center.date <- format(b.data$Act.center.datetime,format="%d-%m-%y")
b.data$Act.center.time <- format(b.data$Act.center.datetime,format="%H:%M")
b.data$Act.center.time.mins <- as.numeric(substr(b.data[,"Act.center.time"],1,2))*60+as.numeric(substr(b.data["Act.center.time"],4,5))
b.data$Day_of_week <- weekdays(b.data[,"Act.center.datetime"])



#Set a Time.interval (in hours) to devide the day by.
time.interval=6

#Now, dividing the activity to the corresponding segment during the day
#Number of segments depend on the Time.interval set
hour.segment.limits=seq(0,24,time.interval)

b.data$interval_of_Act <- cut(b.data[,"Act.center.time.mins"]/60,hour.segment.limits)
levels(b.data$interval_of_Act) <- seq(1:length(hour.segment.limits))



backup.b.data <- b.data


g=startGraph("http://localhost:7474/db/data",username="neo4j",password="password")
clear(g)

#Define activity durations (hours) - small, medium and long duration
small.dur.act=0.25
med.dur.act=3
#Activities that fall beyond medium duration are long duration activities.



b.data <- backup.b.data
#***********************************************
#Set a date from the data and equate to "b.data$Act.center.date", in the below command to get the activities for a day.
#Set a weekday eg: "Tuesday" for aggregating the activity sequences for Tuesday from the data. The below command is for Monday.
b.data <- b.data[b.data$Day_of_week=="Monday",]
#***********************************************

addConstraint(g,"Small_Duration_Activity","Id")
addConstraint(g,"Medium_Duration_Activity","Id")
addConstraint(g,"Long_Duration_Activity","Id")
limit=dim(b.data)[1]
i=1
#This part is there to handle a behaviour of getOrCreateNode. It throws an error if there are no nodes of that label already present. Hence keeping track with
small.dur.count=0
med.dur.count=0
long.dur.count=0

while(i<limit){
    rec=b.data[i,];
    nxt.rec=b.data[i+1,];
    label1 <- ifelse(rec$Act.duration.mins<=small.dur.act*60,"Small_Duration_Activity",ifelse(rec$Act.duration.mins<=med.dur.act*60,"Medium_Duration_Activity","Long_Duration_Activity"))
    label2 <- ifelse(nxt.rec$Act.duration.mins<=small.dur.act*60,"Small_Duration_Activity",ifelse(nxt.rec$Act.duration.mins<=med.dur.act*60,"Medium_Duration_Activity","Long_Duration_Activity"))
    if((label1=="Small_Duration_Activity" & small.dur.count==0)|(label1=="Medium_Duration_Activity" & med.dur.count==0)|(label1=="Long_Duration_Activity" & long.dur.count==0)){
      flag1=1;
    }else{flag1=0}
    if((label2=="Small_Duration_Activity" & small.dur.count==0)|(label2=="Medium_Duration_Activity" & med.dur.count==0)|(label2=="Long_Duration_Activity" & long.dur.count==0)){
      flag2=1;
    }else{flag2=0}
    
    
    
    if(flag1==0){
    recNode=getOrCreateNode(g,label1,Id=paste(rec$interval_of_Act,rec$Activity,sep="_"),Segment_of_Day=rec$interval_of_Act,  Name = paste(rec$Activity, paste("Segment",rec$interval_of_Act,sep=" "),sep=" "),Activity_startTime=rec$Act.startime,Activity_duration=rec$Act.duration);
    } else {
    recNode=createNode(g,label1,Id=paste(rec$interval_of_Act,rec$Activity,sep="_"),Segment_of_Day=rec$interval_of_Act,  Name = paste(rec$Activity, paste("Segment",rec$interval_of_Act,sep=" "),sep=" "),Activity_startTime=rec$Act.startime,Activity_duration=rec$Act.duration);
    }
    
    if(flag2==0){
      nxt.recNode=getOrCreateNode(g,label2,Id=paste(nxt.rec$interval_of_Act,nxt.rec$Activity,sep="_"),Segment_of_Day=nxt.rec$interval_of_Act,  Name = paste(nxt.rec$Activity, paste("Segment",nxt.rec$interval_of_Act,sep=" "),sep=" "),Activity_startTime=nxt.rec$Act.startime,Activity_duration=nxt.rec$Act.duration);
    } else {
      nxt.recNode=createNode(g,label2,Id=paste(nxt.rec$interval_of_Act,nxt.rec$Activity,sep="_"),Segment_of_Day=nxt.rec$interval_of_Act,  Name = paste(nxt.rec$Activity,paste("Segment",nxt.rec$interval_of_Act,sep=" "),sep=" "),Activity_startTime=nxt.rec$Act.startime,Activity_duration=nxt.rec$Act.duration);
    }
    
    if(label1=="Small_Duration_Activity" | label2=="Small_Duration_Activity"){
      small.dur.count=small.dur.count+1;
    }
    
    if(label1=="Medium_Duration_Activity" | label2=="Medium_Duration_Activity"){
      med.dur.count=med.dur.count+1;
    }
    
    if(label1=="Long_Duration_Activity" | label2=="Long_Duration_Activity"){
      long.dur.count=long.dur.count+1;
    }
    
    nxt.recNode=getOrCreateNode(g,label2,Id=paste(nxt.rec$interval_of_Act,nxt.rec$Activity,sep="_"),Segment_of_Day=nxt.rec$interval_of_Act,  Name = paste(nxt.rec$Activity, paste("Segment",nxt.rec$interval_of_Act,sep=" "),sep=" "),Activity_startTime=nxt.rec$Act.startime,Activity_duration=nxt.rec$Act.duration);
  #  nxt.recNode=getOrCreateNode(g,"Activity",Id=paste(nxt.rec$Day_of_week,paste(nxt.rec$interval_of_Act,nxt.rec$Activity,sep="_"),sep="_"),Segment_of_Day=nxt.rec$interval_of_Act,Name = nxt.rec$Activity,Activity=nxt.rec$Activity,Activity_startTime=nxt.rec$Act.startime,Activity_duration=nxt.rec$Act.duration);
    if((getID(recNode)!=getID(nxt.recNode))){
      createRel(recNode, "Next", nxt.recNode);
    }
    i=i+1;
}

#Go to the Neo4j UI at http://localhost:7474 and type the cypher query "match (m) return m" to visualize the nodes and edges formed

#Now, we need to combine these relationships into "NEXT" edges with a probability value 

rel.counts <- cypher(g,"match (node1)-[rel:Next]->(node2) with node1, rel, node2 return node1.Id as Node1,labels(node1) as label1, node2.Id as Node2,labels(node2) as label2, count(rel) as Rel_count")
rel.counts.grouped <- as.data.frame(rel.counts %>% group_by(label1,Node1) %>% summarise(tot_rels=sum(Rel_count)))
rel.counts.merged <- merge(rel.counts,rel.counts.grouped,by=c("label1","Node1"))
rel.counts.merged$prob <- round(rel.counts.merged[,"Rel_count"]/rel.counts.merged[,"tot_rels"],2)


limit=dim(rel.counts.merged)[1]
i=1
#Deleting all relationships
cypher(g,"match ()-[r:Next]->() delete r")
while(i<=limit){
  rec=rel.counts.merged[i,]
  node1=getOrCreateNode(g,rec$label1,Id=rec$Node1)
  node2=getOrCreateNode(g,rec$label2,Id=rec$Node2)
  createRel(node1,"NEXT",node2,Probability=rec$prob)
  i=i+1
}

#Go to the Neo4j UI at http://localhost:7474 and type the cypher query "match (m) return m" to visualize the nodes and aggregated edges
