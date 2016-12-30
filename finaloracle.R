#load data, only include official at bats
pbpfull<-read.csv("download.folder/unzipped/all2016.csv",header=FALSE)
fields<-read.csv("fields.csv")
names(pbpfull)<-fields[,"Header"]
pbpfull<-subset(pbpfull,AB_FL=="TRUE")

#extract needed variables, create hit & out (not a hit) variables
pbpfull.sm<-data.frame(pbpfull$BAT_ID,pbpfull$PIT_ID,pbpfull$H_FL)
names(pbpfull.sm)<-c("BAT_ID","PIT_ID","H_FL")
pbpfull.sm$HITS<-ifelse(pbpfull.sm$H_FL>0,1,0)
pbpfull.sm$OUT<-1-pbpfull.sm$HITS

#create list of batter and pitcher names
batters<-unique(pbpfull.sm$BAT_ID)
batters<-sort(batters)
pitchers<-unique(pbpfull.sm$PIT_ID)
pitchers<-sort(pitchers)

#count number of H,AB, etc by player
H<-aggregate(pbpfull.sm$HITS,list(pbpfull.sm$BAT_ID),sum)
AB<-aggregate(pbpfull.sm$HITS,list(pbpfull.sm$BAT_ID),length)
H<-data.frame(H)
AB<-data.frame(AB)
names(H)<-c("BAT_ID","H")
names(AB)<-c("BAT_ID","AB")
HP<-aggregate(pbpfull.sm$HITS,list(pbpfull.sm$PIT_ID),sum)
ABP<-aggregate(pbpfull.sm$HITS,list(pbpfull.sm$PIT_ID),length)
names(HP)<-c("PIT_ID","H")
names(ABP)<-c("PIT_ID","AB")
HP<-data.frame(HP)
ABP<-data.frame(ABP)


#assign number to each pitcher and batter in the play-by-play frame corresponding
#to list of names

battervec<-c(rep(0,nrow(pbpfull.sm)))
pitchervec<-c(rep(0,nrow(pbpfull.sm)))
for (i in 1:nrow(pbpfull.sm)){
  battervec[i]<-which(batters==pbpfull.sm$BAT_ID[i])
  pitchervec[i]<-which(pitchers==pbpfull.sm$PIT_ID[i])
}

#construct the adjacency matrices
bmatrix<-matrix(0,nrow = length(batters),ncol = length(pitchers))
pmatrix<-matrix(0,nrow = length(pitchers),ncol=length(batters))

for (i in 1:nrow(pbpfull.sm)){
  bmatrix[battervec[i],pitchervec[i]]<-bmatrix[battervec[i],pitchervec[i]]+pbpfull.sm$HIT[i]
#  bmatrix[battervec[i],pitchervec[i]]<-bmatrix[battervec[i],pitchervec[i]]+pbpfull.sm$H_FL[i]
  pmatrix[pitchervec[i],battervec[i]]<-pmatrix[pitchervec[i],battervec[i]]+pbpfull.sm$OUT[i]
}

#combine the matricies into a single adjacency matrix
baug<-matrix(nrow=length(batters),ncol=length(batters),0)
paug<-matrix(nrow=length(pitchers),ncol=length(pitchers),0)
bplus<-cbind(baug,bmatrix)
pplus<-cbind(pmatrix,paug)
oorg<-rbind(bplus,pplus) 

obdown<-(H$H+1)/(AB$AB+2) #oracle down vector  for batters(adjusted batting average)



opdown<-(ABP$AB-HP$H+1)/(ABP$AB+2) #down vector for pitchers
obdown1<-c(obdown,opdown)



oodown<-cbind(oorg,obdown1) #add down vector to adjacency matrix

obup<-c(AB$AB,ABP$AB,0) # up matrix using at bats (the 0 is for the oracle)
#obup<-c(rep(1,(length(batters)+length(pitchers))),0) #up matrix if 1 for all

oopresto<-rbind(oodown,obup) #full pre-stochastic matrix

stoey<-diag(1/colSums(oopresto)) #diagonal matrix with column sums
ostoc<-oopresto%*%stoey #stochastic matrix

library(expm)
ratingsm<-ostoc%^%500 #find stationary vector by exponentiating stochastic matrix

ratevec<-ratingsm[,1] #ratings vector

#split off batters and rescale
brate<-ratevec[1:length(batters)]
brate<-brate/max(brate)

bratings<-data.frame(batters,brate)

#split of pitchers and rescale
prate<-ratevec[(length(batters)+1):(length(ratevec)-1)]
prate<-prate/max(prate)

pratings<-data.frame(pitchers,prate)

names(bratings)<-c("BAT_ID","Rating")
names(pratings)<-c("PIT_ID","Rating")

#creating tables for presentation
BSTAT<-merge(H,AB)
BSTAT$BA<-BSTAT$H/BSTAT$AB
btable<-merge(bratings,BSTAT)



PSTAT<-merge(HP,ABP)
PSTAT$OBA<-PSTAT$H/PSTAT$AB
ptable<-merge(pratings,PSTAT)

master<-read.csv("nmaster.csv")
master2<-master[c("LAST","FIRST","ID")]
names(master2)[3]="playerID"
names(btable)[1]="playerID"

namelookup<-function(ID){
  pos<-which(ID==master2$playerID)
  paste(master2$nameFirst[pos],master2$nameLast[pos],sep=" ")
}

bbb=merge(btable,master2)

head(bbb[order(bbb$Rating,decreasing=TRUE),],10)
names(ptable)[1]="playerID"
ppp=merge(ptable,master2)
head(ppp[order(ppp$Rating,decreasing=TRUE),],10)


#blookup and plookup match IDs with ratings
blookup<-function(player){
  pos<-match(player,bratings$BAT_ID)
  return(bratings$Rating[pos])
  
}

plookup<-function(player){
  pos<-match(player,pratings$PIT_ID)
  return(pratings$Rating[pos])
  
}

#adds picther and batter ratings to play-by-play data frame
pbpfull.sm$br<-blookup(pbpfull.sm$BAT_ID)
pbpfull.sm$pr<-plookup(pbpfull.sm$PIT_ID)

#to save full ratings df
bbbn<-bbb[,2:7]
bbbn$Name<-paste(bbbn$FIRST,bbbn$LAST,sep=" ")
bbbn<-bbbn[,c(7,2,3,4,1)]
write.csv(bbbn,"batters2016.csv",row.names=FALSE)

pppn<-ppp[,2:7]
pppn$Name<-paste(pppn$FIRST,pppn$LAST,sep=" ")
pppn<-pppn[,c(7,2,3,4,1)]
write.csv(pppn,"pitchers2016.csv",row.names=FALSE)
