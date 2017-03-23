##################################################################################

#install.packages("randomForest")







library('randomForest')



system.time(

	for(i in 1:1000){

		

		#set.seed(i)

		iris <- read.csv("./rawfs.csv", dec=".",sep=",",head=TRUE)

		numofcol<-ncol(iris)

		ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.9, 0.1))

		trainData <- iris[ind == 1, ]

		testData <- iris[ind == 2, ]

		rf <- randomForest(trainData[,2:numofcol],trainData[,1], ntree = 1000, proximity = TRUE, importance = TRUE)

		capture.output(print(rf),file="./rawfs_value_result.txt",append=TRUE)



		importancevalue<-importance(rf)

		importancevalue<-importancevalue[order(-importancevalue[,1]),]

			

		numofcol<-numofcol-1

		iv.df<-data.frame(importancevalue[,0],importancevalue[,1])

		names(iv.df)[1]<-c("Count")

						

		ranking<-c(1:numofcol)

		iv.df$rank<-ranking

		

		iv.df<-subset(iv.df, select = -Count)

				

		write.table(iv.df,file="./rawfs_sub_result.csv",append=TRUE,row.names=TRUE,col.names=FALSE,dec=".",sep=",")

	}

)



totalrank <- read.csv("./rawfs_sub_result.csv",dec=".",sep=",",head=FALSE)

conditionmake <-read.csv("./rawfs_sub_result.csv",dec=".",sep=",",head=FALSE,nrows=numofcol)

total.df<-data.frame(totalrank)



mergerank.df<-(conditionmake$V1)

mergerank.mr<-as.matrix(mergerank.df)



sum.mr<-c()

for(i in 1:numofcol){

	

condition<-mergerank.mr[i]

sub.df<-data.frame(total.df[total.df$V1==condition,])

sub.mr<-as.matrix(sub.df$V2)

Sum<-colSums(sub.mr)

sum.mr<-c(sum.mr,Sum)



}



result.df<-data.frame(mergerank.mr,sum.mr)

names(result.df)[1]<-c("Name")

names(result.df)[2]<-c("Total")



result.df<-result.df[order(result.df[,2]),]

write.table(result.df,file="./rawfs_result.csv",row.names=FALSE,col.names=FALSE,dec=".",sep=",")


#########################################################################################################

iris_raw <- read.csv("./rawfs.csv", dec=".",sep=",",head=TRUE)
top20 <- as.data.frame(iris_raw[,"mutation"])
names(top20)<-"mutation"

before_result <-read.csv("./rawfs_result.csv",  dec=".",sep=",",head=FALSE)

for(i in 1:20){
	name <- as.character(before_result[i,1])
	top20_sub <- as.data.frame(iris_raw[,name])
	names(top20_sub)<-name
	top20 <- cbind(top20, top20_sub)
}

dir.create("top20")
setwd("./top20")

write.table(top20,file="./top20.csv",row.names=FALSE,col.names=TRUE,dec=".",sep=",")






















#############################################################################################################
#install.packages("randomForest")
#############################################################################################################


library('randomForest')

for(j in 20:2){

root<-'./top'
rawreadroot<-paste(root,j,'.csv',sep="")
valueroot<-paste(root,j,'_value_result.txt',sep="")
subresultroot<-paste(root,j,'_sub_result.csv',sep="")
resultroot<-paste(root,j,'_result.csv',sep="")
nextroot<-paste(root,j-1,'.csv',sep="")

system.time(

	

	for(i in 1:1000){

		#set.seed(i)
		iris <- read.csv(rawreadroot, dec=".",sep=",",head=TRUE)
		numofcol<-ncol(iris)

		ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.9, 0.1))
		trainData <- iris[ind == 1, ]
		testData <- iris[ind == 2, ]
		rf <- randomForest(trainData[,2:numofcol],trainData[,1], ntree = 1000, proximity = TRUE, importance = TRUE)
		capture.output(print(rf),file=valueroot,append=TRUE)

		importancevalue<-importance(rf)
		importancevalue<-importancevalue[order(-importancevalue[,1]),]
			
		numofcol<-numofcol-1
		iv.df<-data.frame(importancevalue[,0],importancevalue[,1])
		names(iv.df)[1]<-c("Count")
						
		ranking<-c(1:numofcol)
		iv.df$rank<-ranking
		
		iv.df<-subset(iv.df, select = -Count)
				
		write.table(iv.df,file=subresultroot,append=TRUE,row.names=TRUE,col.names=FALSE,dec=".",sep=",")
	}
)

totalrank <- read.csv(subresultroot,dec=".",sep=",",head=FALSE)
conditionmake <-read.csv(subresultroot,dec=".",sep=",",head=FALSE,nrows=numofcol)
total.df<-data.frame(totalrank)

mergerank.df<-(conditionmake$V1)
mergerank.mr<-as.matrix(mergerank.df)

sum.mr<-c()
for(i in 1:numofcol){
	
condition<-mergerank.mr[i]
sub.df<-data.frame(total.df[total.df$V1==condition,])
sub.mr<-as.matrix(sub.df$V2)
Sum<-colSums(sub.mr)
sum.mr<-c(sum.mr,Sum)

}

result.df<-data.frame(mergerank.mr,sum.mr)
names(result.df)[1]<-c("Name")
names(result.df)[2]<-c("Total")

result.df<-result.df[order(result.df[,2]),]
write.table(result.df,file=resultroot,row.names=FALSE,col.names=FALSE,dec=".",sep=",")

condition2<-result.df[j,1]
nextlevel.df<-data.frame(iris)

dellastnextlevel.df<-nextlevel.df[,-which(colnames(nextlevel.df) == condition2)]
write.table(dellastnextlevel.df,file=nextroot,row.names=FALSE,col.names=TRUE,dec=".",sep=",")


}




j<-'1'
rawreadroot<-paste(root,j,'.csv',sep="")
valueroot<-paste(root,j,'_value_result.txt',sep="")



system.time(

	

	for(i in 1:1000){
		
		#set.seed(i)
		iris <- read.csv(rawreadroot, dec=".",sep=",",head=TRUE)
		numofcol<-ncol(iris)

		ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.9, 0.1))
		trainData <- iris[ind == 1, ]
		testData <- iris[ind == 2, ]
		rf <- randomForest(trainData[,1]~trainData[,2], ntree = 1000, proximity = TRUE)
		capture.output(print(rf),file=valueroot,append=TRUE)

									
		
	}
)






