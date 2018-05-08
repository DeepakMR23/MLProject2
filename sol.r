library(caret)
require(Rmosek)

Sigmoid <- function(X)
{
	1/(1 + exp(-X))
}

ProjectSolution <- function()
{
	currentWorkDir <- getwd()
	setwd('C:\\Users\\new\\Desktop\\Studies\\Sem2\\ML\\Project\\BlogFeedback')

	trainAll <- read.csv('blogData_train.csv', header=FALSE)
	set.seed(123)

	trainAllSample <- trainAll[sample(nrow(trainAll), 5000), ]
	trInd <- sample(5000, 3750)
	
	trainAllSampleTr <- trainAllSample[trInd, ]
	trainAllSampleTst <- trainAllSample[-trInd, ]
	
	print(nrow(trainAllSampleTr))
	
	
	optModel <- list()
	optModel$sense <- "min"
	#optModel$c <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
	
	
	require(Rmosek)
	xMatrix <- data.matrix(trainAllSampleTr[,51:60])
	yMatrix <- data.matrix(trainAllSampleTr[,"V281"])
	
	
	#cMatrix <- cbind(yMatrix, xMatrix)
	cMatrix <- crossprod(xMatrix,yMatrix)
	optModel$c <- as.vector(cMatrix)
	
	X2Matrix <- crossprod(xMatrix, xMatrix)
	X2Matrix[upper.tri(X2Matrix)]<-0
	inds <- which(X2Matrix != 0, arr.ind=TRUE)
	optModel$qobj <- list(i = inds[,1], j = inds[,2], v = X2Matrix[inds])
	
	optModel$A <- Matrix( rep(0, 10), nrow = 1, byrow=TRUE, sparse=TRUE )

	blc<- c(-Inf)
	buc<- c(Inf)
	
	#blc<-as.vector(yMatrix)
	#buc<-as.vector(yMatrix)
	
	optModel$bc <- rbind(blc, buc)
	#blx<-c(min(cMatrix[,1]), min(cMatrix[,2]),  min(cMatrix[,3]),  min(cMatrix[,4]),  min(cMatrix[,5]),  min(cMatrix[,6]),  min(cMatrix[,7]),  min(cMatrix[,8]),  min(cMatrix[,9]),  min(cMatrix[,10]))
	#bux<-c(max(cMatrix[,1]), max(cMatrix[,2]),  max(cMatrix[,3]),  max(cMatrix[,4]),  max(cMatrix[,5]),  max(cMatrix[,6]),  max(cMatrix[,7]),  max(cMatrix[,8]),  max(cMatrix[,9]),  max(cMatrix[,10]))
	
	blx<-rep(-Inf, 10)
	bux<-rep(Inf, 10)
	
	optModel$bx<-rbind(blx, bux)
	r<-mosek(optModel)
	
	#print(r)
	
	print("Train Data - Mosek Error")
	CheckErrorMosekParams(trainAllSampleTr[, 51:60], trainAllSampleTr[, "V281"])
	print("Test Data - Mosek Error")
	CheckErrorMosekParams(trainAllSampleTst[, 51:60], trainAllSampleTst[, "V281"])
	
	
	CheckLMErrors(trainAllSampleTr, trainAllSampleTst)
}

CheckErrorMosekParams <- function(data, target){

	yHatMat = data[,1]*-0.20473016 + data[,2]*-0.28727579 + data[,3]*0.02768148 + data[,4]*0.21594995 + data[,5]*0.00000000 + data[,6]*2.30850597 + data[,7]*-3.28659353 + data[,8]*-1.41793199 + data[,9]*-1.78338969
	#print(yHatMat)
	
	errors <- (target - yHatMat)*(target - yHatMat)
	#print(errors)
	
	mse <- mean(errors)

	print(mse)
}

CheckLMErrors <- function(data, testD) {

	datafr <- data[, c(51:60, 281)]
	print(colnames(datafr))
	
	lmodel <- lm(V281~., data=datafr)
	print("Train Data - LM Error")
	print(mean(lmodel$residuals^2))
	#print(lmodel)
	
	predicted <- predict(lmodel, testD[, c(51:60, 281)], se.fit=TRUE)
		
	MSE = mean((predicted$fit - testD[,281])^2)
	print("Test Data - LM Error")
	print(MSE)
}