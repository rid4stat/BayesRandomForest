

################################# comparison of bayesian Random Forest with#####
############################### competitors ####################################


library(caret)
library(rpart)
library(randomForest)
library(datamicroarray)
library(ranger)
library(bartMachine)
library(gbm)


friedmandata=function(N,p){

set.seed(1)

    E = rnorm(N)

    x = scale(data.frame(matrix(runif(N*p),N,p)))

    x1 = x[,1] ; x2 = x[,2] ; x3 = x[,3] ; x4 = x[,4] ; x5 = x[,5]

    y = 10*sin(pi*x1*x2) + 20*(x3 - 0.5)^2 + 10*x4 + 5*x5 + E


         data.frame(y,x)
 }

#fredmandata(10,10)

RMSE = function(yob,yp) { sqrt(mean((yob-yp)^2))}



comp2 = function(datay,t.alpha, fold)

{

      pp = ncol(datay[,-1])

      t.selection <- apply(datay[,-1], 2, function(x)
      coef(summary(lm(datay$y~x)))[2,4])
      t.selection <- sort(t.selection[which(t.selection <= t.alpha)])

      datay2 <- subset(datay, select = c("y", names(t.selection)))  # New data with primarily selected features using t-statistics




x.names = colnames(datay2[,-1])



y = datay[,1]

set.seed(123)
folds=createFolds(y, k = fold, list = TRUE, returnTrain = FALSE)

g = lapply(folds,function(x){


    testIndexes <- folds[[1]]

      test1 <- datay[testIndexes, ]
      train1 <- datay[-testIndexes, ]

      test2 <- datay[testIndexes, c("y",x.names)]
      train2 <- datay[-testIndexes, c("y",x.names) ]



    pr = (nrow(datay)+2)/(4*nrow(datay))


    #nn = nrow(datay)
    #a = round(0.5*nn) ; b = nn-a  ; a.pos = a+1; b.pos = b+nn-1
    wei = dbinom(rbinom(nrow(train1),1,pr),1,pr)

    
    #wei = rbeta(nrow(train1),a.pos,b.pos)
    wei2 = rexp(nrow(train1),1)
    p1 = ncol(train1[,-1])
    p2 = ncol(train2[,-1])
    psp = ifelse(p2>floor(p1/3),floor(p/3),floor(p2*(1-t.alpha)))

    
    brf = randomForest(y~., data = train2,mtry = psp)

    bf = ranger(y~.,data = train1,case.weights = wei2)



    rf = randomForest(y~., data = train1)

    bart <- bartMachine(train1[,-1], train1[,1],verbose=FALSE)


    gbm=gbm(y~.,data=train1,interaction.depth = 4,distribution = "gaussian",
                                       n.trees = 500,
                                       shrinkage = .1,
                                       n.minobsinnode = 1)






    rmse2 = RMSE(test1$y,predict(brf, newdata = test2))
    
    rmse3 = RMSE(test1$y,predict(bf, data = test1)$predictions)


    rmse4 = RMSE(test1$y,predict(rf, newdata = test1))

    rmse5 = RMSE(test1$y,predict(bart, test1[, -1]))

    rmse6 = RMSE(test1$y,predict(gbm, test1[, -1], type = "response",n.trees=500))





    all.rmse = rbind(rmse2,rmse3,rmse4,rmse5,rmse6)






    return(all.rmse)
    })

result = sapply(g,function(x){x})






rownames(result) = c("BRF","BF","RF","BART","GBM")

return(result)
}



re1 = comp2(friedmandata(100,100),0.1,10)
fresult1 = matrix(rowMeans(re1,na.rm=T),5,1,dimnames= list(rownames(re1),"RMSE"))

re2 = comp2(friedmandata(100,500),0.1,10)
fresult2 = matrix(rowMeans(re2,na.rm=T),5,1,dimnames= list(rownames(re2),"RMSE"))

re3 = comp2(friedmandata(100,1000),0.1,10)
fresult3 = matrix(rowMeans(re3,na.rm=T),5,1,dimnames= list(rownames(re3),"RMSE"))

re4 = comp2(friedmandata(100,5000),0.1,10)
fresult4 = matrix(rowMeans(re4,na.rm=T),5,1,dimnames= list(rownames(re4),"RMSE"))

re5 = comp2(friedmandata(100,10000),0.1,10)
fresult5 = matrix(rowMeans(re5,na.rm=T),5,1,dimnames= list(rownames(re4),"RMSE"))

all.folds = cbind(re1,re2,re3,re4,re5)
all.final = cbind(fresult1, fresult2,fresult3,fresult4,fresult5)

 all.final

boxplot(all.folds,col=2:7)
boxplot(t(all.folds),col=2:7)
boxplot(t(all.folds[,1:10]),col=2:7)
boxplot(t(all.folds[,11:20]),col=2:7)
boxplot(t(all.folds[,21:30]),col=2:7)
boxplot(t(all.folds[,31:40]),col=2:7)



df = data.frame(RMSE = c(all.final),
                p = gl(5,5,labels=c("100","500","1000","5000","10000")),
                Method = rep(c("BRF","BF","RF","BART","GBM"),5))


#### plot over p

ggplot(df, aes(x=p, y=RMSE, group=Method)) +
  geom_line(aes(linetype=Method,color=Method),size=1.3)+
  geom_point(aes(shape=Method))