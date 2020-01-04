---
title: "Week 8 Project"
author: "Shingleton"
date: "1/3/2020"
output: 
  html_document: 
    keep_md: yes
---



## Predicting the classification of barbell lifts

This project will use different methods to predict the classe of dumbbell lifts using different measurements taken from an accelerometer on 6 different participants. The classes of lifts are:

-A: exactly according to the specification 
-B: throwing the elbows to the front
-C: lifting the dumbbell only halfway
-D: lowering the dumbbell only halfway
-E: and throwing the hips to the front (Class E).

We are given a training and a test set to predict the results. First, we explored the data. Upon inspection of the data, we found a large number of missing and NA values in various columns. These equated to 19212 values for a data set that contains 19622 values, meaning that these variables were missing 98% of the observations. We took this out of the training data set and also removed: (X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window). We also looked at home many observations of each classification the training set contained.


```r
nas<-data.frame(coln=names(training), nasin=0)
for(i in 1:dim(nas)[1]){
  if(length(summary(training[,i]))==7)
    nas$nasin[i]<-summary(training[,i])[7]
  nas$nasin[i]<-length(training[,i][training[,i]==""])
}

tokeep<-as.character(nas$coln[nas$nasin==0])
training2<-training[,tokeep]
training2<-subset(training2,select=-c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window))

training2%>%
  group_by(classe)%>%
  summarise(count=n())
```

There are more correctly performed repetitions then incorrect and the incorrect seem to be evenly distributed.

We will try four different types of models in this project: classification tree, random forest, boosted regression and linear discriminant. We will also add 10-fold cross validation into each of our models, as the caret package includes this as a parameter. First the classification tree. This is the only model that we will plot, as it looks nice.

```r
glmnet_ctrl <- trainControl(method = "cv", number = 10) ##integrating the cross validation into the model
tree1<-train(classe~., data=training2, method='rpart', trControl = glmnet_ctrl)
fancyRpartPlot(tree1$finalModel)
```

![](Final-Project_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
Now we will create the other three models and see how accurate they were on the training data. We used a subset of 1000 observations for our models, as trying to use the ensure data set took too long.


```r
training4<-sample_n(training2, 1000)
glmnet_ctrl<-trainControl(method="cv", number=10)
preforest<-train(classe~., data=training4, method='rf', trControl = glmnet_ctrl)
g2fit<-train(classe~., data=training4, method='gbm', trControl = glmnet_ctrl)
l2fit<-train(classe~., data=training4, method='lda', trControl = glmnet_ctrl)


preftree<-predict(tree1)
pref<-predict(preforest, newdata=training)
predg<-predict(g2fit, newdata=training)
predl<-predict(l2fit, newdata=training)

df2<-data.frame(true=training2$classe, predtree=preftree,
                predrf=pref,
                predg=predg,
                predl=predl)

treec<-sum(diag(table(training2$classe, preftree)))/dim(training2)[1]*100
rfc<-sum(diag(table(training2$classe, pref)))/dim(training2)[1]*100
gc<-sum(diag(table(training2$classe, predg)))/dim(training2)[1]*100
lc<-sum(diag(table(training2$classe, predl)))/dim(training2)[1]*100
```
Now, we will take a look at the correct prediction percentages for each of the models. This will be evaluated using confusion matrices and the correct prediction is in the title of each matrix.


```r
df3<-df2%>%
  group_by(true, predtree)%>%
  summarise(count=n())

treecm<-ggplot(data= df3, aes(x=predtree, y=true, fill=count)) + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  geom_tile()+
  labs(x="Prediction", y="True", title=paste0("Tree: ", round(treec, 1), "% Correct Prediction"))

df3<-df2%>%
  group_by(true, predrf)%>%
  summarise(count=n())

rfcm<-ggplot(data = df3, aes(x=predrf, y=true, fill=count)) + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  geom_tile()+
  labs(x="Prediction", y="True", title=paste0("Random Forest: ", round(rfc, 1), "% Correct Prediction"))

df3<-df2%>%
  group_by(true, predg)%>%
  summarise(count=n())

gcm<-ggplot(data = df3, aes(x=predg, y=true, fill=count)) + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  geom_tile()+
  labs(x="Prediction", y="True", title=paste0("Boosted Regression: ", round(gc, 1), "% Correct Prediction"))

df3<-df2%>%
  group_by(true, predl)%>%
  summarise(count=n())

lcm<-ggplot(data = df3, aes(x=predl, y=true, fill=count)) + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  geom_tile()+
  labs(x="Prediction", y="True", title=paste0("Linear Discriminant: ", round(lc, 1), "% Correct Prediction"))

grid.arrange(treecm, rfcm, gcm, lcm, ncol=2)
```

![](Final-Project_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Finally, we will use all four of these models to predict on the test set. We will predict on the test set and chose the majority answer given by each of the models as our true prediction.


```r
treetesting<-predict(tree1, newdata=testing)
rftesting<-predict(preforest, newdata=testing)
gtesting<-predict(g2fit, newdata=testing)
ltesting<-predict(l2fit, newdata=testing)

dfinal<-data.frame(treetesting, rftesting, gtesting, ltesting)

dfinal$conclusion<-"Z"
for(i in 1:20){
  
  test1<-dfinal[i,]
  holder1<-as.data.frame(t(test1))
  names(holder1)<-"this"
  holder1<-holder1%>%
    group_by(this)%>%
    summarize(count=n())
  print(holder1)
  
  dfinal$conclusion[i]<-as.character(holder1$this[holder1$count==max(holder1$count)])
}
```

```
## Warning in dfinal$conclusion[i] <- as.character(holder1$this[holder1$count
## == : number of items to replace is not a multiple of replacement length

## Warning in dfinal$conclusion[i] <- as.character(holder1$this[holder1$count
## == : number of items to replace is not a multiple of replacement length
```

Our final prediction for the 20 values:


```r
dfinal
```

```
##    treetesting rftesting gtesting ltesting conclusion
## 1            C         B        B        B          B
## 2            A         A        A        A          A
## 3            C         A        B        B          B
## 4            A         A        C        C          A
## 5            A         A        A        A          A
## 6            C         E        E        E          E
## 7            C         D        D        D          D
## 8            A         B        B        B          B
## 9            A         A        A        A          A
## 10           A         A        A        A          A
## 11           C         B        B        D          B
## 12           C         C        C        A          C
## 13           C         B        B        B          B
## 14           A         A        A        A          A
## 15           C         E        E        E          E
## 16           A         E        E        A          A
## 17           A         A        A        A          A
## 18           A         B        B        B          B
## 19           A         B        B        B          B
## 20           C         B        B        B          B
```

##Out of Sample Error

We do not believe that our out of sample error will be bad for this method. We have around a 90% correct rate on the training data and checking our prediction against the quiz, we were 85% correct.

##Why I chose this method
I decided on using the majority of four different models after seeing the sub par performance of a classification tree. With only 50% correct prediction rate, I decided it would be better to use the majority rule of many different models, than just settling on one model.
