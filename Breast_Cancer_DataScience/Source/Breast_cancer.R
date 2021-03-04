

# df1=read.table("C:/Users/MEHDI/OneDrive/Documents/MS Big Data/Data Science/Ahlame Douzal/Project/data/breast-cancer-wisconsin.data",
#               sep=",")
# remove(df1)

#names of vectors in string list
names <- c('id_number', 'diagnosis', 'radius_mean', 
           'texture_mean', 'perimeter_mean', 'area_mean', 
           'smoothness_mean', 'compactness_mean', 
           'concavity_mean','concave_points_mean', 
           'symmetry_mean', 'fractal_dimension_mean',
           'radius_se', 'texture_se', 'perimeter_se', 
           'area_se', 'smoothness_se', 'compactness_se', 
           'concavity_se', 'concave_points_se', 
           'symmetry_se', 'fractal_dimension_se', 
           'radius_worst', 'texture_worst', 
           'perimeter_worst', 'area_worst', 
           'smoothness_worst', 'compactness_worst', 
           'concavity_worst', 'concave_points_worst', 
           'symmetry_worst', 'fractal_dimension_worst')
#load the data
df=read.table("C:/Users/MEHDI/OneDrive/Documents/MS Big Data/Data Science/Ahlame Douzal/Project/data/wdbc.data",
              sep=",",col.names = names)

#remove id_number, useless info
df$id_number=NULL

#show the first lines of the dataset
head(df)

# view the dimension the dataset
dim(df)

# list the dataset features with type and sample of contained values
str(df)

#any missing data ?
any(is.na(df))

#let's percentage of each class in the data

counts=as.data.frame(table(df$diagnosis))
colnames(counts)[1]="Class"
counts$prc=round(100*counts$Freq/(apply(X = counts[2],MARGIN = 2,FUN = sum)),digits = 2)

library(ggplot2)
library(scales)
ggplot(counts, aes(x="", y=prc, fill=Class)) +
  geom_bar(stat="identity", width=1,) +
  coord_polar("y")+
  ggtitle("Data imbalance")+
  theme( 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold",hjust = 0.5)
  )+
  geom_text(aes(y = prc + c(10, -17),
                label = percent(prc/100)), size=5)+
  geom_text(aes(y = prc + c(5, -12),
                label = paste(Freq, " Obs", sep="")), size=5)


#let's take a look at he summary fo the data
summary(df)
data.frame(unclass(summary(df)), check.names = FALSE, stringsAsFactors = FALSE)
# do.call(cbind, lapply(df, summary))

#to have a reproducible result we will fix the type of used hazard
set.seed(1)

#plot a box plot for the data
par(mar=c(4,8,2,2))
boxplot(df[2:31],
        main="fig1: Boxplot of cells' features",
        horizontal = TRUE,
        notch = T,
        names = colnames(df)[2:31],
        las=2,
        cex.axis=0.7
        
        )


#option: remove big variables a new box plot

#features interdependency check

plot(df[1:31],col=c("red","blue")[1+(df$diagnosis=="M")])

# correlation heatmap

cormat= round(cor(df[2:31]),2)
head(cormat)

library(reshape2)
melted_cormat <- melt(cormat)
melted_cormat$sig= apply(melted_cormat[3],MARGIN = 1,function(x){if(x>0.7 || x< -0.7){x}else{NA}  })
head(melted_cormat)

library(ggplot2)
par(mar=c(8,8,2,2))
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value,label=round(sig,2))) + 
  geom_tile()+
  labs(x = NULL, y = NULL, fill = "Pearson's\nCorrelation",
       title="Correlations between Breast Cancer features",
       subtitle="Only significant Pearson's correlation coefficients shown")+
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  geom_text(size=5) +
  theme_classic() +
  scale_x_discrete(expand=c(0,0))+
  scale_y_discrete(expand=c(0,0))+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,size=12,face = "bold"),
        axis.text = element_text(face = "bold",size = 12))


#Unsupervised Machine Learning
X=df[-1]
y=df[1]
head(X)

#kmeans-------------------------------------------------
#Elbow Method for finding the optimal number of clusters
set.seed(17)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
Xs <- scale(X,center = T,scale = T)
wss <- sapply(1:k.max, 
              function(k){kmeans(Xs, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total clusters'withinness")
km=cbind.data.frame(1:k.max,wss)
colnames(km)[1]="k"
ggplot(km,aes(x = k,y = wss,group = 1 ))+
  geom_line(color="red",size=1)+
  geom_point()+
  labs(title = "Clusters'intravariance per Number of cluster", 
       subtitle = "Kmeans Method",
       x="Number of cluster k",
       y="Clusters' withinness")+
  scale_x_continuous(breaks = seq(1, k.max, by = 1))+
  theme(plot.title=element_text(size=14, face="bold",hjust = 0.5),
        plot.subtitle = element_text(size=14,hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

#plot Kmeans

library(factoextra)
km=kmeans(Xs,2)
fviz_cluster(km, Xs, ellipse.type = "norm")+
  labs(title = "Kmeans Clusters after PCA")+
  theme(plot.title=element_text(size=14, face="bold",hjust = 0.5))

#clusters's centers features

library(reshape2)
gr=kmeans(Xs,2)
gr$centers
medo=as.data.frame(gr$centers)
medo$gr=as.factor(c(1,2))
data_long <- melt(medo,id=c("gr"))
data_long

ggplot(data_long, aes(x = factor(variable), y = value,group=gr, color = factor(gr))) +
  geom_line(size=1,aes())+
  labs(color="Clusters",title = "Clusters' Center features", subtitle = "Kmeans Method",
       x="Features",y="Value")+
  theme(plot.title=element_text(size=14, face="bold",hjust = 0.5),
        plot.subtitle = element_text(size=14,hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5))

#pam----------------------------------------
library(cluster)
gr=pam(X,2)
# mean(gr$silinfo$widths[,3])

par(mfrow=c(1,1))
plot(gr)
attributes(gr)

#Optimal k for PAM

set.seed(17)
# Compute and plot silhouette for k = 2 to k = 15.
# k.max <- 15
# Xs <- scale(X,center = T,scale = T)
sl <- sapply(2:k.max, 
              function(k){mean(pam(Xs, k)$silinfo$widths[,3])})
sl
plot(2:k.max, sl,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Average clusters' silhouette")
km=cbind.data.frame(2:k.max,sl)
colnames(km)[1]="k"
ggplot(km,aes(x = k,y = sl,group = 1 ))+
  geom_line(color="blue",size=1)+
  geom_point()+
  labs(title = "Clusters'average silhouette per Number of cluster", 
       subtitle = "PAM Method",
       x="Number of cluster k",
       y="Average clsuters' silhouette")+
  scale_x_continuous(breaks = seq(2, k.max, by = 1))+
  theme(plot.title=element_text(size=14, face="bold",hjust = 0.5),
        plot.subtitle = element_text(size=14,hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

#plot PAM

library(factoextra)
gr=pam(Xs,2)
fviz_cluster(gr, Xs, ellipse.type = "norm")+
  labs(title = "PAM Clusters after PCA")+
  theme(plot.title=element_text(size=14, face="bold",hjust = 0.5))
plot(gr)
#medoids features
library(reshape2)
# medoids=Xs
# medoids$clus=as.factor(gr$clustering)
# medoids$clus=factor(medoids$clus)
medo=as.data.frame(gr$medoids)
medo$gr=as.factor(c(1,2))
data_long <- melt(medo,id=c("gr"))
# data_long <- gather(medoids, caracteristica, valor, Murder:Rape, factor_key=TRUE)
data_long

ggplot(data_long, aes(x = factor(variable), y = value,group=gr, color = factor(gr))) +
  geom_line(size=1,aes())+
  labs(color="Clusters",title = "Clusters' Medoid features", subtitle = "PAM Method",
       x="Features",y="Value")+
  theme(plot.title=element_text(size=14, face="bold",hjust = 0.5),
        plot.subtitle = element_text(size=14,hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5))




#CAH---------------------------------------------
D=dist(Xs, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
# ??hclust
gr=hclust(D)
gr
round(gr$height,1)[(length(gr$height)-13):length(gr$height)]

hc=cbind.data.frame(1:k.max,rev(round(gr$height,1)[(length(gr$height)-14):length(gr$height)]))
colnames(hc)=c("k","nb_hc")
#optimal number of clusters CAH
ggplot(hc,aes(x = k,y = nb_hc,group = 1 ))+
  geom_line(color="blue",size=1)+
  geom_point()+
  labs(title = "Clusters' height (interdistance) per Number of cluster", 
       subtitle = "Hierarchical-clustering Method",
       x="Number of cluster k",
       y="Average clusters' interdistance gain per split")+
  scale_x_continuous(breaks = seq(1, k.max, by = 1))+
  theme(plot.title=element_text(size=14, face="bold",hjust = 0.5),
        plot.subtitle = element_text(size=14,hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  geom_abline(intercept = hc[1,2],slope = 0,col="red",size=1.05,lty=2)+
  geom_abline(intercept = hc[2,2],slope = 0,col="red",size=1.05,lty=2)+
  geom_abline(intercept = hc[3,2],slope = 0,col="red",size=1.05,lty=2)+
  geom_abline(intercept = hc[4,2],slope = 0,col="red",size=1.05,lty=2)

#Plot CAH
# par(mfrow=c(1,1))
D=dist(Xs, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
gr=hclust(D)
# plot(gr,hang = -1)

library(factoextra)
fviz_dend(gr, cex = 0.5, k = 3,
          color_labels_by_k = TRUE, rect = TRUE)

df[,1]=factor(df[,1])

#supervised learning
#Decision Tree
library(rpart)
nbbt=10
percentage=0.7
# bootstrap=function(X,y,nbbt=10,percentage){
  index=as.numeric(row.names(X))
  test_error=list()
  for (i in 1:nbbt){
    sub=c(sample(index[df[,1]=="B"],replace = T,size = percentage*length(index[df[,1]=="B"])),
          sample(index[df[,1]=="M"],replace = T,size = percentage*length(index[df[,1]=="M"])))
    # X_train=X[tidx,]
    # X_test=X[-tidx,]
    # y_train=y[tidx]
    # y_test=y[-tidx]
    dtr=rpart(factor(df$diagnosis)~.,data = df,subset = sub)
    y_pred=predict(object = dtr,newdata = df[-sub,],type = "class")
    test_error[i]=sum(df[-sub,1]==y_pred)/length(df[-sub,1])
  }
#   return(as.numeric(test_error))
# }

mean(as.numeric(test_error))
var(as.numeric(test_error))

plot(dtr)
text(dtr)
tab=table(predict(dtr, df[-sub,], type="class"), df[-sub, "diagnosis"])
sum(tab)
boxplot(as.numeric(test_error),main="DTree Test Error Distribution")

#DTree Variable Importance
plot(dtr$variable.importance,x = factor(c(names(dtr$variable.importance) )))

data = as.data.frame(dtr$variable.importance)
colnames(data)=c("varimp")
data$fea=factor(c(names(dtr$variable.importance) ))
row.names(data)=NULL

ggplot(data = data,aes(x=fea,y = varimp) )+
  geom_bar(stat = "identity")+
  geom_text(aes(label=round(varimp,0)),vjust=-1)+
  labs(title = "Features Importance in decision tree splitting", 
       subtitle = "Decision Tree Method",
       x="Features",
       y="Features' Importance")+
  theme(plot.title=element_text(size=14, face="bold",hjust = 0.5),
        plot.subtitle = element_text(size=14,hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5))


#Random forest----------------------------------------------------------
library(randomForest)
rf=randomForest(X[,1:30],y =factor(df[,1]),ntree = 100,mtry = 3,strata =factor(df[,1]) )
rf$err.rate

#bootstrapping validation
val_mat=data.frame()
colnames(val_mat)=c("E1","E2")
val_mat[1,]=c(1,2,3)
.rowNamesDF(val_mat, make.names=T) <- c("E1","E2")
row.names(val_mat)=c("E1","E2")

for (i in 1:nbbtv){
  index=as.numeric(row.names(X))
  tidx=sample(index,replace = T,size = 0.6*length(index))
  vidx=sample(index[-tidx],replace = F,size = 0,2*length(index[-tidx]))
  X_train=X[tidx,]
  X_val=X[- index[-vidx],]
  y_train=y[vidx]
  y_val=y[- index[-vidx]]
  for (j in alpha){
    for (k in beta){
      
      rf=randomForest(X_train,y =factor(y_train),ntree = j,mtry = k,strata =factor(y_train) )
      y_pred=predict(object = rf,newdata = X_test)
      
    }
    
    
  }
  
  
}


#Bootstrapping testing assess the model

bootstrap=function(X,y,ntree,mtry,nbbt=10,percentage){
  index=as.numeric(row.names(X))
  test_error=list()
  for (i in 1:nbbt){
    tidx=sample(index,replace = T,size = percentage*length(index))
    X_train=X[tidx,]
    X_test=X[-tidx,]
    y_train=y[tidx]
    y_test=y[-tidx]
    rf=randomForest(X_train,y =y_train,ntree = ntree,mtry = mtry,strata =factor(y_train) )
    y_pred=predict(object = rf,newdata = X_test)
    test_error[i]=sum(y_test==y_pred)/length(y_test)
  }
  return(as.numeric(test_error))
}

test_error=bootstrap(X=X,y=factor(df[,1]),ntree = 100,mtry = 3,nbbt = 100,percentage = 0.7)
mean(test_error)
var(test_error)
rf$confusion
boxplot(as.numeric(test_error),main="RandomForest Test Error Distribution")

table(predict(rf, df[-sub,], type="class"), df[-sub, "diagnosis"])










nbbt=10
test_error=list()
for (i in 1:nbbt){
  tidx=sample(index,replace = T,size = 0.7*length(index))
  X_train=X[tidx,]
  X_test=X[-tidx,]
  y_train=df[tidx,1]
  y_test=df[-tidx,1]
  rf=randomForest(X_train,y =factor(y_train),ntree = 100,mtry = 3,strata =factor(y_train) )
  y_pred=predict(object = rf,newdata = X_test)
  test_error[[i]]=sum(y_test==y_pred)/length(y_test)
}

index=as.numeric(row.names(df))
tidx=sample(index,replace = T,size = 0.8*length(index))
df[-tidx,]

predict(object = rf,newdata = X)


boot::boot(data = X,)

trainControl




#principal decision feature
rf$importance
varImpPlot(rf, main = "Random Forest's Variables Importance")

# Validation set assessment #1: looking at confusion matrix
prediction_for_table <- predict(rf_classifier,validation1[,-5])
table(observed=validation1[,5],predicted=prediction_for_table)



