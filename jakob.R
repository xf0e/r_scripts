library(ggvis)
cvi<- read.csv("/home/grrr/jakob/Datensatz_Kodiert.csv", header = TRUE)
#cvi
head(cvi)
cvi %>% ggvis(~Wundgrund,~Anzahl, fill=~Hauptdiagnose) %>% layer_points()
table(cvi$Hauptdiagnose)
round(prop.table(table(cvi$Hauptdiagnose))*100,digits=1)
summary(cvi)
library(class)
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
normalized_cvi<-as.data.frame(lapply(cvi[2:9], normalize))
summary(cvi)
summary(normalized_cvi)
set.seed(1234)
ind<- sample(2, nrow(cvi), replace = TRUE, prob = c(0.67,0.33))
cvi.training <-cvi[ind==1,3:10]
cvi.test <-cvi[ind==2,3:10]
#cvi.training
cvi.trainLabels <- cvi[ind==1,11]
cvi.testLabels <- cvi[ind==2,11]
cvi_pred <- knn(train = cvi.training, test = cvi.test, cl=cvi.trainLabels, k=3)
#cvi_pred
#cvi.trainLabels
sidebyside <- function(..., width=60){
  l <- list(...)
  p <- lapply(l, function(x){
    xx <- capture.output(print(x, width=width))
    xx <- gsub("\"", "", xx)
    format(xx, justify="left", width=width)
  }
  )
  p <- do.call(cbind, p)
  sapply(seq_len(nrow(p)), function(x)paste(p[x, ], collapse=""))
}
sidebyside(cvi_pred, cvi.testLabels)
library(gplots)
result <- cbind(cvi_pred, cvi.testLabels)
#cvi_pred
#cvi.testLabels
#result
typeof(result)
combineLists <- function(manyLists){
  library(plyr)
  newLists <- list()
  for(ixList in 1:length(manyLists)){
    tmpList <- lapply(manyLists[[ixList]], paste, sep = "", collapse = ", ")
    tmpVec  <- as.character(tmpList)
    newLists[[ixList]] <- tmpVec
  }
  newDF   <- t(ldply(newLists))
  return(newDF)
}
combineLists(list(cvi_pred, cvi.testLabels))


xtab_set <- function(A,B){
  both    <-  union(A,B)
  inA     <-  both %in% A
  inB     <-  both %in% B
  return(table(inA,inB))
}
xtab_set(cvi_pred, cvi.testLabels)

venn(list(first.vector = cvi_pred, second.vector = cvi.testLabels))

t1<- as.data.frame(cvi_pred)
t1
t2<- as.data.frame(cvi.testLabels)
t2
cbind(t1,t2)
setdiff(t1, t2)
sqldf
#install.packages("sqldf")
#install.packages("tcltk")
library(sqldf)
sqldf3 <- sqldf('SELECT * FROM t1 UNION SELECT * FROM t2') # UNION t1 and t2
res <- cbind(cvi.testLabels,cvi_pred)
counter=0
for (i in nrow(res)){
  if (res[i,1] == res[i,2]) {
    counter <- counter + 1
  }  
}
setdiff(t1, t2)
library(gmodels)
CrossTable(x = cvi_pred, y = cvi.testLabels, prop.chisq=FALSE)
