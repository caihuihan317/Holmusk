### multiple linear regression ###
data_test$pred1 <- predict(model1,newdata=data_test[,-cbind(1,2,3,4,5,6,7,8,30,31,35)],type="response")
pred1 <- data_test[order(-data_test$pred1),c(1,32,44)]


### stepwise selection ###
data_test$pred2 <- predict(model2,newdata=data_test[,-cbind(1,2,3,4,5,6,7,8,30,31,35)],type="response")
pred2 <- data_test[order(-data_test$pred2),c(1,32,45)]

### clustering ###
# map each patient in test data with its cluster score
cluster_test <- clustering[as.numeric(year(clustering$date_of_admission))>2014,]

mapc <- cbind(1:20,ctrain_normed)
colnames(mapc)[1] <- "clusterN"

clusters <- 3
while (clusters <= 20){
  mapping <- plyr::mapvalues(cluster_test[,clusters+1], from = mapc$clusterN, to = mapc[,clusters-1])
  cluster_test <- cbind(cluster_test,mapping)
  colnames(cluster_test)[ncol(cluster_test)]<-as.character(100+clusters)    
  clusters <- clusters + 1
}

data_test <- cbind(data_test, cluster_test[,22:39])
pred3 <- data_test[order(-data_test$"103"),c(1,32,46)]
pred4 <- data_test[order(-data_test$"104"),c(1,32,47)]
pred5 <- data_test[order(-data_test$"105"),c(1,32,48)]
pred6 <- data_test[order(-data_test$"106"),c(1,32,49)]
pred7 <- data_test[order(-data_test$"107"),c(1,32,50)]
pred8 <- data_test[order(-data_test$"108"),c(1,32,51)]
pred9 <- data_test[order(-data_test$"109"),c(1,32,52)]
pred10 <- data_test[order(-data_test$"110"),c(1,32,53)]
pred11 <- data_test[order(-data_test$"111"),c(1,32,54)]
pred12 <- data_test[order(-data_test$"112"),c(1,32,55)]
pred13 <- data_test[order(-data_test$"113"),c(1,32,56)]
pred14 <- data_test[order(-data_test$"114"),c(1,32,57)]
pred15 <- data_test[order(-data_test$"115"),c(1,32,58)]
pred16 <- data_test[order(-data_test$"116"),c(1,32,59)]
pred17 <- data_test[order(-data_test$"117"),c(1,32,60)]
pred18 <- data_test[order(-data_test$"118"),c(1,32,61)]
pred19 <- data_test[order(-data_test$"119"),c(1,32,62)]
pred20 <- data_test[order(-data_test$"120"),c(1,32,63)]

write.csv(pred1,"pred1.csv")
write.csv(pred2,"pred2.csv")
write.csv(pred3,"pred3.csv")
write.csv(pred20,"pred20.csv")
