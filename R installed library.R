# the purpose is to reconcile R packages on different computers

lib=as.matrix(installed.packages()[,1],ncol=1)
write.csv(lib,"Rlibrary.csv",row.names=F)

