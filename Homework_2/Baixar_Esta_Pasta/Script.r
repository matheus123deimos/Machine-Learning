library(e1071)
library(ggplot2)

train_y = read.table("solTrainY.txt")
train_x = read.table("solTrainX.txt")
test_x = read.table("solTestX.txt")
test_y = read.table("solTestY.txt")
testx_trans = read.table("solTestXtrans.txt")
trainx_trans = read.table("solTrainXtrans.txt")
Data = rbind(train_x, test_x)
Data_trans = rbind(trainx_trans, testx_trans)
Dados = Data_trans
Dados["y"] = rbind(train_y,test_y)

tabela = function(a){
  k = c(1)
  h = c(1)
  j = c(1)
  l = c(1)
  for(i in 1:dim(a)[2]){
    k[i] = names(a)[i]
    h[i] = round(mean(a[,i]),3) 
    j[i] = round(sd(a[,i]),3)
    l[i] = round(skewness(a[,i]),3)
    }
  df = data.frame(Var=k,Midia=h,Desv_Pad=j,Skewness=l)
  return(df)
}

for(i in 1:2){
  ggplot(Dados,aes(Dados[,i])) + geom_histogram(aes(fill=y))+theme_gray()+
  labs(x=names(Dados[i]),y="Frequência")
  ggsave(paste(names(Dados[i]),".png"))
  dev.off()
}

