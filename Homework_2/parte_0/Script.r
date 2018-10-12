library(e1071)
library(ggplot2)
library(corrplot)


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
Cor = cor(Data_trans[209:228])

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

#Plotagem da Matriz de Correlação somente para as 16 ultimas características
png("Matriz_Correlação_N.png")
corrplot(Cor,type="upper",method="circle")
dev.off()

#Plotagem de todos os histogramas de Dispersão
for(i in 1:228){
  ggplot(Data_trans,aes(Data_trans[,i])) + geom_histogram(color="black",fill="springgreen2")+theme_gray()+
  labs(x=names(Data_trans[i]),y="Frequência")
  ggsave(paste(names(Data_trans[i]),".png"))
  dev.off()
}

#Plotagem Dispersão
for(i in 209:228){
  for(j in 209:228){
    if(i>j){
      ggplot(Dados,aes(Data_trans[,j],Dados[,i]))+ geom_point(aes(color=y)) +labs(x=names(Data_trans[i]),y=names(Data_trans[j]),colour="outcome")
      ggsave(paste(names(Dados[i])," vs ",names(Dados[j]),".png"))
      dev.off()
    }
  }
}

print(tabela(Data_trans))