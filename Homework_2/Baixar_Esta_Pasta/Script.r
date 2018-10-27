
library(e1071)
library(ggplot2)
library(corrplot)
library(factoextra)
library(Metrics)
library(glmnet)
library(tidyverse)
library(caret)
library(foba)
library(ridge)
#library(modelr)

#Recebendo os datasets
train_y = read.table("solTrainY.txt")
train_x = read.table("solTrainX.txt")
test_x = read.table("solTestX.txt")
test_y = read.table("solTestY.txt")


#plotagem de histogramas e dispersão
Data = rbind(train_x,test_x)
Data["y"] = rbind(train_y,test_y)
Cor = cor(Data[209:228])

#Dados para treino e teste
D_treino=train_x
D_treino["y"] = train_y
D_teste = test_x
D_teste["y"] = test_y

#Remover dados Correlacionados
Remover1 = findCorrelation(cor(D_treino), .9)
#Remover2 = findCorrelation(cor(D_teste), .9)
D_treino = D_treino[, -Remover1]
D_teste = D_teste[, -Remover1]

# Escalonar os Dados
#D_treino = data.frame(scale(D_treino[1:191],TRUE,TRUE))
#D_teste = data.frame(scale(D_teste[1:191],TRUE,TRUE))


#Dados para Analise das Componentes Principais
pca.data = prcomp(train_x[1:228],scale=TRUE)
pca_data = data.frame(pca.data$x[,1],pca.data$x[,2])
pca_data["y"] = train_y
pdf("Variância vs Componentes.pdf")
fviz_eig(pca_data,xlab="Componente",ylab="Variância",ncp=2,addlabels = TRUE,main="Variância vs Componentes")
dev.off()
#Plotagem da Matriz de Correlação somente para as 16 ultimas características
pdf("Matriz_Correlação_N.pdf")
corrplot(Cor,type="upper",method="circle")
dev.off()

#Plotagem de todos os histogramas de Dispersão
for(i in 1:228){
  
  ggplot(Data,aes(Data[,i])) + geom_histogram(color="black",fill="springgreen2")+theme_gray()+
    labs(x=names(Data[i]),y="Frequência")
  ggsave(paste(names(Data[i]),".pdf"))
  dev.off()
}

#Plotagem Dispersão
for(i in 209:228){
  for(j in 209:228){
    if(i>j){
      ggplot(Data,aes(Data[,j],Data[,i]))+ geom_point(aes(color=y)) +labs(x=names(Data[i]),y=names(Data[j]),colour="outcome")
      ggsave(paste(names(Data[i])," vs ",names(Data[j]),".pdf"))
      dev.off()
    }
  }
}


#Linear Model
set.seed(14)
train.control_1 = trainControl(method = "cv",number=5)
model1 = train(y~.,data=D_treino,method="lm",trControl=train.control_1,preProc=c("center","scale","YeoJohnson"))
set.seed(15)
train.control_2 = trainControl(method = "cv",number=10)
model2 = train(y~.,data=D_treino,method="lm",trControl=train.control_2,preProc=c("center","scale","YeoJohnson"))

p1 = predict(model1,D_treino)
p2 = predict(model2,D_treino)

model1
model2

pred1 = predict(model1,D_teste)
pred2 = predict(model2,D_teste)

RMSE(p1,D_treino$y)
RMSE(p2,D_treino$y)
RMSE(pred1,D_teste$y)
RMSE(pred2,D_teste$y)

R2(p1,D_treino$y)
R2(p2,D_treino$y)
R2(pred1,D_teste$y)
R2(pred2,D_teste$y)

D1 = data.frame(D_treino$y,p1)
D2 = data.frame(D_treino$y,p2)
D3 = data.frame(D_teste$y,pred1)
D4 = data.frame(D_teste$y,pred2)

pdf("ModeloLinearTreino5fold.pdf")
ggplot(D1,aes(D_treino.y,p1)) + geom_point(color="blue") + labs(title="Modelo_Linear_Treino: 5-fold",x="Predito",y="Observado")
dev.off()
pdf("ModeloLinearTreino10fold.pdf")
ggplot(D2,aes(D_treino.y,p2)) + geom_point(color="blue") + labs(title="Modelo_Linear_Treino: 10-fold",x="Predito",y="Observado")
dev.off()
pdf("ModeloLinearTeste5fold.pdf")
ggplot(D3,aes(D_teste.y,pred1)) + geom_point(color="blue") + labs(title="Modelo_Linear_Teste: 5-fold",x="Predito",y="Observado")
dev.off()
pdf("ModeloLinearTeste10fold.pdf")
ggplot(D4,aes(D_teste.y,pred2)) + geom_point(color="blue") + labs(title="Modelo_Linear_Teste: 10-fold",x="Predito",y="Observado")
dev.off()

#PLS
set.seed(16)
train.control_3 = trainControl(method = "cv",number=5)
model3 = train(y~.,data=D_treino,method="pls",trControl=train.control_3,tuneLength = 13,preProc=c("center","scale","YeoJohnson"))

set.seed(17)
train.control_4 = trainControl(method = "cv",number=10)
model4 = train(y~.,data=D_treino,method="pls",trControl=train.control_4,tuneLength = 13,preProc=c("center","scale","YeoJohnson"))


model3
model4

pred3 = predict(model3,D_treino)
pred4 = predict(model4,D_treino)
pred5 = predict(model3,D_teste)
pred6 = predict(model4,D_teste)

RMSE(pred3,D_treino$y)
RMSE(pred4,D_treino$y)
RMSE(pred5,D_teste$y)
RMSE(pred6,D_teste$y)

R2(pred3,D_treino$y)
R2(pred4,D_treino$y)
R2(pred5,D_teste$y)
R2(pred6,D_teste$y)

D5 = data.frame(D_treino$y,pred3)
D6 = data.frame(D_treino$y,pred4)
D7 = data.frame(D_teste$y,pred5)
D8 = data.frame(D_teste$y,pred6)

pdf("PLSTreino5fold.pdf")
ggplot(D5,aes(D_treino.y,pred3)) + geom_point(color="blue") + labs(title="PLS_Treino: 5-fold",x="Predito",y="Observado")
dev.off()
pdf("PLSTreino10fold.pdf")
ggplot(D6,aes(D_treino.y,pred4)) + geom_point(color="blue") + labs(title="PLS_Treino: 10-fold",x="Predito",y="Observado")
dev.off()
pdf("PLSTeste5fold.pdf")
ggplot(D7,aes(D_teste.y,pred5)) + geom_point(color="blue") + labs(title="PLS_Teste: 5-fold",x="Predito",y="Observado")
dev.off()
pdf("PLSTeste10fold.pdf")
ggplot(D8,aes(D_teste.y,pred6)) + geom_point(color="blue") + labs(title="PLS_Teste: 10-fold",x="Predito",y="Observado")
dev.off()


#L2-penalized linear regression
set.seed(18)
train.control_5 = trainControl(method = "cv", number = 5, returnResamp = "all")
model5 = train(y~.,data=D_treino,method="ridge",trControl=train.control_5,tuneLength = 5,preProc=c("center","scale","YeoJohnson"))

set.seed(19)
train.control_6 = trainControl(method = "cv", number = 10, returnResamp = "all")
model6 = train(y~.,data=D_treino,method="ridge",trControl=train.control_6,tuneLength = 5,preProc=c("center","scale","YeoJohnson"))

model5
model6

pred7 = predict(model5,D_treino)
pred8 = predict(model6,D_treino)
pred9 = predict(model5,D_teste)
pred10 = predict(model6,D_teste)

RMSE(pred7,D_treino$y)
RMSE(pred8,D_treino$y)
RMSE(pred9,D_teste$y)
RMSE(pred10,D_teste$y)

R2(pred7,D_treino$y)
R2(pred8,D_treino$y)
R2(pred9,D_teste$y)
R2(pred10,D_teste$y)

D9 = data.frame(D_treino$y,pred7)
D10 = data.frame(D_treino$y,pred8)
D11 = data.frame(D_teste$y,pred9)
D12 = data.frame(D_teste$y,pred10)

pdf("L2Treino5fold.pdf")
ggplot(D9,aes(D_treino.y,pred7)) + geom_point(color="blue") + labs(title="L2_Treino: 5-fold",x="Predito",y="Observado")
dev.off()
pdf("L2Treino10fold.pdf")
ggplot(D10,aes(D_treino.y,pred8)) + geom_point(color="blue") + labs(title="L2_Treino: 10-fold",x="Predito",y="Observado")
dev.off()
pdf("L2Teste5fold.pdf")
ggplot(D11,aes(D_teste.y,pred9)) + geom_point(color="blue") + labs(title="L2_Teste: 5-fold",x="Predito",y="Observado")
dev.off()
pdf("L2Teste10fold.pdf")
ggplot(D12,aes(D_teste.y,pred10)) + geom_point(color="blue") + labs(title="L2_Teste: 10-fold",x="Predito",y="Observado")
dev.off()

#PCR
set.seed(20)
train.control_7 = trainControl(method = "cv",number=5)
model7 = train(y~.,data=D_treino,method="pcr",trControl=train.control_7,tuneLength = 13,preProc=c("center","scale","YeoJohnson"))

set.seed(21)
train.control_8 = trainControl(method = "cv",number=10)
model8 = train(y~.,data=D_treino,method="pcr",trControl=train.control_8,tuneLength = 13,preProc=c("center","scale","YeoJohnson"))

model7
model8

pred11 = predict(model7,D_treino)
pred12 = predict(model8,D_treino)
pred13 = predict(model7,D_teste)
pred14 = predict(model8,D_teste)

RMSE(pred11,D_treino$y)
RMSE(pred12,D_treino$y)
RMSE(pred13,D_teste$y)
RMSE(pred14,D_teste$y)

R2(pred11,D_treino$y)
R2(pred12,D_treino$y)
R2(pred13,D_teste$y)
R2(pred14,D_teste$y)

D13 = data.frame(D_treino$y,pred11)
D14 = data.frame(D_treino$y,pred12)
D15 = data.frame(D_teste$y,pred13)
D16 = data.frame(D_teste$y,pred14)

pdf("PCRTreino5fold.pdf")
ggplot(D13,aes(D_treino.y,pred11)) + geom_point(color="blue") + labs(title="PCR_Treino: 5-fold",x="Predito",y="Observado")
dev.off()
pdf("PCRTreino10fold.pdf")
ggplot(D14,aes(D_treino.y,pred12)) + geom_point(color="blue") + labs(title="PCR_Treino: 10-fold",x="Predito",y="Observado")
dev.off()
pdf("PCRTeste5fold.pdf")
ggplot(D15,aes(D_teste.y,pred13)) + geom_point(color="blue") + labs(title="PCR_Teste: 5-fold",x="Predito",y="Observado")
dev.off()
pdf("PCRTeste10fold.pdf")
ggplot(D16,aes(D_teste.y,pred14)) + geom_point(color="blue") + labs(title="PCR_Teste: 10-fold",x="Predito",y="Observado")
dev.off()

