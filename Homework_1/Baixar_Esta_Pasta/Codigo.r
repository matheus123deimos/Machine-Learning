#Coloque o essa pasta para ser o diret?io principal do
#trabalho para assim ter acesso ao dataset

library(ggplot2)
library(factoextra)
library(corrplot)
library(e1071)

#Elementos de dados
D = read.csv("winequality-white.csv",sep=";")
pca.D = prcomp(D[1:11],scale=TRUE)
Cor = cor(D[1:11])
d = data.frame(pca.D$x[,1],pca.D$x[,2])
d["quality"] = D["quality"]

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
df = data.frame(Var=k,M�dia=h,Desv_Pad=j,Skewness=l)
return(df)
}

#Strings
Legenda = c("Acdz_fix","Acdz_vol","Acd_ctr","Res_a�r","Clr","Diox_l","Diox_t","Dst","pH","Suft","Alcl")
Nomes = c("Acidez Fixa [g(tartaric acid)/dm3]","Acidez Vol�til [g(acetic acid)/dm3]",
"�cido C�trico (g/dm3)","Residual de A�ucar (g/dm3)",
"Cloreto [g(sodium chloride)/dm3]","Di�xido de Enxofre Livre (mg/dm3)",
"Di�xido de Enxofre Total (mg/dm3)","Densidade (g/cm3)",
"pH","Sulfatos [g(potassium sulphate)/dm3]","�lcool (vol.%)")

#Plotagem dos Histogramas: part_1
for(i in 1:11){
ggplot(D,aes(D[,i])) + geom_histogram(color='black',fill='springgreen2')+theme_gray()+labs(x=Nomes[i],y="Frequ�ncia")
ggsave(paste("Hist_",Legenda[i],".png"))
dev.off()
}

#Plotagem dos Histogramas por Classe: part_2
for(i in 3:9){
for(j in 1:11){
dq = D[D$quality==i,]
ggplot(dq,aes(dq[,j]))+geom_histogram(color='black',fill='springgreen2')+theme_gray()+labs(title=paste("Classe ",toString(i)),x=Nomes[j],y="Frequ�ncia")
ggsave(paste("Cls",toString(i),"_",Legenda[j],".png"))
dev.off()
}
}

#Plotagem dos Graficos de Dispers�o: part_3 
for(i in 1:11){
for(j in 1:11){
if(i>j){
ggplot(D,aes(D[,j],D[,i],colour=as.factor(quality)))+ geom_point() +labs(x=Nomes[j],y=Nomes[i],colour="qualidade")
ggsave(paste("Disp_",Legenda[i]," vs ",Legenda[j],".png"))
dev.off()
}
}
}

#Plotagem da Matriza de Correla��o: part_3
png("Matriz_Correla��o.png")
corrplot(Cor,method="number")
dev.off()

#Plotagem Vari�ncia por Componente: part_4
png("Vari�ncia vs Componentes.png")
fviz_eig(pca.D,xlab="Componente",ylab="Vari�ncia",ncp=2,addlabels = TRUE,main="Vari�ncia vs Componentes")
dev.off()

#Plotagem Dispers�o das duas primeiras componentes principais: part_4
ggplot() + geom_point(data=d, aes(x=d[,1], y=d[,2],color= as.factor(quality))) +labs(color='quality',x="PC1",y="PC2")
ggsave("PC1 vs PC1.png")
dev.off()

print("An�lise Monovariada N�o-Condicional - part1\n\n")
print(tabela(D))

print("An�lise Monovariada Classe-Condicional - part2")
for(i in 3:9){
print(" ")
print(paste("Classe ",toString(i)))
print(" ")
print(tabela(D[D$quality==i,]))
}