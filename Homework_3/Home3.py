
#1) Importando as Funções Necessárias

import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix


#2) Lendo os Dados e Transformando em Dataframes

treino = pd.read_csv("training.csv")
teste = pd.read_csv("testing.csv")
reduce = pd.read_csv("reducedSet.csv")


target_treino = pd.get_dummies(treino["Class"])
target_treino.drop('unsuccessful',axis=1,inplace=True)
target_teste = pd.get_dummies(teste["Class"])
target_teste.drop('unsuccessful',axis=1,inplace=True)

treino = treino[reduce['x']]
teste = teste[reduce['x']]


#4) Criando o Modelo: Logístico

logmodel = LogisticRegression()
logmodel.fit(treino,target_treino.values.ravel())


#5) Teste do Modelo Logistíco

predictions = logmodel.predict(teste)


#a) Precisão 

print(classification_report(target_teste,predictions))


#b) Matriz de Confusão

print(confusion_matrix(target_teste,predictions))


#6) Criando o Modelo: Rede Neural

clf=MLPClassifier(activation = "logistic", alpha=0.0, batch_size =50,hidden_layer_sizes=(126,),
                  learning_rate='constant',learning_rate_init=0.009,max_iter=200, momentum=0.0)


clf.fit(treino,target_treino.values.ravel())  


#7) Teste da Rede Neural

pred = clf.predict(teste)


#a) Precisão

print(classification_report(target_teste,pred))


#b) Matriz de Confuzão

print(confusion_matrix(target_teste,pred))

