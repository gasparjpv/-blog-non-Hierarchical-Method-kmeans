# Função para carga dos pacotes

pacotes <- c("tidyverse","cluster","dendextend","factoextra","fpc",
             "gridExtra","readxl")

#Descrição das livrarias usadas 

#tidyverse - pacote para manipulacao de dados
#cluster - algoritmo de cluster
#dendextend - compara dendogramas
#factoextra - algoritmo de cluster e visualizacao
#fpc - algoritmo de cluster e visualizacao
#gridExtra - para a funcao grid arrange
#readxl - função para carregar arquivo excel

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Agrupando os lanches pelo metodo hierarquico 

#Carregar base de dados: 
mcdonalds <- read.table("MCDONALDS.csv", sep = ";", dec = ",", header = T)

#transformar o nome dos lanches em linhas
rownames(mcdonalds) <- mcdonalds[,1]
mcdonalds <- mcdonalds[,-1]

#Padronizar variaveis
mcdonalds.padronizado <- scale(mcdonalds)

#VERIFICANDO ELBOW 
fviz_nbclust(mcdonalds.padronizado, kmeans, method = "wss")

#Rodar o modelo
mcdonalds.k2 <- kmeans(mcdonalds.padronizado, centers = 2)
#Visualizar os clusters
fviz_cluster(mcdonalds.k2, data = mcdonalds.padronizado, main = "Cluster K2")

#Criar clusters
mcdonalds.k3 <- kmeans(mcdonalds.padronizado, centers = 3)
mcdonalds.k4 <- kmeans(mcdonalds.padronizado, centers = 4)
mcdonalds.k5 <- kmeans(mcdonalds.padronizado, centers = 5)

#Criar graficos
G1 <- fviz_cluster(mcdonalds.k2, geom = "point", data = mcdonalds.padronizado) + ggtitle("k = 2")
G2 <- fviz_cluster(mcdonalds.k3, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 3")
G3 <- fviz_cluster(mcdonalds.k4, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 4")
G4 <- fviz_cluster(mcdonalds.k5, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 5")

#Imprimir graficos na mesma tela
grid.arrange(G1, G2, G3, G4, nrow = 2)

# com base no elbow, plotamos apenas o cluster 4
mcdonalds.k4 <- kmeans(mcdonalds.padronizado, centers = 4)
fviz_cluster(mcdonalds.k4, data = mcdonalds.padronizado, main = "Cluster K4")

# calcular os índices de silhueta para diferentes números de clusters
silhouette_info <- fviz_nbclust(mcdonalds.padronizado, kmeans, method = "silhouette", k.max = 5)
# visualizar o gráfico dos índices de silhueta
plot(silhouette_info)

