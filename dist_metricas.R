#Bootstrap para ver distribuição da medida de perda
library(readr)
#Calcula a entropia de uma distribuição discreta no vetor dist
entropia <- function(dist){
  #Tolerância máxima para a distribuição não somar 1
  tol = 10^-6
  
  n = length(dist)
  entropy = 0
  
  #Se a soma da distribuição não for próxima de 1
  #ou haverem valores negativos, retorna erro
  if(abs(sum(dist)-1) > tol | !all(dist >= -tol) ){
    print("Distribuição Inválida")
    return(-1)
  }
  
  #Soma iterativa da entropia
  for (i in 1:n){
    #Se a probabilidade for zero, soma zero
    if(dist[i]==0){
      entropy = entropy + 0
    } 
    #Caso contrario é o -p*log(p) que é somado
    else {
      entropy = entropy - dist[i]*log2(dist[i])
    }
  }
  
  #Se tudo der certo retorno a entropia
  return(entropy)
}

#Indice da entropia média dos clusters
#Quanto maior o indice maior a impureza dos clusters
#Como nas árvores de decisão
#Primeira variável é o label correto, segunda é os labels da clusterização
imp_index <- function(cat_ref,cat_estim) {
  #Tabela
  tab = table(cat_ref,cat_estim)
  #Numero total de observações
  N = sum(tab)
  
  #Número de clusters
  k = dim(tab)[2] 
  
  #Número de labels verdadeiros
  m = dim(tab)[1]
  
  index = 0
  
  for (i in 1:k){
    dist = as.vector(tab[,i]/sum(tab[,i]))
    s = entropia(dist)
    if(class(s)=="character"){
      print("Classe Inválida")
      return(-1)
    }
    #Ponderado
    #index = index + s*sum(tab[,i])/N
    
    #Não Ponderado
    index = index + s*1/k
  }
  
  #Normaliza
  #index = index/log2(m)
  
  return(index)
} 

#Contras desta metrica:
#Não tem penalização contra número de clusters.
#Se tiver 1 dado em cada cluster ela assumira o valor máximo
#Para nossa aplicação isto não é problema pois o usuário define o k


#Métrica 3

#ATENÇÃO: Ordem dos argumentos é importante pois não é simétrico,
#cat_ref - Labels de refêrencia primeiro
#cat_clust - Labels obtidos na clusterização
#m_perda - Matriz simétrica de pesos, se nenhum argumento for dado usa perda uniforme
perda_gen <- function(cat_ref,cat_clust,m_perda) {
  tab = table(cat_ref,cat_clust)
  
  #Numero total de observações
  N = sum(tab)
  
  #Número de clusters
  k = dim(tab)[2]
  
  #Número de categorias dos labels verdadeiros
  m = dim(tab)[1]
  
  #Se não tem matriz com pesos passada, usa pesos uniformes
  if(missing(m_perda)){
    m_perda = matrix(rep(1,m^2),nrow=m) - diag(m)
  }
  
  #Inicializa em 0 e vai somando um valor de cada vez
  perda = 0
  
  #Loop do cluster
  for (i in 1:k){
    
    L_c = 0
    
    #Tamanho do cluster
    n_c = sum(tab[,i])
    
    #Loop dos pares, testa todos os pares j <= l (se l=j a perda é 0)
    for(j in 1:m){
      for(l in j:m){
        
        #Se for 1 ou 0, acrescenta 0 pois não é possível formar pares
        if(n_c == 1 | n_c == 0) {L_c = L_c + 0}
        else {
          #Acrescenta o termo da função perda associado ao evento de encontrar
          #musicas dos tipos j e l no mesmo cluster, multiplicado pelo custo
          L_c = L_c + tab[j,i]*tab[l,i]/choose(n_c,2)*m_perda[j,l]
        }
      }
    }
    #Acrescenta à função perda total o termo do cluster
    perda = perda + L_c
  }
  
  #Divide pelo numero de clusters pra refletir a escolha uniforme do cluster
  perda = perda/k
  
  #Devole o valor da função
  return(perda)
} 

#Com a matriz de custo uniforme este função perda também pode ser interpretada como
#A probabilidade de duas musicas sorteadas aleatoriamente de um cluster
#serem de generos distintos

#Numero de amostragens
p = 10000

y2 = read_csv("mfcc_R_H_C_J_F_2729_obs.csv")
y2 = y2$genres
y1 = read_csv("mfcc_hip_cla_old.csv")
y1 = y1$genre_top

#y = sample(1:n_gen,4000,replace = TRUE)

perda_dist3 = rep(0,p)
perda_dist5 = rep(0,p)

set.seed(1000)
for (i in 1:p){
  print(i)
  perda_dist3[i] = perda_gen(y1,sample(1:3,length(y1),replace = TRUE))
  perda_dist5[i] = perda_gen(as.vector(y2),sample(1:5,length(y2),replace = TRUE))
  #imp_dist[i] = imp_index(y,sample(1:n_gen,length(y),replace = TRUE))
  #vi_dist[i] = vi.dist(y,sample(1:3,length(y),replace = TRUE))
}

par(mfrow=c(1,2))

hist(perda_dist3,breaks=100)
hist(perda_dist5,breaks=100)
  #hist(vi_dist,breaks=100)
