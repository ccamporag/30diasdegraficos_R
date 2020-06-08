library(tidyverse)

# Creación de grafos
# http://graphonline.ru/en/

# Función para crear un gráfico matricial
require(lattice)
plot.matrix <- function(x,y,z,...){
  panel.levelplot(x,y,z,...)
  z[is.infinite(z)] <- ""
  panel.text(x, y, z)
}


# Elección aleatoria del camino basado en su probabilidad
sampling <- function(prob){
  cumulative <- cumsum(prob)
  position <- which(runif(1) <= cumulative)[1]
  return(position)
}



# adj.matrix: matriz de adyacencia
# n.ants: número de hormigas
# iterations: número de iteraciones/viajes de cada hormiga
# deltaP: depósito de feromona
# alpha: tasa de evaporación de la feromona

ants <- 100
iter <- 700
dP <- 0.5
a <- 0.9

aco <- function(adj.matrix, n.ants=ants, iterations=iter, deltaP=dP, alpha=a){
  
  # Posición inicial ("nido de las hormigas")
  nestId <- 1
  # Posición final ("comida")
  foodId <- nrow(adj.matrix)
  
  # Posición (inicial) de todas las hormigas
  ants.position <- rep(nestId, n.ants)
  # Dirección (inicial) de todas las hormigas
  # 0: de la posición inicial para la posición final (ida)
  # 1: de la posición final para la posición inicial (vuelta)
  ants.direction <- rep(0, n.ants)
  
  # Matriz de feromonas
  pheromone <- matrix(1e-7, nrow=foodId, ncol=foodId)
  
  # Matriz de posicion 
  pos <- matrix(nestId, nrow=iterations, ncol=n.ants)
  
  for(i in 1:iterations){
    # Imprimir el número de la iteración
    cat("Iteration: ", i, "\n")
    
    # Matriz de actualización de feromonas
    pheromone.aux <- matrix(0, nrow=foodId, ncol=foodId)
    
    
    for(j in 1:n.ants){
      # Posición actual de la hormiga j
      position <- ants.position[j]
      
      # Camino de ida
      if(ants.direction[j] == 0){
        # Distancia/coste de cada posible camino
        dists <- adj.matrix[position, (position+1):foodId]
        # Feromonas dejadas en cada posible camino
        pher <- pheromone[position, (position+1):foodId]
        # Probabilidad en cada camino
        prob <- pher/dists
        prob <- as.numeric(prob/sum(prob))
        # Elección del camino
        nextId <- position + sampling(prob)
      }else{ # Camino de vuelta
        # Distancia/coste de cada posible camino
        dists <- adj.matrix[position, 1:(position-1)]
        # Feromonas dejadas en cada posible camino				      
        pher <- pheromone[position, 1:(position-1)]
        # Probabilidad en cada camino				      
        prob <- pher/dists
        prob <- as.numeric(prob/sum(prob))
        # Elección del camino
        nextId <- sampling(prob)
      }
      
      # Cantidad de feromona (deltaP) dejada en la nueva posición de la hormiga j
      pheromone.aux[ants.position[j], nextId] <- pheromone.aux[ants.position[j], nextId] + deltaP
      pheromone.aux[nextId, ants.position[j]] <- pheromone.aux[nextId, ants.position[j]] + deltaP
      
      # Actualiza la posición actual de la hormiga j
      ants.position[j] <- nextId
      pos[i,j] <- nextId
      
      # Si la hormiga j está en la posición inicial, entonces la dirección es de ida (0)
      if(ants.position[j] == nestId){ ants.direction[j] <- 0 }
      # Si la hormiga j está en la posición final, entonces la dirección es de vuelta (1)
      if(ants.position[j] == foodId){ ants.direction[j] <- 1 }
    }
    
    # Actualiza la matriz de feromonas
    pheromone <- alpha * pheromone + pheromone.aux
  }
  
  # Truncamiento de dos decimales para la matriz de feromonas
  pheromone <- round(pheromone,2)
  # Distancia/coste total de la "mejor" ruta
  total.dist <- 0
  
  for(k in 1:foodId){
    # Camino con más feromonas acumuladas a partir de la posición k
    position <- which.max(pheromone[k,])
    if(pheromone[k,position]>0){
      # Cálculo de la distancia/coste
      total.dist <- total.dist + adj.matrix[k,position]
      # Ignora cualquier otro camino
      pheromone[k,-position] <- Inf
      pheromone[position,k] <- 0
    }else{ pheromone[k,] <- Inf }
  }
  
  # Solución
  solution <- list()
  # Matriz de adyacencia (inicial)
  solution$adj.matrix <- adj.matrix
  # Matriz de feromonas con la "mejor" ruta
  solution$pheromone <- pheromone
  # Distancia/coste total con la "mejor" ruta
  solution$total.dist <- total.dist
  # Matriz de posiciones
  solution$pos <- pos
  
  return(solution)
}


# Lectura de los datos de la matriz de adyacencia
mat.adj <- read_csv("https://raw.githubusercontent.com/ccamporag/30diasdegraficos_R/master/adj.mat5.csv",col_names = FALSE)
# Gráfico de la matriz de adyacencia
levelplot(as.matrix(mat.adj),panel=plot.matrix)

# Algoritmo Optimización por Colonia de Hormigas
set.seed(1)
route <- aco(mat.adj)

# Gráfico de la matriz de feromonas con la "mejor" ruta
levelplot(route$pheromone,panel=plot.matrix)

# Distancia/coste total con la "mejor" ruta
route$total.dist

# Cración de data frame de rutas 
position <- (route$pos)
it <-  as.numeric(rownames(as.data.frame(position)))
position <- as.vector(position)
ant <- rep(1:ants,each=iter)
it <- rep(it,ants)
ruta <- as.data.frame(cbind(position,it,ant))


# Coordenada espacial de cada posición
coord <- read_csv("https://raw.githubusercontent.com/ccamporag/30diasdegraficos_R/master/coordinates_ej5.csv")

# Data frame final de rutas y coordenadas
ruta <- left_join(ruta,coord)


for (i in 1:500){
  
  ggplot(data=ruta %>%  filter(it==i, ant %in% c(1:10)),aes(x=x,y=y, col=as.factor(ant)))+
    geom_label(aes(x=0,y=0,label="Nido"))+
    geom_label(aes(x=7,y=7,label="Comida"))+
    geom_point()+
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          plot.background = element_rect(fill = "black"),
          panel.background = element_rect(fill = "black"),
          panel.grid = element_blank(),
          legend.position = "none")+
    xlim(c(0,7))+
    ylim(c(0,7))
  
  
  ggsave(filename = paste0(i,".png"), path = "D:/OneDrive/NuevaCarpeta/Datasets/30diasdegraficos/aco", dpi = 300, width = 8, height = 6)
  
}

setwd("D:/OneDrive/NuevaCarpeta/Datasets/30diasdegraficos/aco")
av::av_encode_video(sprintf("%d.png",seq(1,500,by=1)), framerate = 7,
                    output = "ACO.mp4")


