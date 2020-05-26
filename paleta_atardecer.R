# 30 dias de gráficos en R
# paleta de colores 

library(tidyverse)
atardecer <- c("#8299B8","#AFC0C8","#D7BB93","#CF7D72","#8B7F7F","#5A6174","#656563","#7A7464","#4E413B","#333834")

col= 1200

nrow_cielo=700
cielo <- matrix(ncol = col, nrow = nrow_cielo, data=0)
for (i in 1:nrow_cielo){cielo[i,] <- sample(1:2,col,replace = TRUE,
                                            prob=c(100-exp(4.605*i/nrow_cielo),
                                                   exp(4.605*i/nrow_cielo)))}
nrow_cielo2=150 
cielo2 <- matrix(ncol = col, nrow = nrow_cielo2, data=0)
for (i in 1:nrow_cielo2){cielo2[i,] <- sample(1:3,col,replace = TRUE,
                                              prob=c((5-(i-1)*5/(nrow_cielo2-1))/100,
                                                     (85-(i-1)*(85-50)/(nrow_cielo2-1))/100,
                                                     (10+(i-1)*(50-10)/(nrow_cielo2-1))/100))}
nrow_mont = 90
montaña <- matrix(ncol=col, nrow=nrow_mont, data=0)
for (i in 1:nrow_mont){montaña[i,] <- sample(3:5,col,replace = TRUE,
                                             prob=c(.1,.45,.45))}

nrow_mont2 = 30
montaña2 <- matrix(ncol=col, nrow=nrow_mont2, data=0)
for (i in 1:nrow_mont2){montaña2[i,] <- sample(4:5,col,replace = TRUE,
                                               prob=c(.05,.95))}

nrow_ciudad = 600
ciudad <- matrix(ncol=col, nrow=nrow_ciudad, data=0)
for (i in 1:nrow_ciudad){ciudad[i,] <- sample(6:9,col,replace = TRUE,
                                              prob=c(.2,.2,.4,.2))}


plot <- rbind(cielo,cielo3,montaña,montaña2,ciudad) %>% as_tibble() %>% 
  mutate(y=seq(nrow(plot),1)) %>% pivot_longer(1:col,"columna") %>% 
  mutate(x=rep(seq(1,ncol(plot)),nrow(plot))) %>% mutate(value=as.factor(value))

ggplot(plot,aes(x=x,y=y))+
  geom_tile(aes(fill=value))+
  scale_fill_manual(labels = atardecer,values=atardecer)+
  theme_void()+
  theme(legend.position = "bottom",
        legend.title = element_blank())


