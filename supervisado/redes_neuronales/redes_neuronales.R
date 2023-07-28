
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(nnet)

library(ROCR)
cheese <- read_csv("cheese.csv")

names(cheese)
summary(cheese)

cheese_t <- cheese %>% gather(variable,value) %>% filter(variable!="id")
ggplot(data=cheese_t, mapping=aes(x=variable, y=value)) +
  geom_boxplot()

ggplot(data=cheese, mapping=aes(x="Acetic", y=Acetic)) +
  geom_boxplot()

ggplot(data=cheese, mapping=aes(x="H2S", y=H2S)) +
  geom_boxplot()

ggplot(data=cheese, mapping=aes(x="Lactic", y=Lactic)) +
  geom_boxplot()

ggplot(data=cheese, mapping=aes(x="Taste", y=taste)) +
  geom_boxplot()

pairs(select(cheese,Acetic,H2S,Lactic,taste))

nn <- nnet(taste ~ Acetic + H2S + Lactic, cheese, size=0,
              skip=TRUE, linout=TRUE)

summary(nn)

summary(lm(taste ~ Acetic + H2S + Lactic, cheese))

SSE1 <- sum(nn$residuals^2)
AIC1 <- 2*(dim(cheese)[2]-1+1) + dim(cheese)[1]*log(SSE1/dim(cheese)[1])
AIC1

nn2 = nnet(taste ~ H2S + Lactic, cheese, size=0, skip=TRUE,
              linout=TRUE)
summary(nn2)

SSE2 <- sum(nn2$residuals^2)
AIC2 <- 2*(dim(cheese)[2]-2+1) + dim(cheese)[1]*log(SSE2/dim(cheese)[1])
AIC2

AIC2 < AIC1

nn3 <- nnet(taste ~ Acetic + H2S + Lactic, cheese, size=1,
              linout=TRUE)

cheese_s <- scale(cheese)

nn3 <- nnet(taste ~ Acetic + H2S + Lactic, cheese_s, size=2,
           linout=TRUE)

summary(nn3)

titanic <- read.csv("Titanic.csv")

summary(titanic)

ggplot(data=titanic) +
  geom_bar(mapping=aes(x=Age, fill=Survived)) +
  facet_grid(Class~Sex)

ggplot(data=titanic) +
  geom_bar(mapping=aes(x=Age, fill=Survived), position="fill") +
  facet_grid(Class~Sex)

titanic$Surv <- class.ind(titanic$Survived)

test_rows = sample.int(nrow(titanic), nrow(titanic)/3)
test = titanic[test_rows,]
train = titanic[-test_rows,]

nn_t = nnet(Surv~Sex+Age+Class, train, size=25, softmax=TRUE)

nn_t

summary(nn_t)

table(data.frame(predicted=predict(nn_t, test)[,2] > 0.5,
                 actual=test$Surv[,2]>0.5))

sum(table(data.frame(predicted=predict(nn_t, test)[,2] > 0.5,
                     actual=test$Surv[,2]>0.5))
)

predics <- prediction(predict(nn_t, test)[,2], test$Surv[,2])

str(predics)

predics@fp

perfo=performance(predics,"tpr","fpr")
plot(perfo)

perfo=performance(predics,"auc")
perfo@y.values

perfo<-performance(predics, "cost")
perfo@x.values[[1]][which.min(perfo@y.values[[1]])]

#perfo=performance(predics,"acc")
#plot(perfo)

err = vector("numeric", 100)
d = seq(0.0001, 1, length.out=100)
for(i in 1:length(d)) {
  fit = nnet(Surv~Sex+Age+Class, train, size=1, softmax=TRUE, decay=d[i])
  err[i] = sum(fit$residuals[,2]**2)
}
plot(d, err)

# install.packages("NeuralNetTools")
library(NeuralNetTools)

garson(nn3)
plotnet(nn3)

##
## Cuadrática
##

df_c <- data_frame(x=-10:10,y=(-10:10)^2)

nn_cuad <- nnet(y~x, df_c, size=0, skip=TRUE, linout=TRUE)

ggplot(data=data_frame(x1=c(-10:10), y1=(-10:10)^2, p1=predict(nn_cuad,newdata=data_frame(x=-10:10))[,1])) +
  geom_line(mapping=aes(x=x1,y=y1),color="blue")+
  geom_line(mapping=aes(x=x1,y=p1),color="red")

nn_cuad <- nnet(y~x, df_c, size=4, linout=TRUE)

ggplot(data=data_frame(x1=c(-10:10), y1=(-10:10)^2, p1=predict(nn_cuad,newdata=data_frame(x=-10:10))[,1])) +
  geom_line(mapping=aes(x=x1,y=y1),color="blue")+
  geom_line(mapping=aes(x=x1,y=p1),color="red") +
  ggtitle(paste("Red con ",4," neuronas",sep = ""))

ggplot(data=data_frame(x1=seq(-15,15,.01), y1=seq(-15,15,.01)^2, p1=predict(nn_cuad,newdata = data_frame(x=seq(-15,15,.01)))[,1])) +
  geom_line(mapping=aes(x=x1,y=y1),color="blue")+
  geom_line(mapping=aes(x=x1,y=p1),color="red")+
  ggtitle(paste("Red con ",4," neuronas",sep = ""))

# Escalar para rangos más grandes

df_c <- data_frame(x=-100:100,y=(-100:100)^2)
df_c_s <- data_frame(x=scale(df_c$x)[,1],y=scale(df_c$y)[,1])

nn_cuad <- nnet(y~x, df_c_s, size=4, linout=TRUE)

summary(nn_cuad)

ggplot(data=data_frame(x1=seq(-100,100,.01), y1=seq(-100,100,.01)^2, p1=predict(nn_cuad,newdata = data_frame(x=scale(seq(-100,100,.01))[,1]))[,1]*sd(df_c$y)+mean(df_c$y))) +
  geom_line(mapping=aes(x=x1,y=y1),color="blue")+
  geom_line(mapping=aes(x=x1,y=p1),color="red")+
  ggtitle(paste("Red con ",4," neuronas",sep = ""))

# regularización

df_r <- data_frame(x1=seq(0,1,.01),y1=0.5+0.4*sin(2*pi*seq(0,1,.01)+0.3*rnorm(length(seq(0,1,.01)))))
ggplot(data=df_r) +
  geom_point(mapping=aes(x=x1,y=y1),color="blue")

nn_sin <- nnet(y1~x1, df_r, size=4, linout=TRUE, decay=.01)

ggplot(data=data_frame(x1=df_r$x1, y1=df_r$y1, p1=predict(nn_sin,newdata = data_frame(x1=seq(0,1,.01)))[,1])) +
  geom_line(mapping=aes(x=x1,y=y1),color="blue")+
  geom_line(mapping=aes(x=x1,y=p1),color="red")+
  ggtitle(paste("Red con ",4," neuronas",sep = ""))


df_r <- data_frame(x1=seq(0,1,.01),y1=0.5+0.4*sin(2*pi*seq(0,1,.01)+0.3*rnorm(length(seq(0,1,.01)))))
ggplot(data=df_r) +
  geom_point(mapping=aes(x=x1,y=y1),color="blue")

set.seed(1234)
nn_sin <- nnet(y1~x1, df_r, size=2000, linout=TRUE, decay=0.0, MaxNWts=6500, maxit=200)
set.seed(1234)
nn_sin_reg <- nnet(y1~x1, df_r, size=2000, linout=TRUE, decay=0.001, MaxNWts=6500, maxit=200)

ggplot(data=data_frame(x1=df_r$x1, y1=df_r$y1, 
                       p0=0.5+0.4*sin(2*pi*seq(0,1,.01)),
                       p1=predict(nn_sin,newdata = data_frame(x1=seq(0,1,.01)))[,1],
                       p2=predict(nn_sin_reg,newdata = data_frame(x1=seq(0,1,.01)))[,1])) +
  geom_point(mapping=aes(x=x1,y=y1),color="blue")+
  geom_line(mapping=aes(x=x1,y=p0),color="black")+
  geom_line(mapping=aes(x=x1,y=p1),color="red")+
  geom_line(mapping=aes(x=x1,y=p2),color="green")+
  ggtitle(paste("Red con ",2000," neuronas",sep = ""))


