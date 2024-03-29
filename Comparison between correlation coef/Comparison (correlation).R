##Trabalho n param�trico
setwd("C:\\Users\\ASUS\\Documents\\nparametrico\\Trab 2")


library("mvtnorm")

s=p=k=NULL  ##Vetores vazios
v<-c(5,10,11:1000) ##Tamanho das amostras


set.seed(12345) ##Pra gente conseguir gerar os mesmos valores bb
for(t in 1:length(v)){
  
  ##Gerando valores normais (media 0) com uma correla��o espec�fica (0.72)
valores <- rmvnorm(n = v[t],  
                   mean = c(0,0),
                   matrix(c(1,0.72,0.72,1),2,2)) ##Matrix=matriz de correla��o

    
s[t]<-cor(valores[,1],valores[,2],method = "spearman")  ##Correla��o spearman
p[t]<-cor(valores[,1],valores[,2],method = "pearson")   ##Correla��o Pearson
k[t]<-cor(valores[,1],valores[,2],method = "kendall")   ##Correla��o kendall

}


##          Fazendo gr�fico maneiro no ggplot

##Criar um data.frame
dt<-data.frame(dif=abs(.72-c(s,p,k)))  ##Primeira coluna � o modulo da diferen�a
                                       ##da correla��o real (0.72) com as calculadas

##Coluna com qual m�todo foi usado na correla��o
dt$M�todo<-rep(c("Spearman","Pearson","Kendall"),each=length(dt$dif)/3)

##Coluna com o n�mero de amostras
dt$n<-rep(v,3)

dt$cor<-c(s,p,k)

library(ggplot2)

plot<-ggplot(dt,aes(x=n,y=dif,color=M�todo))+geom_line()+theme_minimal()+theme(panel.background = element_rect(fill="black"),
                                                         panel.grid.major = element_blank(),
                                                         panel.grid.minor=element_blank(),
                                                         legend.position=c(0.85,0.85),
                                                         legend.text = element_text(color ="white" ),
                                                         legend.title = element_text(color ="white" ))
plot<-plot+ylab("|??-cor|")+xlab("Tamanho da amostra")


ggsave("corr.pdf",plot)


##Estat�sticas do erro por m�todo
tapply(dt$dif,dt$metodo,summary)
plot2<-ggplot(dt,aes(x=M�todo,y=dif,col=M�todo))+geom_jitter(size=0.43)+theme_minimal()+ theme(legend.position = c(.85,.85),panel.background = element_rect(fill="black"), panel.grid.major = element_blank(),
                                                                             panel.grid.minor=element_blank(),axis.text.x = element_blank(),
                                                                             axis.ticks.x = element_blank(),axis.title.x = element_blank())
plot2<-plot2+stat_summary(fun.data=mean_sdl,fun.args=list(mult=1),col="white",size=1)
plot2<-plot2+theme(legend.text = element_text(color ="white"),
                   legend.title = element_text(color="white"),legend)
plot2<-plot2+ guides(colour = guide_legend(override.aes = list(size=3)))
plot2<-plot2+ylab("|??-cor|")
ggsave("bonitao.pdf",plot2)

plot3<-ggplot(dt,aes(x=n,y=cor,color=M�todo))+geom_line()+theme_minimal()+theme(panel.background = element_rect(fill="black"),
                                                                                panel.grid.major = element_blank(),
                                                                                panel.grid.minor=element_blank(),
                                                                                legend.position=c(0.85,0.20),
                                                                                legend.text = element_text(color ="white" ),
                                                                                legend.title = element_text(color ="white" ))
plot3<-plot3+ylab("cor")+xlab("Tamanho da amostra")
plot3<-plot3+geom_hline(yintercept = .72,color="white", linetype="dashed")
plot3<-plot3+scale_y_continuous(limits = c(0,1))
plot3
ggsave("corxtamgrand72.pdf",plot3)

##################################Amostra PEquenaaaaaa##########################

##Fazer gr�fico para amostras de 2 a 20 e estat�sticas

v<-c(2:20) ##Tamanho pra parte 2 (amostras pequenas)
s=p=k=NULL  ##Vetores vazios

set.seed(12345) ##Pra gente conseguir gerar os mesmos valores bb
for(t in 1:length(v)){
  
  ##Gerando valores normais (media 0) com uma correla��o espec�fica (0.72)
  valores <- rmvnorm(n = v[t],  
                     mean = c(0,0),
                     matrix(c(1,0.72,0.72,1),2,2)) ##Matrix=matriz de correla��o
  
  
  s[t]<-cor(valores[,1],valores[,2],method = "spearman")  ##Correla��o spearman
  p[t]<-cor(valores[,1],valores[,2],method = "pearson")   ##Correla��o Pearson
  k[t]<-cor(valores[,1],valores[,2],method = "kendall")   ##Correla��o kendall
  
}

dt<-data.frame(dif=abs(.72-c(s,p,k)))  ##Primeira coluna � o modulo da diferen�a
##da correla��o real (0.72) com as calculadas

##Coluna com qual m�todo foi usado na correla��o
dt$M�todo<-rep(c("Spearman","Pearson","Kendall"),each=length(dt$dif)/3)

##Coluna com o n�mero de amostras
dt$n<-rep(v,3)

dt$cor<-c(s,p,k)

plot<-ggplot(dt,aes(x=n,y=dif,color=M�todo))+geom_line()+theme_minimal()+theme(panel.background = element_rect(fill="black"),
                                                                               panel.grid.major = element_blank(),
                                                                               panel.grid.minor=element_blank(),
                                                                               legend.position=c(0.85,0.85),
                                                                               legend.text = element_text(color ="white" ),
                                                                               legend.title = element_text(color ="white" ))
plot<-plot+ylab("|??-cor|")+xlab("Tamanho da amostra")+scale_y_continuous(limits = c(0,.6))

ggsave("erpequena.pdf",plot)



plot2<-ggplot(dt,aes(x=M�todo,y=dif,col=M�todo))+geom_jitter(size=1.8)+theme_minimal()+ theme(legend.position = c(.85,.85),panel.background = element_rect(fill="black"), panel.grid.major = element_blank(),
                                                                                               panel.grid.minor=element_blank(),axis.text.x = element_blank(),
                                                                                               axis.ticks.x = element_blank(),axis.title.x = element_blank())
plot2<-plot2+stat_summary(fun.data=mean_sdl,fun.args=list(mult=1),col="white",size=1)
plot2<-plot2+theme(legend.text = element_text(color ="white"),
                   legend.title = element_text(color="white"))
plot2<-plot2+ guides(colour = guide_legend(override.aes = list(size=3)))
plot2<-plot2+ylab("|??-cor|")
ggsave("bonitaopequena.pdf",plot2)


plot3<-ggplot(dt,aes(x=n,y=cor,color=M�todo))+geom_line()+theme_minimal()+theme(panel.background = element_rect(fill="black"),
                                                                                panel.grid.major = element_blank(),
                                                                                panel.grid.minor=element_blank(),
                                                                                legend.position=c(0.85,0.85),
                                                                                legend.text = element_text(color ="white" ),
                                                                                legend.title = element_text(color ="white" ))
plot3<-plot3+ylab("cor")+xlab("Tamanho da amostra")
plot3<-plot3+geom_hline(yintercept = .72,color="white", linetype="dashed")
ggsave("corpequena.pdf",plot3)


########### Correla��o baixa #####################################################

s=p=k=NULL  ##Vetores vazios
v<-c(5,10,11:1000) ##Tamanho das amostras


set.seed(12345) ##Pra gente conseguir gerar os mesmos valores bb
for(t in 1:length(v)){
  
  ##Gerando valores normais (media 0) com uma correla��o espec�fica (0.12)
  valores <- rmvnorm(n = v[t],  
                     mean = c(0,0),
                     matrix(c(1,0.12,0.12,1),2,2)) ##Matrix=matriz de correla��o
  
  
  s[t]<-cor(valores[,1],valores[,2],method = "spearman")  ##Correla��o spearman
  p[t]<-cor(valores[,1],valores[,2],method = "pearson")   ##Correla��o Pearson
  k[t]<-cor(valores[,1],valores[,2],method = "kendall")   ##Correla��o kendall
  
}


##          Fazendo gr�fico maneiro no ggplot

##Criar um data.frame
dt<-data.frame(dif=abs(.12-c(s,p,k)))  ##Primeira coluna � o modulo da diferen�a
##da correla��o real (0.72) com as calculadas

##Coluna com qual m�todo foi usado na correla��o
dt$M�todo<-rep(c("Spearman","Pearson","Kendall"),each=length(dt$dif)/3)

##Coluna com o n�mero de amostras
dt$n<-rep(v,3)

dt$cor<-c(s,p,k)


plot<-ggplot(dt,aes(x=n,y=dif,color=M�todo))+geom_line()+theme_minimal()+theme(panel.background = element_rect(fill="black"),
                                                                               panel.grid.major = element_blank(),
                                                                               panel.grid.minor=element_blank(),
                                                                               legend.position=c(0.85,0.85),
                                                                               legend.text = element_text(color ="white" ),
                                                                               legend.title = element_text(color ="white" ))
plot<-plot+ylab("|??-cor|")+xlab("Tamanho da amostra")
ggsave("erro(12).pdf",plot)


plot2<-ggplot(dt,aes(x=M�todo,y=dif,col=M�todo))+geom_jitter(size=.4)+theme_minimal()+ theme(legend.position = c(.85,.85),panel.background = element_rect(fill="black"), panel.grid.major = element_blank(),
                                                                                              panel.grid.minor=element_blank(),axis.text.x = element_blank(),
                                                                                              axis.ticks.x = element_blank(),axis.title.x = element_blank())
plot2<-plot2+stat_summary(fun.data=mean_sdl,fun.args=list(mult=1),col="white",size=1)
plot2<-plot2+theme(legend.text = element_text(color ="white"),
                   legend.title = element_text(color="white"))
plot2<-plot2+ guides(colour = guide_legend(override.aes = list(size=3)))
plot2<-plot2+ylab("|??-cor|")
ggsave("bonitao(12).pdf",plot2)


plot3<-ggplot(dt,aes(x=n,y=cor,color=M�todo))+geom_line()+theme_minimal()+theme(panel.background = element_rect(fill="black"),
                                                                                panel.grid.major = element_blank(),
                                                                                panel.grid.minor=element_blank(),
                                                                                legend.position=c(0.85,0.85),
                                                                                legend.text = element_text(color ="white" ),
                                                                                legend.title = element_text(color ="white" ))
plot3<-plot3+ylab("cor")+xlab("Tamanho da amostra")
plot3<-plot3+geom_hline(yintercept = .12,color="white", linetype="dashed")
ggsave("cor(12).pdf",plot3)




##################Correla��o negativa -.85######################################


s=p=k=NULL  ##Vetores vazios
v<-c(5,10,11:1000) ##Tamanho das amostras


set.seed(12345) ##Pra gente conseguir gerar os mesmos valores bb
for(t in 1:length(v)){
  
  ##Gerando valores normais (media 0) com uma correla��o espec�fica (-0.85)
  valores <- rmvnorm(n = v[t],  
                     mean = c(0,0),
                     matrix(c(1,-0.85,-0.85,1),2,2)) ##Matrix=matriz de correla��o
  
  
  s[t]<-cor(valores[,1],valores[,2],method = "spearman")  ##Correla��o spearman
  p[t]<-cor(valores[,1],valores[,2],method = "pearson")   ##Correla��o Pearson
  k[t]<-cor(valores[,1],valores[,2],method = "kendall")   ##Correla��o kendall
  
}


##          Fazendo gr�fico maneiro no ggplot

##Criar um data.frame
dt<-data.frame(dif=abs(-.85-c(s,p,k)))  ##Primeira coluna � o modulo da diferen�a
##da correla��o real (0.72) com as calculadas

##Coluna com qual m�todo foi usado na correla��o
dt$M�todo<-rep(c("Spearman","Pearson","Kendall"),each=length(dt$dif)/3)

##Coluna com o n�mero de amostras
dt$n<-rep(v,3)

dt$cor<-c(s,p,k)


plot<-ggplot(dt,aes(x=n,y=dif,color=M�todo))+geom_line()+theme_minimal()+theme(panel.background = element_rect(fill="black"),
                                                                               panel.grid.major = element_blank(),
                                                                               panel.grid.minor=element_blank(),
                                                                               legend.position=c(0.85,0.85),
                                                                               legend.text = element_text(color ="white" ),
                                                                               legend.title = element_text(color ="white" ))
plot<-plot+ylab("|??-cor|")+xlab("Tamanho da amostra")
ggsave("erro(-85).pdf",plot)


plot2<-ggplot(dt,aes(x=M�todo,y=dif,col=M�todo))+geom_jitter(size=.4)+theme_minimal()+ theme(legend.position = c(.85,.85),panel.background = element_rect(fill="black"), panel.grid.major = element_blank(),
                                                                                             panel.grid.minor=element_blank(),axis.text.x = element_blank(),
                                                                                             axis.ticks.x = element_blank(),axis.title.x = element_blank())
plot2<-plot2+stat_summary(fun.data=mean_sdl,fun.args=list(mult=1),col="white",size=1)
plot2<-plot2+theme(legend.text = element_text(color ="white"),
                   legend.title = element_text(color="white"))
plot2<-plot2+ guides(colour = guide_legend(override.aes = list(size=3)))
plot2<-plot2+ylab("|??-cor|")
ggsave("bonitao(-85).pdf",plot2)


plot3<-ggplot(dt,aes(x=n,y=cor,color=M�todo))+geom_line()+theme_minimal()+theme(panel.background = element_rect(fill="black"),
                                                                                panel.grid.major = element_blank(),
                                                                                panel.grid.minor=element_blank(),
                                                                                legend.position=c(0.85,0.85),
                                                                                legend.text = element_text(color ="white" ),
                                                                                legend.title = element_text(color ="white" ))
plot3<-plot3+ylab("cor")+xlab("Tamanho da amostra")
plot3<-plot3+geom_hline(yintercept = -.85,color="white", linetype="dashed")
plot3<-plot3+scale_y_continuous(limits = c(-1,-.4))
ggsave("cor(-85).pdf",plot3)

