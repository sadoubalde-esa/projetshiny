#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

require(shiny) 
require(e1071)
require(kernlab)  
require(lattice)
require(ggplot2)
require(caret) 
require(plotROC) 
require(tidyverse)  
require(grid) 

require(dplyr)
require(lubridate)
require(xlsx)
require(scales)


require(DT)
require(plotly) 

##data 

source("./helper.R", local=T) 

data <- read.data() # helper.R function 
test <- read.est() # helper.R function 

# Setup inputs


# Define server logic required to draw a histogram
shinyServer(function(input, output){ 
  
  output$G_descript=renderPlot({  
    
    #input pour le N
    N=input$N 
    d=input$d
    
    
    attach(data)   
    
    #creation des graphiques de 9 variables a partir de la d ieme variable: min(d)=1,max(d)=22
    par(mfrow=c(3,3))  
    
    #input type distibution hist ou plot 
    for( i in 1:9){ 
      
      hist(data[,i+d-1],col=c(i:d),prob=T,main=paste("distribution of",c(names(data)[d+i-1])),
           ylab="Frequence", xlab= names(data)[d+i-1]) 
    }  
  })   
  
  output$donnee=renderTable({ 
    
    d=input$d
    E_data=data[,d:(d+8)]
    
    for(j in 1:9){ 
      names(E_data)[j]=paste("E_",c(names(data)[d+j-1])) 
    }   
    
    #sortie Correlations
    # round(cor(E_data),3) 
    E_data[1:20,]
  })  
  
  output$corr=renderTable({  
    
    N=input$N 
    d=input$d
    
    attach(data)  
    round(cor(data),4) 
    
  }) 
  
  output$scatter=renderPlot({   
    N=input$N 
    d=input$d   
   
    plot(data[,2],data[,d],col=c(4,3))
    
  })
  #===========================MODELISATION==============================================# 
  
  
  output$linear=renderPlot({    
    N=input$N
    V=ceiling(0.3*N) 
  
    y=test$Class
    
    RSSl=vector() 
    
    library(e1071)
    for(i in 1:5){  
      M_linear=svm(data$Class~.,data=data,type="C",scale=F,kernel="linear",cost=i) 
      
      #prevision pour le model linear
      p_linear=predict(M_linear,test[,-31],decision.values = T) 
      
      #taux derreur de classification pour le model linear
      
      RSSl[i]= mean(y!=p_linear) 
      
    } 
    
    plot(RSSl,type="l",col=6,main="taux d'erreur du kernel linear")
    
  }) 
  
  output$poly=renderPlot({  
    N=input$N
    V=ceiling(0.3*N) 
    
    #definition de la variable cible test 
    y=test$Class
    RSSp=vector() 
    
    library(e1071)
    for(i in 1:5){ 
      M_pol=svm(data$Class~.,data=data,type="C",scale=F,kernel="polynomial",coef0=0,  degree=2,cost=i)  
      
      p_pol=predict(M_pol,test[,-31],decision.values = T) 
      
      RSSp[i]=mean(y!=p_pol) 
    } 
    
    plot(RSSp,type="l",col=5,xlab="cost",main="taux d'erreur pour le kernel polynomial")
    
    
  }) 
  
  output$sigmoid=renderPlot({  
    
    
    N=input$N
    V=ceiling(0.3*N) 
    
    #definition de la variable cible test 
    y=test$Class  
    RSSs=vector()
    
    for(i in 1:5){
      M_sig=svm(data$Class~.,data=data,type="C",scale=F,kernel="sigmoid",cost=i) 
      
      p_sig=predict(M_sig,test[,-31],decision.values = T)
      RSSs[i]=mean(y!=p_sig)
    } 
    
    plot(RSSs,type="l",main="taux d'erreur pour le kernel sigmoid",xlab="cost") 
    #library(plotly)
    #xa=1:5
    #plot=data.frame(x=xa,z=RSSs)
    
    #plot_ly(plot,x=~x,y=~z,type="scatter",mode="lines") 
    
    
  }) 
  
  
  output$perform=renderPlot({ 
    
    
    N=input$N
    V=ceiling(0.3*N)  
    
    #les input de la performance 
    deg=input$degree 
    co=input$coef
    
    #les library 
    library(kernlab)  
    library(lattice)
    library(ggplot2)
    library(caret) 
    library(plotROC) 
    library(tidyverse) 
    
    
    mod.pol = ksvm(data$Class~.,data=data,kernel="polydot",kpar=list(degree=deg,scale=1,offset=1),C=co,prob.model = TRUE,type="C-svc")
    mod.rad = ksvm(data$Class~.,data=data,kernel="rbfdot",kpar=list(sigma=0.01),C=co,prob.model = TRUE,type="C-svc")
    mod.liner = ksvm(data$Class~.,data=data,kernel="vanilladot",C=co,prob.model = TRUE,type="C-svc") 
    
    
    prev.class.pol = predict(mod.pol,newdata=test[,-31])
    prev.class.rad = predict(mod.rad,newdata=test[,-31]) 
    prev.class.liner = predict(mod.liner,newdata=test[,-31])
    
    prev.prob.pol = predict(mod.pol,newdata=test[,-31],type="probabilities")
    prev.prob.rad = predict(mod.rad,newdata=test[,-31],type="probabilities") 
    prev.prob.liner = predict(mod.liner,newdata=test[,-31],type="probabilities")
    
    
    # prev.class = data.frame(pol=prev.class.pol,rad=prev.class.rad,obs=test$Class) 
    #prev.class %>% summarise_all(funs(err=mean(obs!= .))) %>% select(-obs_err) %>% round(3)  
    
    prev.prob = data.frame(pol=prev.prob.pol[,2],rad=prev.prob.rad[,2],liner=prev.prob.liner[,2],obs=test$Class) 
    df.roc = prev.prob %>% gather(key=Methode,value=score,pol,rad,liner)
    
    ggplot(df.roc)+aes(d=obs,m=score,color=Methode)+geom_roc()+theme_classic()+ ggtitle("Courbe ROC")
    # df.roc %>% group_by(Methode) %>% summarize(AUC=pROC::auc(obs,score))
    
  }) 
  
  
} 
)











