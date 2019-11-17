library(shiny) 
library(e1071)
library(kernlab)  
library(lattice)
library(ggplot2)
library(caret) 
library(plotROC) 
library(tidyverse)  
library(grid) 
library(dashboard) 
library(dplyr)
library(plotly) 
library(ggthemes) 
library(rpart)  
library(tidyverse) 
library(rpart.plot) 
library(gbm) 
library(car)
library(Amelia)

library(shinydashboard)
library(flexdashboard) 
library(scales) 
library(DT) 
library(pROC) 
library(formattable)

# Define server logic required to draw a histogram 

       # data_base=data.frame(OBS,DONNEE)  
    
                   DONNEE=readRDS("./DONNEE.rds")     
                   Time=as.numeric(DONNEE$Time) 
                   DONNEE$Time=Time  
                   
shinyServer(function(input, output,session){   
    
   
    output$G_descript=renderPlot({  
        #input pour le N: taille de la population
        N=input$N 
        d=input$d 
        data=DONNEE %>% sample_n(N)
        par(mfrow=c(3,3))  
       
        p=1:nrow(data)
        for( i in 1:9)
                     { 
           
                     hist(data[,i+d-1],col=c(i:d),prob=T,main=paste("distribution of",c(names(data)[d+i-1])),
                     ylab="Frequence", xlab= names(data)[d+i-1])  
        
                    }   
                
              } 
        )   
  
     output$donnee=DT::renderDataTable({ 
    
        N=input$N 
        d=input$d
        #mes donnees avec tirage alealtoire de N observation
        data=DONNEE %>% sample_n(N)
        E_data=data[,d:(d+4)]
        
        for(j in 1:5)
                     { 
                       names(E_data)[j]=c(names(data)[d+j-1]) 
                    }   
                   # round(cor(E_data),3) 
                 round( E_data,8)
              }     
         )  
  
  
  
    output$corr=renderTable({  
        N=input$N 
        d=input$d
        #matrice de correlation
        data=DONNEE %>% sample_n(N)
        round(cor(data),4) 
      }
   ) 
  
  
    # NUAGE DES  POINTS
    output$scatter=renderPlot({   
       N=input$N 
       d=input$d   
       data=DONNEE %>% sample_n(N) 
       in1=input$in1
      plot(data[,paste(in1)],data[,d],col=c(4,3),main=paste("Nuage de points"),xlab=paste(in1),ylab=c(names(data)[d]))  
       
      
     }
   )
  
    #box plot
    output$cible=renderPlotly({   
      N=input$N 
      d=input$d   
      data=DONNEE %>% sample_n(N) 
      in1=input$in1  
      x=data[,paste(in1)]  
    
      p <- plot_ly(x = ~x, type = "box")%>% add_trace(x = ~data$Class) 
       
    }
    )
    
  #======================================================MODELISATION====================================================# 
  
  
    output$taux=renderTable({    
       n= as.numeric(input$app)
       v= as.numeric(input$valid )   
       t= as.numeric(input$t)
       #echantillonage des donnees
       Brute= DONNEE 
       set.seed(1234)
       population=rbind(filter(Brute, Brute$Class==1),filter(Brute,Brute$Class==0)%>% sample_n(t)) 
       data=  rbind(filter( population,  population$Class==1)%>% sample_frac(n),filter( population, population$Class==0)%>% sample_frac(n))
       test=  rbind(filter( population,  population$Class==1)%>% sample_frac(v),filter( population, population$Class==0)%>% sample_frac(v))
       #conservation de la structure de la population
       taux=cbind(table(population$Class)/nrow(population),table(data$Class)/nrow(data),table(test$Class)/nrow(test))
       colnames(taux)= c("population","apprentissage","validation")
       fraude=rbind("non","oui") 
       conserve=cbind(fraude,taux)  
       colnames(conserve)[1]="fraude" 
       conserve
       } 
      ) 
  
     #fraction de fraude dans la population  
    output$camemb=renderPlot({    
      n= as.numeric(input$app)
      v= as.numeric(input$valid ) 
      t= as.numeric(input$t)
      Brute= DONNEE 
      set.seed(1234)
      population=rbind(filter(Brute, Brute$Class==1),filter(Brute,Brute$Class==0)%>% sample_n(t)) 
      data=  rbind(filter( population,  population$Class==1)%>% sample_frac(n),filter( population, population$Class==0)%>% sample_frac(n))
      test=  rbind(filter( population,  population$Class==1)%>% sample_frac(v),filter( population, population$Class==0)%>% sample_frac(v))
      library(ggplot2)
      df <- data.frame(
        fraude = c("non", "oui"),
        value = c(t, 492)
      )
       
      bp<- ggplot(df, aes(x="", y=value, fill=fraude))+
        geom_bar(width = 1, stat = "identity")
       

      pie <- bp + coord_polar("y", start=0)
      pie+ scale_fill_brewer(palette="Dark2") + scale_fill_brewer(palette="Blues")+
      theme_minimal()
     
     } 
   ) 
    
    ## -----------------------------------------------------------------------------------------------------------------------##
    output$erreur=renderTable({   
      #mes inputs 
      n= as.numeric(input$app)
      v= as.numeric(input$valid ) 
      t= as.numeric(input$t) 
      deg=input$degree 
      co=input$coef
      
      #les donnees
      Brute= DONNEE 
      set.seed(1234)
      population=rbind(filter(Brute, Brute$Class==1),filter(Brute,Brute$Class==0)%>% sample_n(t)) 
      data=  rbind(filter( population,  population$Class==1)%>% sample_frac(n),filter( population, population$Class==0)%>% sample_frac(n))
      test=  rbind(filter( population,  population$Class==1)%>% sample_frac(v),filter( population, population$Class==0)%>% sample_frac(v))
      
      #estimation des modeles: noyaux kernels
      mod.pol = ksvm(data$Class~.,data=data,kernel = "polydot", kpar=list(degree=deg,scale=1,offset=1),C=co,prob.model = TRUE,type="C-svc")
      mod.rad = ksvm(data$Class~.,data=data,kernel = "rbfdot",  kpar=list(sigma=0.01),C=co,prob.model = TRUE,type="C-svc")
      mod.liner = ksvm(data$Class~.,data=data,kernel="vanilladot", C=co,prob.model = TRUE,type="C-svc") 
      
      #prediction des classes 1 ou 0
      prev.class.pol = predict(mod.pol,newdata=test[,-31])
      prev.class.rad = predict(mod.rad,newdata=test[,-31]) 
      prev.class.liner = predict(mod.liner,newdata=test[,-31])
      
      #prediction des probas
      prev.prob.pol = predict(mod.pol,newdata=test[,-31],type="probabilities")
      prev.prob.rad = predict(mod.rad,newdata=test[,-31],type="probabilities") 
      prev.prob.liner = predict(mod.liner,newdata=test[,-31],type="probabilities")
      
     
      prev.class = data.frame(polynomial=prev.class.pol,radial=prev.class.rad,linear=prev.class.liner,obs=test$Class) 
      prev.class %>% summarise_all(funs(err=mean(obs!= .))) %>% select(-obs_err) %>% round(3)# taux d'erreur  
    }
    ) 
    
    #---------------------------------------------------------------------------------------------------------------------------#
  
  
    output$prob=DT::renderDataTable({   
        #mes inputs 
        n= as.numeric(input$app)
        v= as.numeric(input$valid ) 
        t= as.numeric(input$t) 
        deg=input$degree 
        co=input$coef
    
        Brute= DONNEE 
        set.seed(1234)
        population=rbind(filter(Brute, Brute$Class==1),filter(Brute,Brute$Class==0)%>% sample_n(t)) 
        data=  rbind(filter( population,  population$Class==1)%>% sample_frac(n),filter( population, population$Class==0)%>% sample_frac(n))
        test=  rbind(filter( population,  population$Class==1)%>% sample_frac(v),filter( population, population$Class==0)%>% sample_frac(v))
       
        mod.pol = ksvm(data$Class~.,data=data,kernel = "polydot", kpar=list(degree=deg,scale=1,offset=1),C=co,prob.model = TRUE,type="C-svc")
        mod.rad = ksvm(data$Class~.,data=data,kernel = "rbfdot",  kpar=list(sigma=0.01),C=co,prob.model = TRUE,type="C-svc")
        mod.liner = ksvm(data$Class~.,data=data,kernel="vanilladot", C=co,prob.model = TRUE,type="C-svc") 
      
        prev.class.pol = predict(mod.pol,newdata=test[,-31])
        prev.class.rad = predict(mod.rad,newdata=test[,-31]) 
        prev.class.liner = predict(mod.liner,newdata=test[,-31])
      
        prev.prob.pol = predict(mod.pol,newdata=test[,-31],type="probabilities")
        prev.prob.rad = predict(mod.rad,newdata=test[,-31],type="probabilities") 
        prev.prob.liner = predict(mod.liner,newdata=test[,-31],type="probabilities")
      
        #table des probas predictes par noyau kernel
        prev.prob = data.frame(poly=prev.prob.pol,rad=prev.prob.rad,lin=prev.prob.liner,obs=test$Class)  
        round(prev.prob[,-7],4)
         }
        ) 
  #---------------------------------------------------------------------------------------------------------------------------# 
   output$auc=DT::renderDataTable({   
      #mes inputs 
      n= as.numeric(input$app)
      v= as.numeric(input$valid ) 
      t= as.numeric(input$t) 
      deg=input$degree 
      co=input$coef
      
      Brute= DONNEE 
      set.seed(1234)
      population=rbind(filter(Brute, Brute$Class==1),filter(Brute,Brute$Class==0)%>% sample_n(t)) 
      data=  rbind(filter( population,  population$Class==1)%>% sample_frac(n),filter( population, population$Class==0)%>% sample_frac(n))
      test=  rbind(filter( population,  population$Class==1)%>% sample_frac(v),filter( population, population$Class==0)%>% sample_frac(v))
     
      mod.pol = ksvm(data$Class~.,data=data,kernel = "polydot", kpar=list(degree=deg,scale=1,offset=1),C=co,prob.model = TRUE,type="C-svc")
      mod.rad = ksvm(data$Class~.,data=data,kernel = "rbfdot",  kpar=list(sigma=0.01),C=co,prob.model = TRUE,type="C-svc")
      mod.liner = ksvm(data$Class~.,data=data,kernel="vanilladot", C=co,prob.model = TRUE,type="C-svc") 
      
      #prediction des probas
      prev.prob.pol = predict(mod.pol,newdata=test[,-31],type="probabilities")
      prev.prob.rad = predict(mod.rad,newdata=test[,-31],type="probabilities") 
      prev.prob.liner = predict(mod.liner,newdata=test[,-31],type="probabilities")
      
      # mise en place des AUC par noyau
      prev.prob = data.frame(polynomial=prev.prob.pol[,2],radial=prev.prob.rad[,2],linear=prev.prob.liner[,2],obs=test$Class)  
      df.roc = prev.prob %>% gather(key=Noyau_Kernel,value=score,polynomial,radial,linear)
      df.roc %>% group_by(Noyau_Kernel) %>% summarize(AUC=pROC::auc(obs,score)) 
      }
    ) 
  #---------------------------------------------------------------------------------------------------------------------------#
  #Discrimination des courbes roc par noyau kernel
  output$perform=renderPlot({ 
      
      n= as.numeric(input$app)
      v= as.numeric(input$valid )  
      t= as.numeric(input$t)
      
      deg=input$degree 
      co=input$coef
      
      Brute= DONNEE 
      set.seed(1234)
      population=rbind(filter(Brute, Brute$Class==1),filter(Brute,Brute$Class==0)%>% sample_n(t)) 
      data=  rbind(filter( population,  population$Class==1)%>% sample_frac(n),filter( population, population$Class==0)%>% sample_frac(n))
      test=  rbind(filter( population,  population$Class==1)%>% sample_frac(v),filter( population, population$Class==0)%>% sample_frac(v))
      
        mod.pol = ksvm(data$Class~.,data=data,kernel = "polydot", kpar=list(degree=deg,scale=1,offset=1),C=co,prob.model = TRUE,type="C-svc")
        mod.rad = ksvm(data$Class~.,data=data,kernel = "rbfdot",  kpar=list(sigma=0.01),C=co,prob.model = TRUE,type="C-svc")
        mod.liner = ksvm(data$Class~.,data=data,kernel="vanilladot", C=co,prob.model = TRUE,type="C-svc") 
    
        prev.class.pol = predict(mod.pol,newdata=test[,-31])
        prev.class.rad = predict(mod.rad,newdata=test[,-31]) 
        prev.class.liner = predict(mod.liner,newdata=test[,-31])
    
        prev.prob.pol = predict(mod.pol,newdata=test[,-31],type="probabilities")
        prev.prob.rad = predict(mod.rad,newdata=test[,-31],type="probabilities") 
        prev.prob.liner = predict(mod.liner,newdata=test[,-31],type="probabilities")
    
        prev.prob = data.frame(polynomial=prev.prob.pol[,2],radial=prev.prob.rad[,2],linear=prev.prob.liner[,2],obs=test$Class) 
        df.roc = prev.prob %>% gather(key=Noyau_kernel,value=score,polynomial,radial,linear)
        
        ggplot(df.roc)+aes(d=obs,m=score,color=Noyau_kernel)+geom_roc()+ ggtitle("Courbe ROC") 
       } 
      )  
    
    #Benckmark arbre de decision  
    
   output$arbre=renderPlot({   
     n= as.numeric(input$app)
     v= as.numeric(input$valid )  
     t= as.numeric(input$t)
     
     CP= as.numeric(input$cp) 
     mins= as.numeric(input$mins)
     
     Brute= DONNEE 
     set.seed(1234)
     population=rbind(filter(Brute, Brute$Class==1),filter(Brute,Brute$Class==0)%>% sample_n(t)) 
     data=  rbind(filter( population,  population$Class==1)%>% sample_frac(n),filter( population, population$Class==0)%>% sample_frac(n))
     test=  rbind(filter( population,  population$Class==1)%>% sample_frac(v),filter( population, population$Class==0)%>% sample_frac(v))
     
     set.seed(1234)
     bank.arbre3 <- rpart(data$Class~.,data=data,cp=CP,minsplit=mins,method="class") 
     
     cp_opt <- bank.arbre3$cptable %>% as.data.frame() %>%filter(xerror==min(xerror)) %>% select(CP) %>% max() %>% as.numeric()
     bank.arbre.fin <- prune(bank.arbre3,cp=cp_opt)
     rpart.plot(bank.arbre.fin,cex=0.5)
      
    }) 
   
     # coube roc de l'arbre de decision  
   output$roc_arbre=renderPlot({   
       n= as.numeric(input$app)
       v= as.numeric(input$valid )  
       t= as.numeric(input$t)
     
       CP= as.numeric(input$cp) 
       mins= as.numeric(input$mins)
     
       Brute= DONNEE 
       set.seed(1234)
       population=rbind(filter(Brute, Brute$Class==1),filter(Brute,Brute$Class==0)%>% sample_n(t)) 
       data=  rbind(filter( population,  population$Class==1)%>% sample_frac(n),filter( population, population$Class==0)%>% sample_frac(n))
       test=  rbind(filter( population,  population$Class==1)%>% sample_frac(v),filter( population, population$Class==0)%>% sample_frac(v))
     
       set.seed(1234)
       bank.arbre3 <- rpart(data$Class~.,data=data,cp=CP,minsplit=mins,method="class") ##estimation de larbre 
       
        #complexite optimale 
        cp_opt <- bank.arbre3$cptable %>% as.data.frame() %>%filter(xerror==min(xerror)) %>% select(CP) %>% max() %>% as.numeric()
        bank.arbre.fin <- prune(bank.arbre3,cp=cp_opt)
        
        #courbe roc pour les deux preductions 
        score <- data.frame(arbre_opt=predict(bank.arbre3,newdata=test)[,2],arbre_brute=predict(bank.arbre.fin,newdata=test)[,2],obs=test$Class)
        library(plotROC)
        df.roc <- score  %>% gather(key=methode,value=score,arbre_opt,arbre_brute)
        ggplot(df.roc)+aes(d=obs,m=score,color=methode)+geom_roc()+ggtitle("ROC des algorithmes") 
     
      })  
      # mesure d importance  des variables  
   output$importance=renderPlotly({   
     n= as.numeric(input$app)
     v= as.numeric(input$valid )  
     t= as.numeric(input$t)
     
     CP= as.numeric(input$cp) 
     mins= as.numeric(input$mins)
     
     Brute= DONNEE 
     set.seed(1234)
     population=rbind(filter(Brute, Brute$Class==1),filter(Brute,Brute$Class==0)%>% sample_n(t)) 
     data=  rbind(filter( population,  population$Class==1)%>% sample_frac(n),filter( population, population$Class==0)%>% sample_frac(n))
     test=  rbind(filter( population,  population$Class==1)%>% sample_frac(v),filter( population, population$Class==0)%>% sample_frac(v))
     
     set.seed(1234)
     bank.arbre3 <- rpart(data$Class~.,data=data,cp=CP,minsplit=mins,method="class") ##estimation de larbre 
     
     #complexite optimale 
     cp_opt <- bank.arbre3$cptable %>% as.data.frame() %>%filter(xerror==min(xerror)) %>% select(CP) %>% max() %>% as.numeric()
     bank.arbre.fin <- prune(bank.arbre3,cp=cp_opt)
     
     var.imp <- bank.arbre.fin$variable.importance
     nom.var <- substr(names(var.imp),1,3)
     nom.var[c(4,5)] <- c("co.c","co.p") #pas de diplucations des noms
     var.imp1 <- data.frame(var=nom.var,score=var.imp)
     var.imp1$var <- factor(var.imp1$var,levels=nom.var)
     ggplotly(ggplot(var.imp1)+aes(x=var,y=score)+geom_bar(stat="identity")+theme_classic()+ggtitle("Variables pertinantes") ) 
     
   }) 
   
   #mise en place des AUC DES arbres
   output$auc_arbre=DT::renderDataTable({   
     n= as.numeric(input$app)
     v= as.numeric(input$valid )  
     t= as.numeric(input$t)
     
     CP= as.numeric(input$cp) 
     mins= as.numeric(input$mins)
     
     Brute= DONNEE 
     set.seed(1234)
     population=rbind(filter(Brute, Brute$Class==1),filter(Brute,Brute$Class==0)%>% sample_n(t)) 
     data=  rbind(filter( population,  population$Class==1)%>% sample_frac(n),filter( population, population$Class==0)%>% sample_frac(n))
     test=  rbind(filter( population,  population$Class==1)%>% sample_frac(v),filter( population, population$Class==0)%>% sample_frac(v))
     
     set.seed(1234)
     bank.arbre3 <- rpart(data$Class~.,data=data,cp=CP,minsplit=mins,method="class") ##estimation de larbre 
     
     library(plotROC)
     cp_opt <- bank.arbre3$cptable %>% as.data.frame() %>%filter(xerror==min(xerror)) %>% select(CP) %>% max() %>% as.numeric()
     bank.arbre.fin <- prune(bank.arbre3,cp=cp_opt)
     
     score <- data.frame(arbre_opt=predict(bank.arbre3,newdata=test)[,2],arbre_brute=predict(bank.arbre.fin,newdata=test)[,2],obs=test$Class)
     df.roc <- score  %>% gather(key=methode,value=score,arbre_opt,arbre_brute) 
     
     df.roc %>% group_by(methode) %>% summarize(AUC=pROC::auc(obs,score))
    
   })  
   
   #mise en place des AUC du gradient boosting  
  output$roc_GB=renderPlot({   
     n= as.numeric(input$app)
     v= as.numeric(input$valid )  
     t= as.numeric(input$t)
     
     CP= as.numeric(input$cp) 
     mins= as.numeric(input$mins)
     
     Brute= DONNEE 
     set.seed(1234)
     population=rbind(filter(Brute, Brute$Class==1),filter(Brute,Brute$Class==0)%>% sample_n(t)) 
     data = rbind(filter( population,  population$Class==1)%>% sample_frac(n),filter( population, population$Class==0)%>% sample_frac(n))
     test = rbind(filter( population,  population$Class==1)%>% sample_frac(v),filter( population, population$Class==0)%>% sample_frac(v))
     
     ##gradient boosting 
     set.seed(1234)
     # input cvfolds shrinkage ntrees
     mod.ada  <- gbm(data$Class~.,data=data,distribution="adaboost",cv.folds=5, shrinkage=0.01,n.trees=500) 
     Mopt.ada <- gbm.perf(mod.ada,method="cv")  
     prev.ada <- predict(mod.ada,newdata=test,type="response", n.trees=Mopt.ada) 
     
     prev.prob <- data.frame(Boosting=prev.ada,obs=test$Class) 
     df.roc <- prev.prob %>% gather(key=Methode,value=score,Boosting) 
     
     ggplot(df.roc)+aes(d=obs,m=score,color=Methode)+ geom_roc()+theme_classic()
  #----------------------------------------------------------------------------------------------------------------------# 
   }) 
   
   # importance des varibles de l'algo gradient boosting  
   output$import_GB=renderPlot({   
     n= as.numeric(input$app)
     v= as.numeric(input$valid )  
     t= as.numeric(input$t)
     
     CP= as.numeric(input$cp) 
     mins= as.numeric(input$mins)
     
     Brute= DONNEE 
     set.seed(1234)
     population=rbind(filter(Brute, Brute$Class==1),filter(Brute,Brute$Class==0)%>% sample_n(t)) 
     data=  rbind(filter( population,  population$Class==1)%>% sample_frac(n),filter( population, population$Class==0)%>% sample_frac(n))
     test=  rbind(filter( population,  population$Class==1)%>% sample_frac(v),filter( population, population$Class==0)%>% sample_frac(v))
     
     ##gradient boosting 
     set.seed(1234)
     # input cvfolds shrinkage ntrees
     mod.ada <- gbm(data$Class~.,data=data,distribution="adaboost",cv.folds=5, shrinkage=0.01,n.trees=500) 
    
     summary(mod.ada)[1:10,]
  #--------------------------------------------------------------------------------------------------------------------------------------# 
    
   }) 
   
   #AUC du gradient boosting 
  output$auc_GB=DT::renderDataTable({   
     n= as.numeric(input$app)
     v= as.numeric(input$valid )  
     t= as.numeric(input$t)
    
     CP= as.numeric(input$cp) 
     mins= as.numeric(input$mins)
     
     Brute= DONNEE 
     set.seed(1234)
     population=rbind(filter(Brute, Brute$Class==1),filter(Brute,Brute$Class==0)%>% sample_n(t)) 
     data=  rbind(filter( population,  population$Class==1)%>% sample_frac(n),filter( population, population$Class==0)%>% sample_frac(n))
     test=  rbind(filter( population,  population$Class==1)%>% sample_frac(v),filter( population, population$Class==0)%>% sample_frac(v))
     
     ##gradient boosting 
     set.seed(1234)
     
     mod.ada <- gbm(data$Class~.,data=data,distribution="adaboost",cv.folds=5, shrinkage=0.01,n.trees=500) 
     Mopt.ada <- gbm.perf(mod.ada,method="cv")  
     prev.ada <- predict(mod.ada,newdata=test,type="response", n.trees=Mopt.ada) 
     
     prev.prob <- data.frame(Boosting=prev.ada,obs=test$Class) 
     df.roc <- prev.prob %>% gather(key=Methode,value=score,Boosting) 
     
     df.roc %>% group_by(Methode) %>% summarize(AUC=pROC::auc(obs,score))  
  #---------------------------------------------------------------------------------------------------------------------------#
     
   })
   
    
  } 
)
