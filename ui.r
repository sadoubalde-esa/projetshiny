


library(shiny)   
library(tidyverse) 
library(dashboard) 
library(shinythemes) 
library(rmarkdown) 
library(rsconnect)  
library(gridExtra) 
library(grid) 
library(rsconnect) 
library(plotly)  
library(knitr)
h3.align <- 'center'  


DONNEE=readRDS("./DONNEE.rds")   
test=readRDS("./test.rds")  
data=readRDS("./data.rds") 
 


shinyUI(navbarPage( 
  
  title = "Détection de la fraude et svm BALDE Sadou_OUSSEINI Farida", 
  theme = shinytheme("cerulean"),
      shinythemes::themeSelector(),
          ##------------------------------------------------------------------------------------------------------------------------------##
          
  tabPanel("Préambule",mainPanel(column(12, offset = 2, includeMarkdown("preambule.Rmd"))) 
  ),  
  
  tabPanel("Généralité",mainPanel(column(12, offset = 2, includeMarkdown("general.Rmd"))) 
),   
          ##-------------------------------------------------------------------------------------------------------------------------------##
 
          tabPanel(
               title= "Exploration",  
                     sidebarLayout(  
                                sidebarPanel(   
                                    sliderInput("N",label= "Taille de la population N:", value=40000,min=10000,max=284000), 
                                    sliderInput("d",label= "Variables à représenter :Dérouler", value=9,min=1,max=22), 
                                    verbatimTextOutput('out1'),
                                    selectInput('in1', 'Choisir une variable:', choices=colnames(DONNEE)),
                                    hr(), 
                                    helpText("Utile pour les onglets:scatter plot,boxplot")
                                   ), 
                      mainPanel( 
                            tabsetPanel( 
                            tabPanel(p(icon("bar-chart-o"),"Distributions") ,plotOutput("G_descript"),includeMarkdown("graph.Rmd")), 
                            tabPanel(p(icon("table"),"La Base"),DT::dataTableOutput("donnee"),includeMarkdown("base.Rmd")),  
                            tabPanel(p(icon("table"),"Corrélations "),tableOutput("corr"),includeMarkdown("correlation.Rmd")), 
                            tabPanel(p(icon("area-chart"),"Scatterplot"),plotOutput("scatter"),includeMarkdown("nuage.Rmd")), 
                            tabPanel(p(icon("area-chart"),"Boxplot"),plotlyOutput("cible"),includeMarkdown("box.Rmd"))
                           ) 
                    )# mainpanel
                  ) #sidebarlayout
           ),#tabpanel 
 
         ##-------------------------------------------------------------------------------------------------------------------------------##
 
        tabPanel( 
             title = "Modélisation",  
                  sidebarLayout(  
                       sidebarPanel(width = 3,  
                            textInput(inputId="app", label="Apprentissage", value = 0.7, width = NULL,placeholder = NULL),  
                            textInput(inputId="valid", label="Validation", value = 0.3, width = NULL,placeholder = NULL), 
                            textInput(inputId="t", label="Taille de la population", value = 4000, width = NULL,placeholder = NULL),
                            sliderInput("degree",label= "Paramètre du noyau polynomiale:", value=2,min=1,max=3), 
                            sliderInput("coef",label= "Coût de pénalisation du kernel", value=1,min=0,max=3), 
                            selectInput(inputId="mins", label="Minimum d arbres:", choices=c(2,4,6,8,10,12), selected = 6, multiple = FALSE,
                                        selectize = TRUE, width = NULL, size = NULL),
                            numericInput(inputId="cp", label="coût de complexiter de l arbre:", value=0.00001, min = 0.000001, max = 0.0001, step = 0.00002,width = NULL)
                            ), 
      
                   #Modelisation:Modelisation 
                  mainPanel( 
                       tabsetPanel( 
                           tabPanel(p(icon("area-chart"),"Modélisation"),  
                               fluidRow(h3( align=h3.align)),
                                       fluidRow( 
                                            column(10, h4( align="center"),
                                                   includeMarkdown("partition.Rmd")
                                                ),
                                            
                                        fluidRow( 
                                            column(7, h2("Correction oversampling", align="center"),
                                                h6("Proportion de fraudes dans la population", align="center"),
                                                column(7,offset=2,plotOutput("camemb"))),
                       
                                            column(5, h4("Proportion de fraudes dans chaque échantillon.", align="center"),
                                                h6("Objectif: Conserver la structure de la population.", align="center"),
                                                column(5,offset=0,tableOutput("taux")))
                                            ) 
                                         )#fluidrow 
                   
                                     ),#tabpanel 
                       
          
                          #Modelisation: selection
                          tabPanel(p(icon("table"),"Séléction"),  
                              fluidRow(h3( align=h3.align)),
                                       fluidRow(
                                           column(11, h4( align="center"),includeMarkdown("selection.Rmd")),
                     
                                       fluidRow( 
                                           column(7, h3("Probabilités prédites par noyau.", align="center"),
                                                h6("1 Probabilité que la transaction soit frauduleuse, 0 si elle est  non frauduleuse."), 
                                                column(7,offset=-1,DT::dataTableOutput("prob"))),
                       
                                            column(4, h4("Taux d erreur de validation.", align="center"),
                                                h6("Les taux ont été évalué sur l échantillon de validation.") ,
                                                column(3,offset=1,tableOutput("erreur"))
                                                  ) 
                                               ) 
                                         )# fluidrow 
                   
                                   ),#f tabpanel
          
                         #Modelisation:performance
                         tabPanel(p(icon("area-chart"),"Performance"),
                             fluidRow(h3("Performance comparative des noyaux kernels.", align=h3.align)),
                                      fluidRow(
                                           column(7, h4("Anlayse par les courbes ROC", align="center"),
                                               h6("Le meileur kernel est celui ayant la ROC la plus extrême possible.", align="center"),
                                               plotOutput("perform")
                                                  ),
                                           column(4, h4("Analyse  quantitative des courbes ROC: AUC", align="center"),
                                               h6("La performace maximale possible correspond à un AUC de 1.", align="center"),
                                               DT::dataTableOutput("auc"))
                     
                                         )# fluidrow 
                   
                                    ),#tabpanel 
                         #Modelisation: benchmark 
                          tabPanel(p(icon("list-alt"),"Benchmark du svm"),   
                              tabsetPanel( 
                                tabPanel(p(icon("table"),"Arbres de décision"),
                                       fluidRow(h3( align=h3.align)),
                                          fluidRow(
                                              column(11, h4( align="center")
                                                ),
                                    
                                          fluidRow( 
                                              column(11, h3("Arbre de décision", align="center"),
                                                 h6("Les noeuds terminaux permettement de trouver la prédiction des transactions:fraude ou non."), 
                                                 column(11,offset= 2,plotOutput("arbre"))), 
                                              
                                              column(11, h4("Importance des variables", align="center"),
                                                        h6("Un nombre réduit  de variables devrait être utilisé") ,
                                                        column(11,offset = 2, plotlyOutput("importance"))),
                                      
                                              column(7, h4("Arbre brute vs arbre optimisé", align="center"),
                                                        column(7, offset =-2, plotOutput("roc_arbre")), 
                                                     
                                              column(5, h4( align="center"),
                                                     column(5,offset = 2, DT::dataTableOutput("auc_arbre")))
                                              )
                                              )
                                              ) 
                                                ), 
                                #------------------------------------------------------------------------------------------------------------------------# 
                                    tabPanel(p(icon("table"),"Gradient boosting"),
                                            fluidRow(h3( align=h3.align)),
                                                 fluidRow(
                                                     column(11, h4( align="center")
                                                       ),
                                           
                                                  fluidRow( 
                                                     column(11, h3("Performance du Gradient boosting ", align="center"),
                                                           h6("Nous ne présentons ici que les résultats en terme de performance de l algorithme."), 
                                                           column(11,offset= 2,plotOutput("roc_GB"))), 
                                             
                                                     column(11, h4("Importance des variables pour le G Boosting.", align="center"),
                                                           column(11,offset = 2, plotOutput("import_GB"))),
                                             
                                                     column(6, h4("AUC du Gradient boosting", align="center"),
                                                           column(6, offset =-2, DT::dataTableOutput("auc_GB")) 
                                                    
                                                      
                                           )
                                         ) 
                                      )# fluidrow 
                                  
                                 ) # tabpanel
                              )
                         )
                            )#tabsetpanel
                       
                       ) #mainpanel 
                  
                  )#sidebarlayout
             
            )#tabpanel de la fenetre  
  
      )#navbarpage
  )






