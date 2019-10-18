#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
library(shiny)  
library(dashboard) 
library(shinythemes) 
library(rmarkdown) 
library(rsconnect)  
library(gridExtra) 
library(grid) 
library(rsconnect)
h3.align <- 'center'  



shinyUI(navbarPage( 
  
  title = "Dectection de la fraude par les svm", 
  theme = shinytheme("cyborg"),
  shinythemes::themeSelector(),
  tabPanel(
    title= " Description des donnees",  
    sidebarLayout(  
      sidebarPanel(   
        
        sliderInput("N",label= "Taille de lechantillon", value=10000,min=2000,max=40000), 
        #checkboxGroupInput("TG", label = "TYPE de Graphique:",c("Line_plot","Histogram"),selected="Histogram"),
        sliderInput("d",label= "Representer A partir de la:", value=9,min=1,max=22)
      ), 
      mainPanel( 
        tabsetPanel( 
          tabPanel(p(icon("bar-chart-o"),"Graphique") ,plotOutput("G_descript")), 
          tabPanel(p(icon("table"),"donnees"),tableOutput("donnee")), 
          tabPanel(p(icon("table"),"Correlations matrix"),tableOutput("corr")), 
          tabPanel(p(icon("area-chart"),"scatterplot"),plotOutput("scatter")),
          selected="Graphique",id='Graphique',type="pills"
          
        ) 
      )#ferme mainpanel
    ) #ferme sidebarlayout
  ) ,#ferme tabpanel 
  
  tabPanel( 
    title=" Modelisation",  
    sidebarLayout(  
      sidebarPanel(width = 3, 
                   sliderInput("degree",label= "degree du polynome", value=2,min=1,max=3), 
                   sliderInput("coef",label= "Coefficient du kernel", value=1,min=0,max=3)
      ), 
      
      mainPanel( 
        tabsetPanel( 
          tabPanel(p(icon("area-chart"),"Taux linear"),plotOutput("linear")), 
          tabPanel(p(icon("area-chart"),"taux polynomial"),plotOutput("poly")), 
          tabPanel(p(icon("area-chart"),"taux sigmoid"),plotOutput("sigmoid")),   
          
          
          tabPanel(p(icon("area-chart"),"performance"),
                   fluidRow(h3("Analyse comparative des kernels", align=h3.align)),
                   fluidRow(
                     column(4, h4("anlayse par les courbes AUC", align="center"),
                            h6("le meileur kernel est celui ayant l AUC la plus extreme", align="center"),
                            plotOutput("perform")
                     ),
                     column(5, h4("Table anlyse par les matrices de confusions", align="center"),
                            h6("Plus pertinant etant donne que le taux de fraude est faible", align="center"),
                            renderTable("confusion"))
                     
                   )#ferme fluidrow 
                   
          ),#ferme tabpanel
          
          ##################################### 
          tabPanel(p(icon("bar-chart-o"),"analyse"),
                   fluidRow(h3("benchmark du svm", align=h3.align)),
                   fluidRow(
                     column(7, h4("Nous retenons le kernel radial su svm", align="center"),
                            h6("sur la base des AUC", align="center"),
                            plotOutput("salesByStateOut")
                     ),
                     column(5, h4("Gradient Boosting comme benchmar", align="center"),
                            h6("Considerer comme  le meilleur des machine learning", align="center"),
                            plotOutput("salesByLocOut"))
                     
                   )#ferme fluidrow 
                   
          )#ferme tabpanel
          
        )#tabsetpanel
      ) #mainpanel 
    )#sidebarlayout
  )#tabpanel de la fenetre  
  
  
  
)#navbarpage
)






