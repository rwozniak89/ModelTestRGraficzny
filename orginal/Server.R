library(tidyverse)
library(shiny)
library(caTools)
library(randomForest)
library(rpart)
library(e1071)
library(kknn)
library(caret)
library(DT)
shinyServer(
  function(input,output){
    load("ofertyV.RData")
    
    dataAuta<-reactive({
      
      
      d<-ofertyV%>%#%>%filter(marka==input$marka)%>%
        select(cena,Rok_produkcji,Przebieg,Moc,Pojemność_skokowa,Model_pojazdu)%>%
        na.omit()%>%droplevels()#%>% filter(cena>4000,cena<1000000)%>%droplevels()
    })
    
    
    output$marka<-renderUI(
      {
        selectInput(inputId = "marka", label = "marka:",c("Volkswagen") )#choices = uniklalneMarki,selected = uniklalneMarki[1] )
      }
    )
    
    output$model<-renderUI(
      {
        
        unikalneModele<-  (dataAuta()%>%select(Model_pojazdu)%>%unique() )$Model_pojazdu%>%sort()
        
        selectInput(inputId = "model", label = "Model:",choices = unikalneModele,selected = unikalneModele[1] )
      }
    )
    
    output$autaTable<-DT::renderDataTable({
      datatable(dataAuta(),rownames = FALSE,options= list (
        pageLength = 10, scrollX=TRUE, lengthMenu = list(seq(10,100,10),as.character(seq(10,100,10))),
        filter = "none"))
    })
    
    train<-reactive({
      set.seed(123)
      sample<-sample.split(Y=dataAuta(),SplitRatio = .75)
      subset(dataAuta(),sample==TRUE)
    })
    
    test<-reactive({
      #set.seed(123)
      sample<-sample.split(Y=dataAuta(),SplitRatio = .75)
      subset(dataAuta(),sample==FALSE)
    }) 
    
    regrRF<-reactive({
      if(file.exists("modelRF.RData")){
        if(!exists("modelRF"))
          load(file="modelRF.RData")
        modelRF
      }else{
        modelRF<-randomForest(cena~.,data =train() )
        save(modelRF,file="modelRF.RData")
        modelRF
      }
    })
    
    rfpredict<-reactive({
      predict(regrRF(),test())
    })
    
    wynik<-eventReactive( input$wycena,{
      model1<-input$model
      #paliwo1<-input$paliwo
      #paliwo1 <- factor(paliwo1, levels = levels(train()$Rodzaj_paliwa))
      model1 <- factor(model1, levels = levels( dataAuta()$Model_pojazdu))
      row1<-data.frame(cena=as.numeric(0),Rok_produkcji=as.integer(input$rok),
                       Przebieg=as.numeric(input$przebieg),
                       Pojemność_skokowa=as.integer(input$pojemnosc),
                       Moc=as.integer(input$moc),
                       Model_pojazdu=as.factor(model1)#,
                       #Rodzaj_paliwa=as.factor(paliwo1)
                       )
      predicts<-predict(regrRF(),row1)
      #print(predicts)
      predicts
      
    }
      )
    
    output$wynik<- renderPrint(wynik())
  }
  
)