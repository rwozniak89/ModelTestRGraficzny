shinyUI(
  fluidPage(
  titlePanel(title="Otomoto"),
  sidebarLayout(
    sidebarPanel( h3("Szczegoly"),
                  sliderInput("rok","rok produkcji",min=1950,max=2020,value=2010,step=1,round = TRUE,sep= ""),
                  sliderInput("pojemnosc","pojemnosc cm3",min=500,max=5000,value=1995,step=100,round = TRUE,sep= ""),
                  sliderInput("przebieg","przebieg auta",min=0,max=500000,value=120000,step=1000,round = TRUE,sep= ""),
                  sliderInput("moc","moc silnika",min=20,max=700,value=190,step=20,round = TRUE,sep= ""),
                  uiOutput("marka"),
                  uiOutput("model"),
                  actionButton("wycena","wycena")
                  ),
    mainPanel(
            tabsetPanel(type="tabs",
              tabPanel(
                "Wycena Auta",
                verbatimTextOutput("wynik")
                ),
              tabPanel(
                "Tabelka",
                DT::dataTableOutput("autaTable")
              )
            
            )
          )

      )

  )
)