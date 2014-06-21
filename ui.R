library(shiny)

shinyUI(navbarPage(textOutput("title"),
  tabPanel(textOutput("tab1"),
      plotOutput('plot',height="100%"),
      hr()),
  tabPanel(textOutput("tab3"),
           plotOutput('hist',height="100%"),
           hr()),
  tabPanel(textOutput("tab2"),
           plotOutput('scatter',height="100%"),
           hr()),
  tabPanel(textOutput("tab4"),
           dataTableOutput('sumtable1'),
           hr()),
  tabPanel(textOutput("about"), includeMarkdown("readme_eng.md"),includeMarkdown("readme_ukr.md"),includeMarkdown("readme_rus.md")),
      fluidRow(
        column(3,
                radioButtons("lang", label = h4(textOutput("lang")),
                             choices = list("English" = "eng", "УкраЇнська" = "ukr", "Русский" = "rus"))),
        column(3,
               h4(textOutput("filter")),
               sliderInput("sizeRange", textOutput("size"),
                           min = 1, max = 2757, value = c(1,2757),round=0,step=10),
        br(),
        
        sliderInput("timeRange", textOutput("time"),
                    min = 1400965200, max = 1401223800, value = c(1400965200,1401223800), round=0, step=300),
        p(textOutput("selectedTime"))),

    column(3, 
      h4(textOutput("teritorry")),
      selectInput("inputRegion", textOutput("region"),
                  c("All regions"="Всі регіони")),
      selectInput("inputDistrict",textOutput("district_num"),
                  multiple = TRUE,
                  c(1:225))),
    column(3,
           h4(textOutput("makeplot")),
           selectInput('plot1', textOutput("mainplot"),
      c("Turnout" = "Явка")),
      selectInput('plot2', textOutput("secplot"),
      c("Disabled" = "Не доступний"),selected="Не доступний"))
  ),
  fluidRow(
    column(12,p(textOutput("rownums"))))
  )
)
