#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#   
#    http://shiny.rstudio.com/
#
        
library(shiny)
library(tidyverse)
library(arulesViz)

library(reticulate)
library(pmml)
PYTHON_DEPENDENCIES = c('spacy','en-core-web-sm-mirror')

    rule_match<- function(r,li){
    result <- DATAFRAME(r, separate = TRUE, setStart = "", setEnd = "") %>% 
        select(LHS) %>% 
        mutate(LHS = as.character(LHS),
                LHS = strsplit(LHS, split = ","),
                is_matched = map(LHS,is.element,set=li),
                all_matched = map(is_matched,all)) %>%
        pull(all_matched)
    return(unlist(result))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinythemes::shinytheme("yeti"),
    
    # Application title
    titlePanel("Rules Visualization"),
    
    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            textInput("news_head","News Headline",""),
            helpText(" Example : Magnitude 4.5 earthquake strikes Southern California"),
            actionButton("run", "Run"),
            actionButton("reset", "Clear"), 
            hr(),
            #submitButton("Run"),
            tags$br(),
            verbatimTextOutput("processed_text"),
            tags$br(),
            DT::dataTableOutput("rulesDataTable"),
            width = 6
        ),

        # Rules Graph viz
        mainPanel(
        tabsetPanel(id = "tab",
            tabPanel("Leaf Node",
                value = 3,
                visNetwork::visNetworkOutput("graphPlot3", width='100%', height='800px'),
                DT::dataTableOutput("rulesDataTable3")
            ),
            tabPanel("Middle Node",
                     value = 2,
                     visNetwork::visNetworkOutput("graphPlot2", width='100%', height='800px'),
                     DT::dataTableOutput("rulesDataTable2")
            ),
            tabPanel("Parent Node",
                     value = 1,
                     visNetwork::visNetworkOutput("graphPlot1", width='100%', height='800px'),
                     DT::dataTableOutput("rulesDataTable1")
            )
        ),
        width = 6
        )
    )
)
    
# Define server logic
server <- function(input, output) {
    # ------------------ App virtualenv setup (Do not edit) ------------------- #
    virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
    python_path = Sys.getenv('PYTHON_PATH')
    # Create virtual env and install dependencies
    if(length(reticulate::virtualenv_list()) == 0){
    reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
    reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES,ignore_installed=TRUE)
    }
    reticulate::use_virtualenv(virtualenv_dir, required = T)

    # ------------------ App server logic (Edit anything below) --------------- #
    rules1 <- read.PMML("rules1.pmml")
    rules2 <- read.PMML("rules2.pmml")
    rules3 <- read.PMML("rules3.pmml")
    reticulate::source_python('text_preprocess.py')
    
    news_head <- reactiveValues(data = NULL)
    
    observeEvent(input$run, {
        news_head$data <- input$news_head
    },ignoreNULL = FALSE)
    
    observeEvent(input$reset, {
        news_head$data <- ""
    }) 
    
    updated_rules3 <- shiny::reactive({
        if(news_head$data == ""){
            rules3
        }
        else{
            li = unlist(strsplit(test_sent(news_head$data),split = " "))
            r <- subset(rules3,subset = rule_match(rules3,li))
            r
        }
    })
    updated_rules1 <- shiny::reactive({
        if(news_head$data == ""){
            rules1
        }
        else{
            li = unlist(strsplit(test_sent(news_head$data),split = " "))
            r <- subset(rules1,subset = rule_match(rules1,li))
            r
        }
    })
    updated_rules2 <- shiny::reactive({
        if(news_head$data == ""){
            rules2
        }
        else{
            li = unlist(strsplit(test_sent(news_head$data),split = " "))
            r <- subset(rules2,subset = rule_match(rules2,li))
            r
        }
       
    })
    current_DT <- shiny::reactive({
        if(input$tab == "1"){
            updated_rules1()
        }
        else if(input$tab == "2"){
            updated_rules2()
        }
        else{
            updated_rules3()
        }
            
    })
    output$processed_text <- renderText(test_sent(news_head$data))
    output$graphPlot1 <- visNetwork::renderVisNetwork({
        plt <- plot(updated_rules1(), method='graph', engine='htmlwidget',
                    control = list(max=500))
        plt$sizingPolicy <- htmlwidgets::sizingPolicy(
            viewer.paneHeight=1000,
            browser.defaultHeight=1000,
            knitr.defaultHeight=1000,
            defaultHeight=1000,defaultWidth=1000,
            browser.fill=TRUE
        )
        plt$height <- 1000
        plt$x$height <- 1000
        plt
    })
    output$graphPlot2 <- visNetwork::renderVisNetwork({
        plt <- plot(updated_rules2(), method='graph', engine='htmlwidget',
                    control = list(max=500))
        plt$sizingPolicy <- htmlwidgets::sizingPolicy(
            viewer.paneHeight=1000,
            browser.defaultHeight=1000,
            knitr.defaultHeight=1000,
            defaultHeight=1000,defaultWidth=1000,
            browser.fill=TRUE
        )
        plt$height <- 1000
        plt$x$height <- 1000
        plt
    })
    output$graphPlot3 <- visNetwork::renderVisNetwork({
        plt <- plot(updated_rules3(), method='graph', engine='htmlwidget',
                    control = list(max=500))
        plt$sizingPolicy <- htmlwidgets::sizingPolicy(
            viewer.paneHeight=1000,
            browser.defaultHeight=1000,
            knitr.defaultHeight=1000,
            defaultHeight=1000,defaultWidth=1000,
            browser.fill=TRUE
        )
        plt$height <- 1000
        plt$x$height <- 1000
        plt
    })
        
    output$rulesDataTable <- DT::renderDataTable({
        DATAFRAME(current_DT()) %>% 
            mutate(support = round(support, digits = 5),
                   confidence = round(confidence, digits = 5),
                   lift = round(lift, digits = 5))
    }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
