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

#library(reticulate)
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
            submitButton("Run"),
            helpText("Provide empty string to view all rules"),
            tags$br(),
            verbatimTextOutput("processed_text"),
            tags$br(),
            DT::dataTableOutput("rulesDataTable"),
            width = 5
        ),

        # Rules Graph viz   
        mainPanel(
           visNetwork::visNetworkOutput("graphPlot", width='100%', height='800px'),
           width = 7
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
    rules <- read.PMML("rules.pmml")
    reticulate::source_python('text_preprocess.py')
    
    updated_rules <- shiny::reactive({
        if(input$news_head == ""){
            rules
        }
        else{
            li = unlist(strsplit(test_sent(input$news_head),split = " "))
            r <- subset(rules,subset = rule_match(rules,li))
            r
        }
    })
    output$processed_text <- renderText(test_sent(input$news_head))
    output$graphPlot <- visNetwork::renderVisNetwork({
        plt <- plot(updated_rules(), method='graph', engine='htmlwidget',
                    control = list(max=800))
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
        DATAFRAME(updated_rules()) %>% 
            mutate(support = round(support, digits = 5),
                   confidence = round(confidence, digits = 5),
                   lift = round(lift, digits = 5))
    }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
