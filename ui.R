library(shiny)
library(visNetwork)
library(shinydashboard)
require(shinyjs)
require(htmltools)
require(bsplus)
require(DT)

#proponents <- data.frame(x = c("INFANTRY", "MILITARY INTELLIGENCE"))

proponents <- data.frame(x = c("ARMOR", "AVIATION/AVIATION LOGISTICS", "ENGINEERS",  
              "INFANTRY", "MILITARY INTELLIGENCE", "MULTIFUNCTIONAL LOGISTICS"))

# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(title = "Crosswalk Visualizer",
                  titleWidth = 400),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    
  fluidRow(
    box(width = 4,
        
        fluidRow(
          box(title = "Documentation", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 12, collapsed = FALSE,
              
              bs_modal(
                id = "Overview", 
                title = "Crosswalk Visualizer Overview",
                body = includeMarkdown("Overview.md")
              ),
              
              fluidRow(column(width = 11, offset = 1, actionLink("button", "Crosswalk Visualizer Overview") %>%
                                bs_attach_modal("Overview"))),
              
              bs_modal(
                id = "CrosswalkDepth", 
                title = "Explanation of Crosswalk Depth",
                body = includeMarkdown("CrosswalkDepth.md")
              ),
              
              fluidRow(column(width = 11, offset = 1, actionLink("button", "Crosswalk Depth") %>%
                                bs_attach_modal("CrosswalkDepth"))),
              
              bs_modal(
                id = "PayoffComp", 
                title = "Explanation of Payoff and Complexity Calculations",
                body = includeMarkdown("PayoffComp.md")
              ),
              
              fluidRow(column(width = 11, offset = 1, actionLink("button", "Payoff and Complexity Calculations") %>%
                                bs_attach_modal("PayoffComp"))),
              
              bs_modal(
                id = "similarity", 
                title = "Explanation of Similarity Calculation",
                body = includeMarkdown("Similarity.md")
              ),
              fluidRow(column(width = 11, offset = 1, actionLink("button", "Similarity Calculations") %>%
                                bs_attach_modal("similarity")))
              
          )
        ),
        
    fluidRow(
    box(
      title = "Unit Selection", status = "primary", solidHeader = TRUE,
      collapsible = TRUE, width = 12, collapsed = TRUE,
      
      selectInput("Proponent", h5("Select a Proponent"), c("", as.character(proponents$x)), selected = ""),
      uiOutput("TOESelection")
        )
            ),
    conditionalPanel("input.unitType", 
    fluidRow(
      box(
        title = "Task Selection", status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12, collapsed = TRUE,
        
        uiOutput("METList"),
        uiOutput("SCTList")
      )
    )
    ),
    conditionalPanel("input.unitType", 
    fluidRow(            
    box(
      title = "Prioritization Metrics", status = "primary", solidHeader = TRUE,
      collapsible = TRUE, width = 12, collapsed = TRUE,
      
      radioButtons("taskType", label = "Task types to include",
                   choices = list("Collective" = 1, "Individual" = 2, "Both" = 3), 
                   selected = 1),
      
      radioButtons("step", label = "Crosswalk depth",
                   choices = list("1 step - show tasks that directly support a specified task" = 1, 
                                  "1.5 step - include links between the tasks that directly support a specified task" = 1.5, 
                                  "2 step - show tasks that directly and indirectly support a specified task" = 2), 
                   selected = 1) ,
      
      radioButtons("sizeby", label = "Size nodes by",
                   choices = list("payoff - count of the outbound links; number of tasks that depend on proficiency of the specified task" = 1, 
                                  "complexity - count of the inbound links; number of tasks involved in the execution of the specified task" = 2), 
                   selected = 1)
        )
            )
    ),
    
    conditionalPanel("input.unitType", 
    fluidRow(
      box(title = "Similarity Metrics", status = "primary", solidHeader = TRUE,
          collapsible = TRUE, width = 12, collapsed = TRUE,
      
          checkboxInput("showCommunities", label = "Show similar tasks", value = FALSE)
            
      )
    )
    ),
    
    conditionalPanel("input.unitType", 
    fluidRow(
     box(title = "Display Options", status = "primary", solidHeader = TRUE,
         collapsible = TRUE, width = 12, collapsed = TRUE,
                           
         sliderInput("scaleFactor",
                     "Increase node size:",
                     min = 1,
                     max = 5,
                     value = 2),
         
         checkboxInput("labelToggle", label = "Show task descriptions", value = TRUE),
         
         checkboxInput("hidePendants", label = "Hide tasks that only support one other task", value = FALSE)
                       )
                     )
    )
    
    ),
    box(width = 8,
      visNetworkOutput("plot"),
      
      dataTableOutput("DT")
    )
        
    )
  )
)
    