library(shiny)
library(bslib)
library(shinyWidgets)

source('database.R')


ui <- fluidPage(
  
  theme = bs_theme(version = 5, font_scale = 0.8),
  
  h1(id="title", "Harris County Mockup Property List Selector"),
  tags$style(HTML("#title{font-size: 40px; text-align: center;}")),
  br(),
  br(),
  br(),
  
  titlePanel("Select List"),
  
  
  fluidRow(
    virtualSelectInput(
      inputId = "select_analyst",
      label = "Analyst",
      choices = analysts(),
      showValueAsTags = TRUE,
      search = TRUE,
      multiple = FALSE
    ),
    
    virtualSelectInput(
      inputId = "select_list",
      label = "Property List",
      selected = NULL,
      choices = c(),
      showValueAsTags = TRUE,
      search = TRUE,
      multiple = FALSE
    ),
    
    actionButton(inputId = "add_properties",
                 label = "Apply List",
                 align = "center",
                 style = "margin-bottom: 10px;",
                 style = "margin-top: 25px;",
                 style = "width: 100px;",
                 style = "height: 40px;",
                 style = "margin-left: 40px;",
                 style = "margin-right: 40px;"
                 
    ),
    
    selectInput(
      inputId = "selected_properties", 
      label = "Selected Properties (click and delete to remove)",
      choices = "", 
      multiple=T
    ),
  ),
  
  br(),
  br(),
  titlePanel("Add Analyst"),
  
  
  fluidRow(
    
    textInput(
      inputId = "analyst_name",
      label = "New Analyst Name"
    ),
    
    actionButton(
      inputId = "add_analyst",
      label = "Add Analyst",
      align = "center",
      style = "margin-bottom: 10px;",
      style = "margin-top: 25px;",
      style = "width: 100px;",
      style = "height: 40px;",
      style = "margin-left: 40px;",
      style = "margin-right: 40px;"
    ),
    
    span(textOutput("adding_analyst_warning"), style="color:red")
  ),
  
  
  br(),
  br(),
  titlePanel("Add List"),
  
  
  fluidRow(
    
    virtualSelectInput(
      inputId = "select_analyst_for_list_creation",
      label = "Analyst",
      selected = NULL,
      choices = analysts(),
      showValueAsTags = TRUE,
      search = TRUE,
      multiple = FALSE
    ),
    
    textInput(
      inputId = "list_name",
      label = "New List Name"
    ),
    
    textAreaInput(
      inputId = "list_description",
      label = "Description"
    ),
    
    actionButton(
      inputId = "add_list",
      label = "Add List",
      align = "center",
      style = "margin-bottom: 10px;",
      style = "margin-top: 25px;",
      style = "width: 100px;",
      style = "height: 40px;",
      style = "margin-left: 40px;",
      style = "margin-right: 40px;"
    ),
    
    span(textOutput("adding_list_warning"), style="color:red")
    
  ),
  
  br(),
  br(),
  titlePanel("Add Property to List"),
  
  fluidRow(
    
    virtualSelectInput(
      inputId = "select_analyst_for_adding_prop_to_list",
      label = "Analyst",
      choices = analysts(),
      showValueAsTags = TRUE,
      search = TRUE,
      multiple = FALSE
    ),
    
    virtualSelectInput(
      inputId = "select_list_for_adding_prop",
      label = "Property List",
      selected = NULL,
      choices = c(),
      showValueAsTags = TRUE,
      search = TRUE,
      multiple = FALSE
    ),
    
    virtualSelectInput(
      inputId = "select_properties",
      label = "Search for Property ID",
      choices = properties(),
      showValueAsTags = TRUE,
      search = TRUE,
      multiple = TRUE
    ),
    
    actionButton(
      inputId = "add_property_to_list",
      label = "Add Property to List",
      align = "center",
      style = "margin-bottom: 10px;",
      style = "margin-top: 25px;",
      style = "width: 160px;",
      style = "height: 40px;",
      style = "margin-left: 20px;",
      style = "margin-right: 20px;"
    ),
    
    actionButton(
      inputId = "delete_property_from_list",
      label = "Delete Property from List",
      align = "center",
      style = "margin-bottom: 10px;",
      style = "margin-top: 25px;",
      style = "width: 180px;",
      style = "height: 40px;",
      style = "margin-left: 20px;",
      style = "margin-right: 20px;"
    ),
    
    span(htmlOutput("adding_property_warning"), style="color:red")
    
  )
)



server <- function(input, output, session) {
  
  
  observeEvent(input$select_analyst, {
    
    updateVirtualSelect(session = session,
                      inputId = "select_list",
                      choices = get_lists_from_analyst(input$select_analyst),
                      selected = list()
                      )

  })
  
  
  
  observeEvent(input$add_properties, {
    
    updateSelectInput(session = session,
                        inputId = "selected_properties",
                        choices = get_properties_from_list(input$select_list),
                        selected = get_properties_from_list(input$select_list)
                        )
    
  })
  
  
  
  observeEvent(input$add_analyst, {
    
    if (analyst_exists(input$analyst_name)) {
      
      warning_message <- paste0("Analyst ", input$analyst_name, " already exists")
      output$adding_analyst_warning <- renderText({warning_message})
      
    } else if (input$analyst_name == "") {
      
      warning_message <- "Please put an analyst name"
      output$adding_analyst_warning <- renderText({warning_message})
      
    } else {
      
      warning_message <- paste0("Analyst ", input$analyst_name, " added")
      output$adding_analyst_warning <- renderText({warning_message})
      add_analyst(input$analyst_name)
      
      updateVirtualSelect(session = session,
                          inputId = "select_analyst",
                          choices = analysts()
                          )
      
      updateVirtualSelect(session = session,
                          inputId = "select_analyst_for_list_creation",
                          choices = analysts()
      )
      
      updateVirtualSelect(session = session,
                          inputId = "select_analyst_for_adding_prop_to_list",
                          choices = analysts()
                          )
      
    }
  })
  
  
  
  observeEvent(input$add_list, {
    
    if (list_exists(input$list_name)) {
      
      warning_message <- paste0("List ", input$list_name, " already exists")
      output$adding_list_warning <- renderText({warning_message})
      
    } else if (input$list_name ==  "") {
      
      warning_message <- "Please add a list name"
      output$adding_list_warning <- renderText({warning_message})
      
    } else if (input$list_description == "") {
      
      warning_message <- "Please add a description"
      output$adding_list_warning <- renderText({warning_message})
      
    } else {
      
      add_list(input$select_analyst_for_list_creation, input$list_name, input$list_description)
      output$adding_list_warning <- renderText({"List added"})
      
      
      # update the select list in the first row if it has the same analyst name selected
      # very minor touch but I think it makes it work smoother
      if (input$select_analyst == input$select_analyst_for_list_creation) {
        
        updateVirtualSelect(session = session,
                            inputId = "select_list",
                            choices = get_lists_from_analyst(input$select_analyst),
                            selected = list()
        )
        
        updateVirtualSelect(session = session,
                            inputId = "select_list_for_adding_prop",
                            choices = get_lists_from_analyst(input$select_analyst),
                            selected = list()
        )
        
      }
    }
  })
  
  
  
  observeEvent(input$select_analyst_for_adding_prop_to_list, {
    
    updateVirtualSelect(session = session,
                        inputId = "select_list_for_adding_prop",
                        choices = get_lists_from_analyst(input$select_analyst_for_adding_prop_to_list),
                        selected = list()
    )
    
  })
  
  
  
  observeEvent(input$add_property_to_list, {
    
    
    if (input$select_list_for_adding_prop == "") {
      
      warning_message <- "Please select a list"
      output$adding_property_warning <- renderText({warning_message})
      
    } else if (is.null(input$select_properties)) {
      
      warning_message <- "Please select at least one property to add"
      output$adding_property_warning <- renderText({warning_message})
      
    } else {
      
      num_properties_in_list <- get_num_properties_in_list(input$select_list_for_adding_prop)
      print(num_properties_in_list)
      warning_message <- ""
      
      for (property in input$select_properties) {
        
        if (entry_exists(input$select_list_for_adding_prop, property)) {
          
          warning_message <- paste0(warning_message, "Entry exists for property id ", property, " in the list ", input$select_list_for_adding_prop, "<br/>")
          output$adding_property_warning <- renderText({warning_message})
          
        } else {
          
          if (num_properties_in_list > 100) {
            
            warning_message <- paste0(warning_message, "Couldn't add property ", property, ". List length cannot exceed 100 properties")
            break
            
          }
          
          add_entry(input$select_list_for_adding_prop, property)
          num_properties_in_list <- num_properties_in_list + 1
          warning_message <- paste0(warning_message, "Added property ", property, " to ", input$select_list_for_adding_prop, "<br/>")
          output$adding_property_warning <- renderText({warning_message})
          
        }
        
      }
      
      output$adding_property_warning <- renderText({warning_message})
      
    }
    
  })
  
  
  observeEvent(input$delete_property_from_list, {
    
    if (input$select_list_for_adding_prop == "") {
      
      warning_message <- "Please select a list"
      output$adding_property_warning <- renderText({warning_message})
      
    } else if (is.null(input$select_properties)) {
      
      warning_message <- "Please select at least one property to add"
      output$adding_property_warning <- renderText({warning_message})
      
    } else {
      
      warning_message <- ""
      
      for (property in input$select_properties) {
        
        if (entry_exists(input$select_list_for_adding_prop, property)) {
          
          delete_entry(input$select_list_for_adding_prop, property)
          warning_message <- paste0(warning_message, "Property ", property, " deleted from list ", input$select_list_for_adding_prop, "<br/>")
          output$adding_property_warning <- renderText({warning_message})
          
        } else {
          
          warning_message <- paste0(warning_message, "Property ", property, " not found in list ", input$select_list_for_adding_prop, "<br/>")
          output$adding_property_warning <- renderText({warning_message})
          
        }
        
      }
      
      output$adding_property_warning <- renderText({warning_message})
      
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
