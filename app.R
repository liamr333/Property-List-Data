library(shiny)
library(bslib)
library(shinyWidgets)
library(RMySQL)


# your database name here
database_name = '' 

# your password here
password = ''

analysts <- function() {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host='localhost', 
                               port=3306, 
                               user='root', 
                               password=password)
  
  query_result <- dbSendQuery(mysqlconnection, "select * from analyst")
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
  df$Analyst_Name
}

properties <- function() {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host='localhost', 
                               port=3306, 
                               user='root', 
                               password=password)
  
  query_result <- dbSendQuery(mysqlconnection, "select prop_id from property")
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
  df$prop_id
}


get_lists_from_analysts <- function(analyst) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host='localhost', 
                               port=3306, 
                               user='root', 
                               password=password)
  
  query <- paste0("select list_name from list where list_creator = \'", analyst, "\'")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
  df$list_name
}


get_properties_from_list <- function(list_name) {
  # for our example, we are pretending this is a list selector in the Bexar county dashboard
  county = "Bexar"
  
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host='localhost', 
                               port=3306, 
                               user='root', 
                               password=password)
  
  query <- paste0("select property.prop_id from list inner join entry on list.list_name = entry.entry_list_name inner join property on property.prop_id = entry.entry_prop_id where list.list_name = \'",
                  list_name, "\' and property.prop_county = \'", county, "\'")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
  df$prop_id
}


analyst_exists <- function(analyst_name) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host='localhost', 
                               port=3306, 
                               user='root', 
                               password=password)
  
  query <- paste0("select * from analyst where analyst_name = \'", analyst_name, "\'")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
  nrow(df) > 0
}


add_analyst <- function(analyst_name) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host='localhost', 
                               port=3306, 
                               user='root', 
                               password=password)
  
  query <- paste0("insert into analyst (analyst_name) values (\'", analyst_name, "\')")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
}

list_exists <- function(list_name) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host='localhost', 
                               port=3306, 
                               user='root', 
                               password=password)
  
  query <- paste0("select * from list where list_name = \'", list_name, "\'")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  dbDisconnect(mysqlconnection)
  nrow(df) > 0
}

add_list <- function(list_creator, list_name, list_description) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host='localhost', 
                               port=3306, 
                               user='root', 
                               password=password)
  
  query <- paste0("insert into list (list_name, list_creator, list_description) values (\'", list_name, "\', \'", list_creator, "\', \'", list_description, "\')")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  print(df)
  dbDisconnect(mysqlconnection)
}

entry_exists <- function(entry_list_name, entry_prop_id) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host='localhost', 
                               port=3306, 
                               user='root', 
                               password=password)
  
  query <- paste0("select * from entry where entry_list_name = \'", entry_list_name, "\' and entry_prop_id = \'", entry_prop_id, "\'")
  print(query)
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  print(df)
  dbDisconnect(mysqlconnection)
  nrow(df) > 0
}

add_entry <- function(entry_list_name, entry_prop_id) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host='localhost', 
                               port=3306, 
                               user='root', 
                               password=password)
  
  query <- paste0("insert into entry (entry_list_name, entry_prop_id) values (\'", entry_list_name, "\', \'", entry_prop_id, "\')")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  print(df)
  dbDisconnect(mysqlconnection)
}

delete_entry <- function(entry_list_name, entry_prop_id) {
  mysqlconnection <- dbConnect(RMySQL::MySQL(), 
                               dbname=database_name, 
                               host='localhost', 
                               port=3306, 
                               user='root', 
                               password=password)
  
  query <- paste0("delete from entry where entry_list_name = \'", entry_list_name, "\' and entry_prop_id = \'", entry_prop_id, "\'")
  query_result <- dbSendQuery(mysqlconnection, query)
  df <- fetch(query_result, n = -1)
  print(df)
  dbDisconnect(mysqlconnection)
}

ui <- fluidPage(
  
  theme = bs_theme(version = 5, font_scale = 0.8),
  
  h1(id="title", "Bexar County Mockup Property List Selector"),
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
                      choices = get_lists_from_analysts(input$select_analyst),
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
                            choices = get_lists_from_analysts(input$select_analyst),
                            selected = list()
        )
        
        updateVirtualSelect(session = session,
                            inputId = "select_list_for_adding_prop",
                            choices = get_lists_from_analysts(input$select_analyst),
                            selected = list()
        )
        
      }
    }
  })
  
  
  
  observeEvent(input$select_analyst_for_adding_prop_to_list, {
    
    updateVirtualSelect(session = session,
                        inputId = "select_list_for_adding_prop",
                        choices = get_lists_from_analysts(input$select_analyst),
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
      warning_message <- ""
      for (property in input$select_properties) {
        if (entry_exists(input$select_list_for_adding_prop, property)) {
          warning_message <- paste0(warning_message, "Entry exists for property id ", property, " in the list ", input$select_list_for_adding_prop, "<br/>")
        } else {
          add_entry(input$select_list_for_adding_prop, input$select_properties)
          warning_message <- paste0(warning_message, "Added property ", property, " to ", input$select_list_for_adding_prop, "<br/>")
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
        } else {
          warning_message <- paste0(warning_message, "Property ", property, " not found in list ", input$select_list_for_adding_prop, "<br/>")
        }
      }
      output$adding_property_warning <- renderText({warning_message})
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
