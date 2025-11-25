
# This is a standalone sensor app to manage all HOBO sensor operations, inventory and testing independently 
# Developer: RFE
# Started: 7/7/2025

#0.0: load libraries --------------
#shiny
library(shiny)
#pool for database connections
library(pool)
#odbc for database connections
library(odbc)
#DBI
library(DBI)
#tidyverse for data manipulations
library(tidyverse)
#shinythemes for colors
library(shinythemes)
#lubridate to work with dates
library(lubridate)
#shinyjs() to use easy java script functions
library(shinyjs)
#dropdownButton() for summary table in sensors tab
library(shinyWidgets)
#themes
library(reactablefmtr)
#accordions to improve sidebars
library(bslib)
#DT for datatables
library(DT)
#reactable for reactable tables
library(reactable)
#package version control
library(renv)

#0.1: database connection and global options --------

#set default page length for datatables
options(DT.options = list(pageLength = 15))

#set db connection
#using a pool connection so separate connnections are unified
#gets environmental variables saved in local or pwdrstudio environment
#poolConn <- dbPool(odbc::odbc(), dsn = "mars14_datav2", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))

# DB connections & functions
poolConn <- dbPool(RPostgres::Postgres(),
                      host = "PWDMARSDBS1.pwd.phila.local",
                      port = 5434,
                      dbname = "mars_prod",
                      user = Sys.getenv("shiny_uid"),
                      password = Sys.getenv("shiny_pwd")
)


#disconnect from db on stop 
onStop(function(){
  poolClose(poolConn)
})

#js warning about leaving page
jscode <- 'window.onbeforeunload = function() { return "Please use the button on the webpage"; };'

#this function adds a little red star to indicate that a field is required. It uses HTML, hence "html_req"
html_req <- function(label){
  HTML(paste(label, tags$span(style="color:red", tags$sup("*"))))
}

#this function adds a blue dagger to indicate that a field is required for future tests. It uses HTML. it is slightly Christian
future_req <- function(label){
  HTML(paste(label, tags$span(style="color:blue", tags$sup("†"))))
}


#js color code
jsColCode <- 'shinyjs.backgroundCol = function(params) {
                  var defaultParams = {
                  id : null,
                  col : "#88A88A"
                  };
                  params = shinyjs.getParams(params, defaultParams);
                  var el = $("#" + params.id);
                  el.css("background-color", params.col);
                  }'

# #replace special characters with friendlier characters
special_char_replace <- function(note){
  
  note_fix <- note %>%
    str_replace_all(c("•" = "-", "ï‚§" = "-", "“" = '"', '”' = '"'))
  
  return(note_fix)
  
}

#Active deployments
active_deployment <- dbGetQuery(poolConn, "select sensor_serial, cast(date_80percent as DATE) as date_80percent_date, cast(date_100percent as DATE) as date_100percent_date from fieldwork.viw_active_deployments")

#Sensor Model Number options
sensor_model_lookup <- dbGetQuery(poolConn, "select * from fieldwork.tbl_sensor_model_lookup order by sensor_model_lookup_uid")

sensor_status_lookup <- dbGetQuery(poolConn, "select * from fieldwork.tbl_sensor_status_lookup order by sensor_status_lookup_uid")

sensor_issue_lookup <- dbGetQuery(poolConn, "select * from fieldwork.tbl_sensor_issue_lookup order by sensor_issue_lookup_uid")

test_status_lookup <- dbGetQuery(poolConn, "select * from fieldwork.tbl_sensortest_status_lookup")

#Sensor Serial Number List
hobo_list_query <-  "select inv.inventory_sensors_uid, inv.sensor_serial, inv.sensor_model, inv.date_purchased, 
      ow.smp_id, ow.ow_suffix from fieldwork.viw_inventory_sensors_full inv
                          left join fieldwork.tbl_deployment d on d.inventory_sensors_uid = inv.inventory_sensors_uid AND d.collection_dtime is NULL
                            left join fieldwork.tbl_ow ow on ow.ow_uid = d.ow_uid"
hobo_list <- odbc::dbGetQuery(poolConn, hobo_list_query)
sensor_serial <- hobo_list$sensor_serial


ui <- tagList(useShinyjs(), navbarPage("Sensor Hub",
  id = "TabPanelID", theme = shinytheme("cyborg"),
  tabPanel(
    title = "Add/Edit Sensor", value = "add_sensor",
    titlePanel("Add Sensor to Inventory or Edit Existing Sensor"),
    # 1.1 sidebarPanel-----
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          title = "Add/Edit Sensor",
          h4("Add/Edit Sensor"),
          numericInput("serial_no", html_req("Sensor Serial Number"), value = NA),
          selectInput("model_no", html_req("Sensor Model Number"),
            choices = c("", sensor_model_lookup$sensor_model),
            selected = NULL
          ),
          dateInput("date_purchased", "Purchase Date", value = as.Date(NA)),
          selectInput("sensor_status", html_req("Sensor Status"), choices = sensor_status_lookup$sensor_status, selected = "Good Order"),
          conditionalPanel(
            width = 12,
            condition = 'input.sensor_status != "Good Order" & input.sensor_status != "In Testing"',
            selectInput("issue_one", html_req("Issue #1"),
              choices = c("", sensor_issue_lookup$sensor_issue), selected = NULL
            ),
            selectInput("issue_two", "Issue #2",
              choices = c("", sensor_issue_lookup$sensor_issue), selected = NULL
            ),
            checkboxInput("request_data", "Request Data Be Retrieved and Sent to PWD")
          ),
          actionButton("add_sensor", "Add Sensor"),
          # actionButton("add_sensor_deploy", "Deploy this Sensor"),
          actionButton("clear", "Clear Fields")
        ),
        tabPanel(
          title = "Download Options",
          h4("Download Options"),
          selectInput("sensor_status_dl", "Sensor Statuses to Download", choices = c("All", sensor_status_lookup$sensor_status)),
          downloadButton("download", "Download Sensor Inventory")
        ),
        tabPanel(
          title = "Summary Table",
          h4("Summary Table Options"),
          dropdownButton(
            circle = FALSE, label = "Summary Variables",
            tags$style(HTML("#{background-color: #272B30; color: #FFFFFF}")),
            checkboxGroupInput(
              inputId = "sensor_summary_list", label = "Selection",
              choices = c("Model", "Sensor Type", "Sensor Status", "Deployed"),
              tags$style(HTML("background-color: #272B30; color: #FFFFFF"))
            )
          ),
        )
      )
    ),
    mainPanel(tabsetPanel(
      tabPanel(
        "Sensor Status Table",
        DTOutput("sensor_table")
      ),
      tabPanel(
        "Sensor Summary Table",
        DTOutput("sensor_summary_table")
      ),
      tabPanel(
        "Sensor History Table",
        DTOutput("sensor_history_table")
      ),
      tabPanel(
        "Sensor Status History Table",
        DTOutput("sensor_status_table")
      )
    ))
  ),
  tabPanel("Add/Edit Sensor Test",
    value = "test", ## First tab -----
    sidebarLayout(
      sidebarPanel(
        selectInput("sensor_sn", html_req("Sensor Serial Number"), choices = c("", sort(sensor_serial)), selected = ""),
        dateInput("date", html_req("Test Date"), value = as.Date(NA)),
        selectInput("test_type", html_req("Test Type"), choices = c("", "Level", "Baro"), selected = ""),
        conditionalPanel(
          condition = "input.test_type == 'Level'",
          fluidRow(column(
            6,
            numericInput("mean_ae_ft", html_req("Mean Absolute Error (ft):"), 0, min = 0, max = 1000),
          ), column(
            6,
            numericInput("max_ae_ft", html_req("Maximum Absolute Error (ft):"), 0, min = 0, max = 1000)
          ))
        ),
        conditionalPanel(
          condition = "input.test_type == 'Baro'",
          fluidRow(column(
            6,
            numericInput("mean_ae_psi", html_req("Mean Absolute Error (PSI):"), 0, min = 0, max = 1000),
          ), column(
            6,
            numericInput("max_ae_psi", html_req("Maximum Absolute Error (PSI):"), 0, min = 0, max = 1000)
          ))
        ),
        selectInput("sensor_test_status", html_req("Test Status"), choices = c("", test_status_lookup$test_status), selected = ""),
        textAreaInput("test_note", "Notes", height = 100),
        actionButton("add_update", "Add New"),
        actionButton("clear_edit", "Clear All Fields")
      ),
      mainPanel(
        h4(textOutput("test_table_header")),
        reactableOutput("sensor_test_table")
        
      )
    )
  ),
  tabPanel("Sensor Testing Calendar",
    value = "calendar", ## First tab -----
    reactableOutput("calendar_display")
  )
))                              


# Server -----
server <- function(input, output, session) {
  #start reactiveValues for this section
  rv <- reactiveValues()
  
  #2.0.1.2 Tab Name ----
  tab_name <- "Add/Edit Sensor"
  
  #2.1 Query sensor table ----
  #2.1.1 initial query -----
  #Sensor Serial Number List
  sensor_table_query <-  "select * from fieldwork.viw_inventory_sensors_full"
  rv$sensor_table <- odbc::dbGetQuery(poolConn, sensor_table_query)
  
  #2.1.1.1 Query for viewing summary table ----
  sensor_query <- "SELECT * FROM fieldwork.viw_inventory_sensors_status"
  rv$sensor_dt <- reactive(odbc::dbGetQuery(poolConn, sensor_query) %>%
                             mutate(Deployed = !is.na(active_ow_deployment)))
  
  
  
  # observeEvent(deploy$refresh_sensor(),{
  #   rv$sensor_dt <- odbc::dbGetQuery(poolConn, sensor_query)
  # })
  
  rv$summary_cols <- reactive(input$sensor_summary_list %>%
                                str_replace("Model","sensor_model") %>%
                                str_replace("Sensor Type","model_type") %>%
                                str_replace("Sensor Status","sensor_status"))
  
  rv$sensor_summary_display <- reactive(rv$sensor_dt() %>%
                                          mutate(Deployed = !is.na(active_ow_deployment)) %>%
                                          select("sensor_model", "sensor_status","model_type","Deployed") %>%
                                          rename("Model" = "sensor_model", "Sensor Status" = "sensor_status", "Sensor Type" = "model_type") %>%
                                          group_by(across(input$sensor_summary_list)) %>%
                                          summarise(Count = n()))
  
  # colnames(rv$sensor_summary_display())[colnames(rv$sensor_summary_display()) %in% input$sensor_summary_list]
  
  #2.1.1.2 Query for viewing history table ----
  history_query <- paste0("SELECT coalesce(smp_id, site_name) as smp_site_name, * FROM fieldwork.viw_deployment_full")
  
  rv$history_dt <- reactive(odbc::dbGetQuery(poolConn, history_query) %>%
                              select(sensor_serial, smp_site_name, ow_suffix, type, term,
                                     deployment_dtime, collection_dtime,
                                     project_name, notes) %>%
                              dplyr::mutate("Deployment Time" = lubridate::as_date(deployment_dtime),
                                            "Collection Time" = lubridate::as_date(collection_dtime)) %>%
                              dplyr::filter(sensor_serial == input$serial_no))
  
  rv$sensor_history_display <- reactive(rv$history_dt() %>%
                                          dplyr::select(-project_name,-deployment_dtime,-collection_dtime) %>%
                                          rename("Serial Number" = "sensor_serial",
                                                 "SMP ID/Site Name" = "smp_site_name",
                                                 "Location" = "ow_suffix",
                                                 "Type" = "type",
                                                 "Term" = "term",
                                                 "Notes" = "notes"))
  
  
  
  
  #2.1.2 query on update ----
  #upon breaking a sensor in deploy
  # observeEvent(deploy$refresh_sensor(),{
  #   rv$sensor_table <- odbc::dbGetQuery(poolConn, sensor_table_query)
  # })
  # 
  #2.1.3 show sensor table ----
  rv$sensor_table_display <- reactive(rv$sensor_table %>% 
                                        mutate("date_purchased" = as.character(date_purchased)) %>% 
                                        select("sensor_serial", "sensor_model", "date_purchased", "smp_id", 
                                               "site_name", "ow_suffix", "sensor_status", "issue_one", "issue_two", "request_data") %>% 
                                        mutate_at(vars(one_of("request_data")), 
                                                  funs(case_when(. == 1 ~ "Yes"))) %>% 
                                        rename("Serial Number" = "sensor_serial", "Model Number" = "sensor_model", 
                                               "Date Purchased" = "date_purchased", "SMP ID" = "smp_id", "Site" = "site_name", 
                                               "Location" = "ow_suffix", "Status" = "sensor_status", 
                                               "Issue #1" = "issue_one", "Issue #2" = "issue_two", "Request Data" = "request_data")
  )
  
  
  # status log 
  rv$status_log <- reactive(dbGetQuery(poolConn, "SELECT * FROM fieldwork.tbl_sensor_status_log") %>%
                              dplyr::left_join(sensor_status_lookup, by = "sensor_status_lookup_uid") %>%
                              dplyr::left_join(sensor_issue_lookup, by = c("sensor_issue_lookup_uid_one" ="sensor_issue_lookup_uid" )) %>%
                              dplyr::left_join(sensor_issue_lookup, by = c("sensor_issue_lookup_uid_two" = "sensor_issue_lookup_uid" )) %>%
                              dplyr::arrange(desc(date)))
  
  
  output$sensor_table <- renderDT(
    rv$sensor_table_display(),
    selection = "single",
    style = 'bootstrap', 
    class = 'table-responsive, table-hover',
    options = list(scroller = TRUE, 
                   scrollX = TRUE, 
                   scrollY = 550), 
    callback = JS('table.page("next").draw(false);')
  )
  
  output$sensor_summary_table <- renderDT(
    rv$sensor_summary_display(),
    style = 'bootstrap',
    selection = "none",
    options = list(pageLength = 15, lengthChange = FALSE)
  )
  
  output$sensor_history_table <- renderDT(
    rv$sensor_history_display(),
    style = 'bootstrap',
    selection = "none",
    options = list(pageLength = 15, lengthChange = FALSE)
  )
  
  output$sensor_status_table <- renderDT(
    rv$status_log() %>%
      dplyr::filter(sensor_serial == input$serial_no) %>%
      dplyr::select("Serial Number" = sensor_serial, "Status" = sensor_status, Date = date, "Issue #1" = sensor_issue.x, "Issue #2" = sensor_issue.y), 
    style = 'bootstrap',
    selection = "none",
    options = list(pageLength = 15, lengthChange = FALSE)
  )
  
  
  
  
  #2.3 prefilling inputs based on model serial number -----
  
  #select a row in the table to update the serial no. you can also just type in the serial number
  #see above for how updating the serial number updates the rest of the fields
  #this is different than the other tabs because there is no fixed smp_id type of primary key here - so you can either type OR select a row to get to the sensor, and you have to be able to type because you might want to add a new sensor this way. 
  #it works it's just different
  observeEvent(input$sensor_table_rows_selected, {
    updateTextInput(session, "serial_no", value = rv$sensor_table$sensor_serial[input$sensor_table_rows_selected])
  })
  
  #not sure why this is so many of the same ifs. can be consolidated later
  #if input serial number is already in the list, then suggest the existing model number. if it isn't already there, show NULL
  rv$model_no_select <- reactive(if(input$serial_no %in% rv$sensor_table$sensor_serial) dplyr::filter(rv$sensor_table, sensor_serial == input$serial_no) %>% dplyr::select(sensor_model) %>% dplyr::pull() else "")
  
  observe(updateSelectInput(session, "model_no", selected = rv$model_no_select()))
  
  #if input serial number is already in the list, then suggest the date_purchased. if it isn't already there, show NULL
  rv$date_purchased_select <- reactive(if(input$serial_no %in% rv$sensor_table$sensor_serial) dplyr::filter(rv$sensor_table, sensor_serial == input$serial_no) %>% dplyr::select(date_purchased) %>% dplyr::pull() else as.Date(NA))
  
  observe(updateDateInput(session, "date_purchased", value = rv$date_purchased_select()))
  
  observeEvent(input$serial_no, {
    
    #if input serial number is already in the list, then suggest the sensor status if it isn't already there, show "Good Order"
    rv$sensor_status_select <- if(input$serial_no %in% rv$sensor_table$sensor_serial) dplyr::filter(rv$sensor_table, sensor_serial == input$serial_no) %>% dplyr::select(sensor_status) %>% dplyr::pull() else "Good Order"
    
    updateSelectInput(session, "sensor_status", selected = rv$sensor_status_select)
    
    #if input serial number is already in the list, then suggest issue #1
    rv$issue_one_select <- if(input$serial_no %in% rv$sensor_table$sensor_serial) dplyr::filter(rv$sensor_table, sensor_serial == input$serial_no) %>% dplyr::select(issue_one) %>% dplyr::pull() else ""
    
    updateSelectInput(session, "issue_one", selected = rv$issue_one_select)
    
    #if input serial number is already in the list, then suggest issue #2
    rv$issue_two_select <- if(input$serial_no %in% rv$sensor_table$sensor_serial) dplyr::filter(rv$sensor_table, sensor_serial == input$serial_no) %>% dplyr::select(issue_two) %>% dplyr::pull() else ""
    
    updateSelectInput(session, "issue_two", selected = rv$issue_two_select)
    
    #if input serial number is already in the list, then suggest checkbox request data
    rv$request_data_select <- if(input$serial_no %in% rv$sensor_table$sensor_serial) dplyr::filter(rv$sensor_table, sensor_serial == input$serial_no) %>% dplyr::select(request_data) %>% dplyr::pull() else ""
    
    updateCheckboxInput(session, "request_data", value = rv$request_data_select)
    
  })
  
  #2.3 preparing inputs
  #get sensor status uid
  rv$status_lookup_uid <- reactive(sensor_status_lookup %>% dplyr::filter(sensor_status == input$sensor_status) %>% 
                                     select(sensor_status_lookup_uid) %>% pull())
  
  #let date purchased be null
  rv$date_purchased <- reactive(if(length(input$date_purchased) == 0) "NULL" else paste0("'", input$date_purchased, "'"))
  
  #get sensor model lookup UID and let it be NULL 
  rv$model_lookup_uid <- reactive(sensor_model_lookup %>% dplyr::filter(sensor_model == input$model_no) %>% 
                                    select(sensor_model_lookup_uid) %>% pull())
  
  rv$sensor_model_lookup_uid <- reactive(if(nchar(input$model_no) == 0) "NULL" else paste0("'", rv$model_lookup_uid(), "'"))
  
  #get sensor issue lookup uid and let it be NULL (for issue #1 and issue #2)
  rv$issue_lookup_uid_one <- reactive(sensor_issue_lookup %>% dplyr::filter(sensor_issue == input$issue_one) %>% 
                                        select(sensor_issue_lookup_uid) %>% pull())
  
  rv$sensor_issue_lookup_uid_one <- reactive(if(nchar(input$issue_one) == 0) "NULL" else paste0("'", rv$issue_lookup_uid_one(), "'"))
  
  rv$issue_lookup_uid_two <- reactive(sensor_issue_lookup %>% dplyr::filter(sensor_issue == input$issue_two) %>% 
                                        select(sensor_issue_lookup_uid) %>% pull())
  
  rv$sensor_issue_lookup_uid_two <- reactive(if(nchar(input$issue_two) == 0) "NULL" else paste0("'", rv$issue_lookup_uid_two(), "'"))
  
  #let checkbox input be NULL if blank (instead of FALSE)
  rv$request_data <- reactive(if(input$request_data == TRUE) paste0("'TRUE'") else "NULL")
  
  #2.4 toggle states/labels -----
  #enable/disable the "add sensor button" if all fields are not empty
  observe({toggleState(id = "add_sensor", condition = nchar(input$serial_no) > 0 & nchar(input$model_no) > 0)})
  # observe({toggleState(id = "add_sensor_deploy", input$serial_no %in% rv$sensor_table$sensor_serial)})
  
  #enable/disable issue #2 button if issue #1 is filled
  observe(toggleState(id = "issue_two", condition = nchar(input$issue_one) > 0))
  
  #change label from Add to Edit if the sensor already exists in db
  #rv$label <- reactive(if(!(as.numeric(input$serial_no) %in% rv$sensor_table$sensor_serial)) "Add Sensor" else "Edit Sensor")
  rv$label <- reactive(if(!(input$serial_no %in% rv$sensor_table$sensor_serial)) "Add Sensor" else "Edit Sensor")
  observe(updateActionButton(session, "add_sensor", label = rv$label()))
  
  #change measurement label
  
  observeEvent(input$sensor_status, {
    #if Good Order, clear issues fields 
    if(rv$status_lookup_uid() == 1){
      reset("issue_one")
      reset("issue_two")
      reset("request_data")
      # updateSelectInput(session, "issue_one", selected = "")
      # updateSelectInput(session, "issue_two", selected = "")
      # updateCheckboxInput(session, "request_data", value = FALSE)
    }
  })
  
  #2.5 add/edit table ------
  #Write to database when button is clicked
  observeEvent(input$add_sensor, { #write new sensor info to db
    
    if(!(input$serial_no %in% rv$sensor_table$sensor_serial)){
      add_sensor_query <- paste0(
        "INSERT INTO fieldwork.tbl_inventory_sensors (sensor_serial, sensor_model_lookup_uid, date_purchased) 
    	      VALUES (", input$serial_no, ", ",rv$sensor_model_lookup_uid(), ", ",  
        rv$date_purchased(), ")")
      
      odbc::dbGetQuery(poolConn, add_sensor_query)
      
      
      
      #  to keep track of sensor status 
      add_sensor_status_log <- paste0(
        "INSERT INTO fieldwork.tbl_sensor_status_log (sensor_serial, sensor_status_lookup_uid, date, sensor_issue_lookup_uid_one, sensor_issue_lookup_uid_two, request_data)
        VALUES(", input$serial_no, ", ", rv$status_lookup_uid(), ", '", Sys.Date(), "', ", rv$sensor_issue_lookup_uid_one(), ", ", rv$sensor_issue_lookup_uid_two(),", ", rv$request_data(),")"
      )
      
      odbc::dbGetQuery(poolConn, add_sensor_status_log)
      
      
      # log the INSERT query, see utils.R
      # insert.query.log(poolConn,
      #                  add_sensor_query,
      #                  tab_name,
      #                  session)
      
      output$testing <- renderText({
        isolate(paste("Sensor", input$serial_no, "added."))
      })
    }else{ #edit sensor info
      
      update_sensor_query <- paste0("UPDATE fieldwork.tbl_inventory_sensors SET 
                                            sensor_model_lookup_uid = ", rv$sensor_model_lookup_uid(), ",
                                            date_purchased = ", rv$date_purchased(), " 
                                            WHERE sensor_serial = '", input$serial_no, "'")
      
      odbc::dbGetQuery(poolConn, update_sensor_query)
      
      # log the UPDATE query, see utils.R
      # insert.query.log(poolConn,
      #                  update_sensor_query,
      #                  tab_name,
      #                  session)
      
      #  to keep track of sensor status 
      edit_sensor_status_log <- paste0(
        "INSERT INTO fieldwork.tbl_sensor_status_log (sensor_serial, sensor_status_lookup_uid, date, sensor_issue_lookup_uid_one, sensor_issue_lookup_uid_two, request_data)
        VALUES(", input$serial_no, ", ", rv$status_lookup_uid(), ", '", Sys.Date(), "', ", rv$sensor_issue_lookup_uid_one(), ", ", rv$sensor_issue_lookup_uid_two(),", ", rv$request_data() ,")"
      )
      
      odbc::dbGetQuery(poolConn, edit_sensor_status_log)
      
      output$testing <- renderText({
        isolate(paste("Sensor", input$serial_no, "edited."))
      })
    }
    
    #update sensor list following addition
    rv$sensor_table <- odbc::dbGetQuery(poolConn, sensor_table_query)
    rv$active_row <- which(rv$sensor_table$sensor_serial == input$serial_no, arr.ind = TRUE)
    row_order <- order(
      seq_along(rv$sensor_table$sensor_serial) %in% rv$active_row, 
      decreasing = TRUE
    )
    
    rv$sensor_table <- rv$sensor_table[row_order, ]
    dataTableProxy('sensor_table') %>% 
      selectRows(1)
    
    
  })
  
  #switch tabs to "Deploy" and update Sensor ID to the current Sensor ID (if the add/edit button says edit sensor)
  # rv$refresh_serial_no <- 0 
  # observeEvent(input$add_sensor_deploy, {
  #   rv$refresh_serial_no <- rv$refresh_serial_no + 1
  #   updateTabsetPanel(session = parent_session, "inTabset", selected = "deploy_tab")
  # })
  
  #2.6 clear fields ----
  #clear all fields
  #bring up dialogue box to confirm
  observeEvent(input$clear, {
    showModal(modalDialog(title = "Clear All Fields", 
                          "Are you sure you want to clear all fields on this tab?", 
                          modalButton("No"), 
                          actionButton("confirm_clear", "Yes")))
  })
  
  observeEvent(input$confirm_clear, {
    reset("serial_no")
    reset("model_no")
    reset("date_purchased")
    reset("sensor_status")
    removeModal()
  })
  
  #2.7 download ----
  #downloading the sensor table
  #filter based on selected status
  rv$sensor_table_download <- reactive(if(input$sensor_status_dl == "All"){
    rv$sensor_table_display()
  }else{
    rv$sensor_table_display() %>% dplyr::filter(Status == input$sensor_status_dl)
  })
  
  output$download <- downloadHandler(
    filename = function(){
      paste("Sensor_inventory", "_", Sys.Date(), ".csv", sep = "")
    }, 
    content = function(file){
      write.csv(rv$sensor_table_download(), file, row.names = FALSE)
    }
  )
  # 3.0 Add/Edit Sensor Test tab ----
  # toggle submit button
  observe(toggleState(id = "add_update", input$sensor_sn != "" &
    length(input$date) > 0 &
    input$test_type != "" &
    ifelse(input$test_type == "Level", is.numeric(input$mean_ae_ft) & is.numeric(input$max_ae_ft),
      is.numeric(input$mean_ae_psi) & is.numeric(input$max_ae_psi)
    ) &
    input$sensor_test_status != ""))

  # row references
  rv$sensor_test_table_row <- reactive(getReactableState("sensor_test_table", "selected"))

  rv$sensor_tests <- reactive(dbGetQuery(poolConn, "SELECT *, cast(date_purchased as DATE) as date_purchased_asdate FROM fieldwork.tbl_sensor_tests INNER JOIN
                                         fieldwork.tbl_sensor_test_type_lookup USING(test_type_lookup_uid) INNER JOIN
                                         fieldwork.viw_inventory_sensors_full USING(inventory_sensors_uid) LEFT JOIN
                                         fieldwork.tbl_sensortest_status_lookup USING(sensortest_status_lookup_uid)
                                         order by test_date DESC") %>%
    dplyr::filter(sensor_serial == input$sensor_sn))

  # add/edit button toggle
  rv$label_test <- reactive(if (!is.null(rv$sensor_test_table_row())) "Edit Selected" else "Add New")
  observe(updateActionButton(session, "add_update", label = rv$label_test()))

  output$sensor_test_table <- renderReactable(
    reactable(
      rv$sensor_tests() %>%
        select("Serial No" = sensor_serial, "Model" = sensor_model, "Purchase Date" = date_purchased_asdate, "Test Start Date" = test_date, "Test Type" = test_type, "Mean Absolute Error (ft)" = mean_abs_error_ft, "Max Absolute Error (ft)" = max_abs_error_ft, "Mean Absolute Error (PSI)" = mean_abs_error_psi, "Max Absolute Error (PSI)" = max_abs_error_psi, Status = test_status),
      theme = darkly(),
      fullWidth = TRUE,
      selection = "single",
      searchable = TRUE,
      onClick = "select",
      # searchable = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(25, 50, 100),
      defaultPageSize = 25,
      details = function(index) {
        nested_notes <- rv$sensor_tests()[index, ] %>%
          select(Notes = notes)
        htmltools::div(
          style = "padding: 1rem",
          reactable(
            nested_notes,
            theme = darkly(),
            outlined = TRUE
          )
        )
      }
    ),
  )

  # Update sidebar with clicking a row
  # select an closed issue row
  observeEvent(rv$sensor_test_table_row(), {
    updateSelectInput(session, "sensor_sn", selected = rv$sensor_tests()$sensor_serial[rv$sensor_test_table_row()])
    updateSelectInput(session, "date", selected = rv$sensor_tests()$test_date[rv$sensor_test_table_row()])
    updateSelectInput(session, "test_type", selected = rv$sensor_tests()$test_type[rv$sensor_test_table_row()])
    updateSelectInput(session, "mean_ae_ft", selected = rv$sensor_tests()$mean_abs_error_ft[rv$sensor_test_table_row()])
    updateSelectInput(session, "max_ae_ft", selected = rv$sensor_tests()$max_abs_error_ft[rv$sensor_test_table_row()])
    delay(100, updateSelectInput(session, "mean_ae_psi", selected = rv$sensor_tests()$mean_abs_error_psi[rv$sensor_test_table_row()]))
    delay(100, updateSelectInput(session, "max_ae_psi", selected = rv$sensor_tests()$max_abs_error_psi[rv$sensor_test_table_row()]))
    updateTextAreaInput(session, "test_note", value = rv$sensor_tests()$notes[rv$sensor_test_table_row()])
    updateSelectInput(session, "sensor_test_status", selected = rv$sensor_tests()$test_status[rv$sensor_test_table_row()])
  })

  # clear
  observeEvent(input$clear_edit, {
    showModal(modalDialog(
      title = "Clear All Fields",
      "Are you sure you want to clear all fields on this tab?",
      modalButton("No"),
      actionButton("confirm_clear_edit_pcs", "Yes")
    ))
  })

  observeEvent(input$confirm_clear_edit_pcs, {
    reset("sensor_sn")
    reset("date")
    reset("test_type")
    reset("mean_ae_ft")
    reset("max_ae_ft")
    reset("mean_ae_psi")
    reset("max_ae_psi")
    reset("test_note")
    reset("sensor_test_status")


    removeModal()
  })


  # headers
  output$test_table_header <- renderText(
    paste("Add/Edit Sensor Tests for Sensor ", input$sensor_sn)
  )
  
  # On click "submit_btn"
  observeEvent(input$add_update, {
    # process text field to prevent sql injection
    rv$test_note <- reactive(gsub("'", "''", input$test_note))
    rv$test_note_trimmed <- reactive(special_char_replace(rv$test_note()))

    inv_uid <- hobo_list %>%
      dplyr::filter(sensor_serial == input$sensor_sn) %>%
      dplyr::select(inventory_sensors_uid) %>%
      dplyr::pull()
    
    sensortest_status_lookup_uid <- test_status_lookup %>%
      dplyr::filter(test_status == input$sensor_test_status) %>%
      dplyr::select(sensortest_status_lookup_uid) %>%
      dplyr::pull()


    if (is.null(rv$sensor_test_table_row())) {
      new_test_df <- data.frame(
        test_date = input$date,
        test_type_lookup_uid = ifelse(input$test_type == "Level", 1, 2),
        mean_abs_error_ft = ifelse(input$test_type == "Level", input$mean_ae_ft, NA),
        max_abs_error_ft = ifelse(input$test_type == "Level", input$max_ae_ft, NA),
        mean_abs_error_psi = ifelse(input$test_type == "Baro", input$mean_ae_psi, NA),
        max_abs_error_psi = ifelse(input$test_type == "Baro", input$max_ae_psi, NA),
        notes = rv$test_note_trimmed(),
        sensortest_status_lookup_uid = sensortest_status_lookup_uid,
        inventory_sensors_uid = inv_uid
      )

      odbc::dbWriteTable(poolConn, Id(schema = "fieldwork", table = "tbl_sensor_tests"), new_test_df, append = TRUE, row.names = FALSE)
# 
#       sensor_status_lookup_uid <- sensor_status_lookup %>%
#         dplyr::filter(sensor_status == input$sensor_test_status) %>%
#         dplyr::select(sensor_status_lookup_uid) %>%
#         dplyr::pull()
# 
#       edt_sensor_status_q <- paste("Update fieldwork.tbl_inventory_sensors SET sensor_status_lookup_uid = ", sensor_status_lookup_uid, " where inventory_sensors_uid = ", inv_uid, sep = "")
# 
#       odbc::dbGetQuery(poolConn, edt_sensor_status_q)

      # Reload and reset
      rv$sensor_tests <- reactive(dbGetQuery(poolConn, "SELECT *, cast(date_purchased as DATE) as date_purchased_asdate FROM fieldwork.tbl_sensor_tests INNER JOIN
                                         fieldwork.tbl_sensor_test_type_lookup USING(test_type_lookup_uid) INNER JOIN
                                         fieldwork.viw_inventory_sensors_full USING(inventory_sensors_uid) LEFT JOIN
                                         fieldwork.tbl_sensortest_status_lookup USING(sensortest_status_lookup_uid)") %>%
                                    dplyr::filter(sensor_serial == input$sensor_sn))


      # update calendar
      rv$cal_table <- reactive(dbGetQuery(poolConn, "SELECT *, cast(date_purchased as DATE) as date_purchased_asdate FROM fieldwork.tbl_sensor_tests INNER JOIN
                                                                fieldwork.tbl_sensor_test_type_lookup USING(test_type_lookup_uid) RIGHT JOIN
                                                                fieldwork.viw_inventory_sensors_full USING(inventory_sensors_uid)"))
      reset("date")
      reset("test_type")
      reset("mean_ae_ft")
      reset("max_ae_ft")
      reset("mean_ae_psi")
      reset("max_ae_psi")
      reset("test_note")
      reset("sensor_test_status")

      # update other tabs
      rv$sensor_table <- odbc::dbGetQuery(poolConn, sensor_table_query)
    } else {
      inv_uid <- hobo_list %>%
        dplyr::filter(sensor_serial == input$sensor_sn) %>%
        dplyr::select(inventory_sensors_uid) %>%
        dplyr::pull()

      # sensor_status_lookup_uid <- sensor_status_lookup %>%
      #   dplyr::filter(sensor_status == input$sensor_test_status) %>%
      #   dplyr::select(sensor_status_lookup_uid) %>%
      #   dplyr::pull()

      edt_sensor_test_q <- paste0("Update fieldwork.tbl_sensor_tests SET test_date = '",
        input$date,
        "', test_type_lookup_uid = ",
        ifelse(input$test_type == "Level", 1, 2),
        ", mean_abs_error_ft = ",
        ifelse(input$test_type == "Level", input$mean_ae_ft, "NULL"),
        ", max_abs_error_ft = ",
        ifelse(input$test_type == "Level", input$max_ae_ft, "NULL"),
        ", mean_abs_error_psi = ",
        ifelse(input$test_type == "Baro", input$mean_ae_psi, "NULL"),
        ", max_abs_error_psi = ",
        ifelse(input$test_type == "Baro", input$max_ae_psi, "NULL"),
        ", notes = '",
        rv$test_note_trimmed(),
        "', sensortest_status_lookup_uid = ",
        sensortest_status_lookup_uid,
        " where sensor_tests_uid = ",
        rv$sensor_tests()$sensor_tests_uid[rv$sensor_test_table_row()],
        sep = ""
      )

      odbc::dbGetQuery(poolConn, edt_sensor_test_q)

      # # status update
      # edt_sensor_status_qq <- paste("Update fieldwork.tbl_inventory_sensors SET sensor_status_lookup_uid = ", sensor_status_lookup_uid, " where inventory_sensors_uid = ", inv_uid, sep = "")
      # 
      # odbc::dbGetQuery(poolConn, edt_sensor_status_qq)

      # Reload and reset
      rv$sensor_tests <- reactive(dbGetQuery(poolConn, "SELECT *, cast(date_purchased as DATE) as date_purchased_asdate FROM fieldwork.tbl_sensor_tests INNER JOIN
                                         fieldwork.tbl_sensor_test_type_lookup USING(test_type_lookup_uid) INNER JOIN
                                         fieldwork.viw_inventory_sensors_full USING(inventory_sensors_uid) LEFT JOIN
                                         fieldwork.tbl_sensortest_status_lookup USING(sensortest_status_lookup_uid)") %>%
                                    dplyr::filter(sensor_serial == input$sensor_sn))
      
      
      # update calendar
      rv$cal_table <- reactive(dbGetQuery(poolConn, "SELECT *, cast(date_purchased as DATE) as date_purchased_asdate FROM fieldwork.tbl_sensor_tests INNER JOIN
                                                                fieldwork.tbl_sensor_test_type_lookup USING(test_type_lookup_uid) RIGHT JOIN
                                                                fieldwork.viw_inventory_sensors_full USING(inventory_sensors_uid)"))
      reset("date")
      reset("test_type")
      reset("mean_ae_ft")
      reset("max_ae_ft")
      reset("mean_ae_psi")
      reset("max_ae_psi")
      reset("test_note")
      reset("sensor_test_status")

      # update other tab
      rv$sensor_table <- odbc::dbGetQuery(poolConn, sensor_table_query)
    }
  })
  # get row number
  rv$calendar_display_row <- reactive(getReactableState("calendar_display", "selected"))

  # 4.0 Sensor Testing Calendar tab -----
  rv$cal_table <- reactive(dbGetQuery(poolConn, "SELECT *, cast(date_purchased as DATE) as date_purchased_asdate FROM fieldwork.tbl_sensor_tests INNER JOIN
                                                                fieldwork.tbl_sensor_test_type_lookup USING(test_type_lookup_uid) RIGHT JOIN
                                                                fieldwork.viw_inventory_sensors_full USING(inventory_sensors_uid)"))

  rv$cal_table_display <- reactive(rv$cal_table() %>%
    group_by(sensor_serial) %>%
    slice_max(order_by = test_date, with_ties = FALSE) %>%
    ungroup() %>%
    dplyr::filter(is.na(sensor_status) | (sensor_status != "Disposed" & sensor_status != "Out for Repairs")) %>%
    dplyr::mutate(testing_deadline = data.table::fifelse(is.na(test_date), date_purchased_asdate, test_date + lubridate::years(2))) %>% 
    dplyr::left_join(active_deployment, by = "sensor_serial") %>%
    dplyr::arrange(testing_deadline)%>%
    dplyr::arrange(test_date)%>%
    dplyr::select("Serial Number" = sensor_serial, "Model Number" = sensor_model, "Purchase Date" = date_purchased_asdate, "Sensor Status" = sensor_status, "SMP ID" = smp_id, "OW SUffix" = ow_suffix, "Test Type" = test_type, "Latest Test Date" = test_date, "Testing Deadline" = testing_deadline, "Date 100% Full" = date_100percent_date))

  output$calendar_display <- renderReactable(
    reactable(
      rv$cal_table_display(),
      theme = darkly(),
      fullWidth = TRUE,
      selection = "single",
      searchable = TRUE,
      onClick = "select",
      # searchable = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(25, 50, 100),
      defaultPageSize = 25,
      columns = list("Testing Deadline" = colDef(
        style = function(value) {
          if (is.na(value)) {
            # don't color code NAs
          } else if (as.numeric(value - Sys.Date()) < 93 & as.numeric(value - Sys.Date()) > 0) {
            return(list(background = "orange", color = "black", fontweight = "bold"))
          } else if (as.numeric(value - Sys.Date()) < 1) {
            return(list(background = "#A70D2A", color = "white", fontweight = "bold"))
          } else {
            # don't color code other dates
          }
        }
      ))
    ),
  )

  # switch tabs
  observeEvent(rv$calendar_display_row(), {
    updateTabsetPanel(session, "TabPanelID", selected = "test")
    # clean up
    reset("date")
    reset("test_type")
    reset("mean_ae_ft")
    reset("max_ae_ft")
    reset("mean_ae_psi")
    reset("max_ae_psi")
    reset("test_note")
    reset("sensor_test_status")
    # update
    delay(300, updateSelectInput(session, "sensor_sn", selected = rv$cal_table_display()$`Serial Number`[rv$calendar_display_row()]))
  })
}


# Run the application
shinyApp(ui = ui, server = server)

