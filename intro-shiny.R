library(tidyverse)
library(magrittr)
library(dplyr)
library(janitor)
library(shiny)
library(DT)
library(shinyjqui)
require(data.table)
library(shinyWidgets)
library(shinydashboard)
library(plotly)

waterfall <- as.data.frame(fread("src/take_home_waterfall.tsv"))
waterfall[is.na(waterfall)] <- "Not Available"
#3
filter = list(waterfall$age <= 65,
              waterfall$encounter_date >= (Sys.Date() -450), 
              waterfall$stage != "Stage IV"
)
#4
filter_input <- c("Patients Younger than 65 years old",
                  "Patients with recent encounter dates within 15 months", 
                  "Patients not in Stage IV")

options(shiny.sanitize.errors = F)

############################################################################################
ui <- dashboardPage(
  dashboardHeader(title = "Huijun Presentation",
                  titleWidth = 330),
  dashboardSidebar(width = 330,
                   sidebarMenu(
                     menuItem("Self-introduction", tabName = "intro", icon = icon("user"),
                              menuSubItem("About me", tabName = "me", icon = icon("angle-right")),
                              menuSubItem("Experience", tabName = "exp", icon = icon("angle-right")),
                              menuSubItem("Selected projects", tabName = "sp", icon = icon("angle-right")),
                              menuSubItem("Skills", tabName = "skill", icon = icon("angle-right")),
                              menuSubItem("Hobbies", tabName = "hobby", icon = icon("angle-right"))),
                     
                     menuItem("Interesting Project", tabName = "ip", icon = icon("lightbulb"),
                              menuSubItem("Project background", tabName = "pb", icon = icon("angle-right")),
                              menuSubItem("Filter tool project", tabName = "p", icon = icon("angle-right")),
                              menuSubItem("Significance & limitation", tabName = "sig", icon = icon("angle-right"))
                     ),
                     menuItem("Contact", tabName = "contact", icon = icon("address-book"))
                   )),
  dashboardBody(tags$head(tags$style(HTML('
                                          /* logo */
                                          .skin-blue .main-header .logo {
                                          background-color: #045494;
                                          }
                                          
                                          /* logo when hovered */
                                          .skin-blue .main-header .logo:hover {
                                          background-color: #045494;
                                          }
                                          
                                          /* navbar (rest of the header) */
                                          .skin-blue .main-header .navbar {
                                          background-color: #045494;
                                          }  
                                          .main-header .logo {
                                          font-family: "Georgia", Times, "Times New Roman", serif;
                                          font-weight: bold;
                                          font-size: 28px;
                                          }
                                          .main-sidebar { font-size: 22px; }
                                          .sidebar-menu li { margin-bottom: 30px;} 
                                          .treeview-menu>li>a { font-size: 18px!important; }
                                          .modal.in .modal-dialog{
                                          width:100%;
                                          height:100%;
                                          margin:0px;
                                          }
                                          
                                          .modal-content{
                                          width:100%;
                                          height:100%;
                                          background-color: #045494;
                                          color: #F4CCCC;
                                          background-image: url("back.jpg")
                                          }
                                          '))),
                tabItems(# First tab content
                  
                  tabItem(tabName = "me",
                          fluidRow(
                            box(
                              title = h1("About me"), width = NULL, status = "primary", solidHeader = TRUE,
                              tags$head(
                                tags$style(HTML("ul {list-style-type: square;}"))
                              ),
                              tags$div(tags$ul(
                                tags$li(tags$span(h3("Huijun(means wise noble man in Chinese) An"))),
                                tags$li(tags$span(h3("Expected MS in Biostatistics at UCLA (current GPA 3.87), BS in Biochemistry at UCLA"))),
                                tags$li(tags$span(h3("Flexible, thoughtful, and detail-oriented individual who look forward to make real-world contribution in healthcare related data analysis")))))
                            )
                          )),
                  
                  # Second tab content
                  tabItem(tabName = "exp",
                          fluidRow(
                            box(title = h1("Experience"),width = NULL, status = "primary", solidHeader = TRUE,
                                fluidRow(
                                  valueBox(500, strong(h4("Million records dealt with")), icon = icon("th")),
                                  valueBox(4, strong(h4("Years of related experience")), icon = icon("calendar"))
                                ),
                                tags$img(src='exp.png', style="width: 900px")
                            ))
                  ),
                  # Third tab content
                  tabItem(tabName = "sp",
                          fluidRow(
                            box(title = h1("Selected projects"),width = NULL, status = "primary", solidHeader = TRUE,
                                tags$img(src='projects.png', style="width: 900px")
                            ))
                  ),
                  
                  # Fourth tab content
                  tabItem(tabName = "skill",
                          fluidRow(
                            box(title = h1("Skills"),width = NULL, status = "primary", solidHeader = TRUE,
                                tags$img(src='skill.png', style="width: 900px")
                            ))
                  ),
                  # fifth tab content
                  tabItem(tabName = "hobby",
                          fluidRow(
                            box(title = h1("Me outside of work..."),width = NULL, status = "primary", solidHeader = TRUE,
                                tags$head(
                                  tags$style(HTML("ul {list-style-type: square;}"))
                                ),
                                tags$div(tags$ul(
                                  tags$li(tags$span(h3("Keyboard player in a metal band"))))
                                ),
                                tags$img(src='band.jpg'),
                                tags$div(tags$ul(
                                  tags$li(tags$span(h3("Proud owner of beautiful one-year-old cat Qiuqiu"))))
                                ),
                                tags$img(src='qiu.jpg')
                            ))
                  ),
                  # project tab
                  tabItem(tabName = "pb",
                          fluidRow(
                            box(
                              title = h1("Project background"), width = NULL, status = "primary", solidHeader = TRUE,
                              tags$head(
                                tags$style(HTML("ul {list-style-type: square;}"))
                              ),
                              tags$div(tags$ul(
                                tags$li(tags$span(h3("One of my accomplishments at Syapse"))),
                                tags$li(tags$span(h3("Analysts at Syapse were getting follow-up requests from clients about patient counts using different inclusion-exclusion criteria"))),
                                tags$li(tags$span(h3("Though the code for using different criteria is easy, it takes time and efforts for analysts to look over past projects constantly"))),
                                tags$li(tags$span(h3("The idea of creating a flexible deliverable to clients and give them the patient counts of all possible combinations of inclusion-exclusion criteria emerged")))
                              ))
                            )
                          )),
                  tabItem(tabName = "p",
                          fluidRow(
                            box(width = 45, collapsible = T,
                                title = strong("Filter Tool with Sample Data",
                                               style = "color: #00688b ;
                                               font-size: 30px"),
                                column(4, orderInput(
                                  inputId = 'B',
                                  label = 'Selected Filters (please drag the filters in needed order)',
                                  items = as.list(filter_input),
                                  connect = 'A',
                                  placeholder = "None Selected",
                                  width = "150px"
                                ),
                                br()),
                                
                                
                                column(4, orderInput(
                                  inputId = 'A',
                                  label = 'Unused Filters',
                                  items = NULL,
                                  connect = 'B',
                                  placeholder = "Please drag unwanted filters here",
                                  width = "150px"
                                ),
                                br()),
                                column(4, prettyCheckbox(inputId = "checkbox",
                                                         label = "Show Percentage",
                                                         thick = TRUE, shape = "curve", animation = "pulse", status = "info"),
                                       
                                       conditionalPanel(
                                         condition = "input.checkbox == true ",
                                         radioButtons("perc", "Types of percentages", 
                                                      choices = list("Percentage of Total patient count in each row" = "a", 
                                                                     "Percentage of the last patient count in each health system" = "b", 
                                                                     "Percentage of unfiltered patient count in each health system" = "c"),
                                                      selected = "c")),
                                       
                                       downloadButton("downloadData", "Download CSV")))),
                          
                          box(width = 4, 
                              title = strong("Patient Funnel Table",style = "color: #00688b ;
                                             font-size: 24px"), 
                              # verbatimTextOutput('order'),
                              # tableOutput('table2'),
                              tableOutput('table')),
                          
                          box(width = 8, title = strong("Patient Funnel Chart",style = "color: #00688b ;
                                                        font-size: 24px"),
                              plotlyOutput("plot")),
                          verbatimTextOutput("clickevent"),
                          "Note that for best appearance and performance, it is recommended to zoom out or zoom in depending on your browser if the table and graph above is disproportionated."
                          
                          ),
                  tabItem(tabName = "sig",
                          fluidRow(
                            box(title = h1("Project significance and limitation"),width = NULL, status = "primary", solidHeader = TRUE,
                                tags$head(
                                  tags$style(HTML("ul {list-style-type: square;}"))
                                ),
                                tags$div(tags$ul(
                                  
                                  tags$li(tags$span(h3("The filter tool is actually written in the form of a function. 
                                                       An analyst can input the name of dataset, category name, and filters that customers asked for in the function.
                                                       Then the shiny app specifically for the analyst's request will pop out."))),
                                  tags$li(tags$span(h3("This tool only works on cleaned-up datasets, most importantly, no missing data"))),
                                  tags$li(tags$span(h3("This function significantly reduced number of follow up requests for patient funnels and fulfilled potential customer needs. Still in use at Syapse."))),
                                  tags$li(tags$span(h3("From a learning perspective, this project trained me with the idea of coding with consideration of versatility of future use"))))
                                  )
                                  ))
                          ),
                  tabItem(tabName = "contact",
                          fluidRow(
                            box(title = h1("Contact Information"),width = NULL, status = "primary", solidHeader = TRUE,
                                tags$head(
                                  tags$style(HTML("ul {list-style-type: square;}"))
                                ),
                                tags$div(tags$ul(
                                  tags$li(tags$span(h3("Tel: +1(626)200-6331"))),
                                  tags$li(tags$span(h3("Email: huijun.an@gmail.com"))),
                                  tags$li(tags$span(h3(tags$a(href="https://www.linkedin.com/in/huijun-an-97827b77/", "Linkedin")))),
                                  tags$li(tags$span(h3(tags$a(href="https://github.com/huijunan", "Github")))))
                                )
                            ))
                  )
                          )
                          )
                  )

server <- function(input, output) {
  ################# Landing page #################
  d <- 1
  observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = d, { 
    # event will be called when histdata changes, which only happens once, when it is initially calculated
    showModal(modalDialog(
      title = "", 
      h1("Welcome to Huijun An's Personal Introduction"),
      setBackgroundImage(
        src = "Rplot.png"
      )
    ))
  })
  ################# Project table #################
  ## function to get a subsetted dataset
  filter_data <- function(df, filter) {
    
    evaluate = function(predicate) {
      return(predicate)
    }
    
    index = apply(mapply(evaluate, predicate = filter), 1L, all)
    return(df[index,])
    
  }
  ## list for the order selected by the user to display
  list <- reactive({
    list <- NULL
    for (i in 1:length(input$B_order)) {
      list$text[i] <-  input$B_order[[i]]
      list$order[i] <- which(filter_input == input$B_order[[i]])
    }
    list
  })
  ## Patient count without any filter
  raw_all <- 
    waterfall %>%
    group_by(hospital) %>%
    summarise(Without_Filter = n_distinct(patient_id))
  
  ## final table
  
  com <- NULL
  data <- reactive({
    
    #### If need percentage ##############################################################################################################
    if (input$checkbox == TRUE) {
      #### If choose 'a' -----------------------------------------------------------------------------------------------------------------
      if (input$perc == 'a') {
        if (length(input$B_order) == 0) {
          raw_all <- raw_all %>%
            mutate(Patient_Percentage = scales::percent(Without_Filter/(sum(Without_Filter)))) %>%
            adorn_totals() %>%
            mutate(Without_Filter =  prettyNum(Without_Filter, big.mark=",",scientific=FALSE)) %>%
            unite("Without_Filter", c("Without_Filter", "Patient_Percentage"), sep = " (") %>%
            mutate(Without_Filter = paste(Without_Filter, ")", sep = ""))
          
          
          raw_all_1 <- as.data.frame(t(raw_all))
          colnames(raw_all_1) <-
            paste(as.character(unlist(raw_all_1[1,])), ": Patient Count / %")
          raw_all_1[-1, ]
          
        } else {
          
          for (i in 1:length(list()$order)) {
            com[[i]] <- filter_data(waterfall, filter[list()$order[1:i]])  %>%
              group_by(hospital) %>%
              summarise(Patient_Count = n_distinct(patient_id)) %>%
              mutate(Patient_Percentage = scales::percent(Patient_Count /
                                                            (sum(Patient_Count)))) %>%
              adorn_totals() %>%
              mutate(Patient_Count =  prettyNum(
                Patient_Count,
                big.mark = ",",
                scientific = FALSE
              )) %>%
              unite("Patient_Count", c("Patient_Count", "Patient_Percentage"), sep = " (") %>%
              mutate(Patient_Count = paste(Patient_Count, ")", sep = ""))
          }
          combined_1 <-
            com %>% reduce(left_join, by = "hospital")
          
          for (j in 1:ncol(combined_1)) {
            colnames(combined_1)[j] <-
              paste("After filter : ", list()$text[j - 1])
            colnames(combined_1)[1] <- "hospital"
          }
          
          combined_2 <-
            merge(raw_all <- raw_all %>%
                    mutate(Patient_Percentage = scales::percent(Without_Filter/(sum(Without_Filter)))) %>%
                    adorn_totals() %>%
                    mutate(Without_Filter =  prettyNum(Without_Filter, big.mark=",",scientific=FALSE)) %>%
                    unite("Without_Filter", c("Without_Filter", "Patient_Percentage"), sep = " (") %>%
                    mutate(Without_Filter = paste(Without_Filter, ")", sep = "")),
                  
                  combined_1,
                  all = T,
                  by = "hospital")
          combined_2[is.na(combined_2)] <- 0
          combined <- as.data.frame(t(combined_2))
          colnames(combined) <-
            paste(as.character(unlist(combined[1, ])), ": Patient Count")
          combined[-1,]
          
          
        }
      }
      #### If choose 'b' -----------------------------------------------------------------------------------------------------------------
      else if (input$perc == 'b') {
        if (length(input$B_order) == 0) {
          raw_all_1 <- as.data.frame(t(raw_all %>%
                                         adorn_totals()
                                       %>%
                                         mutate(
                                           Without_Filter =  prettyNum(
                                             Without_Filter,
                                             big.mark = ",",
                                             scientific = FALSE
                                           ))
          ))
          colnames(raw_all_1) <-
            paste(as.character(unlist(raw_all_1[1, ])), ": Patient Count / %")
          raw_all_1[-1,]
          
        } else {
          for (i in 1:length(list()$order)) {
            com[[i]] <- filter_data(waterfall, filter[list()$order[1:i]])  %>%
              group_by(hospital) %>%
              summarise(Patient_Count = n_distinct(patient_id)) %>%
              adorn_totals()
          }
          
          combined_1 <-
            com %>% reduce(left_join, by = "hospital")
          
          for (j in 1:ncol(combined_1)) {
            colnames(combined_1)[j] <-
              paste("After filter : ", list()$text[j - 1])
            colnames(combined_1)[1] <- "hospital"
          }
          
          combined_2 <-
            merge(raw_all %>% adorn_totals(), 
                  combined_1,
                  all = T,
                  by = "hospital")
          combined_2[is.na(combined_2)] <- 0
          
          perc <- NULL
          for (k in 2:ncol(combined_2)) {
            perc[[k]] <-
              as.numeric(combined_2[[k]]) / as.numeric(combined_2[[k - 1]])
          }
          
          perc <- as.data.frame(perc[3:length(perc)])
          
          perc[is.na(perc)] <- 0
          perc <- round(perc * 100, 1)
          for (m in 1:ncol(perc)) {
            perc[[m]] <- paste(perc[[m]], "%", sep = "")
          }
          
          
          for (p in 1:ncol(combined_2[2:ncol(combined_2)])) {
            combined_2[2:ncol(combined_2)][[p]] <-
              prettyNum(combined_2[2:ncol(combined_2)][[p]],
                        big.mark = ",",
                        scientific = FALSE)
            
          }
          
          
          for (h in 1:ncol(combined_2[3:ncol(combined_2)])) {
            combined_2[3:ncol(combined_2)][[h]] <-
              paste(combined_2[3:ncol(combined_2)][[h]], " (", perc[[h]], ")", sep =
                      "")
            
          }
          
          combined <- as.data.frame(t(combined_2))
          colnames(combined) <-
            paste(as.character(unlist(combined[1,])), ": Patient Count / %")
          combined[-1, ]
          
        }
      }
      #### If choose 'c' -----------------------------------------------------------------------------------------------------------------
      else if (input$perc == 'c') {
        
        if (length(input$B_order) == 0) {
          raw_all_1 <- as.data.frame(t(raw_all %>%
                                         adorn_totals() 
                                       %>%
                                         mutate(
                                           Without_Filter =  prettyNum(
                                             Without_Filter,
                                             big.mark = ",",
                                             scientific = FALSE
                                           ))
          ))
          colnames(raw_all_1) <-
            paste(as.character(unlist(raw_all_1[1, ])), ": Patient Count / %")
          raw_all_1[-1,]
          
        } else {
          for (i in 1:length(list()$order)) {
            com[[i]] <- filter_data(waterfall, filter[list()$order[1:i]])  %>%
              group_by(hospital) %>%
              summarise(Patient_Count = n_distinct(patient_id)) %>%
              adorn_totals()
          }
          
          combined_1 <-
            com %>% reduce(left_join, by = "hospital")
          
          for (j in 1:ncol(combined_1)) {
            colnames(combined_1)[j] <-
              paste("After filter : ", list()$text[j - 1])
            colnames(combined_1)[1] <- "hospital"
          }
          
          combined_2 <-
            merge(raw_all %>% adorn_totals(), 
                  combined_1,
                  all = T,
                  by = "hospital")
          combined_2[is.na(combined_2)] <- 0
          
          perc <- NULL
          for (k in 2:ncol(combined_2)) {
            perc[[k]] <-
              as.numeric(combined_2[[k]]) / as.numeric(combined_2[[2]])
          }
          
          perc <- as.data.frame(perc[3:length(perc)])
          
          perc[is.na(perc)] <- 0
          perc <- round(perc * 100, 1)
          for (m in 1:ncol(perc)) {
            perc[[m]] <- paste(perc[[m]], "%", sep = "")
          }
          
          
          for (p in 1:ncol(combined_2[2:ncol(combined_2)])) {
            combined_2[2:ncol(combined_2)][[p]] <-
              prettyNum(combined_2[2:ncol(combined_2)][[p]],
                        big.mark = ",",
                        scientific = FALSE)
            
          }
          
          
          for (h in 1:ncol(combined_2[3:ncol(combined_2)])) {
            combined_2[3:ncol(combined_2)][[h]] <-
              paste(combined_2[3:ncol(combined_2)][[h]], " (", perc[[h]], ")", sep =
                      "")
            
          }
          
          combined <- as.data.frame(t(combined_2))
          colnames(combined) <-
            paste(as.character(unlist(combined[1,])), ": Patient Count / %")
          combined[-1, ]
        }
        
      }
    } 
    #### If no need percentage ###########################################################################################################
    else{
      if (length(input$B_order) == 0) {
        raw_all_1 <- as.data.frame(t(raw_all %>% adorn_totals() 
                                     %>%
                                       mutate(Without_Filter =  prettyNum(Without_Filter, big.mark = ",", scientific = FALSE))
        ))
        colnames(raw_all_1) <-
          paste(as.character(unlist(raw_all_1[1, ])), ": Patient Count")
        raw_all_1[-1,]
        
      } else { 
        for (i in 1:length(list()$order)) {
          com[[i]] <- filter_data(waterfall, filter[list()$order[1:i]])  %>%
            group_by(hospital) %>%
            summarise(Patient_Count = n_distinct(patient_id)) %>%
            adorn_totals() %>%
            mutate(Patient_Count =  prettyNum(Patient_Count, big.mark = ",", scientific = FALSE))
        }
        
        combined_1 <-
          com %>% reduce(left_join, by = "hospital")
        
        for (j in 1:ncol(combined_1)) {
          colnames(combined_1)[j] <-
            paste("After filter : ", list()$text[j - 1])
          colnames(combined_1)[1] <- "hospital"
        }
        
        combined_2 <-
          merge(raw_all %>% adorn_totals() %>%
                  mutate(Without_Filter =  prettyNum(Without_Filter, big.mark = ",", scientific = FALSE))
                ,
                combined_1,
                all = T,
                by = "hospital")
        combined_2[is.na(combined_2)] <- 0
        combined <- as.data.frame(t(combined_2))
        colnames(combined) <-
          paste(as.character(unlist(combined[1, ])), ": Patient Count")
        combined[-1,]
        
      }
    }
    
  })
  
  data2 <- reactive({
    if (length(input$B_order) == 0) {
      
      raw_all_1 <- as.data.frame(raw_all %>% adorn_totals())
      rownames(raw_all_1) <- raw_all_1[,1]
      raw_all_1[,1] <- NULL
      as.data.frame(t(raw_all_1))
      
    } else {
    for (i in 1:length(list()$order)) {
      com[[i]] <- filter_data(waterfall, filter[list()$order[1:i]])  %>%
        group_by(hospital) %>%
        summarise(Patient_Count = n_distinct(patient_id)) %>%
        adorn_totals() 
    }
    
    combined_1 <-
      com %>% reduce(left_join, by = "hospital")
    
    for (j in 1:ncol(combined_1)) {
      colnames(combined_1)[j] <-
        paste("After filter : ", list()$text[j - 1])
      colnames(combined_1)[1] <- "hospital"
    }
    
    combined_2 <-
      merge(raw_all %>% adorn_totals() 
            ,
            combined_1,
            all = T,
            by = "hospital")
    combined_2[is.na(combined_2)] <- 0
    
    rownames(combined_2) <- combined_2[,1]
    combined_2[,1] <- NULL
    
    combined <- as.data.frame(t(combined_2))
    combined$names <- rownames(combined)
    combined
    
    }
    
  })
  
  
  output$table <- renderTable(
    
    data(), rownames = TRUE, width = 320
    
  )
  
  # output$order <- renderPrint({
  #   print(colnames(data2()))
  # })
  
  
  #### download
  output$downloadData <- downloadHandler(
    filename = "filtered_data.csv",
    content = function(file) {
      write.csv(data(), file, row.names = TRUE)
    }
  )
  
  
  #### plotly funnel chart
  
  output$plot <- renderPlotly({
    fig <- plot_ly(
      data = data2(),
      type = "funnel",
      name = 'All Saint Hospital',
      y = factor(rownames(data2()), levels = rownames(data2())),
      x = data2()[,1] )
    fig <- fig %>%
      add_trace(
        type = "funnel",
        name = 'Chicago Hope',
        orientation = "h",
        y = factor(rownames(data2()), levels = rownames(data2())),
        x = data2()[,2],
        textposition = "inside")
    fig <- fig %>%
      add_trace(
        type = "funnel",
        name = 'Lennox Hill',
        orientation = "h",
        y = factor(rownames(data2()), levels = rownames(data2())),
        x = data2()[,3],
        textposition = "inside")
    fig <- fig %>%
      add_trace(
        type = "funnel",
        name = 'St. Mungos',
        orientation = "h",
        y = factor(rownames(data2()), levels = rownames(data2())),
        x = data2()[,4],
        textposition = "inside")
    fig
    
  })
  
  # output$clickevent <- renderPrint({
  #   event_data("plotly_click")
  # })
  
  
  
  
}

shinyApp(ui, server)