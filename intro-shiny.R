library(shiny)
library(shinydashboard)
library(shinyjqui)

ui <- dashboardPage(
  dashboardHeader(title = "Huijun An Presentation",
                  titleWidth = 390),
  dashboardSidebar(width = 390,
                   sidebarMenu(
                     menuItem("Self-introduction", tabName = "intro", icon = icon("user"),
                              menuSubItem("About me", tabName = "me", icon = icon("angle-right")),
                              menuSubItem("Experience", tabName = "exp", icon = icon("angle-right")),
                              menuSubItem("Skills", tabName = "skill", icon = icon("angle-right")),
                              menuSubItem("Hobbies", tabName = "hobby", icon = icon("angle-right"))),
                     
                     menuItem("Interesting Project", tabName = "ip", icon = icon("lightbulb")),
                     menuItem("Contact", tabName = "contact", icon = icon("address-book")),
                     menuItem("About", tabName = "about", icon = icon("question"))
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
                                          .sidebar-menu li { margin-bottom: 10px;} 
                                          .treeview-menu>li>a { font-size: 18px!important; }
                                          '))))
)

server <- function(input, output) { }

shinyApp(ui, server)