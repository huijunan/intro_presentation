library(shiny)
library(shinydashboard)
library(shinyjqui)
library(shinyWidgets)


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
                            box(h1("Experience"),width = NULL, status = "primary", solidHeader = TRUE,
                  fluidRow(
                    valueBox(500, strong(h4("Million records dealt with")), icon = icon("th")),
                    valueBox(4, strong(h4("Years of related experience")), icon = icon("calendar"))
                  ),
                  tags$img(src='exp.png')
                  ))
                ),
                # Third tab content
                tabItem(tabName = "skill",
                        fluidRow(
                          box(h1("Skills"),width = NULL, status = "primary", solidHeader = TRUE,
                              tags$img(src='skill.png')
                          ))
                ),
                # fourth tab content
                tabItem(tabName = "hobby",
                        fluidRow(
                          box(h1("Me outside of work..."),width = NULL, status = "primary", solidHeader = TRUE,
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
                )
                )
  )
)

server <- function(input, output) {
  # Landing page
  histdata <- rnorm(500)
  observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = histdata, { 
    # event will be called when histdata changes, which only happens once, when it is initially calculated
    showModal(modalDialog(
      title = "", 
      h1("Welcome to Huijun An's Personal Introduction"),
      setBackgroundImage(
        src = "Rplot.png"
      )
    ))
  })
  
  
}

shinyApp(ui, server)