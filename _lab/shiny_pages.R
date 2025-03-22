library(shiny)
library(glue)
library(plotly)
library(leaflet)
library(shinyFiles)
library(sf)

element <- function(color, height, title) {
  div(
    style = glue("
      background-color: {color};
      color: #3e3e3e;
      font-size: 35px;
      height: calc({height} - 10px);
      line-height: calc({height} - 10px);
      text-align: center;
      margin: 5px 2.5px;
      border-radius: 10px;
    "),
    title
  )
}


##################
###### Home ######
##################
PanelHome = tabPanel(
  title = "Home",

  fluidRow(
    column(
      width = 8,
      element("#55DA5E", "60vh", "Summary of all projects")
    ),
    column(
      width = 4,
      shinyDirButton("project_folder", "Choose a directory", "Upload"),
      #selectInput(inputId = "project_folder", label = "Entry", choices = 1:50),
      selectInput(inputId = "project", label = "project", choices = c(1, 2, 3)),
    )
  ),
  fluidRow(
    column(
      width = 8,
      tags$p("This is a prototype for an interface of a package under development. Some functions are ill implemented.")
    )
  )
)


##################################
###### Exploratory Analysis ######
##################################
PanelExplore = tabPanel(
  title = "Explore",

  fluidRow(
    column(
      width = 7,
      plotOutput(outputId = "expl_density"),
    ),
    column(
      width = 5,
      element("#fdc6a7", "60vh", "Tests")
    )
  )
)


###############################
###### Statistical Model ######
###############################
PanelModel = tabPanel(
  title = "Statistical Model",
  fluidRow(
    column(
      width = 12,
      element("#a8feca", "15vh", "Model")
    )
  ),

  fluidRow(
    column(
      width = 6,
      element("#a8f2fe", "10vh", "Likelihood"),
      uiOutput("mod_likelihood")
    ),
    column(
      width = 6,
      element("#a8f2fe", "10vh", "Priors"),
      uiOutput("mod_prior")
    )
  ),

  fluidRow(
    column(
      width = 12,
      element("#a8f2fe", "10vh", "Info"),
      uiOutput("mod_info")
    )
  )
)


##################################
###### Convergence Diagnose ######
##################################
PanelConvergence = tabPanel(
  title = "Convergence Diagnose",

  fluidRow(
    column(
      width = 12,
      element("#fdc6a7", "6vh", "Select Parameter")
    )
  ),

  fluidRow(
    column(
      width = 6,
      selectInput(inputId = "diag_param",
                  label = "Parameter",
                  choices = c("LP", "alpha", "lambda", "sigma2")
      ),
      selectInput(inputId = "diag_param_loc1",
                  label = "Row/Column",
                  choices = c(1:5),
      ),
      selectInput(inputId = "diag_param_loc2",
                  label = "Row/Column",
                  choices = c(1:50),
      ),
      actionButton(inputId = "select_param", label = "Show diagnose")
    ),
    column(
      width = 6,
      tags$div(style = "height: 40px;"),
      uiOutput("diag_param_selected")
    )
  ),

  fluidRow(
    column(
      width = 12,
      element("#fdc6a7", "6vh", "Traceplot and sampled posterior")
    )
  ),

  fluidRow(
    column(
      width = 6,
      plotOutput(outputId = "diag_traceplot"),
    ),
    column(
      checkboxGroupInput(
        inputId = "diag_density_type",
        label = "Density sample",
        choices = list(
          "histogram" = 1,
          "density" = 2
        ),
        selected = c(1, 2),
        inline = TRUE
      ),
      width = 6,
      plotOutput(outputId = "diag_density"),
    ),
  ),


  fluidRow(
    column(
      width = 12,
      element("#fdc6a7", "6vh", "Gelman-Rubin Diagnose")
    )
  ),

  fluidRow(
    column(
      width = 6,
      plotOutput(outputId = "diag_gr"),
    ),
    column(
      width = 6,
      tags$div(style = "height: 50px;"),
      verbatimTextOutput(outputId = "diag_gr_print")
    )
  ),


  fluidRow(
    column(
      width = 12,
      element("#fdc6a7", "6vh", "Geweke Diagnose")
    )
  ),

  fluidRow(
    column(
      width = 12,
      tags$div(style = "height: 20px;"),
      verbatimTextOutput(outputId = "diag_geweke_print1"),
      tags$div(style = "height: 20px;"),
      verbatimTextOutput(outputId = "diag_geweke_print2")
    )
  ),
)


################################
###### Bayesian Inference ######
################################
PanelInference = tabPanel(
  title = "Bayesian Inference",

  fluidRow(
    column(
      width = 12,
      element("#fdc6a7", "6vh", "Alpha posterior")
    )
  ),

  fluidRow(
    column(
      selectInput(inputId = "alpha_row", label = "Row", choices = c(1,2,3,4,5)),
      width = 6,
      plotlyOutput(outputId = "alpha_hpd")
    ),
    column(
      selectInput(inputId = "contrast_type", label = "Type", choices = c("mean", "median", "hpd_contains_0")),
      width = 6,
      plotlyOutput(outputId = "contrast")
    )
  ),



  fluidRow(
    column(
      width = 12,
      element("#fdc6a7", "6vh", "Lambda posterior")
    )
  ),

  fluidRow(
    column(
      width = 8,
      plotlyOutput("lambdas")
    )
  ),


  fluidRow(
    column(
      width = 12,
      element("#fdc6a7", "6vh", "sigma2 posterior")
    )
  ),

  fluidRow(
    column(
      width = 12,
      plotlyOutput("sigma2")
    )
  ),



  fluidRow(
    column(
      width = 12,
      element("#fdc6a7", "6vh", "Map")
    )
  ),

  fluidRow(
    column(
      actionButton(inputId = "mark_stations", label = "Mark stations"),
      width = 12,
      leafletOutput(
        outputId = "worldmap", width = "100%"
      )
    )
  )
)


############################
###### User Interface ######
############################
ui = navbarPage(
  title = "FAStan App Beta",
  PanelHome, PanelExplore, PanelModel, PanelConvergence, PanelInference
)
