library(shiny)
library(glue)
library(dplyr)
library(plotly)
library(sf)
library(ggplot2)
library(tidyr)
library(coda)
library(ggridges)

#devtools::load_all()
#source("_shiny_online/utils.R")
#source("utils.R")

max_size_in_Mb_for_uploads = 500
options(shiny.maxRequestSize = max_size_in_Mb_for_uploads*1024^2)

element = function(color, height, title) {
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
      element("#55DA5E", "60vh", "List of all projects")
    ),
    column(
      width = 4,
      fileInput("PanelHome.project_file", NULL, buttonLabel = "Choose project", multiple = FALSE, accept = c(".rds"))
    )
  ),
  fluidRow(
    column(
      width = 8,
      tags$p("This is a prototype for an interface of a package under development. Some functions may be ill implemented.")
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
      element("#fdc6a7", "60vh", "GGridges")
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
      element("#87C2CC", "12vh", "Model")
    )
  ),

  fluidRow(
    column(
      width = 6,
      element("#a8f2fe", "8vh", "Likelihood"),
      uiOutput("PanelModel.like")
    ),
    column(
      width = 6,
      element("#a8f2fe", "8vh", "Priors"),
      uiOutput("PanelModel.prior")
    )
  ),

  fluidRow(
    column(
      width = 12,
      element("#a8f2fe", "8vh", "Info"),
      uiOutput("PanelModel.info")
    )
  )
)


##################################
###### Convergence Diagnose ######
##################################
PanelConvergence = tabPanel(
  title = "Convergence Diagnose",

  ### General Diagnose ###

  fluidRow(
    column(
      width = 12,
      element("#1E929E", "12vh", "General diagnose")
    )
  ),

  fluidRow(
    column(
      width = 4,
      verbatimTextOutput("PanelConvergence.general_info_date")
    ),
    column(
      width = 4,
      verbatimTextOutput("PanelConvergence.general_info_time")
    ),
    column(
      width = 4,
      verbatimTextOutput("PanelConvergence.general_info_args")
    )
  ),

  fluidRow(
    column(
      width = 2,
      selectInput("PanelConvergence.general_par",
                  label = "Parameters",
                  choices = c("All", "alpha", "lambda", "sigma2")
                  )
    ),
    column(
      width = 5,
      element("#27BDCC", "8vh", "Neff"),
      plotlyOutput("PanelConvergence.neff_plot"),
      verbatimTextOutput("PanelConvergence.neff_print")
    ),
    column(
      width = 5,
      element("#27BDCC", "8vh", "Rhat"),
      plotlyOutput("PanelConvergence.rhat_plot"),
      verbatimTextOutput("PanelConvergence.rhat_print")
    )
  ),


  ### Specific Diagnose ###

  fluidRow(
    column(
      width = 12,
      uiOutput("PanelConvergence.par_name")
    )
  ),

  fluidRow(
    column(
      width = 3,
      selectInput("PanelConvergence.par",
                  label = "Parameter",
                  choices = c("lp__", "alpha", "lambda", "sigma2")
      )),
    column(
      width = 3,
      selectInput("PanelConvergence.row",
                  label = "Row",
                  choices = 1,
      )),
    column(
      width = 3,
      selectInput("PanelConvergence.col",
                  label = "Column",
                  choices = 1,
      )),
    column(
      width = 3,
      tags$div(style = "height: 25px;"),
      actionButton("PanelConvergence.select", label = "Show diagnose")
    )
  ),

  fluidRow(
    column(
      width = 12,
      element("#27BDCC", "8vh", "Traceplot and sampled posterior")
    )
  ),

  fluidRow(
    column(
      width = 6,
      plotOutput("PanelConvergence.traceplot"),
    ),
    column(
      checkboxGroupInput(
        "PanelConvergence.density_type",
        label = "Density sample",
        choices = list(
          "histogram" = "hist",
          "density" = "dens"
        ),
        selected = c("hist", "dens"),
        inline = TRUE
      ),
      width = 6,
      plotOutput("PanelConvergence.density"),
    )
  ),

  fluidRow(
    column(
      width = 6,
      element("#27BDCC", "8vh", "Gelman-Rubin Diagnose"),
      plotOutput("PanelConvergence.gr_plot"),
      verbatimTextOutput("PanelConvergence.gr_print")
    ),

    column(
      width = 6,
      element("#27BDCC", "8vh", "Rhat & Neff"),
      tags$div(style = "height: 20px;"),
      verbatimTextOutput("PanelConvergence.rhat_neff"),
      tags$div(style = "height: 30px;"),

      element("#27BDCC", "8vh", "Geweke Diagnose"),
      tags$div(style = "height: 20px;"),
      verbatimTextOutput("PanelConvergence.geweke")
    )
  )

)


################################
###### Bayesian Inference ######
################################
PanelInference = tabPanel(
  title = "Bayesian Inference",

  fluidRow(
    column(
      width = 12,
      element("#fdc6a7", "8vh", "Alpha posterior")
    )
  ),

  fluidRow(
    column(
      selectInput("PanelInference.alpha_col", label = "Column", choices = 1),
      width = 6,
      plotlyOutput("alpha_hpd")
    ),
    column(
      selectInput("PanelInference.contrast_type", label = "Type", choices = c("mean", "median", "hpd_contains_0")),
      width = 6,
      plotlyOutput("contrast")
    )
  ),

  fluidRow(
    column(
      width = 12,
      element("#fdc6a7", "8vh", "Lambda posterior")
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
      element("#fdc6a7", "8vh", "sigma2 posterior")
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
      element("#fdc6a7", "8vh", "Map")
    )
  )


)


############################
###### User Interface ######
############################
ui = navbarPage(
  title = "FAstan App",
  PanelHome, PanelExplore, PanelModel, PanelConvergence, PanelInference
)


####################
###### Server ######
####################
server = function(input, output, session) {

  project = reactive(readRDS(input$PanelHome.project_file$datapath))
  real = reactive(!is.null(project()$model$real))

  PanelConvergence.div_par_name = reactiveVal("Select Parameter")
  output$PanelConvergence.par_name = renderUI({
    tags$div(PanelConvergence.div_par_name(),
    style = "
    background-color: #1E929E;
    color: #3e3e3e;
    font-size: 35px;
    height: calc(12vh - 10px);
    line-height: calc(12vh - 10px);
    text-align: center;
    margin: 5px 2.5px;
    border-radius: 10px;
    "
    )
  })

  PanelHome.project_file.clicks  = reactiveVal(0)
  PanelConvergence.select.clicks = reactiveVal(0)


  ### PanelInference ###

  observeEvent(input$PanelHome.project_file, {
    PanelHome.project_file.clicks(PanelHome.project_file.clicks()+1)

    updateSelectInput(
      session, "PanelInference.alpha_col",
      choices = 1:project()$model$dim$al_fac
      )

    output$lambdas = renderPlotly({
      plot_lambda(project()$summary) %>%
        ggplotly()
    })

    output$alpha_hpd = renderPlotly({
      plot_hpd(project()$summary, "alpha", col = input$PanelInference.alpha_col |> as.integer()) %>%
        ggplotly()
    })

    output$contrast = renderPlotly({
      plot_contrast(project()$summary, "alpha", stat = input$PanelInference.contrast_type) %>%
        ggplotly()
    })

    output$sigma2 = renderPlotly({
      plot_hpd(project()$summary, "sigma2", col = 1) %>%
        ggplotly()
    })

  })


  ### PanelConvergence ###

  observeEvent(input$PanelHome.project_file, {
    output$PanelConvergence.general_info_date = renderPrint({
      project()$fit@date |>
        {\(.) cat(
          "MCMC Date\n",
          .,
          sep = ""
        )}()
    })

    output$PanelConvergence.general_info_time = renderPrint({
      table = rstan::get_elapsed_time(project()$fit) / 60
      cat("Elapsed time (mins.)\n")
      print(table |> round(2))
      cat("Total: ", table |> sum() |> round(2))
    })

    output$PanelConvergence.general_info_args = renderPrint({
      project()$fit@stan_args[[1]] |>
        {\(.) cat(
          "STAN arguments",
          "\nChains\t", length(project()$fit@stan_args), "\t\tIter\t"  , .$iter,
          "\nThin\t", .$thin                           , "\t\tWarmup\t", .$warmup
        )}()
    })

  })


  ### PanelConvergence ###

  observeEvent(c(input$PanelHome.project_file, input$PanelConvergence.general_par), {
    if (PanelHome.project_file.clicks()) {

    par = input$PanelConvergence.general_par |>
      {\(.) if (. == "All") NULL  else . }()

    diag_smry = diagnostic_statistics(project()$fit) |>
      {\(.) if(!is.null(par)) dplyr::filter(., par == !!par) else .}() |>
      {\(.) list(rhat = .$rhat,
                 neff = .$neff)}()

    output$PanelConvergence.neff_plot = renderPlotly({
      plot_diagnostic(project()$fit, stat = "neff", par = par) %>%
        ggplotly()
    })

    output$PanelConvergence.rhat_plot = renderPlotly({
      plot_diagnostic(project()$fit, stat = "rhat", par = par) %>%
        ggplotly()
    })

    output$PanelConvergence.neff_print = renderPrint({
      summary(diag_smry$neff)
    })

    output$PanelConvergence.rhat_print = renderPrint({
      summary(diag_smry$rhat)
    })

  }})


  ### PanelConvergence ###

  observeEvent(input$PanelConvergence.par, {

    if        (input$PanelConvergence.par == "lp__") {
      row = col = 1
    } else if (input$PanelConvergence.par == "alpha") {
      row = project()$model$dim$al_row
      col = project()$model$dim$al_fac
    } else if (input$PanelConvergence.par == "lambda") {
      row = project()$model$dim$al_fac
      col = project()$model$dim$al_col
    } else if (input$PanelConvergence.par == "sigma2") {
      row = project()$model$dim$al_row
      col = 1
    }

    updateSelectInput(
      session, "PanelConvergence.row",
      choices = 1:row
    )

    updateSelectInput(
      session, "PanelConvergence.col",
      choices = 1:col
    )
  })


  ### PanelConvergence ###

  observeEvent(input$PanelConvergence.select, {
    PanelConvergence.select.clicks(PanelConvergence.select.clicks() + 1)

    row = as.integer(input$PanelConvergence.row)
    col = as.integer(input$PanelConvergence.col)
    par = input$PanelConvergence.par
    par_name = ifelse(par == "lp__",
                      "lp__", paste0(par, "[", row, ",", col, "]"))
    combinedchains = get_chains_mcmc(project()$fit, par_name)

    PanelConvergence.div_par_name(paste0("Selected parameter: ", par_name))

    output$PanelConvergence.traceplot = renderPlot({
      plot_trace(
        project()$fit,
        par, row, col
      )
    })

    output$PanelConvergence.gr_plot = renderPlot({
      coda::gelman.plot(combinedchains)
    })

    output$PanelConvergence.gr_print = renderPrint({
      coda::gelman.diag(combinedchains)
    })

    output$PanelConvergence.geweke = renderPrint({
      coda::geweke.diag(combinedchains)
    })


    density_type = input$PanelConvergence.density_type |> as.vector()

    output$PanelConvergence.density = renderPlot({
      plot_posterior(project()$fit, par, row, col, density_type)
    })


    output$PanelConvergence.rhat_neff = renderPrint({
      diagnostic_statistics(project()$fit) |>
        dplyr::filter(par == !!par,
                      row == !!row,
                      col == !!col) |>
        dplyr::select(dplyr::all_of(c("neff", "rhat"))) |>
        unlist()
    })

  })


  ### PanelConvergence ###

  observeEvent(input$PanelConvergence.density_type, {
    row  = as.integer(input$PanelConvergence.row)
    col  = as.integer(input$PanelConvergence.col)
    par  = input$PanelConvergence.par
    type = input$PanelConvergence.density_type |> as.vector()

    if (PanelConvergence.select.clicks()) {
      output$PanelConvergence.density = renderPlot({
        plot_posterior(project()$fit, par, row, col, type)
      })
    }
  })


  ### PanelModel ###

  observeEvent(input$PanelHome.project_file, {
    dim = project()$model$dim

    output$PanelModel.like = renderUI({
      paste0(
        "<p>\\[ X = \\alpha \\lambda + \\epsilon \\]</p>",
        "<p><b>Where:</b></p>",
        "<p>(i) \\( X \\in \\mathbb{R}^{",          dim$al_row ,"\\times", dim$al_col, "} \\) is the matrix of observed values;</p>",
        "<p>(ii) \\( \\alpha \\in \\mathbb{R}^{",   dim$al_row, "\\times", dim$al_fac, "} \\) is the loadings matrix;</p>",
        "<p>(iii) \\( \\lambda \\in \\mathbb{R}^{", dim$al_fac, "\\times", dim$al_col, "} \\) is the factor(s) matrix;</p>",
        "<p>(iv) \\( \\epsilon \\in \\mathbb{R}^{", dim$al_row ,"\\times", dim$al_col, "} \\) is the stochastic component such that
       \\( \\epsilon_{i,j} \\sim N(0, \\sigma^2_i) \\) independently, and
       \\( \\sigma^2 = (\\sigma^2_1, \\dots, \\sigma^2_{", dim$al_row, "})' \\).</p>"
      ) |>
        HTML() |>
        withMathJax()
    })

    output$PanelModel.prior = renderUI({
      paste0(
        "<p>\\[ \\lambda_{i, \\bullet} = N_m(0_{", dim$al_col, "\\times 1}, I_{", dim$al_col, "}); \\]</p>",
        "<p>\\[ \\sigma^2 \\sim Gama_n(0.1, 0.1). \\]</p>"
      ) |>
        HTML() |>
        withMathJax()
    })

    output$PanelModel.info = renderUI({
      paste0(
        "<p>About: mock data</p>",
        "<p>Date:  09/02/2025</p>",
        "<p>Model type: FA SC</p>"
      ) |>
        HTML() |>
        withMathJax()
    })

  })

}


##########################
###### Let it Shiny ######
##########################

shiny4fastan = function() {
  shinyApp(ui = ui, server = server)
}

shiny4fastan()
