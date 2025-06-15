library(shiny)
library(plotly)
library(ggplot2)
library(tidyverse)
library(coda)

devtools::load_all()

shinyApp(ui = ui(proj), server = server)

ui = function(proj){

  header_col = function(title, color, height, width) {
    column(
      width = width,
      div(
        style = paste0(
          "background-color:", color, ";",
          "color: #3e3e3e;
          font-size: 35px;
          height: calc(",height," - 10px);",
          "line-height: calc(", height," - 10px);",
          "text-align: center;
          margin: 5px 2.5px;
          border-radius: 10px;
        "),
        title
      )
    )
  }

  ###############################
  ###### Statistical Model ######
  ###############################

  dim = proj$model$dim

  PanelModel = tabPanel(
    title = "Statistical Model",
    fluidRow(header_col("Model", "#87C2CC", "12vh", 12)),
    fluidRow(header_col("Likelihood", "#a8f2fe", "8vh", 6), header_col("Priors", "#a8f2fe", "8vh", 6)),
    fluidRow(
      column(6,
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
      ),

      column(6,
        paste0(
        "<p>\\[ \\lambda_{i, \\bullet} = N_m(0_{", dim$al_col, "\\times 1}, I_{", dim$al_col, "}); \\]</p>",
        "<p>\\[ \\sigma^2 \\sim Gama_n(0.1, 0.1). \\]</p>"
      ) |>
        HTML() |>
        withMathJax()
      )
    ),

    fluidRow(header_col("Info", "#a8f2fe", "8vh", 12)),
    fluidRow(
      column(12,
        paste0(
        "<p>About: mock data</p>",
        "<p>Date:  09/02/2025</p>",
        "<p>Model type: FA SC</p>"
      ) |>
        HTML() |>
        withMathJax()
      )
    )
  )



  navbarPage(
    title = "FAstan App",
    PanelModel, PanelConvergence, PanelInference
  )
}




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
      width = 6,
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
      plotOutput("PanelConvergence.density"),
    )
  ),

  fluidRow(
    column(6),
    column(6,
           verbatimTextOutput("PanelConvergence.stats")
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
      uiOutput("PanelInference.prediction")
    )
  )

)


############################
###### User Interface ######
############################
ui = navbarPage(
  title = "FAstan App",
  PanelModel, PanelConvergence, PanelInference
)


####################
###### Server ######
####################
server = function(input, output, session) {

  #real = reactive(!is.null(proj$model$real))
  real = reactive(F)

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
      choices = 1:proj$model$dim$al_fac
      )

    output$lambdas = renderPlotly({
      if (real()) {
        stat = c("mean", "real")
      } else {
        stat = c("mean")
      }
      #plot_lambda(proj$summary) %>%
      plot_lambda(proj$summary, stat = stat) %>%
        ggplotly()
    })

    output$alpha_hpd = renderPlotly({
      if (real()) {
        stat = c("mean", "real")
      } else {
        stat = c("mean")
      }
      #plot_hpd(proj$summary, "alpha", col = input$PanelInference.alpha_col |> as.integer()) %>%
      plot_hpd(proj$summary, "alpha", stat = stat, col = input$PanelInference.alpha_col |> as.integer()) %>%
        ggplotly()
    })

    output$contrast = renderPlotly({
      plot_contrast(proj$summary, "alpha", stat = input$PanelInference.contrast_type) %>%
        ggplotly()
    })

    output$sigma2 = renderPlotly({
      if (real()) {
        stat = c("mean", "real")
      } else {
        stat = c("mean", "median")
      }
      #plot_hpd(proj$summary, "sigma2", col = 1) %>%
      plot_hpd(proj$summary, "sigma2", stat = stat, col = 1) %>%
        ggplotly()
    })

    if (!is.null(proj$model$pred)) {
    output$PanelInference.prediction = renderUI({
      tagList(
        fluidRow(
          column(
            width = 12,
            element("#fdc6a7", "8vh", "predictions")
          )
        ),

        fluidRow(
          column(
            width = 6,
            plotlyOutput("pred_contrast")
          ),
          column(
            width = 6,
            plotlyOutput("pred_posterior")
          )
        )
      )
    })

    output$pred_contrast = renderPlotly({
      plot_missing(proj$model) %>%
        ggplotly(source = "pred_contrast_source")
    })
    }
  })

  output$pred_posterior = renderPlotly({
    coord = pred_contrast_click()
    print(coord)
    if (!is.null(coord)){
      # pred_arg =
      #   proj$model$pred %>%
      #   dplyr::mutate(
      #     x = 1:nrow(.)
      #   ) %>%
      #   dplyr::filter(row == coord[1], col == coord[2]) %>%
      #   select(x) %>%
      #   purrr::pluck(1)
      #
      # plot_posterior(proj$fit, "pred", row = pred_arg) %>%
      #   ggplotly()
      plot_posterior(proj$fit, "pred", row = coord$y) %>%
        ggplotly()
    }
  })

  pred_contrast_click <- reactive({
    event_data("plotly_click", source = "pred_contrast_source")
  })


  ### PanelConvergence ###

  observeEvent(input$PanelHome.project_file, {
    if (!is.null(proj$model$pred)) {
      updateSelectInput(
        session, "PanelConvergence.par",
        choices = c("lp__", "alpha", "lambda", "sigma2", "pred")
      )
    }

    output$PanelConvergence.general_info_date = renderPrint({
      proj$fit@date |>
        {\(.) cat(
          "MCMC Date\n",
          .,
          sep = ""
        )}()
    })

    output$PanelConvergence.general_info_time = renderPrint({
      table = rstan::get_elapsed_time(proj$fit) / 60
      cat("Elapsed time (mins.)\n")
      print(table |> round(2))
      tot = table |> sum()
      cat("Total: ", tot |> round(2), " mins.  = ", (tot / 60) |> round(2), " h.", sep = "")
    })

    output$PanelConvergence.general_info_args = renderPrint({
      proj$fit@stan_args[[1]] |>
        {\(.) cat(
          "STAN arguments",
          "\nChains\t", length(proj$fit@stan_args), "\t\tIter\t"  , .$iter,
          "\nThin\t", .$thin                           , "\t\tWarmup\t", .$warmup
        )}()
    })

  })


  ### PanelConvergence ###

  observeEvent(c(input$PanelHome.project_file, input$PanelConvergence.general_par), {
    if (PanelHome.project_file.clicks()) {

    par = input$PanelConvergence.general_par |>
      {\(.) if (. == "All") NULL  else . }()

    diag_smry = diagnostic_statistics(proj$fit) |>
      {\(.) if(!is.null(par)) dplyr::filter(., par == !!par) else .}() |>
      {\(.) list(rhat = .$rhat,
                 neff = .$neff)}()

    output$PanelConvergence.neff_plot = renderPlotly({
      plot_diagnostic(proj$fit, stat = "neff", par = par) %>%
        ggplotly()
    })

    output$PanelConvergence.rhat_plot = renderPlotly({
      plot_diagnostic(proj$fit, stat = "rhat", par = par) %>%
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
      row = proj$model$dim$al_row
      col = proj$model$dim$al_fac
    } else if (input$PanelConvergence.par == "lambda") {
      row = proj$model$dim$al_fac
      col = proj$model$dim$al_col
    } else if (input$PanelConvergence.par == "sigma2") {
      row = proj$model$dim$al_row
      col = 1
    } else if (input$PanelConvergence.par == "pred") {
      row = dim(proj$summary$pred)[1]
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
    combinedchains = get_chains_mcmc(proj$fit, par_name)

    PanelConvergence.div_par_name(paste0("Selected parameter: ", par_name))

    output$PanelConvergence.traceplot = renderPlot({
      plot_trace(
        proj$fit,
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

    output$PanelConvergence.stats = renderPrint({
      proj$summary[[par]][row, col, ] |>
        as.matrix() |>
        as.data.frame() |>
        mutate(V1 = V1 |> round(2)) |>
        as.matrix() |>
        t() |>
        `row.names<-`("") |>
        print()
    })

    density_type = input$PanelConvergence.density_type |> as.vector()

    output$PanelConvergence.density = renderPlot({
      plot_posterior(proj$fit, par, row, col, density_type)
    })

    output$PanelConvergence.rhat_neff = renderPrint({
      diagnostic_statistics(proj$fit) |>
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
        plot_posterior(proj$fit, par, row, col, type)
      })
    }
  })


}


##########################
###### Let it Shiny ######
##########################

shiny4fastan = function(proj) {

  shinyApp(ui = ui, server = server)
}

shiny4fastan()
