#' Shiny column with HTML header
#'
#' @param title .
#' @param color .
#' @param height .
#' @param width .
#'
#' @import shiny
header_col = function(title, color, height, width) {
  column(width,
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


#' Shiny UI
#'
#' @import plotly
#' @import shiny
ui = function() {
####################
######  Model ######
####################
PanelModel = tabPanel(
  title = "Statistical Model",
  fluidRow(
    column(6,
      fileInput("General.project_file", NULL, buttonLabel = "Choose project", multiple = FALSE, accept = c(".rds"))
    ),
    column(6,
      downloadButton("Model.export", "Export project")
    )),
  fluidRow(header_col("Model", "#87C2CC", "12vh", 12)),
  fluidRow(header_col("Likelihood", "#a8f2fe", "8vh", 6), header_col("Priors", "#a8f2fe", "8vh", 6)),
  fluidRow(
    column(6,
      uiOutput("Model.like")
    ),
    column(6,
      uiOutput("Model.prior")
    )
  ),

  fluidRow(header_col("Info", "#a8f2fe", "8vh", 6), header_col("Labels", "#a8f2fe", "8vh", 6)),
  fluidRow(
    column(6,
      uiOutput("Model.info")
      ),
    column(6,
      uiOutput("Model.labels")
      )
  )
)


##################################
###### Convergence Diagnose ######
##################################
PanelConvergence = tabPanel(
  title = "Convergence Diagnose",

  fluidRow(header_col("Convergence Diagnose", "#87C2CC", "12vh", 12)),

  fluidRow(
    column(6,
      verbatimTextOutput("Convergence.general_info_time")
    ),
    column(6,
      verbatimTextOutput("Convergence.general_info_args")
    )
  ),

  fluidRow(
    header_col("Rhat",   "#a8f2fe", "8vh", 4),
    header_col("n_eff",   "#a8f2fe", "8vh", 4),
    header_col("Geweke", "#a8f2fe", "8vh", 4)
    ),
  fluidRow(
    column(4,
      plotOutput("Convergence.general_rhat"),
    ),
    column(4,
      plotOutput("Convergence.general_neff"),
    ),
    column(4,
      plotOutput("Convergence.general_geweke"),
    )
  ),


  fluidRow(
    column(12,
      uiOutput("Convergence.par_name")
    )
  ),

  fluidRow(
    column(3,
      selectInput("Convergence.par",
                  label = "Parameter",
                  choices = c("lp__", "alpha", "lambda", "sigma2")
      )),
    column(3,
      selectInput("Convergence.row",
                  label = "Row",
                  choices = 1,
      )),
    column(3,
      selectInput("Convergence.col",
                  label = "Column",
                  choices = 1,
      )),
    column(3,
      tags$div(style = "height: 25px;"),
      actionButton("Convergence.select", label = "Show diagnose")
    )
  ),

  fluidRow(
    column(6,
      plotOutput("Convergence.traceplot"),
    ),
    column(6,
      checkboxGroupInput(
        "Convergence.density_type",
        label = "Density sample",
        choices = list(
          "histogram" = "hist",
          "density" = "dens"
        ),
        selected = c("hist", "dens"),
        inline = TRUE
      ),
      plotOutput("Convergence.density"),
    )
  ),

  fluidRow(
    column(6),
    column(6,
      verbatimTextOutput("Convergence.stats")
    )
  ),

  fluidRow(
    header_col("Gelman-Rubin", "#a8f2fe", "8vh", 4),
    header_col("Rhat & n_eff", "#a8f2fe", "8vh", 4),
    header_col("Geweke", "#a8f2fe", "8vh", 4)
  ),

  fluidRow(
    column(4,
      plotOutput("Convergence.gr_plot"),
      verbatimTextOutput("Convergence.gr_print")
    ),

    column(4,
      tags$div(style = "height: 20px;"),
      verbatimTextOutput("Convergence.rhat_neff"),
      tags$div(style = "height: 30px;"),
    ),

    column(4,
      tags$div(style = "height: 20px;"),
      verbatimTextOutput("Convergence.geweke")
    )
  )

)


########################
######  Inference ######
########################
PanelInference = tabPanel(
  title = "Bayesian Inference",

  fluidRow(header_col("Posterior analysis", "#87C2CC", "12vh", 12)),

  fluidRow(
    column(12,
      uiOutput("Inference.accuracy")
    )
  ),

  fluidRow(header_col("Alpha posterior", "#a8f2fe", "8vh", 12)),

  fluidRow(
    column(7,
      selectInput("Inference.alpha_col", label = "Column", choices = 1),
      plotly::plotlyOutput("alpha_hpd")
    ),
    column(5,
      selectInput("Inference.contrast_type", label = "Type", choices = c("mean", "hpd_contains_0")),
      plotly::plotlyOutput("contrast")
    )
  ),

  fluidRow(header_col("Lambda posterior", "#a8f2fe", "8vh", 12)),

  fluidRow(
    column(10,
      plotly::plotlyOutput("lambdas")
    )
  ),

  fluidRow(header_col("Sigma^2 posterior", "#a8f2fe", "8vh", 12)),

  fluidRow(
    column(12,
      plotly::plotlyOutput("sigma2")
    )
  ),

  fluidRow(
    column(12,
      uiOutput("Inference.prediction")
    )
  )

)

navbarPage(
  title = "fastan app",
  PanelModel, PanelConvergence, PanelInference
)
}


#' Shiny server
#'
#' @param proj .
#' @param input .
#' @param output .
#' @param session .
#'
#' @import plotly
#' @import coda
#' @import rstan
#' @import dplyr
#' @import zip
#' @import purrr
#' @import stats
#' @import shiny
server = function(proj = NULL, input, output, session) {

server0 = function(input, output, session) {

  ### Import/Export ###

  project_rv = reactiveVal()
  if (!is.null(proj)) {
    project_rv(proj)
  }
  observeEvent(input$General.project_file, {
    input$General.project_file$datapath |>
      readRDS() |>
      project_rv()
  })
  project = reactive({
    req(project_rv())
  })

  real = reactive(!is.null(project()$data$real))
  stat = reactive(c("mean") |> {\(.) if (real()) c(., "real") else .}())

  output$Model.export = downloadHandler(
    filename = function() {
      paste0("fastanExport_", format(Sys.time(), "%Y-%m-%d-%Hh%Mm%Ss"), ".zip")
    },
    content = function(file) {
      dir_temp = tempfile("fastanExport")
      dir.create(dir_temp)
      x = export(project(), path_dump = dir_temp)
      zip::zipr(zipfile = file, files = list.files(x, full.names = TRUE))
    },
    contentType = "application/zip"
  )


  ### PanelInference ###

  observeEvent(project(), {
    updateSelectInput(
      session, "Inference.alpha_col",
      choices = 1:n.fac(project())
      )

    output$lambdas = plotly::renderPlotly({
      plot_lambda(project()$summary, stat = ifelse("real" %in% stat(), "real", "mean")) |>
        plotly::ggplotly()
    })

    output$alpha_hpd = plotly::renderPlotly({
      plot_hpd(project()$summary, "alpha", stat = stat(), col = input$Inference.alpha_col |> as.integer()) |>
        plotly::ggplotly()
    })

    output$contrast = plotly::renderPlotly({
      plot_contrast(project()$summary, "alpha", stat = input$Inference.contrast_type) |>
        plotly::ggplotly()
    })

    output$sigma2 = plotly::renderPlotly({
      plot_hpd(project()$summary, "sigma2", stat = stat(), col = 1) |>
        plotly::ggplotly()
    })

    updateSelectInput(
      session, "Inference.contrast_type",
      choices =  c("mean", "hpd_contains_0") |> {\(.) if (real()) c("mean", "real", "hpd_contains_0") else .}()
    )

    if (real()) {
    output$Inference.accuracy = renderUI({
      tagList(
        fluidRow(header_col("Accuracy", "#a8f2fe", "8vh", 12)),

        fluidRow(
          column(12,
            verbatimTextOutput("Inference.accuracy_table")
          )
        )
      )
    })

    output$Inference.accuracy_table = renderPrint(fastan::percentage_hits(project()$summary) |> round(4))
    }

    if (!is.null(project()$data$pred)) {
    output$Inference.prediction = renderUI({
      tagList(
        fluidRow(header_col("Predictions", "#a8f2fe", "8vh", 12)),

        fluidRow(
          column(6,
            plotly::plotlyOutput("Inference.pred_contrast")
          ),
          column(6,
            verbatimTextOutput("Inference.pred_posterior_summary"),
            plotOutput("Inference.pred_posterior_plot")
          )
        )
      )
    })

    output$Inference.pred_contrast = plotly::renderPlotly({
      (plot_missing(project()$data) + theme(axis.text.y = element_blank())) |>
        plotly::ggplotly(tooltip = "text", source = "Inference.pred_contrast_source")
    })
    }
  })


  ### PanelInference - prediction ###

  coor = reactive({
    coor = event_data("plotly_click", source = "Inference.pred_contrast_source")
    if (is.null(coor)) return(NULL)
    row_clicked =
      coor$y |>
      {\(.) rev(1:length(unique(project()$data$pred$row)))[.]}() |>
      {\(.) unique(project()$data$pred$row)[.]}()
    col_clicked = coor$x
    return(list(row = row_clicked, col = col_clicked))
  })

  output$Inference.pred_posterior_summary = renderPrint({
    coor = coor()
    if (is.null(coor)) return(cat("Click on the black squares."))

    df =
      project()$summary$pred |>
      matrix_to_df() |>
      {\(.) dplyr::filter(., .$row_ == coor$row, .$col_ == coor$col)}()

    cat("Row:", coor$row, "  Col.:", coor$col, "\tRow param.:", df$row,"\n")

    df |>
      {\(.) if (real()) dplyr::select(., dplyr::all_of(c("real", "mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp")))
        else dplyr::select(., dplyr::all_of(c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp")))}() |>
      as.data.frame() |>
      round(2) |>
      `rownames<-`("") |>
      print()
  })

  output$Inference.pred_posterior_plot = renderPlot({
    coor = coor()
    if (is.null(coor)) return(ggplot()+annotate("text", x=0, y=0, label="posterior predictive plot")+theme_void())

    row =
      project()$summary$pred |>
      matrix_to_df() |>
      {\(.) dplyr::filter(., .$row_ == coor$row, .$col_ == coor$col)}() |>
      dplyr::select(dplyr::all_of("row")) |>
      purrr::pluck(1)
    plot_posterior(project(), "pred", row = row)
  })


  ### PanelModel ###

  observeEvent(project(), {
    dim = c(project()$data$dim, list(fac = n.fac(project())))

    output$Model.like = renderUI({
      paste0(
        "<p>\\[ X = \\alpha \\lambda + \\epsilon \\]</p>",
        "<p><b>Where:</b></p>",
        "<p>(i) \\( X \\in \\mathbb{R}^{",          dim$row ,"\\times", dim$col, "} \\) is the matrix of observed values;</p>",
        "<p>(ii) \\( \\alpha \\in \\mathbb{R}^{",   dim$row, "\\times", dim$fac, "} \\) is the loadings matrix;</p>",
        "<p>(iii) \\( \\lambda \\in \\mathbb{R}^{", dim$fac, "\\times", dim$col, "} \\) is the factor(s) matrix;</p>",
        "<p>(iv) \\( \\epsilon \\in \\mathbb{R}^{", dim$row ,"\\times", dim$col, "} \\) is the stochastic component such that
       \\( \\epsilon_{i,j} \\sim N(0, \\sigma^2_i) \\) independently, and
       \\( \\sigma^2 = (\\sigma^2_1, \\dots, \\sigma^2_{", dim$row, "})' \\).</p>"
      ) |>
        HTML() |>
        withMathJax()
    })

    output$Model.prior = renderUI({
      paste0(
        "<p>\\[ \\lambda_{i, \\bullet} = N_m(0_{", dim$col, "\\times 1}, I_{", dim$col, "}); \\]</p>",
        "<p>\\[ \\sigma^2 \\sim Gama_n(0.1, 0.1). \\]</p>"
      ) |>
        HTML() |>
        withMathJax()
    })

    output$Model.info = renderUI({
      paste0(
        "<p>Info: ", project()$info, "</p>",
        "<p>Number of groups:\t" , project()$data$dim$group.n, "</p>",
        "<p>Number of factors:\t", n.fac(project()), "</p>"
      ) |>
        HTML() |>
        withMathJax()
    })
  })


  ### PanelConvergence - General ###

  observeEvent(project(), {
    output$Convergence.general_info_time = renderPrint({
      cat("STAN elapsed time (h.)\n")
      elapsed_time_table(project()$fit) |> round(2)
    })

    output$Convergence.general_info_args = renderPrint({
      project()$fit@stan_args[[1]] |>
        {\(.) cat(
          "STAN arguments",
          "\nChains\t", length(project()$fit@stan_args),
          "\nThin\t", .$thin,
          "\nIter\t"  , .$iter,
          "\nWarmup\t", .$warmup
        )}()
    })

    output$Convergence.general_rhat = renderPlot({
      plot_diagnostic(project()$fit, "Rhat")
    })

    output$Convergence.general_neff = renderPlot({
      plot_diagnostic(project()$fit, "n_eff")
    })

    output$Convergence.general_geweke = renderPlot({
      plot_diagnostic(project()$fit, "geweke")
    })
  })


  ### PanelConvergence - Specific - Options ###

  observeEvent(project(), {
    if (!is.null(project()$data$pred)) {
      updateSelectInput(
        session, "Convergence.par",
        choices = c("lp__", "alpha", "lambda", "sigma2", "pred")
      )
    }
  })

  observeEvent(input$Convergence.par, {
    if        (input$Convergence.par == "lp__")   {
      row = col = 1
    } else if (input$Convergence.par == "alpha")  {
      row = project()$data$dim$row
      col = n.fac(project())
    } else if (input$Convergence.par == "lambda") {
      row = n.fac(project())
      col = project()$data$dim$col
    } else if (input$Convergence.par == "sigma2") {
      row = project()$data$dim$row
      col = 1
    } else if (input$Convergence.par == "pred")   {
      row = dim(project()$summary$pred)[1]
      col = 1
    }

    updateSelectInput(
      session, "Convergence.row",
      choices = 1:row
    )

    updateSelectInput(
      session, "Convergence.col",
      choices = 1:col
    )
  })

  Convergence.div_par_name = reactiveVal("Select Parameter")
  output$Convergence.par_name = renderUI({
    header_col(Convergence.div_par_name(), "#a8f2fe", "8vh", 12)
  })


  ### PanelConvergence - Specific - plots ###

  Convergence.select.clicks = reactiveVal(0)

  observeEvent(input$Convergence.select, {
    Convergence.select.clicks(Convergence.select.clicks() + 1)

    row = as.integer(input$Convergence.row)
    col = as.integer(input$Convergence.col)
    par = input$Convergence.par
    par_name = ifelse(par == "lp__",
                      "lp__", paste0(par, "[", row, ",", col, "]"))
    combinedchains = get_chains_mcmc(project()$fit, par_name)

    Convergence.div_par_name(paste0("Selected parameter: ", par_name))

    output$Convergence.traceplot = renderPlot({
      plot_trace(
        project()$fit,
        par, row, col
      )
    })

    output$Convergence.gr_plot = renderPlot({
      coda::gelman.plot(combinedchains)
    })

    output$Convergence.gr_print = renderPrint({
      coda::gelman.diag(combinedchains)
    })

    output$Convergence.geweke = renderPrint({
      coda::geweke.diag(combinedchains)
    })

    output$Convergence.stats = renderPrint({
      if (par == "lp__") {
        rstan::extract(project()$fit, par = "lp__") |>
          purrr::pluck(1) |>
          {\(.) data.frame(mean = mean(.), median = stats::median(.), sd = stats::sd(.), real = loglik(project(), stat = "real"), est_by_mean = loglik(project(), stat = "mean"))}() |>
          round(2) |>
          `row.names<-`("") |>
          print()
      } else {
        project()$summary[[par]][row, col, ] |>
          as.matrix() |>
          as.data.frame() |>
          mutate(V1 = V1 |> round(2)) |>
          as.matrix() |>
          t() |>
          `row.names<-`("") |>
          print()
      }
    })

    density_type = input$Convergence.density_type |> as.vector()

    output$Convergence.density = renderPlot({
      plot_posterior(project()$fit, par, row, col, density_type)
    })

    output$Convergence.rhat_neff = renderPrint({
      diagnostic(project()$fit) |>
        dplyr::filter(par == !!par,
                      row == !!row,
                      col == !!col) |>
        dplyr::select(dplyr::all_of(c("n_eff", "Rhat"))) |>
        unlist() |>
        round(2)
    })

  })


  ### PanelConvergence - Specific - density ###

  observeEvent(input$Convergence.density_type, {
    row  = as.integer(input$Convergence.row)
    col  = as.integer(input$Convergence.col)
    par  = input$Convergence.par
    type = input$Convergence.density_type |> as.vector()

    if (Convergence.select.clicks()) {
      output$Convergence.density = renderPlot({
        plot_posterior(project()$fit, par, row, col, type)
      })
    }
  })

}

server0
}


#' Shiny App
#'
#' @param proj .
#' @param upload_size .
#'
#' @export
shiny = function(proj = NULL, upload_size = 500) {
  options(shiny.maxRequestSize = upload_size*1024^2)
  shinyApp(ui = ui(), server = server(proj))
}
