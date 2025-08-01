#' Shiny column with HTML header
#'
#' @param title string with title.
#' @param color string with hexadecimal color.
#' @param height numeric, height in vh.
#' @param width numeric, width in pixels.
#'
#' @import shiny
header_col = function(title, color, height, width) {
  column(width,
         div(
           style = paste0(
             "background-color:", color, ";",
             "color: #3e3e3e;
             font-size: 35px;
             height: calc(",height,"vh - 10px);",
             "line-height: calc(", height,"vh - 10px);",
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
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
#' @import shiny
shiny_ui = function() {
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
  fluidRow(header_col("Model", "#87C2CC", 12, 12)),
  fluidRow(header_col("Likelihood", "#a8f2fe", 8, 6), header_col("Prior", "#a8f2fe", 8, 6)),
  fluidRow(
    column(6,
      uiOutput("Model.like")
    ),
    column(6,
      uiOutput("Model.prior")
    )
  ),

  fluidRow(header_col("Info", "#a8f2fe", 8, 6), header_col("Labels", "#a8f2fe", 8, 6)),
  fluidRow(
    column(6,
      uiOutput("Model.info"),
      tableOutput("Model.info_groups")
      ),
    column(6,
      uiOutput("Model.label")
      )
  )
)


##################################
###### Convergence Diagnose ######
##################################
PanelConvergence = tabPanel(
  title = "Convergence Diagnose",

  fluidRow(header_col("Convergence Diagnose", "#87C2CC", 12, 12)),

  fluidRow(
    column(6,
      verbatimTextOutput("Convergence.general_info_time")
    ),
    column(6,
      verbatimTextOutput("Convergence.general_info_args")
    )
  ),

  fluidRow(
    header_col("Rhat",   "#a8f2fe", 8, 4),
    header_col("n_eff",  "#a8f2fe", 8, 4),
    header_col("Geweke", "#a8f2fe", 8, 4)
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
        label = "Posterior density",
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
      div(style = "margin-top: 1em; display: table; margin-left: auto; margin-right: auto;",
          tableOutput("Convergence.stats")
      )
    )
  ),

  fluidRow(
    header_col("Gelman-Rubin", "#a8f2fe", 8, 4),
    header_col("Rhat & n_eff", "#a8f2fe", 8, 4),
    header_col("Geweke",       "#a8f2fe", 8, 4)
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

  fluidRow(header_col("Posterior analysis", "#87C2CC", 12, 12)),

  fluidRow(
    column(12,
      uiOutput("Inference.accuracy")
    )
  ),

  fluidRow(header_col("Alpha posterior", "#a8f2fe", 8, 12)),

  fluidRow(
    column(7,
      selectInput("Inference.alpha_col", label = "Column", choices = 1),
      checkboxInput("Inference.alpha0", "Only not zero alpha", value = FALSE),
      plotly::plotlyOutput("alpha_hpd")
    ),
    column(5,
      selectInput("Inference.contrast_type", label = "Type", choices = c("mean", "hpd_contains_0")),
      plotly::plotlyOutput("contrast")
    )
  ),

  fluidRow(header_col("Lambda posterior", "#a8f2fe", 8, 12)),

  fluidRow(
    column(10,
      plotly::plotlyOutput("lambdas")
    )
  ),

  fluidRow(header_col("Sigma^2 posterior", "#a8f2fe", 8, 12)),

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


###################
######  Maps ######
###################
PanelMaps = tabPanel(
  title = "Maps",

  fluidRow(header_col("Maps", "#87C2CC", 12, 12)),

  fluidRow(
    column(12,
      uiOutput("Maps.general")
    )
  )

)

navbarPage(
  title = "fastan app",
  PanelModel, PanelConvergence, PanelInference, PanelMaps
)
}


#' Shiny server
#'
#' @param proj `fastan::project` object, optional.
#' @param input shiny input.
#' @param output shiny output.
#' @param session shiny server.
#'
#' @importFrom coda gelman.plot gelman.diag geweke.diag
#' @import dplyr
#' @importFrom gridExtra grid.arrange
#' @importFrom plotly event_data ggplotly plotlyOutput renderPlotly subplot
#' @import purrr
#' @importFrom rstan extract
#' @import shiny
#' @importFrom stats median sd
#' @importFrom zip zipr
shiny_server = function(proj = NULL, input, output, session) {

server0 = function(input, output, session) {

  ### Import/Export ###

  project_rv = reactiveVal()
  if (!is.null(proj)) {
    project_rv(proj)
  }
  observeEvent(input$General.project_file, {
    proj =
      input$General.project_file$datapath |>
      readRDS()

    if (is.null(proj$diagnostic)) {
      proj = set_diagnostic(proj)
    }
    if (is.null(proj$summary)) {
      proj = set_summary(proj)
    }

    project_rv(proj)
  })
  projeto = reactive({
    req(project_rv())
  })

  #real = reactive(!is.null(projeto()$data$real))
  real = reactive(
    lapply( c("alpha", "lambda", "sigma2"), function(x) ("real" %in% dimnames(projeto()$summary[[x]])[[3]]) ) |>
      unlist() |>
      all()
  )

  real_pred = reactive("real" %in% dimnames(projeto()$summary$pred)[[3]])
  stat = reactive(c("mean") |> {\(.) if (real()) c(., "real") else .}())

  output$Model.export = downloadHandler(
    filename = function() {
      paste0("fastanExport-", format(Sys.time(), "%Y_%m_%d-%Hh%Mm%Ss"), ".zip")
    },
    content = function(file) {
      dir_temp = tempfile("fastanExport")
      dir.create(dir_temp)
      x = export(projeto(), path_dump = dir_temp)
      zip::zipr(zipfile = file, files = list.files(x, full.names = TRUE))
    },
    contentType = "application/zip"
  )


  ### PanelInference ###

  observeEvent(projeto(), {
    updateSelectInput(
      session, "Inference.alpha_col",
      choices = 1:n.fac(projeto())
      )

    output$lambdas = plotly::renderPlotly({
      plot_lambda(projeto()$summary, stat = ifelse("real" %in% stat(), "real", "mean")) |>
        plotly::ggplotly()
    })

    output$alpha_hpd = plotly::renderPlotly({
      plot_hpd(projeto(), "alpha", stat = stat(), col = input$Inference.alpha_col |> as.integer(), omit.alpha0 = F) |>
        plotly::ggplotly()
    })

    output$contrast = plotly::renderPlotly({
      plot_contrast(projeto()$summary, "alpha", stat = input$Inference.contrast_type) |>
        plotly::ggplotly()
    })

    output$sigma2 = plotly::renderPlotly({
      plot_hpd(projeto(), "sigma2", stat = stat(), col = 1) |>
        plotly::ggplotly()
    })

    updateSelectInput(
      session, "Inference.contrast_type",
      choices =  c("mean", "hpd_contains_0") |> {\(.) if (real()) c("mean", "real", "hpd_contains_0") else .}()
    )

    if (real() | real_pred()) {
    output$Inference.accuracy = renderUI({
      tagList(
        fluidRow(header_col("Accuracy", "#a8f2fe", 8, 12)),

        fluidRow(
          column(6,
            div(style = "margin-top: 1em; display: table; margin-left: auto; margin-right: auto;",
                tableOutput("Inference.accuracy_table")
            )
          ),
          column(6,
            plotOutput("Inference.bias", height = ifelse(real(), "50vh", "30vh"))
          )
        )
      )
    })

    output$Inference.accuracy_table = renderTable(accuracy(projeto()$summary, correct = T) |> round(4), rownames = T)

    output$Inference.bias = renderPlot({
      if (real()) {
        bias = list()
        for (param in c("all", names(projeto()$summary))) {
          bias[[param]] = plot_bias(projeto(), param)
        }
        gridExtra::grid.arrange(grobs = bias, ncol=2)
      } else if (real_pred()) {
        plot_bias(projeto(), "pred")
      }
    })
    }

    if (!is.null(projeto()$data$pred)) {
    output$Inference.prediction = renderUI({
      tagList(
        fluidRow(header_col("Predictions", "#a8f2fe", 8, 12)),

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
      (plot_missing(projeto()$data) + theme(axis.text.y = element_blank())) |>
        plotly::ggplotly(tooltip = "text", source = "Inference.pred_contrast_source")
    })
    }
  })

  ### PanelInference - omit alpha0 ###

  Inference.alpha_change = reactive({
    list(input$Inference.alpha0, input$Inference.alpha_col)
  })

  observeEvent(Inference.alpha_change(), {
    if (input$Inference.alpha0 & (as.integer(input$Inference.alpha_col) < n.fac(projeto())) & projeto()$prior$semi.conf) {
      p = plot_hpd(projeto(), "alpha", stat = stat(), col = input$Inference.alpha_col |> as.integer(), omit.alpha0 = T, omit.alpha0.list = T)
      output$alpha_hpd = plotly::renderPlotly({
        plotly::subplot(p[[1]], p[[2]], nrows = 1)
      })
    } else {
      output$alpha_hpd = plotly::renderPlotly({
        plot_hpd(projeto(), "alpha", stat = stat(), col = input$Inference.alpha_col |> as.integer(), omit.alpha0 = input$Inference.alpha0) |>
          plotly::ggplotly()
      })
    }
  })

  # observeEvent(input$Inference.alpha_col, {
  #   if (input$Inference.alpha0) {
  #     p = plot_hpd(projeto(), "alpha", stat = stat(), col = input$Inference.alpha_col |> as.integer(), omit.alpha0 = T, omit.alpha0.list = T)
  #     output$alpha_hpd = plotly::renderPlotly({
  #       plotly::subplot(p[[1]], p[[2]], nrows = 1)
  #     })
  #   } else {
  #     output$alpha_hpd = plotly::renderPlotly({
  #       plot_hpd(projeto(), "alpha", stat = stat(), col = input$Inference.alpha_col |> as.integer(), omit.alpha0 = F) |>
  #         plotly::ggplotly()
  #     })
  #   }
  # })

  ### PanelInference - prediction ###

  coor = reactive({
    coor = plotly::event_data("plotly_click", source = "Inference.pred_contrast_source")
    if (is.null(coor)) return(NULL)
    row_clicked =
      coor$y |>
      {\(.) rev(1:length(unique(projeto()$data$pred$row)))[.]}() |>
      {\(.) unique(projeto()$data$pred$row)[.]}()
    col_clicked = coor$x
    return(list(row = row_clicked, col = col_clicked))
  })

  output$Inference.pred_posterior_summary = renderPrint({
    coor = coor()
    if (is.null(coor)) return(cat("Click on the black squares."))

    df =
      summary_as_df(projeto(), "pred")[["pred"]] |>
      {\(.) dplyr::filter(., .$row_ == coor$row, .$col_ == coor$col)}()

    cat("Row:", coor$row, "  Col.:", coor$col, "\tRow param.:", df$row,"\n")

    df |>
      {\(.) if (real() | real_pred()) dplyr::select(., dplyr::all_of(c("real", "mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp", "bias")))
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
      summary_as_df(projeto(), "pred")[["pred"]] |>
      {\(.) dplyr::filter(., .$row_ == coor$row, .$col_ == coor$col)}() |>
      dplyr::select(dplyr::all_of("row")) |>
      purrr::pluck(1)
    plot_posterior(projeto(), "pred", row = row)
  })


  ### PanelModel ###

  observeEvent(projeto(), {
    dim = c(projeto()$data$dim, list(fac = n.fac(projeto())))

    df.groups =
      data.frame(
        group = projeto()$data$label$group |> as.character(),
        size = projeto()$data$dim$group.sizes
      ) |>
      {\(.)
        dplyr::mutate(.,
                      starts = group_limits(.$size)[[1]] |> as.integer(),
                      ends   = group_limits(.$size)[[2]] |> as.integer()
        )
      }()

    if (!is.null(projeto()$prior)) { if (projeto()$prior$semi.conf) {
      df.groups[nrow(df.groups),1] = paste(df.groups[nrow(df.groups),1], "(group extra)")
    }}

    output$Model.like = renderUI({
      paste0(
        "<p>\\[ X = \\alpha \\lambda + \\epsilon \\]</p>",
        "<p><b>Where:</b></p>",
        "<p>(i) \\( X \\in \\mathbb{R}^{",          dim$row ,"\\times", dim$col, "} \\) is the matrix of observed values (mutually independent, given the parameters);</p>",
        "<p>(ii) \\( \\alpha \\in \\mathbb{R}^{",   dim$row, "\\times", dim$fac, "} \\) is the loadings matrix;</p>",
        "<p>(iii) \\( \\lambda \\in \\mathbb{R}^{", dim$fac, "\\times", dim$col, "} \\) is the factor(s) matrix;</p>",
        "<p>(iv) \\( \\epsilon \\in \\mathbb{R}^{", dim$row ,"\\times", dim$col, "} \\) is the stochastic component such that
       \\( \\epsilon_{i,j} \\sim N(0, \\sigma^2_i) \\) independently, and
       \\( \\sigma^2 = (\\sigma^2_1, \\dots, \\sigma^2_{", dim$row, "})' \\).</p>"
      ) |>
        HTML() |>
        withMathJax()
    })

    if (is.null(projeto()$prior)) {
      output$Model.prior = renderUI({
        "No prior on project."
      })
    } else {
      output$Model.prior = renderUI({
        tagList(
          paste0(
            "<p>\\[ \\alpha_{\\bullet, j} \\sim N_{", dim$row, "}(mean\\ \\alpha_j, cov\\ \\alpha_j); \\]</p>",
            "<p>\\[ \\lambda_{i, \\bullet} \\sim N_{", dim$col, "}(mean\\ \\lambda_i, cov\\ \\lambda_i); \\]</p>",
            "<p>\\[ \\sigma^2 \\sim Gama_{", dim$row, "}( shape = ", projeto()$prior$sigma2$shape, ",\\ rate = ", projeto()$prior$sigma2$rate, "); \\]</p>",
            "<p>\\[ var\\ Y = \\bigr( var(Y_{i,j}) \\bigr)_{i,j}\\ , \\ Y = \\alpha, \\lambda; \\]</p>",
            "<p>\\[ mean\\ Y = \\bigr( mean(Y_{i,j}) \\bigr)_{i,j}\\ , \\ Y = \\alpha, \\lambda. \\]</p>"
          ) |>
            HTML() |>
            withMathJax(),
          div(
            style = "text-align: center; margin-top: 1em;",
            actionButton("Model.prior_hyp", "See prior hyperparameters")
          )
        )
      })
    }


    output$Model.info = renderUI({
      paste0(
        "<p>Info: ", projeto()$info, "</p>",
        "<p>Number of groups:\t" , length(projeto()$data$dim$group.sizes), "</p>",
        "<p>Number of factors:\t", n.fac(projeto()), "</p>",
        "<p>Missing proportion:\t", prop.missing(projeto()), "</p>",
        "<p><br></p>",
        "<p>Group sizes</p>"
      ) |>
        HTML() |>
        withMathJax()
    })

    output$Model.info_groups = renderTable({
      df.groups
    }, rownames = FALSE)

    output$Model.label = renderUI({
      tagList(
        fluidRow(column(6, "Loading"), column(6, "Factor level")),
        # fluidRow(
        #   column(6,
        #          div(style = "text-align: center; margin-top: 1em;", "Loading")
        #   ),
        #   column(6,
        #          div(style = "text-align: center; margin-top: 1em;", "Factor level")
        #   )
        # ),
        fluidRow(
          column(2,
            selectInput("Model.loading_index", "X Row", choices = seq_along(projeto()$data$label$loading), selected = 1)
          ),
          column(4,
            selectInput("Model.loading_value", "label", choices = projeto()$data$label$loading, selected = projeto()$data$label$loading[1])
          ),
          column(2,
            selectInput("Model.factor_index", "X Col", choices = seq_along(projeto()$data$label$factor_score), selected = 1)
          ),
          column(4,
            selectInput("Model.factor_value", "label", choices = projeto()$data$label$factor_score, selected = projeto()$data$label$loading[1])
          )
        )
      )
    })
  })


  ### PanelConvergence - General ###

  observeEvent(projeto(), {
    output$Convergence.general_info_time = renderPrint({
      cat("STAN elapsed time (h.)\n")
      elapsed_time_table(projeto()$fit) |> round(2)
    })

    output$Convergence.general_info_args = renderPrint({
      projeto()$fit@stan_args[[1]] |>
        {\(.) cat(
          "STAN arguments",
          "\nChains\t", length(projeto()$fit@stan_args),
          "\nThin\t", .$thin,
          "\nIter\t"  , .$iter,
          "\nWarmup\t", .$warmup
        )}()
    })

    output$Convergence.general_rhat = renderPlot({
      plot_diagnostic(projeto()$fit, "Rhat")
    })

    output$Convergence.general_neff = renderPlot({
      plot_diagnostic(projeto()$fit, "n_eff")
    })

    output$Convergence.general_geweke = renderPlot({
      plot_diagnostic(projeto()$fit, "geweke")
    })
  })


  ### PanelConvergence - Specific - Options ###

  observeEvent(projeto(), {
    if (!is.null(projeto()$data$pred)) {
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
      row = projeto()$data$dim$row
      col = n.fac(projeto())
    } else if (input$Convergence.par == "lambda") {
      row = n.fac(projeto())
      col = projeto()$data$dim$col
    } else if (input$Convergence.par == "sigma2") {
      row = projeto()$data$dim$row
      col = 1
    } else if (input$Convergence.par == "pred")   {
      row = dim(projeto()$summary$pred)[1]
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
    header_col(Convergence.div_par_name(), "#a8f2fe", 8, 12)
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
    combinedchains = get_chains_mcmc(projeto()$fit, par_name)

    Convergence.div_par_name(paste0("Selected parameter: ", par_name))

    output$Convergence.traceplot = renderPlot({
      plot_trace(
        projeto()$fit,
        par, row, col
      )
    })

    output$Convergence.gr_plot = renderPlot({
      if (length(projeto()$fit@stan_args) > 1) {
        coda::gelman.plot(combinedchains)
      }
    })

    output$Convergence.gr_print = renderPrint({
      if (length(projeto()$fit@stan_args) > 1) {
        coda::gelman.diag(combinedchains)
      } else {
        cat("At least two chains are necessary for this diagnose.")
      }
    })

    output$Convergence.geweke = renderPrint({
      coda::geweke.diag(combinedchains)
    })

    output$Convergence.stats = renderTable({
      if (par == "lp__") {
        rstan::extract(projeto()$fit, par = "lp__") |>
          purrr::pluck(1) |>
          {\(.) data.frame(mean = mean(.), median = stats::median(.), sd = stats::sd(.), real = loglik(projeto(), stat = "real"), est_by_mean = loglik(projeto(), stat = "mean"))}() |>
          round(2) |>
          `row.names<-`("")
      } else {
        projeto()$summary[[par]][row, col, ] |>
          as.matrix() |>
          as.data.frame() |>
          {\(.) dplyr::mutate(., V1 = .$V1 |> round(2))}() |>
          as.matrix() |>
          t() |>
          `row.names<-`("")
      }
    })

    density_type = input$Convergence.density_type |> as.vector()

    output$Convergence.density = renderPlot({
      plot_posterior(projeto()$fit, par, row, col, density_type)
    })

    output$Convergence.rhat_neff = renderPrint({
      diagnostic(projeto()$fit) |>
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
        plot_posterior(projeto()$fit, par, row, col, type)
      })
    }
  })


  ###  PanelModel - prior hyperparameter  ###

  output$Model.par_mean = renderPlot({
    req(input$Model.prior_loc)
    req(input$Model.prior_par)
    col = ifelse(input$Model.prior_loc == "all", "all", as.integer(input$Model.prior_loc))
    plot_normal_prior(projeto(), input$Model.prior_par, "mean", col)
  })
  output$Model.par_cov = renderPlot({
    req(input$Model.prior_loc)
    req(input$Model.prior_par)
    col = ifelse(input$Model.prior_loc == "all", "all", as.integer(input$Model.prior_loc))
    plot_normal_prior(projeto(), input$Model.prior_par, "cov", col)
  })

  observeEvent(input$Model.prior_hyp, {
    showModal(modalDialog(
      title = "Prior hyperparameters",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),

      div(style = "height:80vh;",
          fluidPage(
            fluidRow(
              column(4,
                selectInput("Model.prior_par", "Parameter",
                            choices = c("alpha", "lambda"),
                            selected = "alpha")
              ),
              column(4,
                selectInput("Model.prior_loc", "Column",
                            choices = c("all", 1:length(projeto()$prior$alpha$mean)),
                            selected = "all")
              )),
            fluidRow(
              column(4,
                plotOutput("Model.par_mean", height = "80vh")
              ),
              column(8,
                plotOutput("Model.par_cov", height = "80vh")
              )
            )

          ))
    ))
  })


  ###  PanelModel - prior hyperparameter  ###

  observeEvent(input$Model.prior_par, {
    label = if (input$Model.prior_par == "alpha") "Column" else "Row"

    updateSelectInput(
      inputId = "Model.prior_loc",
      label = label,
      choices = c("all", 1:length(projeto()$prior[[input$Model.prior_par]]$mean)),
      selected = input$Model.prior_loc
    )
  })


  ###  PanelModel - label  ###

  observeEvent(input$Model.loading_index, {
    updateSelectInput(session, "Model.loading_value", selected = proj$data$label$loading[as.integer(input$Model.loading_index)])
  })
  observeEvent(input$Model.loading_value, {
    updateSelectInput(session, "Model.loading_index", selected = which(proj$data$label$loading == input$Model.loading_value)[1])
  })

  observeEvent(input$Model.factor_index, {
    updateSelectInput(session, "Model.factor_value", selected = proj$data$label$factor_score[as.integer(input$Model.factor_index)])
  })
  observeEvent(input$Model.factor_value, {
    updateSelectInput(session, "Model.factor_index", selected = which(proj$data$label$factor_score == input$Model.factor_value)[1])
  })


  ###  PanelMaps - UI  ###

  observeEvent(projeto(), {
    if (is.null(projeto()$space)) {
      output$Maps.general = renderUI({
        tagList(
          "No spatial data in project."
        )
      })
    } else {
      output$Maps.general = renderUI({
        tagList(
          fluidRow(header_col("Data map", "#a8f2fe", 8, 12)),

          fluidRow(
            column(10,
              selectInput("Maps.data_type", label = "Column",
                          choices = c("group", "mean", "var"),
                          selected = "mean"),
              plotly::plotlyOutput("Maps.map_data", height = "80vh")
            )
          ),

          fluidRow(header_col("Posterior map", "#a8f2fe", 8, 12)),

          fluidRow(
            column(3,
              selectInput("Maps.post_par", label = "Parameter",
                          choices = c("alpha", "sigma2"),
                          selected = "alpha"),
            ),
            column(3,
              selectInput("Maps.post_col", label = "Column",
                          choices = 1:dim(proj$summary$alpha)[2],
                          selected = 1),
            ),
            column(3,
              selectInput("Maps.post_stat", label = "Satistic",
                          choices = dimnames(proj$summary$alpha)[[3]],
                          selected = dimnames(proj$summary$alpha)[[3]][1]),
            )
          ),
          fluidRow(
            column(10,
              plotly::plotlyOutput("Maps.map_post", height = "80vh")
            )
          ),
          fluidRow(
            column(12,
              uiOutput("Maps.map_sc")
            )
          )
        )
      })

      if (projeto()$prior$semi.conf) {
        output$Maps.map_sc = renderUI({
          tagList(
            fluidRow(header_col("Posterior factor association", "#a8f2fe", 8, 12)),

            fluidRow(
              column(10,
                plotOutput("Maps.map_sc_plot", height = "80vh")
              ),
              column(2,
                checkboxInput("Maps.map_sc_extra", "Only group extra", value = FALSE),
              )
            )
          )
        })
      }


    }
  })


  observeEvent(input$Maps.post_par, {
    if (input$Maps.post_par == "alpha") {
      updateSelectInput(
        session, "Maps.post_stat",
        choices = dimnames(proj$summary$alpha)[[3]],
        selected = dimnames(proj$summary$alpha)[[3]][1]
      )

      updateSelectInput(
        session, "Maps.post_col",
        choices = 1:dim(proj$summary$alpha)[2],
        selected = 1
      )

    } else {
      updateSelectInput(
        session, "Maps.post_stat",
        choices = dimnames(proj$summary$sigma2)[[3]],
        selected = dimnames(proj$summary$sigma2)[[3]][1]
      )
      updateSelectInput(
        session, "Maps.post_col",
        choices = 1,
        selected = 1
      )

    }
  })


  ###  PanelMaps - maps  ###

  output$Maps.map_data = plotly::renderPlotly({
    plot_map_data(projeto(), input$Maps.data_type) |>
      plotly::ggplotly()
  })

  output$Maps.map_post = plotly::renderPlotly({
    plot_map_post(projeto(), input$Maps.post_par, as.numeric(input$Maps.post_col), input$Maps.post_stat) |>
      plotly::ggplotly()
  })

  observeEvent(input$Maps.map_sc_extra, {
    output$Maps.map_sc_plot = renderPlot({
      plot_map_post_factor(projeto(), input$Maps.map_sc_extra, 2)
    })
  })


}

server0
}


#' Shiny App
#'
#' @param proj `fastan::project` object, optional.
#' @param upload.size numeric, default = 500, maximum upload size (in Mb).
#'
#' @return shinyApp
#'
#' @export
shiny = function(proj = NULL, upload.size = 500) {
  options(shiny.maxRequestSize = upload.size*1024^2)
  shinyApp(ui = shiny_ui(), server = shiny_server(proj))
}
