library(shiny)

# Lista de datasets
df_list <- list(
  mtcars = mtcars,
  iris = iris,
  airquality = airquality
)

ui <- navbarPage(
  "Tabs Condicionais com navbarPage",
  id = "nav",

  # Aba 1: Dados
  tabPanel("Dados",
           sidebarLayout(
             sidebarPanel(
               selectInput("dataset", "Escolha o dataset:",
                           choices = names(df_list))
             ),
             mainPanel(
               tableOutput("tabela")
             )
           )
  ),

  # Aba 2: Análise (só com 'hp')
  tabPanel("Análise",
           verbatimTextOutput("summary")
  ),

  # Aba 3: Extras (só com 'cyl')
  tabPanel("Extras",
           verbatimTextOutput("cyl_info")
  ),

  # JavaScript para desabilitar abas
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleTab', function(data) {
        var tab = $('a[data-value=\"' + data.tabName + '\"]');
        if (data.disable) {
          tab.addClass('disabled');
          tab.css('pointer-events', 'none');
          tab.css('color', 'gray');
        } else {
          tab.removeClass('disabled');
          tab.css('pointer-events', 'auto');
          tab.css('color', '');
        }
      });
    "))
  )
)

server <- function(input, output, session) {
  # Dataset reativo
  selected_df <- reactive({
    df_list[[input$dataset]]
  })

  # Renderiza tabela principal
  output$tabela <- renderTable({
    head(selected_df())
  })

  # Aba Análise: requer 'hp'
  output$summary <- renderPrint({
    req("hp" %in% colnames(selected_df()))
    summary(selected_df()$hp)
  })

  # Aba Extras: requer 'cyl'
  output$cyl_info <- renderPrint({
    req("cyl" %in% colnames(selected_df()))
    table(selected_df()$cyl)
  })

  # Observa se deve habilitar/desabilitar abas
  observe({
    cols <- colnames(selected_df())

    # Análise
    has_hp <- "hp" %in% cols
    session$sendCustomMessage("toggleTab", list(
      tabName = "Análise",
      disable = !has_hp
    ))
    if (input$nav == "Análise" && !has_hp) {
      updateNavbarPage(session, "nav", selected = "Dados")
    }

    # Extras
    has_cyl <- "cyl" %in% cols
    session$sendCustomMessage("toggleTab", list(
      tabName = "Extras",
      disable = !has_cyl
    ))
    if (input$nav == "Extras" && !has_cyl) {
      updateNavbarPage(session, "nav", selected = "Dados")
    }
  })
}

shinyApp(ui, server)














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
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
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
    fluidRow(header_col("Likelihood", "#a8f2fe", "8vh", 6), header_col("Prior", "#a8f2fe", "8vh", 6)),
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
             div(style = "margin-top: 1em; display: table; margin-left: auto; margin-right: auto;",
                 tableOutput("Convergence.stats")
             )
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


  ###################
  ######  Maps ######
  ###################
  PanelMaps = tabPanel(
    title = "Maps",

    fluidRow(header_col("Maps", "#87C2CC", "12vh", 12)),

    fluidRow(
      column(12,
             uiOutput("Maps.general")
      )
    )

  )

  navbarPage(
    title = "fastan app",
    id = "nav",
    PanelModel, PanelConvergence, PanelInference, PanelMaps,
    tags$head(
      tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleTab', function(data) {
        var tab = $('a[data-value=\"' + data.tabName + '\"]');
        if (data.disable) {
          tab.addClass('disabled');
          tab.css('pointer-events', 'none');
          tab.css('color', 'gray');
        } else {
          tab.removeClass('disabled');
          tab.css('pointer-events', 'auto');
          tab.css('color', '');
        }
      });
    "))
    )
  )
}


#' Shiny server
#'
#' @param proj .
#' @param input .
#' @param output .
#' @param session .
#'
#' @importFrom coda gelman.plot gelman.diag geweke.diag
#' @import dplyr
#' @importFrom gridExtra grid.arrange
#' @importFrom plotly event_data ggplotly plotlyOutput renderPlotly
#' @import purrr
#' @importFrom rstan extract
#' @import shiny
#' @importFrom stats median sd
#' @importFrom zip zipr
server = function(proj = NULL, input, output, session) {

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
    project = reactive({
      req(project_rv())
    })

    real = reactive(!is.null(project()$data$real))
    real_pred = reactive("real" %in% dimnames(project()$summary$pred)[[3]])
    stat = reactive(c("mean") |> {\(.) if (real()) c(., "real") else .}())

    output$Model.export = downloadHandler(
      filename = function() {
        paste0("fastanExport-", format(Sys.time(), "%Y_%m_%d-%Hh%Mm%Ss"), ".zip")
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

      if (real() | real_pred()) {
        output$Inference.accuracy = renderUI({
          tagList(
            fluidRow(header_col("Accuracy", "#a8f2fe", "8vh", 12)),

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

        output$Inference.accuracy_table = renderTable(accuracy(project()$summary) |> round(4), rownames = T)

        output$Inference.bias = renderPlot({
          if (real()) {
            bias = list()
            for (param in c("all", names(project()$summary))) {
              bias[[param]] = plot_bias(project(), param)
            }
            gridExtra::grid.arrange(grobs = bias, ncol=2)
          } else if (real_pred()) {
            plot_bias(project(), "pred")
          }
        })
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
      coor = plotly::event_data("plotly_click", source = "Inference.pred_contrast_source")
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

      df.groups =
        data.frame(
          group = project()$data$label$group |> as.character(),
          size = project()$data$dim$group.sizes
        ) |>
        {\(.)
          dplyr::mutate(.,
                        starts = fiat_groups_limits(.$size)[[1]] |> as.integer(),
                        ends   = fiat_groups_limits(.$size)[[2]] |> as.integer()
          )
        }()

      if (!is.null(project()$prior)) { if (project()$prior$semi.conf) {
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

      if (is.null(project()$prior)) {
        output$Model.prior = renderUI({
          "No prior on project."
        })
      } else {
        output$Model.prior = renderUI({
          tagList(
            paste0(
              "<p>\\[ \\alpha_{\\bullet, j} \\sim N_{", dim$row, "}(mean\\ \\alpha_j, cov\\ \\alpha_j); \\]</p>",
              "<p>\\[ \\lambda_{i, \\bullet} \\sim N_{", dim$col, "}(mean\\ \\lambda_i, cov\\ \\lambda_i); \\]</p>",
              "<p>\\[ \\sigma^2 \\sim Gama_{", dim$row, "}( shape = ", project()$prior$sigma2$shape, ",\\ rate = ", project()$prior$sigma2$rate, "); \\]</p>",
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
          "<p>Info: ", project()$info, "</p>",
          "<p>Number of groups:\t" , project()$data$dim$group.n, "</p>",
          "<p>Number of factors:\t", n.fac(project()), "</p>",
          "<p>Missing proportion:\t", prop.missing(project()), "</p>",
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
                   selectInput("Model.loading_index", "X Row", choices = seq_along(project()$data$label$loading), selected = 1)
            ),
            column(4,
                   selectInput("Model.loading_value", "label", choices = project()$data$label$loading, selected = project()$data$label$loading[1])
            ),
            column(2,
                   selectInput("Model.factor_index", "X Col", choices = seq_along(project()$data$label$factor_level), selected = 1)
            ),
            column(4,
                   selectInput("Model.factor_value", "label", choices = project()$data$label$factor_level, selected = project()$data$label$loading[1])
            )
          )
        )
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
        if (length(project()$fit@stan_args) > 1) {
          coda::gelman.plot(combinedchains)
        }
      })

      output$Convergence.gr_print = renderPrint({
        if (length(project()$fit@stan_args) > 1) {
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
          rstan::extract(project()$fit, par = "lp__") |>
            purrr::pluck(1) |>
            {\(.) data.frame(mean = mean(.), median = stats::median(.), sd = stats::sd(.), real = loglik(project(), stat = "real"), est_by_mean = loglik(project(), stat = "mean"))}() |>
            round(2) |>
            `row.names<-`("")
        } else {
          project()$summary[[par]][row, col, ] |>
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


    ###  PanelModel - prior hyperparameter  ###

    output$Model.par_mean = renderPlot({
      req(input$Model.prior_loc)
      req(input$Model.prior_par)
      col = ifelse(input$Model.prior_loc == "all", "all", as.integer(input$Model.prior_loc))
      plot_normal_prior(project(), input$Model.prior_par, "mean", col)
    })
    output$Model.par_cov = renderPlot({
      req(input$Model.prior_loc)
      req(input$Model.prior_par)
      col = ifelse(input$Model.prior_loc == "all", "all", as.integer(input$Model.prior_loc))
      plot_normal_prior(project(), input$Model.prior_par, "cov", col)
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
                                   choices = c("all", 1:length(project()$prior$alpha$mean)),
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
        choices = c("all", 1:length(project()$prior[[input$Model.prior_par]]$mean)),
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
      updateSelectInput(session, "Model.factor_value", selected = proj$data$label$factor_level[as.integer(input$Model.factor_index)])
    })
    observeEvent(input$Model.factor_value, {
      updateSelectInput(session, "Model.factor_index", selected = which(proj$data$label$factor_level == input$Model.factor_value)[1])
    })


    ###  PanelMaps - UI  ###

    observeEvent(project(), {
      if (is.null(project()$space)) {
        output$Maps.general = renderUI({
          tagList(
            "No spatial data in project."
          )
        })
      } else {
        output$Maps.general = renderUI({
          tagList(
            fluidRow(header_col("Data map", "#a8f2fe", "8vh", 12)),

            fluidRow(
              column(10,
                     selectInput("Maps.data_type", label = "Column",
                                 choices = c("group", "mean", "var"),
                                 selected = "mean"),
                     plotly::plotlyOutput("Maps.map_data", height = "80vh")
              )
            ),

            fluidRow(header_col("Posterior map", "#a8f2fe", "8vh", 12)),

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
            )
          )
        })
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

    output$Maps.map_data = renderPlotly({
      plot_map_data(project(), input$Maps.data_type) |>
        plotly::ggplotly()
    })

    output$Maps.map_post = renderPlotly({
      plot_map_post(project(), input$Maps.post_par, as.numeric(input$Maps.post_col), input$Maps.post_stat) |>
        plotly::ggplotly()
    })


    ###  Block unblock tabs  ###

    observe({
      # Diagnostic
      diag <- any(c("diagnostic", "fit") %in% names(project()))
      session$sendCustomMessage("toggleTab", list(
        tabName = "PanelDiagnostic",
        disable = !diag
      ))

      # Summary
      smry <- "summary" %in% names(project())
      session$sendCustomMessage("toggleTab", list(
        tabName = "PanelInference",
        disable = !smry
      ))
    })


  }

  server0
}


#' Shiny App
#'
#' @param proj .
#' @param upload.size .
#'
#' @export
shiny = function(proj = NULL, upload.size = 500) {
  options(shiny.maxRequestSize = upload.size*1024^2)
  shinyApp(ui = ui(), server = server(proj))
}
