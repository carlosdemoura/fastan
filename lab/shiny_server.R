write_mod_like = function(data) {
  paste("\\begin{equation*}
  X = \\alpha \\lambda + \\epsilon,
  \\end{equation*}

  where:

  \\begin{itemize}
  \\item $X \\in \\mathbb R^{", data$dim$lines ,"\\times", data$dim$columns, "}$ is the matrix of observed values;
  \\item $\\alpha \\in \\mathbb R^{", data$dim$lines, "\\times", data$groups$factors, "}$ is the loadings matrix;
  \\item $\\lambda \\in \\mathbb R ^{", data$groups$factors, "\\times", data$dim$columns, "}$ is the factor(s) matrix;
  \\item $\\epsilon \\in \\mathbb R^{", data$dim$lines ,"\\times", data$dim$columns, "}$ is the stochastic component such that  $\\epsilon_{i,j} \\sim N(0, \\sigma^2_i)$ independents, and $\\sigma^2 = (\\sigma^2_1, \\dots, \\sigma^2_", data$dim$lines, ")'$.
  \\end{itemize}")
}

write_mod_like = function(data) {
  paste0(
    "<p>\\[ X = \\alpha \\lambda + \\epsilon \\]</p>",
    "<p><b>Where:</b></p>",
    "<p>(i) \\( X \\in \\mathbb{R}^{", data$dim$lines ,"\\times", data$dim$columns, "} \\) is the matrix of observed values;</p>",
    "<p>(ii) \\( \\alpha \\in \\mathbb{R}^{", data$dim$lines, "\\times", data$groups$factors, "} \\) is the loadings matrix;</p>",
    "<p>(iii) \\( \\lambda \\in \\mathbb{R}^{", data$groups$factors, "\\times", data$dim$columns, "} \\) is the factor(s) matrix;</p>",
    "<p>(iv) \\( \\epsilon \\in \\mathbb{R}^{", data$dim$lines ,"\\times", data$dim$columns, "} \\) is the stochastic component such that
     \\( \\epsilon_{i,j} \\sim N(0, \\sigma^2_i) \\) independently, and
     \\( \\sigma^2 = (\\sigma^2_1, \\dots, \\sigma^2_{", data$dim$lines, "})' \\).</p>"
  )
}

write_mod_prior = function(data) {
  paste0(
    "<p>\\[ \\lambda_{i, \\bullet} = N_m(0_{", data$dim$columns, "\\times 1}, I_{", data$dim$columns, "}); \\]</p>",
    "<p>\\[ \\sigma^2 \\sim Gama_n(0.1, 0.1). \\]</p>"
  )
}

write_mod_info = function(data) {
  paste0(
    "<p>About: mock data</p>",
    "<p>Date: 09/02/2025</p>",
    "<p>Model type: FA SC</p>"
  )
}


server <- function(input, output, session) {

  shinyDirChoose(
    input,
    'project_folder',
    roots = c(home = '~'),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )


  observeEvent(input$project, {
    load(file = file.path(path_root, "data", input$project, "samp.Rdata"), envir = globalenv())
    load(file = file.path(path_root, "data", input$project, "data.Rdata"), envir = globalenv())
    load(file = file.path(path_root, "data", input$project, "inf_m.Rdata"), envir = globalenv())
    load(file = file.path(path_root, "data", input$project, "inf_df.Rdata"), envir = globalenv())

    output$lambdas = renderPlotly({
      plot_lambda(inf_df, data, T) %>%
        ggplotly()
    })

    output$sigma2 = renderPlotly({
      plot_hpd(inf_df, "sigma2", 1) %>%
        ggplotly()
    })

    output$alpha_hpd <- renderPlotly({
      plot_hpd(inf_df, "alpha", as.integer(input$alpha_row)) %>%
        ggplotly()
    })

    output$contrast <- renderPlotly({
      plot_contrast_gg(inf_m, stat = as.character(input$contrast_type)) %>%
        ggplotly()
    })

    output$mod_likelihood <- renderUI({
      withMathJax(HTML(write_mod_like(data)))
    })

    output$mod_prior <- renderUI({
      withMathJax(HTML(write_mod_prior(data)))
    })

    output$mod_info <- renderUI({
      withMathJax(HTML(write_mod_info(data)))
    })

    output$expl_density <- renderPlot({
      plot_density_expl(data$x)
    })
  })



  observeEvent(input$select_param, {
    loc1 = as.integer(input$diag_param_loc1)
    loc2 = as.integer(input$diag_param_loc2)

    if (input$diag_param == "LP") {
      samp <<- stan.samp$lp__
    } else if (input$diag_param == "alpha") {
      samp <<- stan.samp$alpha[,loc2,loc1]
    } else if (input$diag_param == "lambda") {
      samp <<- stan.samp$lambda[,loc1,loc2]
    } else if (input$diag_param == "sigma2") {
      samp <<- stan.samp$sigma2[,loc2]
    }

    combinedchains = mcmc.list(as.mcmc(matrix(samp[1:250], ncol = 1)),
                               as.mcmc(matrix(samp[251:500], ncol = 1)))

    output$diag_traceplot <- renderPlot({
      traceplot(combinedchains)
    })

    output$diag_density <- renderPlot({
      hist(samp, probability = T)
      lines(density(samp), lwd  = 3, col = "red")
    })

    output$diag_gr <- renderPlot({
      gelman.plot(combinedchains)
    })

    output$diag_gr_print <- renderPrint({
      gelman.diag(combinedchains)
    })

    output$diag_geweke_print1 <- renderPrint({
      geweke.diag(combinedchains)[[1]]
    })

    output$diag_geweke_print2 <- renderPrint({
      geweke.diag(combinedchains)[[2]]
    })

    output$diag_param_selected <- renderUI({
      st = "font-weight: bold; padding: 10px; color: black; font-size: 30px;"
      if (input$diag_param == "LP") {
        tags$div("Log-likelihood", style = st)
      } else if (input$diag_param == "alpha") {
        tags$div(paste0("alpha[", loc1, ",", loc2, "]"), style = st)
      } else if (input$diag_param == "lambda") {
        tags$div(paste0("lambda[", loc2, ",", loc1, "]"), style = st)
      } else if (input$diag_param == "sigma2") {
        tags$div(paste0("sigma^2[", loc1, "]"), style = st)
      }
    })

  })

  observeEvent(input$diag_density_type, {
    if (length(input$diag_density_type) == 1) {
      if (input$diag_density_type == "1") {
        output$diag_density <- renderPlot({
          hist(samp, probability = T)
        })
      } else {
        output$diag_density <- renderPlot({
          plot(density(samp), lwd = 2)
        })
      }
    } else {
      output$diag_density <- renderPlot({
        hist(samp, probability = T)
        lines(density(samp), lwd  = 3, col = "red")
      })
    }
  })

  output$my_table <- renderDataTable({
    datatable(
      cheeses,
      rownames = FALSE,
      options = list(pageSize = 10),
      selection = "single"
    )
  })

  output$worldmap <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addPolygons(
        data = worldmap,
        color = "transparent",
        popup = ~name,
        layerId = ~name
      ) %>%
      setView(lng = -55, lat = -15, zoom = 4)
  })

  observeEvent(input$mark_stations, {
    leafletProxy("worldmap") %>%
      addMarkers(lng = -47, lat = -15, layerId = "xxx")
  })

}


worldmap = st_read(file.path(path_root, "data/worldmap/"))

estacoes = read.csv(file = file.path(path_root, "data", "estacoes.csv"))


shiny = function() {
  shinyApp(ui = ui, server = server)
}
