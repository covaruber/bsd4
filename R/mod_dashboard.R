#' dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(


    tags$br(),

    mainPanel( width = 12,
               tabsetPanel( id=ns("tabsMain"),
                            type = "tabs",
                            # tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                            tabsetPanel(
                              tabPanel("Dashboard", icon = icon("file-image"),

                                       column(width=4,
                                              selectInput(ns("trait3Rgg"), "Trait to visualize regression over years.", choices = NULL, multiple = FALSE),
                                              selectInput(ns("param3Rgg"), "Parameters to visualize.", choices = NULL, multiple = TRUE),
                                              numericInput(ns("cohort"), label = "Cohort in each scheme to consider", value = 1, min=1, step=1),
                                       ),
                                       column(width=6,
                                              DT::DTOutput(ns("metricsRgg")),
                                       ),
                                       column(width=12,
                                              plotly::plotlyOutput(ns("plotPredictionsCleanOut")),
                                       ),

                              ),

                            )
                            # )# end of output panel
               )) # end mainpanel

  )
}

#' dashboard Server Functions
#'
#' @noRd
mod_dashboard_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    # output$plotDataDependencies <- shiny::renderPlot({ dependencyPlot() })
    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ##############################################################################################
    ##############################################################################################
    ##############################################################################################

    ##############################################################################################
    ##############################################################################################
    ##############################################################################################
    ##  actual run
    ## render result of "run" button click

    ##  render plots
    observeEvent(c(data() ), { # update trait
      req(data())
      dtRgg <- data()
      dtRgg <- dtRgg$metrics
      traitRggInput <- unique(dtRgg$trait)
      updateSelectInput(session, "trait3Rgg", choices = traitRggInput)
    })

    observeEvent(c(data() ), { # update trait
      req(data())
      dtRgg <- data()
      dtRgg <- dtRgg$metrics
      paramRggInput <- unique(dtRgg$metric)
      updateSelectInput(session, "param3Rgg", choices = paramRggInput)
    })

    output$metricsRgg <-  DT::renderDT({

      req(data())
      req(input$trait3Rgg)
      req(input$param3Rgg)
      req(input$cohort)

      dtRgg <- data()$metrics
      dtRgg <- dtRgg[which(dtRgg$trait %in% input$trait3Rgg),] # only traits that have been QA
      dtRgg <- dtRgg[which(dtRgg$metric %in% input$param3Rgg),] # only traits that have been QA

      dtRggSplit <- split(dtRgg, dtRgg$scheme)
      gens <- lapply(dtRggSplit, function(x){unique(x$generationGrowing)})

      for(iGen in 1:length(gens)){
        dtRggSplit[[iGen]] <- dtRggSplit[[iGen]][which(dtRggSplit[[iGen]]$generationGrowing %in% gens[[iGen]][input$cohort]),]
      }

      tmp <- do.call(rbind, dtRggSplit)
      tmp <- unique(tmp[,c("trait","scheme","generationGrowing")])
      numeric.output <- c("span","MEAN", "SE")
      rownames(tmp) <- NULL
      # DT::formatRound(
      DT::datatable(tmp, extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
      )
      # , numeric.output)
    }, server = FALSE)

    output$plotPredictionsCleanOut <- plotly::renderPlotly({ # update plot
      req(data())
      req(input$trait3Rgg)
      req(input$param3Rgg)
      req(input$cohort)
      library(ggplot2)
      library(plyr)

      dtRgg <- data()$metrics
      dtRgg <- dtRgg[which(dtRgg$trait %in% input$trait3Rgg),] # only traits that have been QA
      dtRgg <- dtRgg[which(dtRgg$metric %in% input$param3Rgg),] # only traits that have been QA

      dtRggSplit <- split(dtRgg, dtRgg$scheme)
      gens <- lapply(dtRggSplit, function(x){unique(x$generationGrowing)})

      for(iGen in 1:length(gens)){
        dtRggSplit[[iGen]] <- dtRggSplit[[iGen]][which(dtRggSplit[[iGen]]$generationGrowing %in% gens[[iGen]][input$cohort]),]
      }

      tmp <- do.call(rbind, dtRggSplit)
      # tmp = plyr::ddply(tmp0,c("metric","span","generationGrowing", "scheme"), summarize,
      #             MEAN = mean(value),
      #             SE = sd(value)/sqrt(10))

      p1 <- ggplot(tmp,aes(x=span,y=MEAN,color=scheme))+
        geom_smooth(se=FALSE) +
        # geom_line(size=1)+
        geom_ribbon(aes(x=span,ymin=MEAN-SE,ymax=MEAN+SE,
                        fill=scheme),alpha=0.2,linetype=0)+
        facet_wrap(~metric, scales = "free_y") +
        guides(alpha=FALSE)+ # theme(legend.position = "none")+
        ylab("Trait value") + ggtitle(input$trait3Rgg) +
        xlab("Year of breeding")
      plotly::ggplotly(p1)

    })



  })
}

## To be copied in the UI
# mod_dashboard_ui("dashboard_1")

## To be copied in the server
# mod_dashboard_server("dashboard_1")
