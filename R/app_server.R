#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
options(shiny.maxRequestSize=8000*1024^2) # 8GB or 8,000Mb
app_server <- function(input, output, session) {

  data <- reactiveVal()
  observe({data <- cgiarBase::create_getData_object()})
  # Your application server logic


  # Your application server logic
  mod_upload_server("upload_1", data=data)
  mod_dashboard_server("dashboard_1", data=data)
}
