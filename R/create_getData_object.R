create_getData_object <- function() {
  obj <- list()

  obj$metrics <- data.frame(metric = character(),
                            generationGrowing = character(),
                            span = character(),
                            rep = character(),
                            nInd = character(),
                            nFam = character(),
                            scheme = double(),
                            trait = double(),
                            value = double(),
                            valueS = double()
                            )

  obj$status <- data.frame(module = character(),
                           analysisId = character(),
                           analysisIdName=character())

  return(obj)
}
