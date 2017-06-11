  #' execute decision tree with metrics.
  #'
  #' @param data_train  dataset of train.
  #' @param data_test   dataset of test.
  #' @param metrics     types:MID,RMI....
  #' @param label_class class of dataset
  #' @import jsonlite
  #' @import downloader
  #' @export
  monotonicTree <- function(data_train= train,data_test = test,metrics = metrics,label_class = label_class, 
                            pruned = TRUE, confidence = 0.25, pruning_factor = 0,
                            importance_monotonicity = 10){
    
  #Check library and input format
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package needed for this function to work. Please install it.",call. = FALSE)
  }
  if (!requireNamespace("downloader", quietly = TRUE)) {
    stop("downloader package needed for this function to work. Please install it.",call. = FALSE)
  }
  if(class(data_train) != "data.frame"){
    stop("data_train must be data.frame",call. = FALSE)
  }
  if(class(data_test) != "data.frame"){
      stop("data_train must be data.frame",call. = FALSE)
  }
    
     # 1 Download jar file
    descargar_jar()
    
    # 2 Create dataset train keel
    path_train <- create_dataset(data_train,0,label_class)
    
    # 3 Create dataset test keel
    path_test <- create_dataset(data_test,1,label_class)
    
    # 4 Create config file
    create_config(path_train,path_test)
    
    # Pruned,confidence,importante,leaf,
    
    insert_attributes(pruned,confidence,importance_monotonicity,2,metrics,pruning_factor)
    
    # 5 Execute
    switch(metrics, 
       MID={
         jar <- "MID.jar"
       },
       RMI={
         jar <- "RMI.jar"
       },
       RSD={
         jar <- "RSD.jar"
         cat(jar)
       },
       RGD={
         jar <- "RGD.jar"
       },
       {
         stop("Metrics no valid\n",call. = FALSE)
       }
    )
    path_jar <- shQuote(gsub("/","\\\\",file.path(system.file(package = "monotonicTree"), "download",jar),perl=TRUE))
    config <- shQuote(gsub("/","\\\\",file.path(system.file(package = "monotonicTree"),"files","config0.txt"),perl=TRUE))
   
    execute(path_jar,config)
    
    # 6º return result
    resultado <- result()
    
    # 7º Borrra ficheros Jar
     unlink(file.path(system.file(package = "monotonicTree"), "download"),force = TRUE,recursive = TRUE)
     unlink(file.path(system.file(package = "monotonicTree"), "files"),force = TRUE,recursive = TRUE)    
    
    resultado$decision_tree <- NULL
    cat("\n[ok] Finished\n")
    return(resultado)

}

loadDatasetMonotonic <- function(name = "era"){
  # cat("==================================================================\n")
  cat("[1] Create folder\n")
  dir.create(file.path(system.file(package = "monotonicTree"), "dataset"),showWarnings = FALSE)
  cat("[2] Download Dataset\n\n")
  path <- file.path(system.file(package = "monotonicTree"), "dataset","dataset.zip")
  downloader::download(url="https://github.com/raulopez/monotonicTree/raw/master/data/dataset.zip",destfile = path, mode ="wb")
  cat("[3] Unzip files\n")
  unzip(zipfile = path, exdir = file.path(system.file(package = "monotonicTree"), "dataset"))
  
  datasetname <- "era"
  dataset <- load(file.path(system.file(package = "monotonicTree"), "dataset","esl.rdata"))
  
  
}

