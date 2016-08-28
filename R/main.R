  #' execute decision tree with metrics.
  #'
  #' @param data_train  dataset of train.
  #' @param data_test   dataset of test.
  #' @param metrics     types:MID,RMI....
  #' @param label_class class of dataset
  #' @import jsonlite
  #' @import downloader
  #' @export
  monotonicTree <- function(data_train=train,data_test = test,metrics = metrics,label_class = label_class){
    
  #Librerias y archivos
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package needed for this function to work. Please install it.",call. = FALSE)
  }
  if (!requireNamespace("downloader", quietly = TRUE)) {
    stop("downloader package needed for this function to work. Please install it.",call. = FALSE)
  }
  else{
    # data_train <- df_esl
    # data_test <- df_esl
    # label_class <- "Out"
    # metrics <- "MID"
    # monotonicTree(iris,iris,"MID","Species")
    
     # 1 Descargar ficheros jar
    descargar_jar()
    
    # 2 Crear dataset train keel
    path_train <- create_dataset(data_train,0,label_class)
    
    # 3 Crear dataset test keel
    path_test <- create_dataset(data_test,1,label_class)
    
    # 4 Crear fichero config
    create_config(path_train,path_test)
    
    # Pruned,confidence,importante,leaf,
    insert_attributes(TRUE,0.25,1,2,metrics)
    
    # 5 Ejecutar código
  
    switch(metrics, 
       MID={
         jar <- "MID.jar"
       },
       RMI={
         jar <- "RMI.jar"
       },
       {
         cat("Metrics no valid\n")
         return(0)
       }
    )
    
    jar <- shQuote(gsub("/","\\\\",file.path(system.file(package = "monotonicTree"), "download",jar),perl=TRUE))
    config <- shQuote(gsub("/","\\\\",file.path(system.file(package = "monotonicTree"),"files","config0.txt"),perl=TRUE))
    
    ejecutar(jar,config)
    
    # 6º Mostrar Resultado
    resultado <- ver_resultados()
    
    # 7º Borrra ficheros Jar
    unlink(file.path(system.file(package = "monotonicTree"), "download"),force = TRUE,recursive = TRUE)
    unlink(file.path(system.file(package = "monotonicTree"), "files"),force = TRUE,recursive = TRUE)    
    
    resultado$decision_tree <- NULL
    cat("\n[ok] Finished\n")
    return(resultado)
  }
  
}



