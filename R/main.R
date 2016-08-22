#' execute decision tree with metrics.
#'
#' @param data_train  dataset of train.
#' @param data_test   dataset of test.
#' @param metrics     types:MID,RMI....
#' @param label_class class of dataset
#' @import jsonlite
#' @export
monotonicTree <- function(data_train=train,data_test = test,metrics = metrics,label_class = label_class){

  #Librerias y archivos
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package needed for this function to work. Please install it.",call. = FALSE)
  }
  else{
    # data_train <- iris
    # data_test <- iris
    # label_class <- "Species"
    # metrics <- "MID"
    # 1 Descargar el JAR de algoritmo
    #FALTA

    dir.create(file.path(system.file(package = "monotonicTree"), "download"))
    download(url="http://github.com/raulopez/monotonicTree/tree/master/jar/MID.jar",destfile = paste0(getwd(),"/download/MID.jar"),quiet = FALSE,mode = "w")


    # 2 Crear dataset train keel
    path_train <- create_dataset(data_train,0,label_class)

    # 3 Crear dataset test keel
    path_test <- create_dataset(data_test,1,label_class)

    # 4 Crear fichero config
    create_config(path_train,path_test)

    # Pruned,confidence,importante,leaf,
    insert_attributes(TRUE,0.25,1,2,metrics)

    # 5 Ejecutar código

    jar <- shQuote(gsub("/","\\\\",paste(getwd(),"jar/MID.jar",sep = "/"),perl=TRUE))
    config <- shQuote(gsub("/","\\\\",paste(getwd(),"config0.txt",sep = "/"),perl=TRUE))
    # ejecutar(jar,config)

    # 6º Mostrar Resultado
    resultado <- ver_resultados()

    # 7º Borrra ficheros Jar
    unlink(paste0(getwd(),"/download"),force = TRUE,recursive = TRUE)

    resultado$decision_tree <- NULL
    return(resultado)
  }

}



