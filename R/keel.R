
#' execute algorithm
#'
#' @param jar      jar path.
#' @param config   config path.
ejecutar <- function(jar,config){
  cat("[6] Run Algorithm\n")
  ejecutar <- paste("java -jar",jar,config,sep=" ")
  # cat(ejecutar)
  system(ejecutar,intern = TRUE,show.output.on.console = FALSE)
  #show.output.on.console = FALSE
  cat("\nFIN ejecucion\n")
}

#' create new dataset for use in keel
#'
#' @param dataset  dataset to convert in keel format.
#' @param train   if dataset is train (train=0) or test (train=1).
#' @param label_class   class attribute of dataset
#' @return file path
#CREAR DATASET CON FORMATO KEEL
create_dataset <- function(dataset,train,label_class){
# train <- 0
# dataset <- df_esl
# label_class <- "Out"

  if(train == 0){
    cat("[4] Create dataset for Train\n")
    name <- "dataset_monotonic-tra.dat"
  }else{
    cat("[5] Create dataset for Test\n")
    name <- "dataset_monotonic-test.dat"
  }
  name_file <- file.path(system.file(package = "monotonicTree"),"files",name)


  # label_class <- "Species"
  output <- ""
  input <-NULL
  string <- "@relation dataset monotonic"
  write(string,file=name_file,append = TRUE)

  for(i in 1:length(names(dataset))){

    if(names(dataset)[i]!=label_class){
      input <- c(input,names(dataset)[i])
    }
    else{
      output <- names(dataset)[i]
    }
    line <- NULL
    
    if(names(dataset)[i] == label_class && (class(dataset[1,i])=="numeric" || class(dataset[1,i])=="integer")){
      head <- paste("@attribute",names(dataset)[i],sep=" ")
      level <- paste(seq(min(dataset[,i]),max(dataset[,i])),collapse = ",")
      interval <- paste("{",level,"}",sep = "")
      line <- paste(head,interval,sep =" ")
    }
    
    else if(class(dataset[1,i])=="numeric"){
      
      head <- paste("@attribute",names(dataset)[i],"real",sep=" ")
      interval <- paste("[",min(dataset[,i]),", ",max(dataset[,i]),"]",sep = "")
      line <- paste(head,interval,sep =" ")
      
    }else if(class(dataset[1,i])=="integer"){
      
      head <- paste("@attribute",names(dataset)[i],"integer",sep=" ")
      interval <- paste("[",min(dataset[,i]),", ",max(dataset[,i]),"]",sep = "")
      line <- paste(head,interval,sep =" ")
      
    }else if(class(dataset[1,i])=="factor"){
      head <- paste("@attribute",names(dataset)[i],sep=" ")
      level <- paste(levels(dataset[,5]),collapse=", ")
      interval <- paste("{",level,"}",sep = "")
      line <- paste(head,interval,sep =" ")
    }
    if(!is.null(line)){
      write(line,file=name_file,append = TRUE)  
    }
    
  }

  label_input <- paste(input,collapse=", ")
  line <- paste("@inputs ",label_input ,sep = "")
  write(line,file=name_file,append = TRUE)
  line <- paste("@outputs ",output ,sep = "")
  write(line,file=name_file,append = TRUE)
  write("@data",file=name_file,append = TRUE)
  write.table(dataset, file = name_file, append = TRUE, quote = FALSE, sep = ", ",eol = "\n", na = "NA", dec = ".", row.names = FALSE,col.names = FALSE)

  return(name_file)
}

#CREAR FICHERO CONFIG PARA KEEL
#' create config file for use in keel
#'
#' @param train dataset train path
#' @param test dataset test path
create_config <- function(train,test){

  name <- "config0.txt"
  name_file <- file.path(system.file(package = "monotonicTree"),"files",name)

  string <- "algorithm = Monotonic Induction Decision"
  write(string,file=name_file,append = TRUE)
  input <- paste("inputData =",shQuote(train),shQuote(test),shQuote(test),sep=' ')
  write(input,file=name_file,append = TRUE)

  output_train <- file.path(system.file(package = "monotonicTree"),"files","result0.tra")
  output_test <- file.path(system.file(package = "monotonicTree"),"files","result0.tst")
  output_resultado <- file.path(system.file(package = "monotonicTree"),"files","result0e0.txt")
  output_json <- file.path(system.file(package = "monotonicTree"),"result0e0.json")

  output <- paste("outputData =",shQuote(output_train),shQuote(output_test),shQuote(output_resultado),shQuote(output_json),sep=' ')
  write(output,file=name_file,append = TRUE)

}
#' insert attributes necessary according measured
#'
#' @param pruned if pruned tree (pruned=TRUE) or not pruned tree (pruned=FALSE)
#' @param confidence value of confidence
#' @param importance double for MID measure value of importance [0,1]
#' @param leaf number of instances for leaf
#' @param metric string value of measure
#' @param porcentage double [0,1] value for porcentage of pruning. 0 mean model 100% monotonic, 1 mean no pruning
insert_attributes <- function(pruned,confidence,importance = 10,leaf,metric,porcentage){


  name_file <- file.path(system.file(package = "monotonicTree"),"files","config0.txt")

  if(class(pruned) != "logical"){
    stop("PRUNED not logical value",call. = FALSE)
  }
  
  if(confidence > 1) confidence <- 1
  if(confidence < 0) confidence <- 0
  write(paste("confidence = ",as.character(confidence),sep=""),file=name_file,append = TRUE)
  
  if(metric == "MID"){
    write(paste("relative_importance_monotonicity =",as.character(importance),sep=""),file=name_file,append = TRUE)
  }
  write(paste("instancesPerLeaf = ",leaf,sep=""),file=name_file,append = TRUE)
  
  if(porcentage > 1) porcentage <- 1
  if(porcentage < 0) porcentage <- 0
  write(paste("porcentage_antimonotonic = ",as.character(porcentage),sep=""),file=name_file,append = TRUE)
}

#' download files necessary for the package work
descargar_jar <- function(){
  # cat("==================================================================\n")
  cat("[1] Create folder\n")
  dir.create(file.path(system.file(package = "monotonicTree"), "download"),showWarnings = FALSE)
  dir.create(file.path(system.file(package = "monotonicTree"), "files"),showWarnings = FALSE)
  cat("[2] Download JAR\n\n")
  path <- file.path(system.file(package = "monotonicTree"), "download","JarFiles.zip")
  downloader::download(url="https://github.com/raulopez/monotonicTree/raw/master/jar/JarFiles.zip",destfile = path, mode ="wb")
  cat("[3] Unzip files\n")
  unzip(zipfile = path, exdir = file.path(system.file(package = "monotonicTree"), "download"))
  # cat("==================================================================\n")
  
}
