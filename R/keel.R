
#' execute algorithm
#'
#' @param jar      jar path.
#' @param config   config path.
ejecutar <- function(jar,config){

  ejecutar <- paste("java -jar",jar,config,sep=" ")
  cat(ejecutar)
  system(ejecutar,intern = FALSE)
  #show.output.on.console = FALSE
}

#' create new dataset for use in keel
#'
#' @param dataset  dataset to convert in keel format.
#' @param train   if dataset is train (train=0) or test (train=1).
#' @param label_class   class attribute of dataset
#' @return file path

#CREAR DATASET CON FORMATO KEEL
create_dataset <- function(dataset,train,label_class){

  if(train == 0){
    cat("Create dataset for Train\n")
    name <- "dataset_monotonic-tra.dat"
  }
  else{
    cat("Create dataset for Test\n")
    name <- "dataset_monotonic-test.dat"
  }
  name_file <- file.path(system.file(package = "monotonicTree"),name)


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
    if(class(dataset[1,i])=="numeric"){
      head <- paste("@attribute",names(dataset)[i],"real",sep=" ")
      interval <- paste("[",min(dataset[,i]),", ",max(dataset[,i]),"]",sep = "")
      line <- paste(head,interval,sep =" ")
    }
    else if(class(dataset[1,i])=="factor"){
      head <- paste("@attribute",names(dataset)[i],sep=" ")
      level <- paste(levels(dataset[,5]),collapse=", ")
      interval <- paste("{",level,"}",sep = "")
      line <- paste(head,interval,sep =" ")
    }

    write(line,file=name_file,append = TRUE)
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
  name_file <- file.path(system.file(package = "monotonicTree"),name)

  string <- "algorithm = Monotonic Induction Decision"
  write(string,file=name_file,append = TRUE)
  input <- paste("inputData =",shQuote(train),shQuote(test),shQuote(test),sep=' ')
  write(input,file=name_file,append = TRUE)

  output_train <- file.path(system.file(package = "monotonicTree"),"result0.tra")
  output_test <- file.path(system.file(package = "monotonicTree"),"result0.tst")
  output_resultado <- file.path(system.file(package = "monotonicTree"),"result0e0.txt")
  output_test <- file.path(system.file(package = "monotonicTree"),"result0e0.json")

  output <- paste("outputData =",shQuote(output_train),shQuote(output_test),shQuote(output_resultado),shQuote(output_json),sep=' ')
  write(output,file=name_file,append = TRUE)

}
#' insert attributes necessary according measured
#'
#' @param pruned if pruned tree (pruned=TRUE) or not pruned tree (pruned=FALSE)
#' @param confidence value of confidence
#' @param importance for MID measure value of importance
#' @param leaf number of instances for leaf
#' @param metric string value of measure
insert_attributes <- function(pruned,confidence,importance,leaf,metric){


  name_file <- file.path(system.file(package = "monotonicTree"),"config0.txt")


  write(paste("\npruned =",as.character(pruned),sep=""),file=name_file,append = TRUE)
  write(paste("confidence = ",as.character(confidence),sep=""),file=name_file,append = TRUE)
  if(metric == "MID"){
    write(paste("relative_importance_monotonicity =",as.character(importance),sep=""),file=name_file,append = TRUE)
  }

  write(paste("instancesPerLeaf = ",leaf,sep=""),file=name_file,append = TRUE)

}
#' download and unzip jar
descargar_jar <- function(){
  
  dir.create(file.path(system.file(package = "monotonicTree"), "download"))
  path <- file.path(system.file(package = "monotonicTree"), "download","JarFiles.zip")
  downloader::download(url="https://github.com/raulopez/monotonicTree/raw/master/jar/JarFiles.zip",destfile = path, mode ="wb")
  unzip(zipfile = path, exdir = file.path(system.file(package = "monotonicTree"), "download"))
  
}
