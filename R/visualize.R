#' read result
#' @return dataframe with result
ver_resultados <- function(){

  path_json <- file.path(system.file(package = "monotonicTree"),"result0e0.json")
  # cat(path_json)
  mydf <- jsonlite::fromJSON(path_json)
  dim(mydf$matrix_test) <- c(dim(mydf$matrix_test)[1],dim(mydf$matrix_test)[1])
  dim(mydf$matrix_train) <- c(dim(mydf$matrix_train)[1],dim(mydf$matrix_train)[1])
  return(mydf)
}

#' read result
#' @return print decision_tree
#' @export
monotonic_tree <- function(){

  path_json <- file.path(system.file(package = "monotonicTree"),"result0e0.json")
  #cat(path_json)
  if(file.exists(path_json)){
    mydf <- jsonlite::fromJSON(path_json)
    o <- lapply(mydf$decision_tree,function(x){cat(x)})
  }else{
    cat("[X]ERROR: Decision tree no exists\n")
  }
}
