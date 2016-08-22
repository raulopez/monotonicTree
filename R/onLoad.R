.onLoad <- function(libname = find.package("monotonicTree"), pkgname = "monotonicTree"){
  
  dir.create(file.path(system.file(package = "monotonicTree"), "download"))
  path <- file.path(system.file(package = "monotonicTree"), "download","JarFiles.zip")
  downloader::download(url="https://github.com/raulopez/monotonicTree/raw/master/jar/JarFiles.zip",destfile = path, mode ="wb")
  unzip(zipfile = path, exdir = file.path(system.file(package = "monotonicTree"), "download"))
  
  if(file.info(path)$size < 1000000){
    unlink(file.path(system.file(package = "monotonicTree"), "download"),force = TRUE,recursive = TRUE)
    warning("Jar files could not be downloaded.")
  }
}