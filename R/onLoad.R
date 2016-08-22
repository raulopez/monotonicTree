.onLoad <- function(libname = find.package("monotonicTree"), pkgname = "monotonicTree"){
  
  cat("Download JAR..\n")
  dir.create(file.path(system.file(package = "monotonicTree"), "download"))
  path <- file.path(system.file(package = "monotonicTree"), "download","JarFiles.zip")
  downloader::download(url="https://github.com/raulopez/monotonicTree/raw/master/jar/JarFiles.zip",destfile = path, mode ="wb")
  unzip(zipfile = path, exdir = file.path(system.file(package = "monotonicTree"), "download"))

}