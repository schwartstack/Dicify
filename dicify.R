library(tidyverse)
library(imager)

d = list()
for(i in 1:6){
  d[[i]] = resize(load.image(paste0("https://github.com/schwartstack/Dicify/raw/main/dice/",i,".png")), 119, 119)
}

#x is a numeric vector on [1,6]
#makeRow returns an image of a row of dice
makeRow <- function(x){
  return(imappend(lapply(x, function(x)d[[x]]), "x"))
}
#rows is a list of images of rows of dice
#combineRows returns a single image of a matrix of dice
combineRows <- function(rows){
  return(imappend(rows, "y"))
}
#m is a numeric matrix on [1,6]
makeDice <- function(m){
  rows = list()
  for(i in 1:ncol(m)){
    rows[[i]] = makeRow(m[,i])
  }
  return(combineRows(rows))
}
#img is a cimg object. It outputs a cimg object of a dicified version of the original image.
dicify <- function(img, width = 60, height = 60) {
  require(imager)
  img = grayscale(resize(img, width, height))
  bins = cut(as.matrix(img), 6)
  m = matrix(as.numeric(as.character(factor(bins, labels = 6:1))), nrow=nrow(img))
  return(makeDice(m))
}
