library(tidyverse)
library(imager)

d = list()
for(i in 1:6){
  d[[i]] = resize(load.image(paste0("~/Desktop/dice/",i,".png")), 119, 119)
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
#inpath is a file path to an image you want to dicify
#outpath is a file path where you want to save the new image to. If null, it'll save it to the same directory as the original and add "dicified" to the end
# dicify <- function(inpath, outpath = NULL, width = 60, height = 60) {
#   require(imager)
#   img = grayscale(resize(load.image(path), width, height))
#   plot(img)
#   bins = cut(as.matrix(img), 6)
#   m = matrix(as.numeric(as.character(factor(bins, labels = 6:1))), nrow=nrow(img))
#   if(is.null(outpath)){
#     outpath = gsub("\\.", "_dicified\\.", inpath)
#   }
#   save.image(makeDice(m), outpath, 1)
#   print("Done.")
#   return(makeDice(m))
# }

dicify <- function(img, width = 60, height = 60) {
  require(imager)
  img = grayscale(resize(img, width, height))
  bins = cut(as.matrix(img), 6)
  m = matrix(as.numeric(as.character(factor(bins, labels = 6:1))), nrow=nrow(img))
  return(makeDice(m))
}


image = load.image("~/Desktop/mona-lisa_u-L-Q1HT4P50.jpg")
dice = dicify(image, 20, 20)
plot(dice, axes = F)

#dicify("~/Desktop/mona-lisa_u-L-Q1HT4P50.jpg", 80, 80)

#dicify("~/Desktop/photo.jpg", 50,70)


#dicify("~/Desktop/dog.jpg", 80, 100)


#dicify("~/Desktop/mom2.jpg", 100, 110)




# ###########
# library(magick)
# 
# img = image_read("~/Desktop/dog.jpg") %>%
#   image_quantize(colorspace = 'gray') %>%
#   image_resize("80x80") %>%
#   magick2cimg()
# img %>%
#   cimg2magick() %>%
#   str
# 
# 
# readbitmap::read.bitmap("~/Desktop/mona-lisa_u-L-Q1HT4P50.jpg") %>%
#   print
# 
# ml = image_read("~/Desktop/mona-lisa_u-L-Q1HT4P50.jpg")
# ml %>%
#   image_quantize(colorspace = 'gray') %>%
#   image_fuzzycmeans %>%
#   image_resize("100x100") %>%
#   magick2cimg() -> m
# 
# m[1,1]
# 
# img = dicify("~/Desktop/dog.jpg", NULL, 10, 10)
# print(img)
