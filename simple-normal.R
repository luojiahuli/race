load_data <- function() {
  library(pixmap)
  library(doMC)
  registerDoMC()
  
  data.train.dir <- '/usr/local/courses/datasets/face-nonface/train'
  data.train.face.dir <- paste(data.train.dir, "face", sep="/")
  data.train.non_face.dir <- paste(data.train.dir, "non-face", sep="/")
  
  data.train.face <- data.frame(list.files(data.train.face.dir))
  colnames(data.train.face) <- "file"
  # use head for debug
  #  data.train.face <- head(data.train.face)
  data.train.face.im <- foreach(file=data.train.face[,"file"], .combine=rbind) %dopar% {
    im = read.pnm(paste(data.train.face.dir, file, sep="/"))
    matrix(im@grey, nrow = 1, byrow = T)
  }
  rownames(data.train.face.im) <- data.train.face[, "file"]
  data.train.face.im;
}

show_image <- function(im) {
  image(1:19,1:19, t(apply(matrix(im,nrow=19,byrow = T), 1, rev)), col=gray((0:255)/255))
}




