##################################
## my code to download the data 
## this code will create `./Data` subfolder in current folder
## and download the datatests to the subfolders in ./Data 
##################################

library('tools')

download_data <- function(url, dst_folder) {
  file_name <-  tail(strsplit(url, '/')[[1]], 1)
  print(dst_folder)
  if (!dir.exists(dst_folder)) {
    dir.create(dst_folder, recursive = TRUE)
  }
  local_file <- file.path(dst_folder, file_name)
  print(url)
  download.file(url = url, destfile = local_file)
  file_extension = file_ext(file_name)
  if (file_extension %in% c('zip', 'gz')) {
    if (file_extension == 'zip') {
      unzip(local_file, exdir = dst_folder)
    } else if (file_extension == 'gz') {
      gzfile(local_file)
      gz <- gzfile(local_file, 'rb')
      writeLines(readLines(gz), con = file_path_sans_ext(local_file))
      close(gz)
    }
    file.remove(local_file)
  }
}

urls <- list(
  spambase = c('http://archive.ics.uci.edu/static/public/94/spambase.zip',
               'https://hastie.su.domains/ElemStatLearn/datasets/spam.traintest'),
  zip = c('https://hastie.su.domains/ElemStatLearn/datasets/zip.train.gz',
          'https://hastie.su.domains/ElemStatLearn/datasets/zip.test.gz'),
  prostate = c('https://hastie.su.domains/ElemStatLearn/datasets/prostate.data'),
  microarray = c('https://hastie.su.domains/ElemStatLearn/datasets/nci.data.csv',
                 'https://hastie.su.domains/ElemStatLearn/datasets/nci.label.txt')
)

for (key in names(urls)) {
  dst_folder <- paste('./Data', key, sep = '/')
  for (url in urls[[key]]) {
    download_data(url, dst_folder)
  }
}

#######################
## code from the task
## I changed only paths
#######################
data.spam=read.table("./Data/spambase/spambase.data",header=F,sep = ",")
colnames(data.spam)[58]="Y"

data.prostate=read.table("./Data/prostate/prostate.data",header=T)
pairs(data.prostate[,1:9])

data.zip=read.table("./Data/zip/zip.train", header=F)
colnames(data.zip)[1]="Y"

data.microarray=read.csv("./Data/microarray/nci.data.csv",header=T)
rownames(data.microarray)=data.microarray[,1]
data.micro=data.frame(t(data.microarray[,-1]))
label=read.csv("./Data/microarray/nci.label.txt",header=F)