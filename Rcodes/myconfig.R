
#需要的程序包
packages <- c('caret','ggplot2','tuneR', 'seewave', 'fftw', 
'caTools', 'randomForest', 'warbleR', 'mice','plyr','dplyr', 
'rpart', 'xgboost', 'e1071','C50','readr')

#检查是否安装，如没有则安装程序包
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}


#加载程序包
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(ggplot2))
#suppressPackageStartupMessages(library(gmodels))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tuneR))
suppressPackageStartupMessages(library(seewave))
suppressPackageStartupMessages(library(fftw))
suppressPackageStartupMessages(library(caTools))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(warbleR))
suppressPackageStartupMessages(library(mice))
suppressPackageStartupMessages(library(rpart))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(C50))
suppressPackageStartupMessages(library(readr))

#加载自编函数

source('Rcodes/extractFrequency.R')
source('Rcodes/sentenceSeparator.R')
source('Rcodes/speakerIdentification.R')
source('Rcodes/identify2Speakers2.R')

source('Rcodes/idNum2Wav.R')
source('Rcodes/myResample.R')
source('Rcodes/cutAudio.R')






