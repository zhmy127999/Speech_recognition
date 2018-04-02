
identify2Speakers<-function(SpeechUrl,speakerID=c('8633','7944'),diffGender=TRUE, speaker1.sound=NULL,speaker2.sound=NULL){
 
#source('Rcodes/myconfig.R')

sep.values=separateSentences(SpeechUrl);
nofiles=length(sep.values$length.speeches)
speeches=processFolder(sep.values$dir);
data=speeches;
data$duration <- NULL
data$selec <- NULL
data$peakf <- NULL
data$label <- 1 #设定speaker1为主要说话者
data[2,]$label=2;
data$label <- factor(data$label, labels=c('speaker1', 'speaker2'))
indComplete=complete.cases(data)
voices=data[indComplete,]

if(diffGender==TRUE){#如果是男女对话，调用已有性别判别器进行判断
load('Rcodes/genderRecog.Rdata')#调用三种性别判别器rpart，c50，glm

if(FALSE){
#用三个判别器分别判断
rpart_fit_all <- suppressWarnings(predict(gender_rpart, voices))
c50_fit_all <- suppressWarnings(predict(gender_c50, voices))
glm_fit_all <- suppressWarnings(predict(gender_glm, voices))

#综合三个判别器结果
combo_fit_all=as.numeric(data$label)
combo_fit_all[indComplete]=as.numeric(rpart_fit_all)+as.numeric(c50_fit_all)+as.numeric(glm_fit_all)
ind=(combo_fit_all<5)
ind2=(combo_fit_all>4)
combo_fit_all[ind]=1;
combo_fit_all[ind2]=2;
data$label=combo_fit_all;
}

}else{#否则利用两个speaker的给定语音speaker1.sound，speaker2.sound训练出判别器

sep1.values=separateSentences(speaker1.sound,0.1,2);
sep2.values=separateSentences(speaker2.sound,0.1,2);

speaker1=processFolder(sep1.values$dir)
speaker2=processFolder(sep2.values$dir)
speaker1$label <- 1
speaker2$label <- 2
speakers12 <- rbind(speaker1, speaker2)
speakers12$label <- factor(speakers12$label, labels=c('speaker1', 'speaker2'))

speakers12$duration <- NULL
speakers12$sound.files <- NULL
speakers12$selec <- NULL
speakers12$peakf <- NULL

# Remove rows containing NA's.
speakers12 <- speakers12[complete.cases(speakers12),]

fitControl <- trainControl(method="cv",
                            number = 5,
                            preProcOptions = list(thresh = 0.99),     classProbs = TRUE,
                            summaryFunction = twoClassSummary)

gender_rpart <- suppressWarnings(caret::train(label ~ ., 
                      data = speakers12, 
                      method = "rpart",
                      preProcess = c("BoxCox", "center", "scale", "nzv"),
                      tuneLength = 10, 
                      trControl = fitControl, metric = 'Accuracy'))
if(FALSE){
gender_c50 <- suppressWarnings(caret::train(label ~ ., 
                                  data = speakers12, 
                                  method = "C5.0",
                                  preProcess = c("BoxCox", "center", "scale", "nzv"),
                                  tuneLength = 7, 
                                  trControl = fitControl,
                                  metric = 'Accuracy'))

gender_glm <- suppressWarnings(caret::train(label ~ ., 
                                         data = speakers12, 
                                         method = "glm",
                                         family = "binomial",
                                         preProcess = c("BoxCox", "center", "scale", "nzv"),
                                         tuneLength = 5, 
                                         trControl = fitControl,
                                         metric = 'Accuracy'))

rf_fit <- caret::train(label ~ ., 
                data = speakers12, 
                method = "rf",
                tuneLength = 5,
                preProcess = c("BoxCox", "center", "scale", "nzv"),
                trControl = fitControl,
                ntree = 150,
                metric = 'Accuracy')

}
}

#数用判别器gender_rpart判断
rpart_fit_all <- suppressWarnings(predict(gender_rpart, voices))
combo_fit_all=as.numeric(data$label)
combo_fit_all[indComplete]=as.numeric(rpart_fit_all)
data$label=combo_fit_all;




data$fileorder=1;#小段语音的编号
data$duration=1;#小段语音的长度
lengths=sep.values$length.speeches#切割语音时记录的小段语音的长度
for (j in c(1:nofiles)){

filename1=as.character(data[j,]$sound.files);
filename1=strsplit(filename1,"[.]");
data[j,]$fileorder=as.numeric(filename1[[1]][1]);#得到小段语音的编号

#filepath=paste0(speechPath,"/",data[j,]$soundfiles);
#r=tuneR::readWave(filepath)
#data[j,]$duration=max(length(r@left),length(r@right));
data[j,]$duration=lengths[data[j,]$fileorder];#得到小段语音的长度
}

data=data[order(data$fileorder),];#将数据按照文件编号（即时间顺序）排序
data[1,]$label=2;#假定头一句话总是系统女生，系统性别可以由监狱制定，或公司系统指定


#登记两个speaker的时间段,speaker1time,speaker2time皆是一列偶数长度向量，每两个元素记录起始时间
speaker1time=c();
speaker2time=c();

 first.ind=0;
 for(j in c(1:nofiles)){
  next.ind=first.ind+data[j,]$duration;
  if (data[j,]$label==1){
  speaker1time=c(speaker1time,first.ind,next.ind)#speaker1的时间段
  }else{
  speaker2time=c(speaker2time,first.ind,next.ind)#speaker2的时间段
  }
  first.ind=next.ind;
  }
#合并相邻的时间段
indspeaker1=c(1:length(speaker1time))
indspeaker11=duplicated(speaker1time)
indspeaker1=indspeaker1[indspeaker11];
indspeaker1=sort(union(indspeaker1,indspeaker1-1));
speaker1time=speaker1time[-indspeaker1]

#合并相邻的时间段
indspeaker2=c(1:length(speaker2time))
indspeaker22=duplicated(speaker2time)
indspeaker2=indspeaker2[indspeaker22];
indspeaker2=sort(union(indspeaker2,indspeaker2-1));
speaker2time=speaker2time[-indspeaker2]

#除以采样率，得到单位为毫秒的起始时间
speaker1time=round(speaker1time/sep.values$samp.rate,3)
speaker2time=round(speaker2time/sep.values$samp.rate,3)

############在每句话开始，插入说话者标识数字语音，生成新语音
dateTime=format(Sys.time(), "-%Y-%m-%d-%H-%M")
newpath2=paste0('ID',dateTime,'.wav')
newfilepath=sub(pattern='.wav',replacement=newpath2,x=SpeechUrl)

speaker1wav=idNum2Wav(speakerID[1],FALSE,'Rcodes/digits',sep.values$samp.rate);
speaker2wav=idNum2Wav(speakerID[2],FALSE,'Rcodes/digits',sep.values$samp.rate);
q=paste0(sep.values$dir,'/',as.character(data[1,]$fileorder),'.wav');
r1=suppressWarnings(tuneR::readWave(q))#读第一句话
r1@stereo=FALSE;
r1@right=numeric(0)

if(data[1,]$label==1){
r1=tuneR::bind(speaker1wav,r1)
}else{
r1=tuneR::bind(speaker2wav,r1)
}
for(j in c(2:nrow(data))){
q=paste0(sep.values$dir,'/',as.character(data[j,]$fileorder),'.wav');
r2=suppressWarnings(tuneR::readWave(q))#读第j句话
r2@stereo=FALSE;
r2@right=numeric(0)
if(data[j,]$label==data[j-1,]$label){
r1=tuneR::bind(r1,r2)
}else{
if(data[j,]$label==1){
r1=tuneR::bind(r1,speaker1wav,r2)
}else{
r1=tuneR::bind(r1,speaker2wav,r2)
}
}
}
tuneR::writeWave(r1,newfilepath);


##########################
#返回结果
return.values=list(new.speechID=newfilepath,samp.rate=sep.values$samp.rate,speech.data=data,smallspeech.dir=sep.values$dir,new.speech=sep.values$new.speech,speaker1=speaker1time,speaker2=speaker2time);
return(return.values);
}