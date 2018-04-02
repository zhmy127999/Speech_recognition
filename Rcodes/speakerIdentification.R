
identify2Speakers<-function(SpeechUrl,speakerID=c('8633','7944'),diffGender=TRUE, speaker1.sound=NULL,speaker2.sound=NULL){
 
#source('Rcodes/myconfig.R')

sep.values=separateSentences(SpeechUrl);
nofiles=length(sep.values$length.speeches)
speeches=processFolder(sep.values$dir);
data=speeches;
data$duration <- NULL
data$selec <- NULL
data$peakf <- NULL
data$label <- 1 #�趨speaker1Ϊ��Ҫ˵����
data[2,]$label=2;
data$label <- factor(data$label, labels=c('speaker1', 'speaker2'))
indComplete=complete.cases(data)
voices=data[indComplete,]

if(diffGender==TRUE){#�������Ů�Ի������������Ա��б��������ж�
load('Rcodes/genderRecog.Rdata')#���������Ա��б���rpart��c50��glm

if(FALSE){
#�������б����ֱ��ж�
rpart_fit_all <- suppressWarnings(predict(gender_rpart, voices))
c50_fit_all <- suppressWarnings(predict(gender_c50, voices))
glm_fit_all <- suppressWarnings(predict(gender_glm, voices))

#�ۺ������б������
combo_fit_all=as.numeric(data$label)
combo_fit_all[indComplete]=as.numeric(rpart_fit_all)+as.numeric(c50_fit_all)+as.numeric(glm_fit_all)
ind=(combo_fit_all<5)
ind2=(combo_fit_all>4)
combo_fit_all[ind]=1;
combo_fit_all[ind2]=2;
data$label=combo_fit_all;
}

}else{#������������speaker�ĸ�������speaker1.sound��speaker2.soundѵ�����б���

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

#�����б���gender_rpart�ж�
rpart_fit_all <- suppressWarnings(predict(gender_rpart, voices))
combo_fit_all=as.numeric(data$label)
combo_fit_all[indComplete]=as.numeric(rpart_fit_all)
data$label=combo_fit_all;




data$fileorder=1;#С�������ı��
data$duration=1;#С�������ĳ���
lengths=sep.values$length.speeches#�и�����ʱ��¼��С�������ĳ���
for (j in c(1:nofiles)){

filename1=as.character(data[j,]$sound.files);
filename1=strsplit(filename1,"[.]");
data[j,]$fileorder=as.numeric(filename1[[1]][1]);#�õ�С�������ı��

#filepath=paste0(speechPath,"/",data[j,]$soundfiles);
#r=tuneR::readWave(filepath)
#data[j,]$duration=max(length(r@left),length(r@right));
data[j,]$duration=lengths[data[j,]$fileorder];#�õ�С�������ĳ���
}

data=data[order(data$fileorder),];#�����ݰ����ļ���ţ���ʱ��˳������
data[1,]$label=2;#�ٶ�ͷһ�仰����ϵͳŮ����ϵͳ�Ա�����ɼ����ƶ�����˾ϵͳָ��


#�Ǽ�����speaker��ʱ���,speaker1time,speaker2time����һ��ż������������ÿ����Ԫ�ؼ�¼��ʼʱ��
speaker1time=c();
speaker2time=c();

 first.ind=0;
 for(j in c(1:nofiles)){
  next.ind=first.ind+data[j,]$duration;
  if (data[j,]$label==1){
  speaker1time=c(speaker1time,first.ind,next.ind)#speaker1��ʱ���
  }else{
  speaker2time=c(speaker2time,first.ind,next.ind)#speaker2��ʱ���
  }
  first.ind=next.ind;
  }
#�ϲ����ڵ�ʱ���
indspeaker1=c(1:length(speaker1time))
indspeaker11=duplicated(speaker1time)
indspeaker1=indspeaker1[indspeaker11];
indspeaker1=sort(union(indspeaker1,indspeaker1-1));
speaker1time=speaker1time[-indspeaker1]

#�ϲ����ڵ�ʱ���
indspeaker2=c(1:length(speaker2time))
indspeaker22=duplicated(speaker2time)
indspeaker2=indspeaker2[indspeaker22];
indspeaker2=sort(union(indspeaker2,indspeaker2-1));
speaker2time=speaker2time[-indspeaker2]

#���Բ����ʣ��õ���λΪ�������ʼʱ��
speaker1time=round(speaker1time/sep.values$samp.rate,3)
speaker2time=round(speaker2time/sep.values$samp.rate,3)

############��ÿ�仰��ʼ������˵���߱�ʶ��������������������
dateTime=format(Sys.time(), "-%Y-%m-%d-%H-%M")
newpath2=paste0('ID',dateTime,'.wav')
newfilepath=sub(pattern='.wav',replacement=newpath2,x=SpeechUrl)

speaker1wav=idNum2Wav(speakerID[1],FALSE,'Rcodes/digits',sep.values$samp.rate);
speaker2wav=idNum2Wav(speakerID[2],FALSE,'Rcodes/digits',sep.values$samp.rate);
q=paste0(sep.values$dir,'/',as.character(data[1,]$fileorder),'.wav');
r1=suppressWarnings(tuneR::readWave(q))#����һ�仰
r1@stereo=FALSE;
r1@right=numeric(0)

if(data[1,]$label==1){
r1=tuneR::bind(speaker1wav,r1)
}else{
r1=tuneR::bind(speaker2wav,r1)
}
for(j in c(2:nrow(data))){
q=paste0(sep.values$dir,'/',as.character(data[j,]$fileorder),'.wav');
r2=suppressWarnings(tuneR::readWave(q))#����j�仰
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
#���ؽ��
return.values=list(new.speechID=newfilepath,samp.rate=sep.values$samp.rate,speech.data=data,smallspeech.dir=sep.values$dir,new.speech=sep.values$new.speech,speaker1=speaker1time,speaker2=speaker2time);
return(return.values);
}