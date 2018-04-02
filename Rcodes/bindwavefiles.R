setwd("D:\\haizhang\\GMM-HMM\\GenderAndSpeakerIdentification")

setwd("C:/haizhang/GenderAndSpeakerIdentification")

setwd("D:/Haizhang")

source('Rcodes/myconfig.R')
######下面代码从目录dir中选取特定音频合并成一段30秒左右的训练语音
dir='prison/对话-2018-03-21-01-53-05'

#ffiles=c(2,5,47,59,72,120,121,123,126,130,151,167,174,180,181)

mfiles=c(6,7,8,10,11,12,13,14,15,17,18,20,21,25,27,32)

q1=paste0(dir,'/',as.character(ffiles[1]),'.wav')
q2=paste0(dir,'/',as.character(mfiles[1]),'.wav')

r1=suppressWarnings(tuneR::readWave(q1))
r2=suppressWarnings(tuneR::readWave(q2))

silenceSound=r1;

silenceSound@left=c(1:round(0.3*r1@samp.rate))*0;#添加0.3秒静音

for (j in ffiles[-1]){
q1=paste0(dir,'/',as.character(j),'.wav')

r11=suppressWarnings(tuneR::readWave(q1))
r1=tuneR::bind(r1,silenceSound,r11)

}


for (j in mfiles[-1]){
q2=paste0(dir,'/',as.character(j),'.wav')

r22=suppressWarnings(tuneR::readWave(q2))
r2=tuneR::bind(r2,silenceSound,r22)

}

femalepath=paste0(dir,'/','femaletest','.wav')

writeWave(r1,femalepath);

malepath=paste0(dir,'/','maletest','.wav')

writeWave(r2,malepath);
####################################

#合并之后的训练文件放在目录prison中
dir='Rcodes/prison'
femalepath=paste0(dir,'/','femaletest','.wav')
malepath=paste0(dir,'/','maletest','.wav')
########################################


######下面代码从合成标识身份者的数字代号语音，如

maleids=c('4266','2974','8633','3741')
femaleids=c('9635','6755','7944','5587')

idNum='2657'

dir='Rcodes/digits'
for (idNum in maleids){
idNum2Wav(idNum,FALSE)
}

for (idNum in femaleids){
idNum2Wav(idNum)
}

####################################

SpeechUrl='Rcodes/prison/对话.wav'
dir='Rcodes/prison'
femalepath=paste0(dir,'/','femaletest','.wav')
malepath=paste0(dir,'/','maletest','.wav')

speaker1.sound=malepath

speaker2.sound=femalepath

diffGender=FALSE
speakerID=c('Rcodes/digits/喜出望外.wav','Rcodes/digits/自强不息.wav')


#下面代码展示身份识别后的结果
funvalues=identify2Speakers2(SpeechUrl,speakerID,FALSE, speaker1.sound,speaker2.sound)
str(funvalues)

#funvalues有如下值：
# 带有说话者标识的新语音文件 $ new.speechID   : chr "Rcodes/prison/对话ID-2018-03-27-01-11.wav"
#采样率 $ samp.rate      : int 8000
#分割后每句话的数据 $ speech.data    :'data.frame':        763 obs. of  24 variables:
  ..$ label      : num [1:763] 2 2 1 2 2 1 1 1 2 1 ...
  ..$ fileorder  : num [1:763] 1 2 3 4 5 6 7 8 9 10 ...
  ..$ duration   : num [1:763] 10103 10950 7667 20845 12801 ...
#分割后句子所在目录 $ smallspeech.dir: chr "D:/Haizhang/对话-2018-03-27-01-10-04"
#不带有说话者标识的新语音文件（与原文件有微小差别) $ new.speech     : chr "D:/Haizhang/new对话-2018-03-27-01-10-04.wav"
#speaker1起始时间 $ speaker1       : num [1:286] 2.63 3.59 7.8 12.09 12.69 ...
#speaker2起始时间 $ speaker2       : num [1:286] 0 2.63 3.59 7.8 12.09 ...

#下面展示结果

data=funvalues$speech.data

time22=data$duration/funvalues$samp.rate;
ctime22=time22;
for(j in c(1:length(time22))){
ctime22[j]=sum(time22[1:j]);
}
funvalues$speaker1
funvalues$speaker2
data.frame(data$fileorder,data$label,time22,ctime22)



###########16K采样率结果


dir='Rcodes/prison'
femalepath=paste0(dir,'/','femaletest','.wav')
malepath=paste0(dir,'/','maletest','.wav')
femalepath2=paste0(dir,'/','femaletest16k','.wav')
malepath2=paste0(dir,'/','maletest16k','.wav')



SpeechUrl='Rcodes/prison/对话.wav'

SpeechUrl2='Rcodes/prison/对话16k.wav'
myResample(SpeechUrl,SpeechUrl2,16000)
myResample(femalepath,femalepath2,16000)
myResample(malepath,malepath2,16000)
speaker1.sound=malepath2

speaker2.sound=femalepath2


diffGender=FALSE


#下面代码展示身份识别后的结果
funvalues=identify2Speakers(SpeechUrl2, c('86','79'),FALSE, speaker1.sound,speaker2.sound)
str(funvalues)

#funvalues有如下值：
# 带有说话者标识的新语音文件 $ new.speechID   : chr "Rcodes/prison/对话ID-2018-03-27-01-11.wav"
#采样率 $ samp.rate      : int 8000
#分割后每句话的数据 $ speech.data    :'data.frame':        763 obs. of  24 variables:
  ..$ label      : num [1:763] 2 2 1 2 2 1 1 1 2 1 ...
  ..$ fileorder  : num [1:763] 1 2 3 4 5 6 7 8 9 10 ...
  ..$ duration   : num [1:763] 10103 10950 7667 20845 12801 ...
#分割后句子所在目录 $ smallspeech.dir: chr "D:/Haizhang/对话-2018-03-27-01-10-04"
#不带有说话者标识的新语音文件（与原文件有微小差别) $ new.speech     : chr "D:/Haizhang/new对话-2018-03-27-01-10-04.wav"
#speaker1起始时间 $ speaker1       : num [1:286] 2.63 3.59 7.8 12.09 12.69 ...
#speaker2起始时间 $ speaker2       : num [1:286] 0 2.63 3.59 7.8 12.09 ...

#下面展示结果

data=funvalues$speech.data

time22=data$duration/funvalues$samp.rate;
ctime22=time22;
for(j in c(1:length(time22))){
ctime22[j]=sum(time22[1:j]);
}
funvalues$speaker1
funvalues$speaker2
data.frame(data$fileorder,data$label,time22,ctime22)

###########
path2=cutAudio(SpeechUrl,silenceTime=0.3)


