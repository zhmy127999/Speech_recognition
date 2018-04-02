idNum2Wav<-function(idNum,writeFile=TRUE,dir='Rcodes/digits',samp.rate=16000){
#此函数生成标识说话者ID的语音文件，如4266，返回wave object
#dir:原始0-9数字音频所在目录，生成ID语音文件也放在此目录，默认采样率16000
filename=paste0(dir,'/',idNum,'.wav');

nid=nchar(idNum)

q1=paste0(dir,'/',substr(idNum,1,1),'.wav')

r1=suppressWarnings(tuneR::readWave(q1))

if(r1@samp.rate!=samp.rate){
r1=seewave::resamp(r1,r1@samp.rate,samp.rate,"Wave")
}
r1@stereo=FALSE;
r1@right=numeric(0)
silenceSound=r1;
silenceSound@left=c(1:round(0.01*samp.rate))*0;#添加0.01秒静音

for(j in c(2:nid)){
q1=paste0(dir,'/',substr(idNum,j,j),'.wav')
r2=suppressWarnings(tuneR::readWave(q1))
if(r2@samp.rate!=samp.rate){
r2=seewave::resamp(r2,r2@samp.rate,samp.rate,"Wave")
}
r2@stereo=FALSE;
r2@right=numeric(0)

r1=tuneR::bind(r1,silenceSound,r2)
}
if(writeFile){
if(file.exists(filename)){
file.remove(filename)
}
tuneR::writeWave(r1,filename);
}

return(r1);

}