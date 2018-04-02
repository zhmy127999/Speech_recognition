idNum2Wav<-function(idNum,writeFile=TRUE,dir='Rcodes/digits',samp.rate=16000){
#�˺������ɱ�ʶ˵����ID�������ļ�����4266������wave object
#dir:ԭʼ0-9������Ƶ����Ŀ¼������ID�����ļ�Ҳ���ڴ�Ŀ¼��Ĭ�ϲ�����16000
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
silenceSound@left=c(1:round(0.01*samp.rate))*0;#����0.01�뾲��

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