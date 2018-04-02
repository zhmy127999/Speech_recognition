cutAudio<-function(SpeechUrl,timeLen=60,silenceTime=0.3){
#�и���ƵΪʱ��������timeLen��С��Ƶ������������������С��Ƶ��ŵ�Ŀ¼����ԭ�ļ���ͬһ·���£�
#silenceTime �����������������������Ĭ��Ϊ0.3�룬�ɵ���
suppressPackageStartupMessages(library(tuneR))
suppressPackageStartupMessages(library(seewave))


r=suppressWarnings(tuneR::readWave(SpeechUrl))

input <- inputw(wave = r, f = r@samp.rate)

wave=input$w
wave=as.matrix(wave)
fs=r@samp.rate


ind=abs(wave)<0.05*max(abs(wave))

ind2=c(1:length(wave))

ind3=ind2[ind]

conind=FindContinuousIndex(ind3)

ind4=((conind[,2]-conind[,1])>silenceTime*fs);
sepind=conind[ind4,1];
sepind=union(1,sepind);
sepind=union(sepind,length(wave));


path1=strsplit(SpeechUrl,"[.]");
path1=path1[[1]][1];
dateTime=format(Sys.time(), "-%Y-%m-%d-%H-%M-%S")
path2=paste0(path1,'Cut',dateTime)#С����Ƶ���Ŀ¼
dir.create(path2);

nofiles=0;
startind=1;
j=1;
endind=sepind[j];
totalLength=length(r@left);
lengthSep=length(sepind);
while(startind<=totalLength){
       while(j<=lengthSep&sepind[j]-startind<timeLen*fs){
         j=j+1;
         }
        if(j>lengthSep){
          endind=sepind[lengthSep];
         }else{
          endind=sepind[j-1];
         }
        nofiles=nofiles+1;
        r2=r;
         r2@left=r@left[startind:endind];
         r2@right=numeric(0)#���ɵ�����
         r2@stereo=FALSE
         q2=paste0(path2,"/",as.character(nofiles),'.wav');
         writeWave(r2,q2);#�����ļ�
         startind=endind+1;
   }
 return(path2);
}

