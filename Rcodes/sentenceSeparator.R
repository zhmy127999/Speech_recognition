separateSentences<-function(SpeechUrl,silenceTime=0.1,skip=1.5){

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

nofiles=0;
filename=strsplit(SpeechUrl,"/");
lenfilename=length(filename[[1]])
filename=filename[[1]][lenfilename];

filename=strsplit(filename,"\\\\");
lenfilename=length(filename[[1]])
filename=filename[[1]][lenfilename];

filename=strsplit(filename,"[.]");
filename=filename[[1]][1];

path1=getwd();
dateTime=format(Sys.time(), "-%Y-%m-%d-%H-%M-%S")
path2=paste0(path1,"/",filename,dateTime)
nofiles=0;
dir.create(path2);
newspeechind=c();
lengthofspeeches=c();
for (j in c(1:(length(sepind)-1))){
    soundind=c(sepind[j]:sepind[j+1]);
    nosilence=sum(ind[soundind]==FALSE)
    if (nosilence>skip*silenceTime*fs){
        nofiles=nofiles+1;
        newspeechind=c(newspeechind,soundind);
        lengthofspeeches=c(lengthofspeeches,length(soundind));
        q2=paste0(path2,"/",as.character(nofiles),'.wav')
        r2=r;
        #if(length(r@left)>length(soundind)){
         r2@left=r@left[soundind];
         r2@right=numeric(0)#生成单声道
         r2@stereo=FALSE
         #}else
         #{r2@right=r@right[soundind];
          #}
        writeWave(r2,q2);#存入文件
   }
}
   dateTime2=format(Sys.time(), "-%Y-%m-%d-%H")#取当前时间
   newspeechpath=paste0(path1,"/",'new',filename,dateTime,'.wav')
   r2=r;
        #if(length(r@left)>length(newspeechind)){
         r2@left=r@left[newspeechind];
         r2@right=numeric(0)#生成单声道
         r2@stereo=FALSE
         #}else
         #{r2@right=r@right[newspeechind];
          #}
        writeWave(r2,newspeechpath);#存入文件

 funvalue=list(dir=path2,samp.rate=fs,new.speech=newspeechpath,length.speeches=lengthofspeeches);

 return(funvalue);
}

FindContinuousIndex<-function(inde){
#此函数寻找给定指标列inde中两个以上连续指标的位置，返回列数=2的矩阵，第一列表明连续指标的起点，第二列标明终点
#by HZ,2017-12-15
len1=length(inde)
indeprime=c(c(inde[1]), inde[1:len1-1]+1);
indofinde=inde-indeprime>0;

ind2=c(1:len1);

indofinde=ind2[indofinde];

indofinde2=c(inde[indofinde[1]-1], inde[indofinde]);
mm=length(indofinde2);
numofinde=c(1:mm);
numofinde[1]=indofinde2[1]-inde[1]+1;
numofinde[2:(mm-1)]=diff(indofinde);
numofinde[mm]=inde[length(inde)]-indofinde2[length(indofinde2)]+1;
conde=c(1:(2*mm))

conde[2*(2:(mm-1))-1]=indofinde2[2:(mm-1)];
conde[2*(2:(mm-1))]=conde[2*(2:(mm-1))-1]+numofinde[2:(mm-1)]-1;
conde[2]=indofinde2[1];
conde[1]=conde[2]-numofinde[1]+1;
conde[length(conde)-1]=indofinde2[mm];
conde[length(conde)]=indofinde2[mm]+numofinde[mm]-1;

conindex=matrix(,mm,2);
s1=seq(1,length(conde)-1,2)
conindex[,1]=conde[s1];
s2=seq(2,length(conde),2)
conindex[,2]=conde[s2];

ind5=numofinde>1;
conindex=conindex[ind5,];

return(conindex)
}



