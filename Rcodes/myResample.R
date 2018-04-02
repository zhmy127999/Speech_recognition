myResample<-function(oldSpeech,newSpeech,newRate){
#�ز�����Ƶ�ļ����������ļ���newSpeech
r1=suppressWarnings(tuneR::readWave(oldSpeech))
if(r1@samp.rate!=newRate){
r1=seewave::resamp(r1,r1@samp.rate,newRate,"Wave")
}
r1@stereo=FALSE;
r1@right=numeric(0)
tuneR::writeWave(r1,newSpeech);

}