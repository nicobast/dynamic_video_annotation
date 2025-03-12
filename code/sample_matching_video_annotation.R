#matching video annotations
load("C:/Users/nico/PowerFolders/data_AFFIP/rct_preprocessed/affip_janohlenmacher_19022025.Rdata")
df_jan<-df_jan[!duplicated(df_jan$scrnno),]
df_jan$IQ_est<-with(df_jan,test_age/agedemo*100)

#sample description
table(df_jan$gender)
hist(df_jan$agedemo)
hist(df_jan$IQ_est)
table(df_jan$rndm)


annotated_videos<-c(37,38,41,73,68)
selected_cases<-df_jan$scrnno %in% annotated_videos

#current annotated demographics
table(df_jan$gender[selected_cases])
table(df_jan$agedemo[selected_cases])
table(df_jan$IQ_est[selected_cases])
table(df_jan$rndm[selected_cases])


#new selection based on current to balance
df_jan$scrn[which(df_jan$rndm=='A-FFIP' & df_jan$gender=='männlich' & df_jan$IQ_est<65)]
to_annotate<-c(11,17,42,48,50,62,66,90,91,100,113)
selected_cases<-df_jan$scrnno %in% c(annotated_videos,to_annotate)
table(selected_cases)
table(df_jan$gender[selected_cases])
table(df_jan$agedemo[selected_cases])
table(df_jan$IQ_est[selected_cases])
table(df_jan$rndm[selected_cases])

table(df_jan$gender[selected_cases],df_jan$rndm[selected_cases])
with(df_jan[selected_cases,],by(IQ_est,rndm,mean,na.rm=T))


