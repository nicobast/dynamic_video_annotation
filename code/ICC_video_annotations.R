# define interrater reliability as ICC of manual annotations of videos

# setup ####

## packages
require(openxlsx)

## paths
path<-getwd()

##required functions
###time conversion
fun_timeconv<-function(x){
  as.POSIXct(x,format = "%H:%M:%OS") #consider fractional seconds = milliseconds with %OS
}

###task selection function
fun_seltask<-function(x,var_name){
x<-x[,names(x) %in% c('Number','Onset_Time','Offset_Time',var_name)]
}

# read and select data ####

#read rater data
rater2<-read.xlsx(paste0(path,'/data/FRA_038_T5_ESCS_Übereinstimmung.xlsx'),sheet = 2)
rater1<-read.xlsx(paste0(path,'/data/FRA_038_T5_ESCS_Übereinstimmung.xlsx'),sheet = 1,rows = 1:309)

#convert time data
rater1$Offset_Time<-fun_timeconv(rater1$Offset_Time)
rater1$Onset_Time<-fun_timeconv(rater1$Onset_Time)
rater2$Offset_Time<-fun_timeconv(rater2$Offset_Time)
rater2$Onset_Time<-fun_timeconv(rater2$Onset_Time)

#select a task based on timestamp
cutoff<-as.POSIXct("00:05:00",format = "%H:%M:%OS")
rater1_task1<-rater1[rater1$Onset_Time<cutoff,]
rater2_task1<-rater2[rater2$Onset_Time<cutoff,]


#select a rating category
rater1_task1_mutual<-fun_seltask(rater1_task1,'Share')
table(rater1_task1_mutual$Share)

rater2_task1_mutual<-fun_seltask(rater2_task1,'Share')
table(rater2_task1_mutual$Share)


# convert to matrix to define inter-rater reliability

##create a matrix - function takes interact ratings and creates vector in tenth of second format with according rating label
fun_rater_vector<-function(rating_df){

###get max time
max_time<-max(rating_df$Offset_Time)
#max_time<-format(max_time,"%H:%M:%OS2") #backconverted to character
max_time<-format(max_time,"%H:%M:%S") #backconverted to character


###calculate the number of tens of seconds 
# vector_rows<-as.numeric(substr(max_time,1,2))*360000+
#              as.numeric(substr(max_time,4,5))*6000+
#              as.numeric(substr(max_time,7,8))*100+
#              as.numeric(substr(max_time,10,11))
vector_rows<-as.numeric(substr(max_time,1,2))*3600+
  as.numeric(substr(max_time,4,5))*60+
  as.numeric(substr(max_time,7,8))

#length of rater vector based on rated time
rater_vector<-rep(NA,vector_rows)

#function that fills a vector with categories based on ratings
for(i in 1:nrow(rating_df)){
#start_row<-round(as.numeric(difftime(rating_df$Onset_Time[i],min(rating_df$Onset_Time)))*100)
#end_row<-round(as.numeric(difftime(rating_df$Offset_Time[i],min(rating_df$Onset_Time)))*100)
start_row<-round(as.numeric(difftime(rating_df$Onset_Time[i],min(rating_df$Onset_Time)))*100)
end_row<-round(as.numeric(difftime(rating_df$Offset_Time[i],min(rating_df$Onset_Time)))*100)
categorie<-rating_df$Share[i] #TODO: REMOVE MAGIC VARIABLE
if(is.na(categorie)){categorie<-FALSE}
rater_vector[start_row:end_row]<-categorie
}

return(rater_vector)
  
}

test_vector<-fun_rater_vector(rating_df=rater1_task1_mutual)
test2_vector<-fun_rater_vector(rating_df=rater2_task1_mutual)

table(test_vector)
table(test2_vector)


length_diff<-length(test_vector)-length(test2_vector)
test_vector<-c(test_vector,rep(NA,abs(length_diff)))

rating_df<-data.frame(test_vector,test2_vector)
table(rating_df[,1],rating_df[,2]) #look into contingency table

# require(irr)
# icc(rating_df,model="twoway",type='consistency',unit='single')
# 
# kappam.fleiss(rating_df,exact=T,detail=T)

require(psych)
cohen.kappa(rating_df)

