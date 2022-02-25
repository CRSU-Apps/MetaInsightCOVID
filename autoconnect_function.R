# A function to automatically take a dataset, find the default connected #
# network and add a column to the data indicating which studies are #
# connected #

# Author: Clareece Nevill
# Started 27th August 2020

# Packages needed: data.table

auto_connect <- function(data,treatment_labels,studyid,treatment,trtcode,base_treatments=c(1),auto_drop=FALSE, auto_recode=FALSE, study_pos, trt_pos, trtcode_pos, zero_arms=FALSE){
# data = data from which you want to find connected studies #
# treatment_labels = dataset of treatment labels if they are stored separately
# studyid = column header in 'data' indicating the study ID
# treatment = column header in 'data' indicating the treatment
# trtcode = column header in 'treatment_labels' of the numeric code for treatments
# base_treatments = a list of treatments (numeric or string) that define which studies the network should be connected with (eg standard_care)
# auto_drop = option to automatically drop studies that are not connected
# auto_recode = option to automatically rename number the studies and treatments to be a consecutive list (e.g. for plotting)
# if auto_recode is TRUE, the study_pos, trt_pos and trtcode_pos are the positions as which data$studyID, data$T, and labels$Number are positioned in dataframe
# zero_arms = option to remove studies which have zeros for both arms of a treatment comparison prior to finding connected network (useful for frequentist analysis)
 
# identify variables
names(data)[names(data)==studyid] <- "StudyID"
names(data)[names(data)==treatment] <- "T"
names(treatment_labels)[names(treatment_labels)==trtcode] <- "T" 

# set up variables to describe dataset
no_studies<-max(data$StudyID) #number of studies (assuming numbered consecutively)
no_rows<-nrow(data) #number of rows in the data frame
no_treatments<-nrow(treatment_labels) #number of treatments (if separate labels)
cols_data <- ncol(data) #number of columns in data
cols_label <- ncol(treatment_labels) #number of columns in treatment labels data
  
# Zero arms option #
#------------------#
# option to remove studies which have zeros for both arms of a treatment comparison before seeing if the remaining studies are connected #
# three arm studies with one non-zero arm can technically work if only one comparison is chosen - remove for now, then see if the user can 'add it in'? #
# this default to removing studies with 2+ zero arms.

if (zero_arms==TRUE) {
  data$zeroarms<-NA #empty column ready
  data$zeroarms_total<-NA 
  class(data$zeroarms_total) <- "list"
  for (i in 1:no_rows) {
    if (data$R[i]==0) {data$zeroarms[i]<-1} else {data$zeroarms[i]<-0} # indicate if data row had outcome of zero
  }
  for (i in 1:no_studies) {
    data$zeroarms_total[data$StudyID==i] <- sum(data$zeroarms[data$StudyID==i]) #add up number of zero arms
  }
  data<-data[data$zeroarms_total<2,] # only keep studies with 1 or no zero-arms
  data$zeroarms<-NULL
  data$zeroarms_total<-NULL #no longer needed
  no_rows<-nrow(data) #recalculate number of rows in data
}
  
   
# set up #
#--------#
  
# add a column that gives a list of all treatments within the respective study #
data$Treatments<-NA # empty column ready
class(data$Treatments) <- "list" #change format of column
for (i in 1:no_studies) { #for each study
data$Treatments[data$StudyID==i]<-list(data$T[data$StudyID==i]) # fill all rows of respective study with a list containing all treatments 
}

# set up base vectors for connected studies and included treatments #
connected_studies<-vector() #starts empty
connected_treatments<-base_treatments #starts with the base treatments the user wants the network to be connected to (e.g. standard care)

# run code to detect connected studies #
#--------------------------------------#

for (z in 1:no_studies) { # repeat process for as many trials as there are to guarantee everything is captured

# add study ID and other treatments in study to connected studies and included treatments vector resp. if the study contains any of the treatments in the included treatments vector (cyclic process) #
for (i in 1:no_rows) {
  if (max(sapply(data$Treatments[i], is.element, el = connected_treatments))==1) { #if any of the treatments in the study's treatment list equals any of the entries currently in 'connected treatments'
    connected_studies<-c(connected_studies,data$StudyID[i]) #add said studyID
    connected_treatments<-c(connected_treatments,data$T[i]) #add study treatments (have to take T[i] rather than Treatments[i] as Treatments[i] is a list whereas T[i] is an integer - by design, all T[i] for said study will be captured)
  }
}

# remove duplicates to give a 'single' list of studies and treatments
connected_studies<-unique(connected_studies)
connected_treatments<-unique(connected_treatments)
}

# indicate in dataframe which studies are connected #
#---------------------------------------------------#

data$Connected<-NA # initialise new column
for (i in 1:no_rows) {
data$Connected[i]<-max(sapply(data$StudyID[i], is.element, el = connected_studies)) #equals 1 if studyID is in the connected studies list, 0 if not
}

data$Treatments<-NULL # not needed

# auto_drop option - automatically removes those not connected #
#--------------------------------------------------------------#

if (auto_drop==TRUE) {
  data<-data[data$Connected==1,] # only keep rows where the Connected column has indicated its a connected study
  data$Connected<-NULL # not needed for this option
}

# auto_recode option - reset numbering system of studies and treatments to be consecutive (can be needed for plotting) #
#----------------------------------------------------------------------------------------------------------------------#

# moving last column to specific place function
move_last <- function(df,x,n) { # df is data frame, x is specified position, n is number of columns
  if (x==1) {
    setcolorder(df,c(n)) # if want it to go to front
  } else if (x==n) {
    # do nothing
  } else {
    x1<-x-1
    n1<-n-1
    setcolorder(df,c(1:x1,n,x:n1))
  }
}

# recode study IDs #
if (auto_recode==TRUE) {
  data <- data[order(data$StudyID),] #order studies by their original order
  setDT(data) [, NewID := .GRP, by = .(StudyID)] # within creating a data.table, adds new column of consecutive IDs grouped by current Study ID
  data <- subset(data, select=-c(StudyID)) #remove old ID
  names(data)[names(data)=="NewID"] <- studyid # give it original name
  move_last(df=data,x=study_pos,n=cols_data) #move studyID back to original position
}

# recode treatments (if they are a separate set of labels) #
if (auto_recode==TRUE & !missing(treatment_labels)) {
  treatment_labels$Connected<-NA # initialise new column
  for (i in 1:no_treatments) {
    treatment_labels$Connected[i]<-max(sapply(treatment_labels$T[i], is.element, el = connected_treatments)) #equals 1 if treatment number is in the connected treatments list, 0 if not
  }
  treatment_labels<-treatment_labels[treatment_labels$Connected==1,] # only keep rows where the Connected column has indicated its a connected treatment
  treatment_labels$Connected<-NULL # not needed for this option
  setDT(treatment_labels) [, NewID := .GRP, by = .(T)] # within creating a data.table, adds new column of consecutive IDs grouped by current Treatment labels
  # append new treatment IDs to data
  treatment_labels_add<-subset(treatment_labels, select=c(T,NewID)) #extract only the columns needed (old ID and new ID)
  data<-merge(data,treatment_labels_add,by="T",all=TRUE,sort=FALSE) #append new IDs to data #
  data<-subset(data, select=-c(T)) # remove old ID
  names(data)[names(data)=="NewID"] <- "T" # back to 'original' name
  move_last(df=data,x=trt_pos,n=cols_data) #move treatment ID back to original position
  treatment_labels<-subset(treatment_labels, select=-c(T)) #remove old ID
  names(treatment_labels)[names(treatment_labels)=="NewID"] <-"T" # back to 'original' name
  move_last(df=treatment_labels,x=trtcode_pos,n=cols_label) #move treatment ID back to original position
}

# output #
#--------#

names(data)[names(data)=="StudyID"] <- studyid # give it original name (if not renumbered)
names(data)[names(data)=="T"] <- treatment #give original name back
names(treatment_labels)[names(treatment_labels)=="T"] <- trtcode

connected<-list("data"=data, "labels"=treatment_labels) # outputs data and treatment labels as given
return(connected)

}