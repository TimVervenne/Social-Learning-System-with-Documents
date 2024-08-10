# Packages ----------------------------------------------------------------
# If packages not installed, install them with use of the function install.packages()
library("gplots")
library("ggplot2")
library("plyr")
library("emmeans")
library("moments")

# Functions ---------------------------------------------------------------
# Create a summary dataframe with the N, mean, sd, SE and CI for a given variable from a given dataframe
# Input: data = dataframe, measurevar = variable you want to generate statistics over, groupvars = grouping variable
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This part executes the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Loading CSV files -------------------------------------------------------
# directory is the directory where the folders with all the simulation data are stored
directory<-"C:\\Users\\timve\\Documents\\Werk\\UvA\\Evolution of Behaviour and Mind\\Research Project 1\\Resultaten\\Simulatie dataframes\\"
names_col_long<-c("MTiS","MTA","SLRS","SLRSA","RMSLPA","MRCPA") # vector containing the names of the variables

# Read all replicate values from all conditions except the mixed learning strategy conditions with the 4_9 skill tree
df_replicates_9_4_MTiS<-read.csv(paste0(directory,"sp4 pd9\\df_overall_meanTraitsInSystem_replicates_sp4_pd9"))
#df_replicates_9_4_MFSiS<-read.csv(paste0(directory,"sp4 pd9\\df_overall_meanSkillsInSystem_replicates_sp4_pd9"))
df_replicates_9_4_MTA<-read.csv(paste0(directory,"sp4 pd9\\df_overall_meanTraitsAgents_replicates_sp4_pd9"))
#df_replicates_9_4_MFSAA<-read.csv(paste0(directory,"sp4 pd9\\df_overall_meanSkillsWithAgents_replicates_sp4_pd9"))
df_replicates_9_4_SLRS<-read.csv(paste0(directory,"sp4 pd9\\df_overall_meanSLrelativeSuccess_replicates_sp4_pd9"))
df_replicates_9_4_SLRSA<-read.csv(paste0(directory,"sp4 pd9\\df_overall_meanSLrealtiveSuccessAgents_replicates_sp4_pd9"))
df_replicates_9_4_RMSLPA<-read.csv(paste0(directory,"sp4 pd9\\df_overall_meanSLpayoffsAgentsRelative_replicates_sp4_pd9"))
df_replicates_9_4_MRCPA<-read.csv(paste0(directory,"sp4 pd9\\df_overall_meanCumulativePayoffsAgentsRelative_replicates_sp4_pd9"))

# Create vectors with all the means from each variable for the 4_9 skill tree
vector_overall_means_9_4_MTiS<-read.csv(paste0(directory,"sp4 pd9\\df_overallMeans_sp4_pd9"))[,11]
#vector_overall_means_9_4_MFSiS<-read.csv(paste0(directory,"sp4 pd9\\df_overallMeans_sp4_pd9"))[,2]
vector_overall_means_9_4_MTA<-read.csv(paste0(directory,"sp4 pd9\\df_overallMeans_sp4_pd9"))[,10]
#vector_overall_means_9_4_MFSAA<-read.csv(paste0(directory,"sp4 pd9\\df_overallMeans_sp4_pd9"))[,3]/37
vector_overall_means_9_4_SLRS<-read.csv(paste0(directory,"sp4 pd9\\df_overallMeans_sp4_pd9"))[,12]
vector_overall_means_9_4_SLRSA<-read.csv(paste0(directory,"sp4 pd9\\df_overallMeans_sp4_pd9"))[,13]
vector_overall_means_9_4_RMSLPA<-read.csv(paste0(directory,"sp4 pd9\\df_overallMeans_sp4_pd9"))[,9]
vector_overall_means_9_4_MRCPA<-read.csv(paste0(directory,"sp4 pd9\\df_overallMeans_sp4_pd9"))[,5]

# Allocate vectors with NA's to put in mean values later
long_vector_replicates_9_4_MTiS<-rep(NA,1200)
#long_vector_replicates_9_4_MFSiS<-rep(NA,1200)
long_vector_replicates_9_4_MTA<-rep(NA,1200)
#long_vector_replicates_9_4_MFSAA<-rep(NA,1200)
long_vector_replicates_9_4_SLRS<-rep(NA,1200)
long_vector_replicates_9_4_SLRSA<-rep(NA,1200)
long_vector_replicates_9_4_RMSLPA<-rep(NA,1200)
long_vector_replicates_9_4_MRCPA<-rep(NA,1200)

# Fill the long vectors with the means from the different replicates
for (i in 1:length(df_replicates_9_4_MTiS[,1])){ #first the row = the simulation
  for (j in 1:length(df_replicates_9_4_MTiS[1,])){ #then the column = single replicates
    long_vector_replicates_9_4_MTiS[(40*(i-1))+j]<-df_replicates_9_4_MTiS[i,j]
    #long_vector_replicates_9_4_MFSiS[(40*(i-1))+j]<-df_replicates_9_4_MFSiS[i,j]
    long_vector_replicates_9_4_MTA[(40*(i-1))+j]<-df_replicates_9_4_MTA[i,j]
    #long_vector_replicates_9_4_MFSAA[(40*(i-1))+j]<-df_replicates_9_4_MFSAA[i,j]
    long_vector_replicates_9_4_SLRS[(40*(i-1))+j]<-df_replicates_9_4_SLRS[i,j]
    long_vector_replicates_9_4_SLRSA[(40*(i-1))+j]<-df_replicates_9_4_SLRSA[i,j]
    long_vector_replicates_9_4_RMSLPA[(40*(i-1))+j]<-df_replicates_9_4_RMSLPA[i,j]
    long_vector_replicates_9_4_MRCPA[(40*(i-1))+j]<-df_replicates_9_4_MRCPA[i,j]
  }
}

# Create a dataframe of all variable values per replicate out of the long vectors
long_format_4_9<-data.frame(cbind(long_vector_replicates_9_4_MTiS,
                                  #long_vector_replicates_9_4_MFSiS,
                                  long_vector_replicates_9_4_MTA,
                                  #long_vector_replicates_9_4_MFSAA,
                                  long_vector_replicates_9_4_SLRS,
                                  long_vector_replicates_9_4_SLRSA,
                                  long_vector_replicates_9_4_RMSLPA,
                                  long_vector_replicates_9_4_MRCPA))
colnames(long_format_4_9)<-names_col_long

### Read all replicate values from the mixed learning strategy condition with the 4_9 skill tree
df_replicates_mixed_9_4_MTiS<-read.csv(paste0(directory,"Mixed sp4 pd9\\df_overall_meanTraitsInSystem_replicates_mixed_sp4_pd9"))
#df_replicates_mixed_9_4_MFSiS<-read.csv(paste0(directory,"Mixed sp4 pd9\\df_overall_meanSkillsInSystem_replicates_mixed_sp4_pd9"))
df_replicates_mixed_9_4_MTA<-read.csv(paste0(directory,"Mixed sp4 pd9\\df_overall_meanTraitsAgents_replicates_mixed_sp4_pd9"))
#df_replicates_mixed_9_4_MFSAA<-read.csv(paste0(directory,"Mixed sp4 pd9\\df_overall_meanSkillsWithAgents_replicates_mixed_sp4_pd9"))
df_replicates_mixed_9_4_SLRS<-read.csv(paste0(directory,"Mixed sp4 pd9\\df_overall_meanSLrelativeSuccess_replicates_mixed_sp4_pd9"))
df_replicates_mixed_9_4_SLRSA<-read.csv(paste0(directory,"Mixed sp4 pd9\\df_overall_meanSLrealtiveSuccessAgents_replicates_mixed_sp4_pd9"))
df_replicates_mixed_9_4_RMSLPA<-read.csv(paste0(directory,"Mixed sp4 pd9\\df_overall_meanSLpayoffsAgentsRelative_replicates_mixed_sp4_pd9"))
df_replicates_mixed_9_4_MRCPA<-read.csv(paste0(directory,"Mixed sp4 pd9\\df_overall_meanCumulativePayoffsAgentsRelative_replicates_mixed_sp4_pd9"))

# Create vectors with all the means from each variable for the 4_9 skill tree mixed learning strategy condition
vector_overall_means_mixed_9_4_MTiS<-read.csv(paste0(directory,"Mixed sp4 pd9\\df_overallMeans_mixed_sp4_pd9"))[,11]
#vector_overall_means_mixed_9_4_MFSiS<-read.csv(paste0(directory,"Mixed sp4 pd9\\df_overallMeans_mixed_sp4_pd9"))[,2]
vector_overall_means_mixed_9_4_MTA<-read.csv(paste0(directory,"Mixed sp4 pd9\\df_overallMeans_mixed_sp4_pd9"))[,10]
#vector_overall_means_mixed_9_4_MFSAA<-read.csv(paste0(directory,"Mixed sp4 pd9\\df_overallMeans_mixed_sp4_pd9"))[,3]/37
vector_overall_means_mixed_9_4_SLRS<-read.csv(paste0(directory,"Mixed sp4 pd9\\df_overallMeans_mixed_sp4_pd9"))[,12]
vector_overall_means_mixed_9_4_SLRSA<-read.csv(paste0(directory,"Mixed sp4 pd9\\df_overallMeans_mixed_sp4_pd9"))[,13]
vector_overall_means_mixed_9_4_RMSLPA<-read.csv(paste0(directory,"Mixed sp4 pd9\\df_overallMeans_mixed_sp4_pd9"))[,9]
vector_overall_means_mixed_9_4_MRCPA<-read.csv(paste0(directory,"Mixed sp4 pd9\\df_overallMeans_mixed_sp4_pd9"))[,5]

# Fill the long vectors with the means from the different replicates
long_vector_replicates_9_4_mixed_MTiS<-rep(NA,240)
#long_vector_replicates_9_4_mixed_MFSiS<-rep(NA,240)
long_vector_replicates_9_4_mixed_MTA<-rep(NA,240)
#long_vector_replicates_9_4_mixed_MFSAA<-rep(NA,240)
long_vector_replicates_9_4_mixed_SLRS<-rep(NA,240)
long_vector_replicates_9_4_mixed_SLRSA<-rep(NA,240)
long_vector_replicates_9_4_mixed_RMSLPA<-rep(NA,240)
long_vector_replicates_9_4_mixed_MRCPA<-rep(NA,240)

# Fill the long vectors with the means from the different replicates
for (i in 1:length(df_replicates_mixed_9_4_MTiS[,1])){ #first the row = the simulation
  for (j in 1:length(df_replicates_mixed_9_4_MTiS[1,])){ #then the column = single replicates
    long_vector_replicates_9_4_mixed_MTiS[(40*(i-1))+j]<-df_replicates_mixed_9_4_MTiS[i,j]
    #long_vector_replicates_9_4_mixed_MFSiS[(40*(i-1))+j]<-df_replicates_mixed_9_4_MFSiS[i,j]
    long_vector_replicates_9_4_mixed_MTA[(40*(i-1))+j]<-df_replicates_mixed_9_4_MTA[i,j]
    #long_vector_replicates_9_4_mixed_MFSAA[(40*(i-1))+j]<-df_replicates_mixed_9_4_MFSAA[i,j]
    long_vector_replicates_9_4_mixed_SLRS[(40*(i-1))+j]<-df_replicates_mixed_9_4_SLRS[i,j]
    long_vector_replicates_9_4_mixed_SLRSA[(40*(i-1))+j]<-df_replicates_mixed_9_4_SLRSA[i,j]
    long_vector_replicates_9_4_mixed_RMSLPA[(40*(i-1))+j]<-df_replicates_mixed_9_4_RMSLPA[i,j]
    long_vector_replicates_9_4_mixed_MRCPA[(40*(i-1))+j]<-df_replicates_mixed_9_4_MRCPA[i,j]
  }
}

# Create a dataframe of all variable values per replicate out of the long vectors
long_format_4_9_mixed<-data.frame(cbind(long_vector_replicates_9_4_mixed_MTiS,
                                        #long_vector_replicates_9_4_mixed_MFSiS,
                                        long_vector_replicates_9_4_mixed_MTA,
                                        #long_vector_replicates_9_4_mixed_MFSAA,
                                        long_vector_replicates_9_4_mixed_SLRS,
                                        long_vector_replicates_9_4_mixed_SLRSA,
                                        long_vector_replicates_9_4_mixed_RMSLPA,
                                        long_vector_replicates_9_4_mixed_MRCPA))
colnames(long_format_4_9_mixed)<-names_col_long

### Read all replicate values from the all conditions except the mixed learning strategy condition with the 12_3 skill tree
df_replicates_12_3_MTiS<-do.call(rbind,lapply(paste0(directory,"sp12 pd3\\",list.files(path=paste0(directory,"sp12 pd3"), pattern="meanTraitsInSystem")), read.csv))
#df_replicates_12_3_MFSiS<-do.call(rbind,lapply(paste0(directory,"sp12 pd3\\",list.files(path=paste0(directory,"sp12 pd3"), pattern="meanSkillsInSystem")), read.csv))
df_replicates_12_3_MTA<-do.call(rbind,lapply(paste0(directory,"sp12 pd3\\",list.files(path=paste0(directory,"sp12 pd3"), pattern="meanTraitsAgents")), read.csv))
#df_replicates_12_3_MFSSA<-do.call(rbind,lapply(paste0(directory,"sp12 pd3\\",list.files(path=paste0(directory,"sp12 pd3"), pattern="meanSkillsWithAgents")), read.csv))
df_replicates_12_3_SLRS<-do.call(rbind,lapply(paste0(directory,"sp12 pd3\\",list.files(path=paste0(directory,"sp12 pd3"), pattern="meanSLrelativeSuccess")), read.csv))
df_replicates_12_3_SLRSA<-do.call(rbind,lapply(paste0(directory,"sp12 pd3\\",list.files(path=paste0(directory,"sp12 pd3"), pattern="meanSLrealtiveSuccessAgents")), read.csv))
df_replicates_12_3_RMSLPA<-do.call(rbind,lapply(paste0(directory,"sp12 pd3\\",list.files(path=paste0(directory,"sp12 pd3"), pattern="meanSLpayoffsAgentsRelative")), read.csv))
df_replicates_12_3_MRCPA<-do.call(rbind,lapply(paste0(directory,"sp12 pd3\\",list.files(path=paste0(directory,"sp12 pd3"), pattern="meanCumulativePayoffsAgentsRelative")), read.csv))

# Create vectors with all the means from each variable for the 12_3 skill tree
vector_overall_mean_12_3_MTiS<-do.call(rbind,lapply(paste0(directory,"sp12 pd3\\",list.files(path=paste0(directory,"sp12 pd3"), pattern="overallMeans")), read.csv))[,11]
#vector_overall_mean_12_3_MFSiS<-do.call(rbind,lapply(paste0(directory,"sp12 pd3\\",list.files(path=paste0(directory,"sp12 pd3"), pattern="overallMeans")), read.csv))[,2]
vector_overall_mean_12_3_MTA<-do.call(rbind,lapply(paste0(directory,"sp12 pd3\\",list.files(path=paste0(directory,"sp12 pd3"), pattern="overallMeans")), read.csv))[,10]
#vector_overall_mean_12_3_MFSSA<-do.call(rbind,lapply(paste0(directory,"sp12 pd3\\",list.files(path=paste0(directory,"sp12 pd3"), pattern="overallMeans")), read.csv))[,3]/37
vector_overall_mean_12_3_SLRS<-do.call(rbind,lapply(paste0(directory,"sp12 pd3\\",list.files(path=paste0(directory,"sp12 pd3"), pattern="overallMeans")), read.csv))[,12]
vector_overall_mean_12_3_SLRSA<-do.call(rbind,lapply(paste0(directory,"sp12 pd3\\",list.files(path=paste0(directory,"sp12 pd3"), pattern="overallMeans")), read.csv))[,13]
vector_overall_mean_12_3_RMSLPA<-do.call(rbind,lapply(paste0(directory,"sp12 pd3\\",list.files(path=paste0(directory,"sp12 pd3"), pattern="overallMeans")), read.csv))[,9]
vector_overall_mean_12_3_MRCPA<-do.call(rbind,lapply(paste0(directory,"sp12 pd3\\",list.files(path=paste0(directory,"sp12 pd3"), pattern="overallMeans")), read.csv))[,5]

# Allocate vectors with NA's to put in mean values later
long_vector_replicates_12_3_MTiS<-rep(NA,1200)
#long_vector_replicates_12_3_MFSiS<-rep(NA,1200)
long_vector_replicates_12_3_MTA<-rep(NA,1200)
#long_vector_replicates_12_3_MFSAA<-rep(NA,1200)
long_vector_replicates_12_3_SLRS<-rep(NA,1200)
long_vector_replicates_12_3_SLRSA<-rep(NA,1200)
long_vector_replicates_12_3_RMSLPA<-rep(NA,1200)
long_vector_replicates_12_3_MRCPA<-rep(NA,1200)

# Fill the long vectors with the means from the different replicates
for (i in 1:length(df_replicates_12_3_MTiS[,1])){ #first the row = the simulation
  for (j in 1:length(df_replicates_12_3_MTiS[1,])){ #then the column = single replicates
    long_vector_replicates_12_3_MTiS[(40*(i-1))+j]<-df_replicates_12_3_MTiS[i,j]
    #long_vector_replicates_12_3_MFSiS[(40*(i-1))+j]<-df_replicates_12_3_MFSiS[i,j]
    long_vector_replicates_12_3_MTA[(40*(i-1))+j]<-df_replicates_12_3_MTA[i,j]
    #long_vector_replicates_12_3_MFSAA[(40*(i-1))+j]<-df_replicates_12_3_MFSAA[i,j]
    long_vector_replicates_12_3_SLRS[(40*(i-1))+j]<-df_replicates_12_3_SLRS[i,j]
    long_vector_replicates_12_3_SLRSA[(40*(i-1))+j]<-df_replicates_12_3_SLRSA[i,j]
    long_vector_replicates_12_3_RMSLPA[(40*(i-1))+j]<-df_replicates_12_3_RMSLPA[i,j]
    long_vector_replicates_12_3_MRCPA[(40*(i-1))+j]<-df_replicates_12_3_MRCPA[i,j]
  }
}

# Create a dataframe of all variable values per replicate out of the long vectors
long_format_12_3<-data.frame(cbind(long_vector_replicates_12_3_MTiS,
                                   #long_vector_replicates_12_3_MFSiS,
                                   long_vector_replicates_12_3_MTA,
                                   #long_vector_replicates_12_3_MFSAA,
                                   long_vector_replicates_12_3_SLRS,
                                   long_vector_replicates_12_3_SLRSA,
                                   long_vector_replicates_12_3_RMSLPA,
                                   long_vector_replicates_12_3_MRCPA))
colnames(long_format_12_3)<-names_col_long

### Read all replicate values from the mixed learning strategy condition with the 12_3 skill tree
df_replicates_mixed_12_3_MTiS<-read.csv(paste0(directory,"Mixed sp12 pd3\\df_overall_meanTraitsInSystem_replicates_mixed,sp12_pd3"))
df_replicates_mixed_12_3_MFSiS<-read.csv(paste0(directory,"Mixed sp12 pd3\\df_overall_meanSkillsInSystem_replicates_mixed,sp12_pd3"))
df_replicates_mixed_12_3_MTA<-read.csv(paste0(directory,"Mixed sp12 pd3\\df_overall_meanTraitsAgents_replicates_mixed,sp12_pd3"))
df_replicates_mixed_12_3_MFSAA<-read.csv(paste0(directory,"Mixed sp12 pd3\\df_overall_meanSkillsWithAgents_replicates_mixed,sp12_pd3"))
df_replicates_mixed_12_3_SLRS<-read.csv(paste0(directory,"Mixed sp12 pd3\\df_overall_meanSLrelativeSuccess_replicates_mixed,sp12_pd3"))
df_replicates_mixed_12_3_SLRSA<-read.csv(paste0(directory,"Mixed sp12 pd3\\df_overall_meanSLrealtiveSuccessAgents_replicates_mixed,sp12_pd3"))
df_replicates_mixed_12_3_RMSLPA<-read.csv(paste0(directory,"Mixed sp12 pd3\\df_overall_meanSLpayoffsAgentsRelative_replicates_mixed,sp12_pd3"))
df_replicates_mixed_12_3_MRCPA<-read.csv(paste0(directory,"Mixed sp12 pd3\\df_overall_meanCumulativePayoffsAgentsRelative_replicates_mixed,sp12_pd3"))

# Create vectors with all the means from each variable for the 12_3 skill tree for mixed learning strategy 
vector_overall_means_mixed_12_3_MTiS<-read.csv(paste0(directory,"Mixed sp12 pd3\\df_overallMeans_sp12_pd3"))[,11]
vector_overall_means_mixed_12_3_MFSiS<-read.csv(paste0(directory,"Mixed sp12 pd3\\df_overallMeans_sp12_pd3"))[,2]
vector_overall_means_mixed_12_3_MTA<-read.csv(paste0(directory,"Mixed sp12 pd3\\df_overallMeans_sp12_pd3"))[,10]
vector_overall_means_mixed_12_3_MFSAA<-read.csv(paste0(directory,"Mixed sp12 pd3\\df_overallMeans_sp12_pd3"))[,3]/37
vector_overall_means_mixed_12_3_SLRS<-read.csv(paste0(directory,"Mixed sp12 pd3\\df_overallMeans_sp12_pd3"))[,12]
vector_overall_means_mixed_12_3_SLRSA<-read.csv(paste0(directory,"Mixed sp12 pd3\\df_overallMeans_sp12_pd3"))[,13]
vector_overall_means_mixed_12_3_RMSLPA<-read.csv(paste0(directory,"Mixed sp12 pd3\\df_overallMeans_sp12_pd3"))[,9]
vector_overall_means_mixed_12_3_MRCPA<-read.csv(paste0(directory,"Mixed sp12 pd3\\df_overallMeans_sp12_pd3"))[,5]

# Allocate vectors with NA's to put in mean values later
long_vector_replicates_12_3_mixed_MTiS<-rep(NA,240)
#long_vector_replicates_12_3_mixed_MFSiS<-rep(NA,240)
long_vector_replicates_12_3_mixed_MTA<-rep(NA,240)
#long_vector_replicates_12_3_mixed_MFSAA<-rep(NA,240)
long_vector_replicates_12_3_mixed_SLRS<-rep(NA,240)
long_vector_replicates_12_3_mixed_SLRSA<-rep(NA,240)
long_vector_replicates_12_3_mixed_RMSLPA<-rep(NA,240)
long_vector_replicates_12_3_mixed_MRCPA<-rep(NA,240)

# Fill the long vectors with the means from the different replicates
for (i in 1:length(df_replicates_mixed_12_3_MTiS[,1])){ #first the row = the simulation
  for (j in 1:length(df_replicates_mixed_12_3_MTiS[1,])){ #then the column = single replicates
    long_vector_replicates_12_3_mixed_MTiS[(40*(i-1))+j]<-df_replicates_mixed_12_3_MTiS[i,j]
    #long_vector_replicates_12_3_mixed_MFSiS[(40*(i-1))+j]<-df_replicates_mixed_12_3_MFSiS[i,j]
    long_vector_replicates_12_3_mixed_MTA[(40*(i-1))+j]<-df_replicates_mixed_12_3_MTA[i,j]
    #long_vector_replicates_12_3_mixed_MFSAA[(40*(i-1))+j]<-df_replicates_mixed_12_3_MFSAA[i,j]
    long_vector_replicates_12_3_mixed_SLRS[(40*(i-1))+j]<-df_replicates_mixed_12_3_SLRS[i,j]
    long_vector_replicates_12_3_mixed_SLRSA[(40*(i-1))+j]<-df_replicates_mixed_12_3_SLRSA[i,j]
    long_vector_replicates_12_3_mixed_RMSLPA[(40*(i-1))+j]<-df_replicates_mixed_12_3_RMSLPA[i,j]
    long_vector_replicates_12_3_mixed_MRCPA[(40*(i-1))+j]<-df_replicates_mixed_12_3_MRCPA[i,j]
  }
}

# Create a dataframe of all variable values per replicate out of the long vectors
long_format_12_3_mixed<-data.frame(cbind(long_vector_replicates_12_3_mixed_MTiS,
                                        #long_vector_replicates_12_3_mixed_MFSiS,
                                        long_vector_replicates_12_3_mixed_MTA,
                                        #long_vector_replicates_12_3_mixed_MFSAA,
                                        long_vector_replicates_12_3_mixed_SLRS,
                                        long_vector_replicates_12_3_mixed_SLRSA,
                                        long_vector_replicates_12_3_mixed_RMSLPA,
                                        long_vector_replicates_12_3_mixed_MRCPA))
colnames(long_format_12_3_mixed)<-names_col_long

# Dataframe long format -------------------------------------------
# Generating labels of the long format
labels_doc_long_format<-c(rep("000",200),rep("001",200),rep("010",200),rep("050",200),rep("100",200),rep("500",200),
                          rep(c(rep("000",40),rep("001",40),rep("010",40),rep("050",40),rep("100",40),rep("500",40)),7))
labels_skill_tree_long_format<-c(rep("4_9",1440),rep("12_3",1440))
labels_LS_long_format<-c(rep(c(rep("0",40),rep("1",40),rep("2",40),rep("3",40),rep("4",40)),6),
                         rep("mixed",240),
                         rep("0",240),rep("1",240),rep("2",240),rep("3",240),rep("4",240),rep("mixed",240))

# Combining all the dataframes to one single dataframe and write to a datafile
all_df_long_format<-rbind(long_format_4_9,long_format_4_9_mixed,long_format_12_3,long_format_12_3_mixed)
all_df_long_format<-cbind(all_df_long_format,as.factor(labels_doc_long_format),as.factor(labels_LS_long_format),as.factor(labels_skill_tree_long_format))
colnames(all_df_long_format)<-names_col_long<-c("MTiS","MTA","SLRS","SLRSA","RMSLPA","MRCPA","num_doc","LS","skill_tree")
#write.csv(all_df_long_format, file="All Replicates Long Format")


# Both Skill Trees Data--------------------------------------------------
# Create summary statistics for the different variables and conditions
MTiS_summary_SE<-summarySE(all_df_long_format, measurevar="MTiS", groupvars=c("num_doc", "LS"))
MTA_summary_SE<-summarySE(all_df_long_format, measurevar="MTA", groupvars=c("num_doc","LS"))
SLRSA_summary_SE<-summarySE(all_df_long_format, measurevar="SLRSA", groupvars=c("num_doc","LS"))
RMSLPA_summary_SE<-summarySE(all_df_long_format, measurevar="RMSLPA", groupvars=c("num_doc","LS"))
MRCPA_summary_SE<-summarySE(all_df_long_format, measurevar="MRCPA", groupvars=c("num_doc","LS"))

MTiS_summary_SE_doc<-summarySE(all_df_long_format, measurevar="MTiS", groupvars=c("num_doc"))
MTA_summary_SE_doc<-summarySE(all_df_long_format, measurevar="MTA", groupvars=c("num_doc"))
SLRSA_summary_SE_doc<-summarySE(all_df_long_format, measurevar="SLRSA", groupvars=c("num_doc"))
RMSLPA_summary_SE_doc<-summarySE(all_df_long_format, measurevar="RMSLPA", groupvars=c("num_doc"))
MRCPA_summary_SE_doc<-summarySE(all_df_long_format, measurevar="MRCPA", groupvars=c("num_doc"))

MTiS_summary_SE_LS<-summarySE(all_df_long_format, measurevar="MTiS", groupvars=c("LS"))
MTA_summary_SE_LS<-summarySE(all_df_long_format, measurevar="MTA", groupvars=c("LS"))
SLRSA_summary_SE_LS<-summarySE(all_df_long_format, measurevar="SLRSA", groupvars=c("LS"))
RMSLPA_summary_SE_LS<-summarySE(all_df_long_format, measurevar="RMSLPA", groupvars=c("LS"))
MRCPA_summary_SE_LS<-summarySE(all_df_long_format, measurevar="MRCPA", groupvars=c("LS"))

# Both Skill Trees Plots -------------------------------------------------------------------
# Number of documents + LS
ggplot(MTiS_summary_SE, aes(x = num_doc, y = MTiS, color = LS, group = LS)) +
  geom_line(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=MTiS-ci, ymax=MTiS+ci), colour="black",width=.7, position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.2) ,size=3, shape=21, fill="white") +
  coord_cartesian(ylim = c(0.14, 0.30)) +
  labs(x = "\nAmount of documents", y = "Means of relative MTiS\n") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  #ggtitle("Means of relative mean number of traits among all individuals in \nsystems differing in learning strategies and amount of documents\n") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
        axis.text=element_text(size=10),
        axis.title=element_text(size=11),
        legend.title = element_text(size=11), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size)

ggplot(MTA_summary_SE, aes(x = num_doc, y = MTA, group = LS, color = LS)) +
  geom_line(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=MTA-ci, ymax=MTA+ci), colour="black",width=.5, position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.2) ,size=3, shape=21, fill="white") +
  coord_cartesian(ylim = c(0.1375, 0.185)) +
  labs(x = "\nAmount of documents", y = "Means of relative MTA\n") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  #ggtitle("Relative mean amount of traits among agents in \nsystems differing in learning strategies and amount of documents\n") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
        axis.text=element_text(size=10),
        axis.title=element_text(size=11),
        legend.title = element_text(size=11), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size)

ggplot(SLRSA_summary_SE, aes(x = num_doc, y = SLRSA, group = LS, color = LS)) +
  geom_line(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=SLRSA-ci, ymax=SLRSA+ci), colour="black",width=.5, position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.2) ,size=3, shape=21, fill="white") +
  labs(x = "\nAmount of documents", y = "Means of mean SLSRA\n") +
  coord_cartesian(ylim = c(0.2475, 0.3375)) +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  #ggtitle("Means of agents' mean social learning success rate of agents in \nsystems differing in learning strategies and amount of documents\n") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
        axis.text=element_text(size=10),
        axis.title=element_text(size=11),
        legend.title = element_text(size=11), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size)

ggplot(RMSLPA_summary_SE, aes(x = num_doc, y = RMSLPA, group = LS, color = LS)) +
  geom_line(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=RMSLPA-ci, ymax=RMSLPA+ci), colour="black",width=.5, position=position_dodge(0.2)) +
  coord_cartesian(ylim = c(0.58, 0.83)) +
  geom_point(position=position_dodge(0.2) ,size=3, shape=21, fill="white") +
  labs(x = "\nAmount of documents", y = "Means of RMSLPA\n") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  #ggtitle("Means of mean relative social learning pay-off of agents in \nsystems differing in learning strategies and amount of documents\n") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
        axis.text=element_text(size=10),
        axis.title=element_text(size=11),
        legend.title = element_text(size=11), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size)

ggplot(MRCPA_summary_SE, aes(x = num_doc, y = MRCPA, group = LS, color = LS)) +
  geom_line(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=MRCPA-ci, ymax=MRCPA+ci), colour="black",width=.5, position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.2) ,size=3, shape=21, fill="white") +
  coord_cartesian(ylim = c(0.07, 0.12)) +
  labs(x = "\nAmount of documents", y = "Means of MRCPA\n") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  #ggtitle("Means of relative mean cumulative pay-off earnings by agents in \nsystems differing in learning strategies and amount of documents\n") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
        axis.text=element_text(size=10),
        axis.title=element_text(size=11),
        legend.title = element_text(size=11), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size)

# Number of documents
ggplot(MTiS_summary_SE_doc, aes(x = num_doc, y = MTiS)) +
  geom_errorbar(aes(ymin=MTiS-ci, ymax=MTiS+ci), colour="black",width=.1) +
  geom_point(size=3, shape=21, fill="black") +
  coord_cartesian(ylim = c(0.14, 0.30)) +
  labs(x = "\nAmount of documents", y = "Means of relative MTiS\n") +
  #ggtitle("Means of relative mean number of traits among all individuals \nin systems differing in amount of documents\n") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
        axis.text=element_text(size=10),
        axis.title=element_text(size=11),
        legend.position="none")

ggplot(MTA_summary_SE_doc, aes(x = num_doc, y = MTA)) +
  geom_errorbar(aes(ymin=MTA-ci, ymax=MTA+ci), colour="black",width=.1) +
  geom_point(size=3, shape=21, fill="black") +
  labs(x = "\nAmount of documents", y = "Means of relative MTA\n") +
  coord_cartesian(ylim = c(0.1375, 0.185)) +
  #ggtitle("Relative mean amount of traits among agents \nin systems differing in amount of documents\n") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11),
    legend.position="none")

ggplot(SLRSA_summary_SE_doc, aes(x = num_doc, y = SLRSA)) +
  geom_errorbar(aes(ymin=SLRSA-ci, ymax=SLRSA+ci), colour="black",width=.1) +
  geom_point(size=3, shape=21, fill="black") +
  coord_cartesian(ylim = c(0.2475, 0.3375)) +
  labs(x = "\nAmount of documents", y = "Means of mean SLSRA\n") +
  #ggtitle("Means of agents' mean social learning success rate of agents \nin systems differing in amount of documents\n") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11),
    legend.position="none")

ggplot(RMSLPA_summary_SE_doc, aes(x = num_doc, y = RMSLPA)) +
  geom_errorbar(aes(ymin=RMSLPA-ci, ymax=RMSLPA+ci), colour="black",width=.1) +
  geom_point(size=3, shape=21, fill="black") +
  coord_cartesian(ylim = c(0.58, 0.83)) +
  labs(x = "\nAmount of documents", y = "Means of RMSLPA\n") +
  #ggtitle("Means of mean relative social learning pay-off of agents \nin systems differing in amount of documents\n") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11),
    legend.position="none") 

ggplot(MRCPA_summary_SE_doc, aes(x = num_doc, y = MRCPA)) +
  geom_errorbar(aes(ymin=MRCPA-ci, ymax=MRCPA+ci), colour="black",width=.1) +
  geom_point(size=3, shape=21, fill="black") +
  coord_cartesian(ylim = c(0.07, 0.12)) +
  labs(x = "\nAmount of documents", y = "Means of MRCPA\n") +
  #ggtitle("Means of relative mean cumulative pay-off earnings by agents \nin systems differing in amount of documents\n") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11),
    legend.position="none") 

# Learning Strategies
ggplot(MTiS_summary_SE_LS, aes(x = LS, y = MTiS, color = LS)) +
  geom_errorbar(aes(ymin=MTiS-ci, ymax=MTiS+ci), colour="black",width=.1) +
  geom_point(size=3, shape=21, fill="white") +
  coord_cartesian(ylim = c(0.14, 0.30)) +
  scale_x_discrete(label = c("Random","Conformist","Age-based","Similarity","Pay-off", "Mixed")) +
  labs(x = "\nLearning Strategy", y = "Means of relative MTiS\n") +
  #ggtitle("Means of relative mean number of traits among all individuals \nin systems differing in learning strategies\n") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11),
    axis.text.x = element_text(angle = 20, hjust=0.6, vjust=0.6),
    legend.position="none")

ggplot(MTA_summary_SE_LS, aes(x = LS, y = MTA, color = LS)) +
  geom_errorbar(aes(ymin=MTA-ci, ymax=MTA+ci), colour="black",width=.1) +
  geom_point(size=3, shape=21, fill="white") +
  coord_cartesian(ylim = c(0.1375, 0.185)) +
  scale_x_discrete(label = c("Random","Conformist","Age-based","Similarity","Pay-off", "Mixed")) +
  labs(x = "\nLearning Strategy", y = "Means of relative MTA\n") +
  #ggtitle("Relative mean amount of traits among agents \nin systems differing in learning strategies\n") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11),
    axis.text.x = element_text(angle = 20, hjust=0.6, vjust=0.6),
    legend.position="none")

ggplot(SLRSA_summary_SE_LS, aes(x = LS, y = SLRSA, color = LS)) +
  geom_errorbar(aes(ymin=SLRSA-ci, ymax=SLRSA+ci), colour="black",width=.1) +
  geom_point(size=3, shape=21, fill="white") +
  coord_cartesian(ylim = c(0.2475, 0.3375)) +
  scale_x_discrete(label = c("Random","Conformist","Age-based","Similarity","Pay-off", "Mixed")) +
  labs(x = "\nLearning Strategy", y = "Means of mean SLSRA\n") +
  #ggtitle("Means of agents' mean social learning success rate of agents \nin systems differing in learning strategies\n") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11),
    axis.text.x = element_text(angle = 20, hjust=0.6, vjust=0.6),
    legend.position="none")

ggplot(RMSLPA_summary_SE_LS, aes(x = LS, y = RMSLPA, color = LS)) +
  geom_errorbar(aes(ymin=RMSLPA-ci, ymax=RMSLPA+ci), colour="black",width=.1) +
  geom_point(size=3, shape=21, fill="white") +
  coord_cartesian(ylim = c(0.58, 0.83)) +
  scale_x_discrete(label = c("Random","Conformist","Age-based","Similarity","Pay-off", "Mixed")) +
  labs(x = "\nLearning Strategy", y = "Means of RMSLPA\n") +
  #ggtitle("Means of mean relative social learning pay-off of agents \nin systems differing in learning strategies\n") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11),
    axis.text.x = element_text(angle = 20, hjust=0.6, vjust=0.6),
    legend.position="none")

ggplot(MRCPA_summary_SE_LS, aes(x = LS, y = MRCPA, color = LS)) +
  geom_errorbar(aes(ymin=MRCPA-ci, ymax=MRCPA+ci), colour="black",width=.1) +
  geom_point(size=3, shape=21, fill="white") +
  coord_cartesian(ylim = c(0.07, 0.12)) +
  scale_x_discrete(label = c("Random","Conformist","Age-based","Similarity","Pay-off", "Mixed")) +
  labs(x = "\nLearning Strategy", y = "Means of MRCPA\n") +
  #ggtitle("Means of relative mean cumulative pay-off earnings by agents \nin systems differing in learning strategies\n") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11),
    axis.text.x = element_text(angle = 20, hjust=0.6, vjust=0.6),
    legend.position="none")

# Both Skill Trees Statistics --------------------------------------------------------------
# Checken assumpties factorial ANOVA
plot(lm(MTiS ~ num_doc + LS + skill_tree, all_df_long_format))
plot(lm(MTA ~ num_doc + LS + skill_tree, all_df_long_format))
plot(lm(SLRSA ~ num_doc + LS + skill_tree, all_df_long_format))
plot(lm(RMSLPA ~ num_doc + LS + skill_tree, all_df_long_format))
plot(lm(MRCPA ~ num_doc + LS + skill_tree, all_df_long_format))

# Generating Statistics MTiS
summary(lm(MTiS~num_doc + LS, all_df_long_format))
summary(lm(MTiS~num_doc,data=all_df_long_format))
summary(lm(MTiS~LS,data=all_df_long_format))
summary(lm(MTiS~skill_tree,data=all_df_long_format))
#summary(lm(MTiS~num_doc*LS,data=all_df_long_format))
#summary(lm(MTiS~num_doc*skill_tree, data=all_df_long_format))
#summary(lm(MTiS~LS*skill_tree, data=all_df_long_format))
#summary(lm(MTiS~num_doc*LS*skill_tree,data=all_df_long_format))

pairs(emmeans(lm(MTiS~LS,data=all_df_long_format[all_df_long_format$num_doc=="000",]),spec=c("LS")))
pairs(emmeans(lm(MTiS~num_doc+LS,data=all_df_long_format),spec=c("num_doc")))
#pairs(emmeans(lm(MTiS~num_doc*LS,data=all_df_long_format),spec=c("LS")))
#pairs(emmeans(lm(MTiS~num_doc*LS*skill_tree,data=all_df_long_format),spec=c("LS","skill_tree")))
#pairs(emmeans(lm(MTiS~num_doc*LS*skill_tree,data=all_df_long_format),spec=c("num_doc","skill_tree")))

# Generating Statistics MTA
summary(lm(MTA~num_doc+LS, all_df_long_format))
summary(lm(MTA~num_doc,data=all_df_long_format))
summary(lm(MTA~LS,data=all_df_long_format))
summary(lm(MTA~skill_tree,data=all_df_long_format))
#summary(lm(MTA~num_doc*LS,data=all_df_long_format))
#summary(lm(MTA~num_doc*skill_tree, data=all_df_long_format))
#summary(lm(MTA~LS*skill_tree, data=all_df_long_format))
#summary(lm(MTA~num_doc*LS*skill_tree,data=all_df_long_format))

pairs(emmeans(lm(MTA~num_doc+LS,data=all_df_long_format),spec=c("num_doc")))
pairs(emmeans(lm(MTA~LS,data=all_df_long_format[all_df_long_format$num_doc=="000",]),spec=c("LS")))
#pairs(emmeans(lm(MTA~num_doc*LS*skill_tree,data=all_df_long_format),spec=c("num_doc","LS")))
#pairs(emmeans(lm(MTA~num_doc*LS*skill_tree,data=all_df_long_format),spec=c("LS","skill_tree")))
#pairs(emmeans(lm(MTA~num_doc*LS*skill_tree,data=all_df_long_format),spec=c("num_doc","skill_tree")))

# Generating Statistics SLRSA
summary(lm(SLRSA~num_doc+LS,data=all_df_long_format))
summary(lm(SLRSA~num_doc,data=all_df_long_format))
summary(lm(SLRSA~LS,data=all_df_long_format))
summary(lm(SLRSA~skill_tree,data=all_df_long_format))
#summary(lm(SLRSA~num_doc*LS,data=all_df_long_format))
#summary(lm(SLRSA~num_doc*skill_tree, data=all_df_long_format))
#summary(lm(SLRSA~LS*skill_tree, data=all_df_long_format))
#summary(lm(SLRSA~num_doc*LS*skill_tree,data=all_df_long_format))

#pairs(emmeans(lm(SLRSA),spec=c("num_doc","LS")))
pairs(emmeans(lm(SLRSA~num_doc+LS,data=all_df_long_format),spec=c("num_doc")))
pairs(emmeans(lm(SLRSA~LS,data=all_df_long_format[all_df_long_format$num_doc=="000",]),spec=c("LS")))
#pairs(emmeans(lm(SLRSA~num_doc*LS*skill_tree,data=all_df_long_format),spec=c("LS","skill_tree")))
#pairs(emmeans(lm(SLRSA~num_doc*LS*skill_tree,data=all_df_long_format),spec=c("num_doc","skill_tree")))

# Generating Statistics RMSLPA
summary(lm(RMSLPA~num_doc+LS,data=all_df_long_format))
summary(lm(RMSLPA~num_doc,data=all_df_long_format))
summary(lm(RMSLPA~LS,data=all_df_long_format))
summary(lm(RMSLPA~skill_tree,data=all_df_long_format))
#summary(lm(RMSLPA~num_doc*LS,data=all_df_long_format))
#summary(lm(RMSLPA~num_doc*skill_tree, data=all_df_long_format))
#summary(lm(RMSLPA~LS*skill_tree, data=all_df_long_format))
#summary(lm(RMSLPA~num_doc*LS*skill_tree,data=all_df_long_format))

pairs(emmeans(lm(RMSLPA~num_doc+LS,data=all_df_long_format),spec=c("num_doc")))
pairs(emmeans(lm(RMSLPA~num_doc+LS,data=all_df_long_format),spec=c("LS")))
pairs(emmeans(lm(RMSLPA~LS,data=all_df_long_format[all_df_long_format$num_doc=="000",]),spec=c("LS")))
#pairs(emmeans(lm(RMSLPA~num_doc*LS*skill_tree,data=all_df_long_format),spec=c("num_doc","LS")))
#pairs(emmeans(lm(RMSLPA~num_doc*LS*skill_tree,data=all_df_long_format),spec=c("LS","skill_tree")))
#pairs(emmeans(lm(RMSLPA~num_doc*LS*skill_tree,data=all_df_long_format),spec=c("num_doc","skill_tree")))

# Generating Statistics MRCPA
summary(lm(MRCPA~num_doc+LS, all_df_long_format))
summary(lm(MRCPA~num_doc,data=all_df_long_format))
summary(lm(MRCPA~LS,data=all_df_long_format))
summary(lm(MRCPA~skill_tree,data=all_df_long_format))
#summary(lm(MRCPA~num_doc*LS,data=all_df_long_format))
#summary(lm(MRCPA~num_doc*skill_tree, data=all_df_long_format))
#summary(lm(MRCPA~LS*skill_tree, data=all_df_long_format))
#summary(lm(MRCPA~num_doc*LS*skill_tree,data=all_df_long_format))

#pairs(emmeans(lm(MRCPA~num_doc*LS*skill_tree,data=all_df_long_format),spec=c("num_doc","LS")))
pairs(emmeans(lm(MRCPA~num_doc+LS,data=all_df_long_format),spec=c("num_doc")))
pairs(emmeans(lm(MRCPA~num_doc+LS,data=all_df_long_format),spec=c("LS")))
#pairs(emmeans(lm(MRCPA~num_doc*LS*skill_tree,data=all_df_long_format),spec=c("LS","skill_tree")))
#pairs(emmeans(lm(MRCPA~num_doc*LS*skill_tree,data=all_df_long_format),spec=c("num_doc","skill_tree")))

# 4_9 Data ---------------------------------------------------------------------
df_4_9_long_format<-all_df_long_format[all_df_long_format$skill_tree=="4_9",]

MTiS_4_9_summary_SE<-summarySE(df_4_9_long_format, measurevar="MTiS", groupvars=c("num_doc","LS"))
MTA_4_9_summary_SE<-summarySE(df_4_9_long_format, measurevar="MTA", groupvars=c("num_doc","LS"))
SLRSA_4_9_summary_SE<-summarySE(df_4_9_long_format, measurevar="SLRSA", groupvars=c("num_doc","LS"))
RMSLPA_4_9_summary_SE<-summarySE(df_4_9_long_format, measurevar="RMSLPA", groupvars=c("num_doc","LS"))
MRCPA_4_9_summary_SE<-summarySE(df_4_9_long_format, measurevar="MRCPA", groupvars=c("num_doc","LS"))


# 4_9 Plots ---------------------------------------------------------------
# Mean Traits in System
ggplot(MTiS_4_9_summary_SE, aes(x = num_doc, y = MTiS, group = LS, color = LS)) +
  geom_line(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=MTiS-ci, ymax=MTiS+ci), colour="black",width=.5, position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.2) ,size=3, shape=21, fill="white") +
  labs(x = "Amount of documents", y = "Mean number of traits in system") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Mean amount of traits among all individuals \nwith varying document amounts between systems") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# 4_9 Statistics ----------------------------------------------------------
plot(aov(MTiS ~ num_doc + LS,data=df_4_9_long_format))

summary(aov(MTiS ~ num_doc + LS, df_4_9_long_format))
summary(lm(MTiS ~ num_doc * LS,data=df_4_9_long_format))
summary(lm(MTiS~LS,data=df_4_9_long_format))
summary(lm(MTiS~skill_tree,data=df_4_9_long_format))
#summary(lm(MTiS~num_doc*LS,data=df_4_9_long_format))
#summary(lm(MTiS~num_doc*skill_tree, data=df_4_9_long_format))
#summary(lm(MTiS~LS*skill_tree, data=df_4_9_long_format))
#summary(lm(MTiS~num_doc*LS*skill_tree,data=df_4_9_long_format))

pairs(emmeans(lm(MTiS~num_doc*LS*skill_tree,data=df_4_9_long_format),spec=c("num_doc","LS")))
#pairs(emmeans(lm(MTiS~num_doc*LS*skill_tree,data=df_4_9_long_format),spec=c("num_doc")))
#pairs(emmeans(lm(MTiS~num_doc*LS*skill_tree,data=df_4_9_long_format),spec=c("LS")))
#pairs(emmeans(lm(MTiS~num_doc*LS*skill_tree,data=df_4_9_long_format),spec=c("LS","skill_tree")))
#pairs(emmeans(lm(MTiS~num_doc*LS*skill_tree,data=df_4_9_long_format),spec=c("num_doc","skill_tree")))

# Mean Traits Agents
ggplot(MTA_4_9_summary_SE, aes(x = num_doc, y = MTA, group = LS, color = LS)) +
  geom_line(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=MTA-ci, ymax=MTA+ci), colour="black",width=.5, position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.2) ,size=3, shape=21, fill="white") +
  labs(x = "Amount of documents", y = "Mean number of traits with agents") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Mean amount of traits among agents \nwith varying document amounts between systems") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Mean Social Learning Relative Success Agents
ggplot(SLRSA_4_9_summary_SE, aes(x = num_doc, y = SLRSA, group = LS, color = LS)) +
  geom_line(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=SLRSA-ci, ymax=SLRSA+ci), colour="black",width=.5, position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.2) ,size=3, shape=21, fill="white") +
  labs(x = "Amount of documents", y = "Mean number of traits in agents") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Mean relative success rate of agents by varying\n learning strategies and amount of documents in the system") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Relative mean social learning payoff agents
ggplot(RMSLPA_4_9_summary_SE, aes(x = num_doc, y = RMSLPA, group = LS, color = LS)) +
  geom_line(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=RMSLPA-ci, ymax=RMSLPA+ci), colour="black",width=.5, position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.2) ,size=3, shape=21, fill="white") +
  labs(x = "Amount of documents", y = "Mean relative social learning payoff of agents") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Mean relative social learning pay-off of agents by varying\n learning strategies and amount of documents in the system") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Mean Relative Cumulative Pay-off Agents
ggplot(MRCPA_4_9_summary_SE, aes(x = num_doc, y = MRCPA, group = LS, color = LS)) +
  geom_line(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=MRCPA-ci, ymax=MRCPA+ci), colour="black",width=.5, position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.2) ,size=3, shape=21, fill="white") +
  labs(x = "Amount of documents", y = "Mean relative cumulative pay-off of agents") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Mean relative cumulative pay-off of agents by varying\n learning strategies and amount of documents in the system") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

boxplot(MTiS~num_doc,data=df_4_9_long_format)
boxplot(MTA~num_doc,data=df_4_9_long_format)
boxplot(SLRSA~num_doc,data=df_4_9_long_format)
boxplot(RMSLPA~num_doc,data=df_4_9_long_format)
boxplot(MRCPA~num_doc,data=df_4_9_long_format)

boxplot(MTiS~LS,data=df_4_9_long_format)
boxplot(MTA~LS,data=df_4_9_long_format)
boxplot(SLRSA~LS,data=df_4_9_long_format)
boxplot(RMSLPA~LS,data=df_4_9_long_format)
boxplot(MRCPA~LS,data=df_4_9_long_format)

summary(lm(MTiS~num_doc+LS,data=df_4_9_long_format))
pairwise.wilcox.test(df_4_9_long_format$MTiS,df_4_9_long_format$num_doc,p.adjust.method = "bonferroni")
pairwise.wilcox.test(df_4_9_long_format$MTiS,df_4_9_long_format$LS,p.adjust.method = "bonferroni")

# # 12_3 Data------------------------------------------------------------------
df_12_3_long_format<-all_df_long_format[all_df_long_format$skill_tree=="12_3",]

MTiS_12_3_summary_SE<-summarySE(df_12_3_long_format, measurevar="MTiS", groupvars=c("num_doc","LS"))
MTA_12_3_summary_SE<-summarySE(df_12_3_long_format, measurevar="MTA", groupvars=c("num_doc","LS"))
SLRSA_12_3_summary_SE<-summarySE(df_12_3_long_format, measurevar="SLRSA", groupvars=c("num_doc","LS"))
RMSLPA_12_3_summary_SE<-summarySE(df_12_3_long_format, measurevar="RMSLPA", groupvars=c("num_doc","LS"))
MRCPA_12_3_summary_SE<-summarySE(df_12_3_long_format, measurevar="MRCPA", groupvars=c("num_doc","LS"))

# 12_3 Plots ---------------------------------------------------------------
ggplot(MTiS_12_3_summary_SE, aes(x = num_doc, y = MTiS, group = LS, color = LS)) +
  geom_line(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=MTiS-ci, ymax=MTiS+ci), colour="black",width=.5, position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.2) ,size=3, shape=21, fill="white") +
  labs(x = "Amount of documents", y = "Mean number of traits in system") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Mean amount of traits among all individuals \nwith varying document amounts between systems") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(MTA_12_3_summary_SE, aes(x = num_doc, y = MTA, group = LS, color = LS)) +
  geom_line(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=MTA-ci, ymax=MTA+ci), colour="black",width=.5, position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.2) ,size=3, shape=21, fill="white") +
  labs(x = "Amount of documents", y = "Mean number of traits with agents") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Mean amount of traits among agents \nwith varying document amounts between systems") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(SLRSA_12_3_summary_SE, aes(x = num_doc, y = SLRSA, group = LS, color = LS)) +
  geom_line(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=SLRSA-ci, ymax=SLRSA+ci), colour="black",width=.5, position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.2) ,size=3, shape=21, fill="white") +
  labs(x = "Amount of documents", y = "Mean number of traits in agents") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Mean relative success rate of agents by varying\n learning strategies and amount of documents in the system") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(RMSLPA_12_3_summary_SE, aes(x = num_doc, y = RMSLPA, group = LS, color = LS)) +
  geom_line(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=RMSLPA-ci, ymax=RMSLPA+ci), colour="black",width=.5, position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.2) ,size=3, shape=21, fill="white") +
  labs(x = "Amount of documents", y = "Mean relative social learning payoff of agents") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Mean relative social learning pay-off of agents by varying\n learning strategies and amount of documents in the system") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(MRCPA_12_3_summary_SE, aes(x = num_doc, y = MRCPA, group = LS, color = LS)) +
  geom_line(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=MRCPA-ci, ymax=MRCPA+ci), colour="black",width=.5, position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.2) ,size=3, shape=21, fill="white") +
  labs(x = "Amount of documents", y = "Mean relative cumulative pay-off of agents") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Mean relative cumulative pay-off of agents by varying\n learning strategies and amount of documents in the system") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

boxplot(MTiS~num_doc,data=df_12_3_long_format)
boxplot(MTA~num_doc,data=df_12_3_long_format)
boxplot(SLRSA~num_doc,data=df_12_3_long_format)
boxplot(RMSLPA~num_doc,data=df_12_3_long_format)
boxplot(MRCPA~num_doc,data=df_12_3_long_format)

boxplot(MTiS~LS,data=df_12_3_long_format)
boxplot(MTA~LS,data=df_12_3_long_format)
boxplot(SLRSA~LS,data=df_12_3_long_format)
boxplot(RMSLPA~LS,data=df_12_3_long_format)
boxplot(MRCPA~LS,data=df_12_3_long_format)


# 12_3 Statistics ---------------------------------------------------------
summary(lm(MTiS~num_doc+LS,data=df_12_3_long_format))
pairwise.wilcox.test(df_12_3_long_format$MTiS,df_12_3_long_format$num_doc,p.adjust.method = "bonferroni")
pairwise.wilcox.test(df_12_3_long_format$MTiS,df_12_3_long_format$LS,p.adjust.method = "bonferroni")


# Plotting Differences Between Skill trees --------------------------------------------------

#Plot difference skill trees
MTiS_summary_SE_both_trees<-summarySE(all_df_long_format, measurevar="MTiS", groupvars=c("num_doc", "skill_tree"))
MTiS_summary_SE_both_trees_LS<-summarySE(all_df_long_format, measurevar="MTiS", groupvars=c("LS", "skill_tree"))
MTA_summary_SE_both_trees<-summarySE(all_df_long_format, measurevar="MTA", groupvars=c("num_doc","skill_tree"))
SLRSA_summary_SE_both_trees<-summarySE(all_df_long_format, measurevar="SLRSA", groupvars=c("num_doc","skill_tree"))
RMSLPA_summary_SE_both_trees<-summarySE(all_df_long_format, measurevar="RMSLPA", groupvars=c("num_doc","skill_tree"))
MRCPA_summary_SE_both_trees<-summarySE(all_df_long_format, measurevar="MRCPA", groupvars=c("num_doc","skill_tree"))

ggplot(MTiS_summary_SE_both_trees, aes(x = num_doc, y = MTiS, color = skill_tree, group = skill_tree)) +
  geom_line() +
  geom_point(size=2.5, shape=21, fill="white") +
  labs(x = "\nAmount of documents", y = "Means of relative MTiS\n") +
  scale_colour_hue(name="Skill Tree",    # Legend label, use darker colors
                   breaks=c("4_9", "12_3"),
                   labels=c("4 arms, 9 skills deep", "12 arms, 3 skills deep"),
                   l=40) +                    # Use darker colors, lightness=40
  #ggtitle("Mean amount of traits among all individuals \nwith varying document amounts between systems") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
        axis.text=element_text(size=10),
        axis.title=element_text(size=11),
        legend.title = element_text(size=11), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size)
        legend.position="none")


ggplot(MTiS_summary_SE_both_trees_LS, aes(x = LS, y = MTiS, color = skill_tree, group = skill_tree)) +
  geom_line() +
  geom_point(size=2.5, shape=21, fill="white") +
  scale_x_discrete(label = c("Random","Conformist","Age-based","Similarity","Pay-off", "Mixed")) +
  labs(x = "\nLearning Strategy", y = "Means of relative MTiS\n") +
  scale_colour_hue(name="Skill Tree",    # Legend label, use darker colors
                   breaks=c("4_9", "12_3"),
                   labels=c("4 arms, \n9 skills deep", "12 arms, \n3 skills deep"),
                   l=40) +                    # Use darker colors, lightness=40
  #ggtitle("Means of relative mean number of traits among all individuals \nin systems differing in learning strategies\n") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11),
    axis.text.x = element_text(angle = 20, hjust=0.6, vjust=0.6),
    legend.text = element_text(size=10),
    legend.title = element_text(size=11))

ggplot(MTA_summary_SE_both_trees, aes(x = num_doc, y = MTA, color = skill_tree, group = skill_tree)) +
  geom_line() +
  geom_point(size=2.5, shape=21, fill="white") +
  labs(x = "\nAmount of documents", y = "Means of relative MTA\n") +
  scale_colour_hue(name="Skill Tree",    # Legend label, use darker colors
                   breaks=c("4_9", "12_3"),
                   labels=c("4 arms, \n9 skills deep", "12 arms, \n3 skills deep"),
                   l=40) +                    # Use darker colors, lightness=40
  #ggtitle("Mean amount of traits among all individuals \nwith varying document amounts between systems") +
  theme_bw() +
  theme(#plot.title = element_text(hjust = 0.5, size = 20),
        axis.text=element_text(size=11),
        axis.title=element_text(size=12),
        legend.text = element_text(size=11),
        legend.title = element_text(size=12))

ggplot(SLRSA_summary_SE_both_trees, aes(x = num_doc, y = SLRSA, color = skill_tree, group = skill_tree)) +
  geom_line() +
  geom_point(size=2.5, shape=21, fill="white") +
  labs(x = "Amount of documents", y = "Mean number of traits in system") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Mean amount of traits among all individuals \nwith varying document amounts between systems") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(RMSLPA_summary_SE_both_trees, aes(x = num_doc, y = RMSLPA, color = skill_tree, group = skill_tree)) +
  geom_line() +
  geom_point(size=2.5, shape=21, fill="white") +
  labs(x = "Amount of documents", y = "Mean number of traits in system") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Mean amount of traits among all individuals \nwith varying document amounts between systems") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(MRCPA_summary_SE_both_trees, aes(x = num_doc, y = MRCPA, color = skill_tree, group = skill_tree)) +
  geom_line() +
  geom_point(size=2.5, shape=21, fill="white") +
  labs(x = "Amount of documents", y = "Mean number of traits in system") +
  scale_colour_hue(name="Learning Strategy",    # Legend label, use darker colors
                   breaks=c("0", "1", "2", "3", "4","mixed"),
                   labels=c("Random", "Conformist", "Age-based", "Similarity-based", "Pay-off-based", "Mixed"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Mean amount of traits among all individuals \nwith varying document amounts between systems") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Individual simulations --------------------------------------------------
### Random 9_4
sim_random_0_9_4<-read.csv(paste0(directory,"sp4 pd9\\1_df_sim_1_exampleSkillLearnedPerTimestep_0_0sp4_pd9"))
sim_random_1_9_4<-read.csv(paste0(directory,"sp4 pd9\\6_df_sim_6_exampleSkillLearnedPerTimestep_1_0sp4_pd9"))
sim_random_10_9_4<-read.csv(paste0(directory,"sp4 pd9\\11_df_sim_11_exampleSkillLearnedPerTimestep_10_0sp4_pd9"))
sim_random_50_9_4<-read.csv(paste0(directory,"sp4 pd9\\16_df_sim_16_exampleSkillLearnedPerTimestep_50_0sp4_pd9"))
sim_random_100_9_4<-read.csv(paste0(directory,"sp4 pd9\\21_df_sim_21_exampleSkillLearnedPerTimestep_100_0sp4_pd9"))
sim_random_500_9_4<-read.csv(paste0(directory,"sp4 pd9\\26_df_sim_26_exampleSkillLearnedPerTimestep_500_0sp4_pd9"))

matplot(sim_random_0_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_random_1_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_random_10_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_random_50_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_random_100_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_random_500_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")

### Conformity 9_4
sim_conformity_0_9_4<-read.csv(paste0(directory,"sp4 pd9\\2_df_sim_2_exampleSkillLearnedPerTimestep_0_1sp4_pd9"))
sim_conformity_1_9_4<-read.csv(paste0(directory,"sp4 pd9\\7_df_sim_7_exampleSkillLearnedPerTimestep_1_1sp4_pd9"))
sim_conformity_10_9_4<-read.csv(paste0(directory,"sp4 pd9\\12_df_sim_12_exampleSkillLearnedPerTimestep_10_1sp4_pd9"))
sim_conformity_50_9_4<-read.csv(paste0(directory,"sp4 pd9\\17_df_sim_17_exampleSkillLearnedPerTimestep_50_1sp4_pd9"))
sim_conformity_100_9_4<-read.csv(paste0(directory,"sp4 pd9\\22_df_sim_22_exampleSkillLearnedPerTimestep_100_1sp4_pd9"))
sim_conformity_500_9_4<-read.csv(paste0(directory,"sp4 pd9\\27_df_sim_27_exampleSkillLearnedPerTimestep_500_1sp4_pd9"))

matplot(sim_conformity_0_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_conformity_1_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_conformity_10_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_conformity_50_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_conformity_100_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_conformity_500_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")

### Pay-off 9_4
sim_pay_off_0_9_4<-read.csv(paste0(directory,"sp4 pd9\\5_df_sim_5_exampleSkillLearnedPerTimestep_0_4sp4_pd9"))
sim_pay_off_1_9_4<-read.csv(paste0(directory,"sp4 pd9\\10_df_sim_10_exampleSkillLearnedPerTimestep_1_4sp4_pd9"))
sim_pay_off_10_9_4<-read.csv(paste0(directory,"sp4 pd9\\15_df_sim_15_exampleSkillLearnedPerTimestep_10_4sp4_pd9"))
sim_pay_off_50_9_4<-read.csv(paste0(directory,"sp4 pd9\\20_df_sim_20_exampleSkillLearnedPerTimestep_50_4sp4_pd9"))
sim_pay_off_100_9_4<-read.csv(paste0(directory,"sp4 pd9\\25_df_sim_25_exampleSkillLearnedPerTimestep_100_4sp4_pd9"))
sim_pay_off_500_9_4<-read.csv(paste0(directory,"sp4 pd9\\30_df_sim_30_exampleSkillLearnedPerTimestep_500_4sp4_pd9"))

matplot(sim_pay_off_0_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_pay_off_1_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_pay_off_10_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_pay_off_50_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_pay_off_100_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_pay_off_500_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")

### Mixed 9_4
sim_mixed_0_9_4<-read.csv(paste0(directory,"Mixed sp4 pd9\\1_df_sim_1_exampleSkillLearnedPerTimestep_0_mixed_sp4_pd9"))
sim_mixed_1_9_4<-read.csv(paste0(directory,"Mixed sp4 pd9\\2_df_sim_2_exampleSkillLearnedPerTimestep_1_mixed_sp4_pd9"))
sim_mixed_10_9_4<-read.csv(paste0(directory,"Mixed sp4 pd9\\3_df_sim_3_exampleSkillLearnedPerTimestep_10_mixed_sp4_pd9"))
sim_mixed_50_9_4<-read.csv(paste0(directory,"Mixed sp4 pd9\\4_df_sim_4_exampleSkillLearnedPerTimestep_50_mixed_sp4_pd9"))
sim_mixed_100_9_4<-read.csv(paste0(directory,"Mixed sp4 pd9\\5_df_sim_5_exampleSkillLearnedPerTimestep_100_mixed_sp4_pd9"))
sim_mixed_500_9_4<-read.csv(paste0(directory,"Mixed sp4 pd9\\6_df_sim_6_exampleSkillLearnedPerTimestep_500_mixed_sp4_pd9"))

matplot(sim_mixed_0_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_mixed_1_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_mixed_10_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_mixed_50_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_mixed_100_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
matplot(sim_mixed_500_9_4, type="l",xlab="Timestep", ylab="# ind. with skill")
