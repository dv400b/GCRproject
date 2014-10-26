library(data.table)
library(plyr)

#define top-level directory and files
sdir        <- "UCI HAR Dataset/"
sfileact    <- paste0(sdir, "activity_labels.txt")
sfilefeat   <- paste0(sdir, "features.txt")

#read global feature and activity codes
dffileact   <- as.data.frame(fread(sfileact))
dffilefeat  <- as.data.frame(fread(sfilefeat))
vactivity   <- tolower(as.vector(dffileact[,2]))
vfeature    <- sub("()", "", as.vector(dffilefeat[,2]), fixed=TRUE)  #remove "()"
names(vactivity) <- as.character(as.vector(dffileact[,1]))
names(vfeature)  <- as.character(as.vector(dffilefeat[,1]))

#define subdirectories to read
vdir2 <- c("train", "test")
ldir2 <- as.list(vdir2)
ldf   <- lapply(ldir2 , function(sdir2){
   ##given a directory name sdir2:
   #define files to import
   sfiles <- paste0(sdir, sdir2, "/subject_", sdir2, ".txt")
   sfiley <- paste0(sdir, sdir2, "/y_", sdir2, ".txt")
   sfilex <- paste0(sdir, sdir2, "/X_", sdir2, ".txt")
   #import files
   vfiles <- readLines(sfiles)
   vfiley <- readLines(sfiley)
   lfilex <- as.list(readLines(sfilex))
   #process X file
   mfilex <- sapply(lfilex, function(string){
      lsplit  <- strsplit(string, "\\d +", fixed=FALSE) # delimiter = digit + space(s)
      vx <- as.numeric(lsplit[[1]])
      return(vx)
   })
   dimnames(mfilex)[[1]] <- vfeature
   #build data.frame from subject, Y, and X data
   df     <- data.frame(subject=as.integer(vfiles), 
                        activity=as.integer(vfiley),  #keep activity numeric for now
                        t(mfilex))
   return(df)
})
names(ldf) <- vdir2

#combine data.frames from all subdirectores (train, test)
df.all <- NULL
for(sdir2 in vdir2){
   df.all <- rbind(df.all, ldf[[sdir2]])
}

#subset complete data by subject and activity
ldfsub    <- dlply(df.all, .variables=.(subject, activity))
mdfsubavg <- t(sapply(ldfsub, colMeans))
df.summary<- as.data.frame(mdfsubavg)

#replace activity numbers with activity names
df.all$activity     <- vactivity[as.character(df.all$activity)]
df.summary$activity <- vactivity[as.character(df.summary$activity)]

write.table(df.summary, "run_analysis_summary.txt", row.names=FALSE)
