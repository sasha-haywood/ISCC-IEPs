library(readr)
library(readr)
library(dplyr)
data2013 = read.csv("foranalysis2013.csv")
data2013 = data2013[, - c(8, 9, 10, 17)] # remove istep-related variables
data2013 = data2013[, - c(11, 12, 14, 15, 16, 18)] # remove other stuff
data2013 = unique(data2013) # remove duplicate entries

corp = data2013 %>%
  filter(ADM_TYPE == 2)
corp$corp = corp$legal_corp

corp2 = data2013 %>%
  filter(ADM_TYPE != 2)
corp2$corp = corp2$reporting_corp

clean2013 = rbind(corp, corp2) # make sure we have the correct corp 
clean2013 = clean2013[, - c(23, 24, 25, 33)] # remove corp info we no longer need


test1 = data2013[order(data2013[,15], data2013[,14], data2013[,28], decreasing = T),]
# It looks like having gradematch=1 usually means more demographic info is included
# and gradematch=NA means most is missing
# having locmatch=1 has more info that locmatch=0 or NA
# One those are ordered (meaning we have as much info as possible), we want the most 
# recent entry.  If the most recent entry has less demographic info, then we want the
# older entry with more info

test = test1 %>%
  distinct(TEACHER_ALTERNATE_ID, STUDENT_ALTERNATE_ID, .keep_all = T)
# One we have ordered everything so we have the most info first and, the most recent
# first, then we can filter for distinct teacher/student combination.
# This should give us one entry for each combination, including as much demographic 
# info as possible.


# If there is an easy way to do this, this would be a better way to make sure we 
# aren't accidentally throwing away any demographic info.
# I would like a function that will do this for each variable we want:
# match student ID with columns 8-11
# match teacher ID with columns 4-5
# match corp with columns 17-21

# I'm waiting to talk to Hannah to see if this is necessary or whether the first method
# is safe.

test3 = data2013 %>%
  filter(Student_Gender  != "")
test3 = test3[,c("STUDENT_ALTERNATE_ID", "Student_Gender")]
test3 = unique(test3)

clean2013 = left_join(data2013, test3, by = "STUDENT_ALTERNATE_ID", suffix = c("", ".y"))
clean2013$Student_Gender = clean2013$Student_Gender.y
clean2013 = clean2013[,-40]





