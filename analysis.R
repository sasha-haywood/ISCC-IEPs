library(readr)
library(ggplot2)
library(Hmisc)
library(dplyr)
data = read.csv("allData.csv", na.strings=c("","NA"))
data = data[,c(2:11, 14, 16:18, 20, 24:26)]
# only keep variables we are interested in

data[data$Teacher_Gender == "-",3] = NA 
# change Teacher_Gender = "-" to NA

data[data$SOCIO_ECON_STATUS_CODE == "-1",10] = NA
# change SOCIO_ECON_STATUS_CODE = "-1" to NA
    
data$SCHOOL_YEAR_ID = as.factor(data$SCHOOL_YEAR_ID)
data$PRIMARY_EXCEPTION = as.factor(data$PRIMARY_EXCEPTION)
# change variables to factors

data$TOTAL_ENROLLMENT = as.double(data$TOTAL_ENROLLMENT)
# cchange variable from integer to double

levels(data$PLACEMENT_TYPE) = c("1","0")
# change high inclusion to 1 and low inclusion to 0

data$GRADE_CODE_SE = relevel(data$GRADE_CODE_SE, "KG")
data$GRADE_CODE_SE = relevel(data$GRADE_CODE_SE, "PK")
# put grades in order

data = droplevels(data)
# remove factor levels that are blank

test = unique(data[,c(15:18)])
test = na.omit(test)
rcorr(as.matrix(test), type = "pearson")
# Beware: correlations between Grad_rate, PercentFreeReduced, expend_per_student

unique_student = data[,c(2, 7:18)] %>%
  unique()

m1 = glm(PLACEMENT_TYPE ~ Student_Gender + Student_Ethnicity + 
           ENGLISH_LEARNER_STATUS_CODE + SOCIO_ECON_STATUS_CODE +
           GRADE_CODE_SE + PRIMARY_EXCEPTION + GRAD_RATE + PercentFreeReduced +
           TOTAL_ENROLLMENT + expend_per_student, data = na.omit(unique_student), 
         family = "binomial")
summary(m1)
# Initially, it looks like SOCIO_ECON_STATUS_CODE, Student_Gender, and GRAD_RATE 
# are insignificant

step(m1)
# AIC improved when we remove GRAD_RATE but not the others

m2 = glm(PLACEMENT_TYPE ~ Student_Gender + Student_Ethnicity + 
            ENGLISH_LEARNER_STATUS_CODE + SOCIO_ECON_STATUS_CODE +
            GRADE_CODE_SE + PRIMARY_EXCEPTION + PercentFreeReduced +
            TOTAL_ENROLLMENT + expend_per_student, data = na.omit(unique_student), 
          family = "binomial")
summary(m2)

prop.table(table(unique_student[,c(2, 9)]), 1)

ggplot(unique_student, aes(x=Student_Gender, fill = forcats::fct_rev(PLACEMENT_TYPE))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent Inclusion") +
  xlab("Gender of Student") +
  ggtitle("Inclusion Rate by Gender of Student") +
  theme(legend.position = "none") +
  scale_fill_grey(start = 1, end = 0)

ggplot(unique_student, aes(x=Student_Ethnicity, fill = forcats::fct_rev(PLACEMENT_TYPE))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent Inclusion") +
  xlab("Ethnicity of Student") +
  ggtitle("Inclusion Rate by Student Ethnicity") +
  theme(legend.position = "none") +
  scale_fill_grey(start = 1, end = 0) +
  coord_flip()

ggplot(unique_student, aes(x=ENGLISH_LEARNER_STATUS_CODE, fill = forcats::fct_rev(PLACEMENT_TYPE))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent Inclusion") +
  xlab("ESL status") +
  ggtitle("Inclusion Rate by ESL status") +
  theme(legend.position = "none") +
  scale_fill_grey(start = 1, end = 0) +
  coord_flip()

ggplot(unique_student, aes(x=GRADE_CODE_SE, fill = forcats::fct_rev(PLACEMENT_TYPE))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent Inclusion") +
  xlab("Grade Level") +
  ggtitle("Inclusion Rate by Grade Level") +
  theme(legend.position = "none") +
  scale_fill_grey(start = 1, end = 0)


ggplot(unique_student, aes(x=PRIMARY_EXCEPTION, fill = forcats::fct_rev(PLACEMENT_TYPE))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent Inclusion") +
  xlab("Primary Exception") +
  ggtitle("Inclusion Rate by Primary Exception") +
  theme(legend.position = "none") +
  scale_fill_grey(start = 1, end = 0) +
  coord_flip()


ggplot(unique_student, aes(y=PercentFreeReduced, x=PLACEMENT_TYPE)) +
  geom_boxplot()

ggplot(unique_student, aes(y=TOTAL_ENROLLMENT, x=PLACEMENT_TYPE)) +
  geom_boxplot()

ggplot(unique_student, aes(y=expend_per_student, x=PLACEMENT_TYPE)) +
  geom_boxplot()

