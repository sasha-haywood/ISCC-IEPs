library(readr)
library(ggplot2)
library(Hmisc)
library(dplyr)
library(lme4)
library(scales)
data = read.csv("allData.csv", na.strings=c("","NA", "-", "-1"))
data = data[,c(2:11, 14, 16:18, 20, 24:26)]
# only keep variables we are interested in
    
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

exception = c("Multiple Disabilities", "Orthopedic Impairment", "Blind or Low Vision",
              "Deaf or Hard of Hearing", "Emotional Disability (Full Time)",
              "Emotional Disability (Other)", "Specific Learning Disability",
              "Developmental Delay (Ages 3-5A only)", "Mild Cognitive Disability",
              "Moderate Cognitive Disability", "Severe Cognitive Disability",
              "Deaf-Blind", "Autism Spectrum Disorder","Traumatic Brain Injury",
              'Other Health Impairment')
code = c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 14, 15, 16, 17)
exceptionCodes = data.frame(code, exception)


test = unique(data[,c(15:18)])
test = na.omit(test)
rcorr(as.matrix(test), type = "pearson")
# Beware: correlations between Grad_rate, PercentFreeReduced, expend_per_student

unique_student = data[,c(2, 7:18)] %>%
  unique()


####### Visualizations

prop.table(table(unique_student[,c(2, 9)]), 1)

ggplot(unique_student, aes(x=Student_Gender, fill = forcats::fct_rev(PLACEMENT_TYPE))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent Inclusion") +
  xlab("Gender of Student") +
  ggtitle("Inclusion Rate by Gender of Student") +
  theme(legend.position = "none") +
  scale_fill_grey(start = 1, end = 0) +
  coord_flip()

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
  scale_fill_grey(start = 1, end = 0) +
  coord_flip()


ggplot(unique_student, aes(x=PRIMARY_EXCEPTION, fill = forcats::fct_rev(PLACEMENT_TYPE))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent Inclusion") +
  ggtitle("Inclusion Rate by Primary Exception") +
  scale_x_discrete("Primary Exception", breaks = code, labels=exception) + 
  theme(legend.position = "none") +
  scale_fill_grey(start = 1, end = 0) +
  coord_flip()


ggplot(unique_student, aes(y=PercentFreeReduced, x=PLACEMENT_TYPE)) +
  geom_boxplot() +
  ylab("Proportion of Corp. Receiving Free/Reduced Lunch") +
  ggtitle("Inclusion By Free/Reduced Rate of Corporation") +
  scale_x_discrete("Inclusion Level", labels = c("High", "Low"))

ggplot(unique_student, aes(y=TOTAL_ENROLLMENT, x=PLACEMENT_TYPE)) +
  geom_boxplot() +
  ggtitle("Inclusion By Enrollment of Corporation") +
  scale_x_discrete("Inclusion Level", labels = c("High", "Low")) +
  scale_y_continuous("Total Enrollment of Corporation", label = comma)

ggplot(unique_student, aes(y=expend_per_student, x=PLACEMENT_TYPE)) +
  geom_boxplot() +
  ggtitle("Inclusion By Corporation Per-Student Expenditure") +
  scale_x_discrete("Inclusion Level", labels = c("High", "Low")) +
  scale_y_continuous("Per-Student Expenditure",label = dollar)

elmGrades = c("PK", "KG", "01", "02", "03", "04", "05")

elementary = data %>%
  filter(GRADE_CODE_SE %in% elmGrades)
# older students will be linked to more teachers

test = unique(elementary[c("STUDENT_ALTERNATE_ID", "SCHOOL_YEAR_ID")])
# these students are, on average, linked to about 1.5 teachers per year

ggplot(elementary, aes(x=Teacher_Gender, fill = forcats::fct_rev(PLACEMENT_TYPE))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent Inclusion") +
  ggtitle("Inclusion Rate by Teacher Gender") +
  scale_x_discrete("Teacher Gender") + 
  theme(legend.position = "none") +
  scale_fill_grey(start = 1, end = 0) +
  coord_flip()

ggplot(elementary, aes(x=Teacher_Ethnicity, fill = forcats::fct_rev(PLACEMENT_TYPE))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent Inclusion") +
  ggtitle("Inclusion Rate by Teacher Ethnicity") +
  scale_x_discrete("Teacher Ethnicity") + 
  theme(legend.position = "none") +
  scale_fill_grey(start = 1, end = 0) +
  coord_flip()

principalOnly = data %>%
  filter(!is.na(Principal_Gender))
principalOnly = principalOnly[,c(2, 5, 6, 11, 14)]
principalOnly = unique(principalOnly)
# one row for each student/year

ggplot(principalOnly, aes(x=Principal_Gender, fill = forcats::fct_rev(PLACEMENT_TYPE))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent Inclusion") +
  ggtitle("Inclusion Rate by Principal Gender") +
  scale_x_discrete("Principal Gender") + 
  theme(legend.position = "none") +
  scale_fill_grey(start = 1, end = 0) +
  coord_flip()

ggplot(principalOnly, aes(x=Principal_Ethnicity, fill = forcats::fct_rev(PLACEMENT_TYPE))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent Inclusion") +
  ggtitle("Inclusion Rate by Principal Ethnicity") +
  scale_x_discrete("Principal Ethnicity") + 
  theme(legend.position = "none") +
  scale_fill_grey(start = 1, end = 0) +
  coord_flip()


########## Let's Make a Model

m1 = glmer(PLACEMENT_TYPE ~ Student_Gender + Student_Ethnicity + 
             ENGLISH_LEARNER_STATUS_CODE + SOCIO_ECON_STATUS_CODE +
             GRADE_CODE_SE + PRIMARY_EXCEPTION + GRAD_RATE + PercentFreeReduced +
             TOTAL_ENROLLMENT + expend_per_student + (1|SCHOOL_YEAR_ID), 
           data = na.omit(unique_student), family = binomial,
           control=glmerControl(optimizer="bobyqa"))
# control suggested at https://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/
# Supposed to make to more efficent but I can't get this to run either way.
# Just gets hung up. But I am using R on IU AnyWare.

m1 = glm(PLACEMENT_TYPE ~ Student_Gender + Student_Ethnicity + 
             ENGLISH_LEARNER_STATUS_CODE + SOCIO_ECON_STATUS_CODE +
             GRADE_CODE_SE + PRIMARY_EXCEPTION + GRAD_RATE + PercentFreeReduced +
             TOTAL_ENROLLMENT + expend_per_student, 
           data = na.omit(unique_student), family = "binomial")

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



########## A model for the principal demographics

p1 = glmer(PLACEMENT_TYPE ~ Principal_Gender + Principal_Ethnicity + 
             (1|SCHOOL_YEAR_ID), data = principalOnly, family = binomial)
p2 = glmer(PLACEMENT_TYPE ~ Principal_Ethnicity + 
             (1|SCHOOL_YEAR_ID), data = principalOnly, family = binomial)
anova(p1, p2) 
# no signficant difference.  Can drop principal gender.

