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
data$SOCIO_ECON_STATUS_CODE = relevel(data$SOCIO_ECON_STATUS_CODE, "Reduced")
data$SOCIO_ECON_STATUS_CODE = relevel(data$SOCIO_ECON_STATUS_CODE, "Full")

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

unique_student = data[,c(2, 5:18)] %>%
  unique()
# mostly unique.  7% are duplicates

####### Visualizations

prop.table(table(unique_student[,c(2, 9)]), 1)
prop.table(table(unique_student[,c(3, 9)]), 1)
prop.table(table(unique_student[,c(4, 9)]), 1)
prop.table(table(unique_student[,c(5, 9)]), 1)
prop.table(table(unique_student[,c(6, 9)]), 1)
prop.table(table(unique_student[,c(7, 9)]), 1)
prop.table(table(unique_student[,c(8, 9)]), 1)


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

ggplot(unique_student, aes(x=SOCIO_ECON_STATUS_CODE, fill = forcats::fct_rev(PLACEMENT_TYPE))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent Inclusion") +
  xlab("Free/Reduced Lunch status") +
  ggtitle("Inclusion Rate by Free/Reduced Lunch status") +
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
# But 40% have no teacher

prop.table(table(elementary[,c(3, 14)]), 1)
prop.table(table(elementary[,c(4, 14)]), 1)

ggplot(data, aes(x=Teacher_Gender, fill = forcats::fct_rev(PLACEMENT_TYPE))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent Inclusion") +
  ggtitle("Inclusion Rate by Teacher Gender") +
  scale_x_discrete("Teacher Gender") + 
  theme(legend.position = "none") +
  scale_fill_grey(start = 1, end = 0) +
  coord_flip()

ggplot(data, aes(x=Teacher_Ethnicity, fill = forcats::fct_rev(PLACEMENT_TYPE))) +
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

prop.table(table(principalOnly[,c(2, 5)]), 1)
prop.table(table(principalOnly[,c(3, 5)]), 1)

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
             TOTAL_ENROLLMENT + expend_per_student + Principal_Gender + Principal_Ethnicity + 
             (1|SCHOOL_YEAR_ID), 
           data = na.omit(unique_student), family = binomial,
           control=glmerControl(optimizer="bobyqa"))
# control suggested at https://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/
# Supposed to make to more efficent but I can't get this to run either way.
# Just gets hung up. But I am using R on IU AnyWare.

m1 = glm(PLACEMENT_TYPE ~ Student_Gender + Student_Ethnicity + 
             ENGLISH_LEARNER_STATUS_CODE + SOCIO_ECON_STATUS_CODE +
             GRADE_CODE_SE + PRIMARY_EXCEPTION + GRAD_RATE + PercentFreeReduced +
             TOTAL_ENROLLMENT + expend_per_student + Principal_Gender + Principal_Ethnicity,
           data = na.omit(unique_student), family = "binomial")

min = glm(PLACEMENT_TYPE ~ 1, data = na.omit(unique_student), 
          family = "binomial")
best = step(min, scope = formula(m1), direction = "forward")

summary(best)

########## A model for the teacher demographics

t1 = glmer(PLACEMENT_TYPE ~ Teacher_Gender + Teacher_Ethnicity + 
             (1|SCHOOL_YEAR_ID), data = data, family = binomial)
t2 = glmer(PLACEMENT_TYPE ~ Teacher_Ethnicity + 
             (1|SCHOOL_YEAR_ID), data = data, family = binomial)
anova(t1, t2) 
# no signficant difference.  Can drop teacher gender.
