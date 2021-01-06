# importing all the libraries used in this project 
library('readxl')
library(psych)
library(tidyverse)
library(sqldf) # <- make sure you have this install, inatall.packages('sqldf')
library(ggplot2)
library(gridExtra)
library(cowplot)
library(e1071) 
library(car)
library(semTools)
library(pastecs)
library(sjstats) 
library(userfriendlyscience)
library(generalhoslem)
library(regclass)
library(lm.beta)
library(stargazer)
library(broom)
library(Epi)
library(arm)
library(DescTools)
library(foreign)
library(olsrr)
library('dvmisc')

# make sure the data is in local directory to this script
data <- read_excel('data_academic_performance.xlsx')
# make copy of original dataset
df<-data
names(df)
# to summarise the data properally, convert characters to factors
# df[] <- lapply( df, factor)
col_names <- c('GENDER','EDU_FATHER','EDU_MOTHER','OCC_FATHER','OCC_MOTHER','STRATUM','SISBEN','PEOPLE_HOUSE','INTERNET','TV','COMPUTER','WASHING_MCH','MIC_OVEN','CAR','DVD','FRESH','PHONE','MOBILE','REVENUE','JOB','SCHOOL_NAME','SCHOOL_NAT','SCHOOL_TYPE','Cod_SPro','UNIVERSITY','ACADEMIC_PROGRAM')
# convert  character to factors using lapply
df[col_names] <- lapply(df[col_names] , factor)
df
# to get statistical measures of the data
summary(df)

# selecting only the variables of interest only using sql syntax, make sure to install sqldf
filter_data <- sqldf('select GENDER, STRATUM ,EDU_FATHER,EDU_MOTHER,OCC_FATHER,OCC_MOTHER,SISBEN,PEOPLE_HOUSE,INTERNET,TV,COMPUTER,WASHING_MCH,MIC_OVEN,CAR,DVD,PHONE,MOBILE,REVENUE,JOB,
                        SCHOOL_NAT,SCHOOL_TYPE,MAT_S11,CR_S11,BIO_S11,ENG_S11,G_SC,CC_S11 FROM df')



filter_data
summary(filter_data)
# only categorical data
cat_data <- sqldf('select GENDER, EDU_FATHER,EDU_MOTHER,OCC_FATHER,OCC_MOTHER,SISBEN,PEOPLE_HOUSE,INTERNET,TV,COMPUTER,WASHING_MCH,MIC_OVEN,CAR,DVD,PHONE,MOBILE,REVENUE,JOB,
                        SCHOOL_NAT,SCHOOL_TYPE from df')

cat_data


# Categorical data barcharts
bar_cat_plot_list <- list()
col_names <- colnames(cat_data)
for(i in col_names){
  #     print(i)
  gg <- ggplot(cat_data, aes_string(x = i)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=5))
  bar_cat_plot_list[[i]] <- gg
} # end of loop

plot_grid(plotlist = bar_cat_plot_list)


# find null data, will remove later using dummy variables to isolate these
sqldf('select count(JOB) from cat_data where JOB == 0')
sqldf('select count(people_house) from cat_data where people_house == 0')

# continous data only
cont_data <- sqldf('select MAT_S11,CR_S11,BIO_S11,ENG_S11,CC_S11,G_SC from df')
# get statistical measure
describe(cont_data)

# find missing value
sqldf('select count(MAT_S11) from cont_data where MAT_S11 is null')
sqldf('select count(CR_S11) from cont_data where CR_S11 is null')
sqldf('select count(BIO_S11) from cont_data where BIO_S11 is null')
sqldf('select count(ENG_S11) from cont_data where ENG_S11 is null')
sqldf('select count(G_SC) from cont_data where G_SC is null')


# create normal plot
col_names <- colnames(cont_data)
col_names
# hold all the plots created in the loop
plot_list <- list()
col_names <- colnames(cont_data)
for(i in col_names){
  print(i)
  print(mean(cont_data[,i]))
  gg <- ggplot(cont_data , aes_string(i))  
  gg <- gg + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
  gg<-gg+scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
  gg<-gg+stat_function(fun=dnorm, color="red",args=list(mean=mean(cont_data[,i], na.rm=TRUE), sd=sd(cont_data[,i], na.rm=TRUE)))
  plot_list[[i]] <- gg
} # end of loop
plot_grid(plotlist = plot_list)


#Create boxplots
# hold all theplots created in the loop
box_plot_list <- list()
for(i in col_names){
  print(i)
  gg <- ggplot(cont_data, aes_string(y=i)) + geom_boxplot() + theme(text = element_text(size=20))
  box_plot_list[[i]] <- gg
} # end of loop

plot_grid(plotlist = box_plot_list)
# qq plot for each numerical data above
qqnorm(cont_data$MAT_S11 ,main='MAT_S11')+qqline(cont_data$MAT_S11, col=2) #show a line on theplot
qqnorm(cont_data$CR_S11 ,main='CR_S11')+qqline(cont_data$CR_S11, col=2) #show a line on theplot
qqnorm(cont_data$BIO_S11 ,main='BIO_S11')+qqline(cont_data$BIO_S11, col=2) #show a line on theplot
qqnorm(cont_data$ENG_S11 ,main='ENG_S11')+qqline(cont_data$ENG_S11, col=2) #show a line on theplot
qqnorm(cont_data$CC_S11 ,main='CC_S11')+qqline(cont_data$CC_S11, col=2) #show a line on theplot
qqnorm(cont_data$G_SC ,main='G_SC')+qqline(cont_data$G_SC, col=2) #show a line on theplot


# removig data that is NA,using complete.cases in case i missed finding data with Null values
head(filter_data)
filter_data<-filter_data[complete.cases(filter_data),]
filter_data


######################################################################################################### handling missing values #####################################################3
# make copy
#clean_data <- filter_data
#clean_data <- clean_data[!(clean_data$JOB == "0"),]
#clean_data <- clean_data[!(clean_data$PEOPLE_HOUSE == "0"),]
#clean_data <- clean_data[!(clean_data$EDU_FATHER == "0"),]
clean_data


# removing outliers
# removing outliers from maths, first find ranges, i use
lowerbound <- 56-1.5 * IQR(cont_data$MAT_S11)
upperbound = 72+1.5 * IQR(cont_data$MAT_S11)

# found a total of 137 outliers, these need to be removed
fn$sqldf("select count(MAT_S11) from clean_data where MAT_S11 <$lowerbound or MAT_S11 > $upperbound")
clean_data_v2<-clean_data[!(clean_data$MAT_S11 > upperbound | clean_data$MAT_S11 < lowerbound),]

# removing outliers from critical, first find ranges
lowerbound <- 54-1.5 * IQR(cont_data$CR_S11)
upperbound = 67+1.5 * IQR(cont_data$CR_S11)
fn$sqldf("select count(CR_S11) from clean_data_v2 where CR_S11 <$lowerbound or CR_S11 > $upperbound")
# found 146 in remaining set outliers
clean_data_v2<-clean_data_v2[!(clean_data_v2$CR_S11 > upperbound | clean_data_v2$CR_S11 < lowerbound),]

# removing outliers from biology, first find ranges
lowerbound <- 56-1.5 * IQR(cont_data$BIO_S11)
upperbound = 71+1.5 * IQR(cont_data$BIO_S11)
lowerbound
upperbound
fn$sqldf("select count(BIO_S11) from clean_data_v2 where BIO_S11 <$lowerbound or BIO_S11 > $upperbound")
# found 121 in remaining set outliers
clean_data_v2<-clean_data_v2[!(clean_data_v2$BIO_S11 > upperbound | clean_data_v2$BIO_S11 < lowerbound),]


# removing outliers from english, first find ranges
lowerbound <- 50-1.5 * IQR(cont_data$ENG_S11)
upperbound = 72+1.5 * IQR(cont_data$ENG_S11)
lowerbound
upperbound
fn$sqldf("select count(ENG_S11) from t where ENG_S11 <$lowerbound or ENG_S11 > $upperbound")
# found 0 in remaining set outliers
clean_data_v2<-clean_data_v2[!(clean_data_v2$ENG_S11 > upperbound | clean_data_v2$ENG_S11 < lowerbound),]


# removing outliers from CC, first find ranges
lowerbound <- 54-1.5 * IQR(cont_data$CC_S11)
upperbound = 67+1.5 * IQR(cont_data$CC_S11)
lowerbound
upperbound
fn$sqldf("select count(CC_S11) from t where CC_S11 <$lowerbound or CC_S11 > $upperbound")
# found 151 in remaining set outliers
clean_data_v2<-clean_data_v2[!(clean_data_v2$CC_S11 > upperbound | clean_data_v2$CC_S11 < lowerbound),]


# removing outliers from G_SC, first find ranges
lowerbound <- 147-1.5 * IQR(cont_data$CC_S11)
upperbound = 179+1.5 * IQR(cont_data$CC_S11)
lowerbound
upperbound
fn$sqldf("select count(G_SC) from t where G_SC <$lowerbound or G_SC > $upperbound")
# found 1258 in remaining set outliers
clean_data_v2<-clean_data_v2[!(clean_data_v2$G_SC > upperbound | clean_data_v2$G_SC < lowerbound),]


##################################################################################################### Normality testing ################################################33
# use this to help you pick the numbers
summary(cont_data)
# Getting Kurtosis and skew values of Maths, also standardised score:
math_skew <- semTools::skew(clean_data$MAT_S11)
math_kurt <- semTools::kurtosis(clean_data$MAT_S11)

# standardise the values
math_skew[1]/math_skew[2]
math_kurt[1]/math_kurt[2]

math_score_range<- abs(scale(clean_data$MAT_S11))

FSA::perc(as.numeric(math_score_range), 1.96, "gt")
FSA::perc(as.numeric(math_score_range), 3.29, "gt") #0%

# trimming maths to see if it reduces skew 
y <- trim(clean_data$MAT_S11, p = 0.05)

# Getting Kurtosis and skew values of Maths, also standardised score:
math_skew <- semTools::skew(y)
math_kurt <- semTools::kurtosis(y)

# standardise the values
math_skew[1]/math_skew[2]
math_kurt[1]/math_kurt[2]

math_score_range<- abs(scale(y))

FSA::perc(as.numeric(math_score_range), 1.96, "gt")
FSA::perc(as.numeric(math_score_range), 3.29, "gt") #0%

# reduced by 8

# performing same but this time with the removed outliers
y <- trim(clean_data_v2$MAT_S11, p = 0.05)
# Getting Kurtosis and skew values of Maths, also standardised score:
math_skew <- semTools::skew(y)
math_kurt <- semTools::kurtosis(y)

# standardise the values
math_skew[1]/math_skew[2]
math_kurt[1]/math_kurt[2]

math_score_range<- abs(scale(y))

FSA::perc(as.numeric(math_score_range), 1.96, "gt")
FSA::perc(as.numeric(math_score_range), 3.29, "gt") #0%

# Getting Kurtosis and skew values of Maths, also standardised score:
math_skew <- semTools::skew(clean_data$MAT_S11)
math_kurt <- semTools::kurtosis(clean_data$MAT_S11)

# standardise the values
math_skew[1]/math_skew[2]
math_kurt[1]/math_kurt[2]

math_score_range<- abs(scale(clean_data$MAT_S11))

FSA::perc(as.numeric(math_score_range), 1.96, "gt")
FSA::perc(as.numeric(math_score_range), 3.29, "gt") #0%
# reduced by another 2



# Getting Kurtosis and skew values of Critical reading, also standardised score:
reading_skew <- semTools::skew(clean_data$CR_S11)
reading_kurt <- semTools::kurtosis(clean_data$CR_S11)

# standardise the values
reading_skew[1]/reading_skew[2]
reading_kurt[1]/reading_kurt[2]

reading_score_range<- abs(scale(clean_data$CR_S11))

FSA::perc(as.numeric(reading_score_range), 1.96, "gt")
FSA::perc(as.numeric(reading_score_range), 3.29, "gt") #0%



# Getting Kurtosis and skew values of Critical reading, also standardised score:
# This time removing outliers
reading_skew <- semTools::skew(clean_data_v2$CR_S11)
reading_kurt <- semTools::kurtosis(clean_data_v2$CR_S11)

# standardise the values
reading_skew[1]/reading_skew[2]
reading_kurt[1]/reading_kurt[2]

reading_score_range<- abs(scale(clean_data_v2$CR_S11))

FSA::perc(as.numeric(reading_score_range), 1.96, "gt")
FSA::perc(as.numeric(reading_score_range), 3.29, "gt") #0%
# reduces skew by 7



# Getting Kurtosis and skew values of Biology, also standardised score:
biology_skew <- semTools::skew(clean_data$BIO_S11)
biology_kurt <- semTools::kurtosis(clean_data$BIO_S11)

# standardise the values
biology_skew[1]/biology_skew[2]
biology_kurt[1]/biology_kurt[2]

biology_score_range<- abs(scale(clean_data$BIO_S11))

FSA::perc(as.numeric(biology_score_range), 1.96, "gt")
FSA::perc(as.numeric(biology_score_range), 3.29, "gt") #0%

# same as above but this time trimming the values
y <- trim(clean_data$BIO_S11, p = 0.05)
# Getting Kurtosis and skew values of Biology, also standardised score:
biology_skew <- semTools::skew(y)
biology_kurt <- semTools::kurtosis(y)

# standardise the values
biology_skew[1]/biology_skew[2]
biology_kurt[1]/biology_kurt[2]

biology_score_range<- abs(scale(y))

FSA::perc(as.numeric(biology_score_range), 1.96, "gt")
FSA::perc(as.numeric(biology_score_range), 3.29, "gt") #0%
# reduced skew by 10



# Getting Kurtosis and skew values of English, also standardised score:
english_skew <- semTools::skew(clean_data$ENG_S11)
english_kurt <- semTools::kurtosis(clean_data$ENG_S11)

# standardise the values
english_skew[1]/english_skew[2]
english_kurt[1]/english_kurt[2]

english_score_range<- abs(scale(clean_data$ENG_S11))

FSA::perc(as.numeric(english_score_range), 1.96, "gt")
FSA::perc(as.numeric(english_score_range), 3.29, "gt") #0%

# performing trim on above
y <- trim(clean_data$ENG_S11, p = 0.05)

# Getting Kurtosis and skew values of English, also standardised score:
english_skew <- semTools::skew(y)
english_kurt <- semTools::kurtosis(y)

# standardise the values
english_skew[1]/english_skew[2]
english_kurt[1]/english_kurt[2]

english_score_range<- abs(scale(y))

FSA::perc(as.numeric(english_score_range), 1.96, "gt")
FSA::perc(as.numeric(english_score_range), 3.29, "gt") #0%

#reduced by 5


# Getting Kurtosis and skew values of Global score, also standardised score:
global_skew <- semTools::skew(clean_data$G_SC)
global_kurt <- semTools::kurtosis(clean_data$G_SC)

# standardise the values
global_skew[1]/global_skew[2]
global_kurt[1]/global_kurt[2]

global_score_range<- abs(scale(clean_data$G_SC))

FSA::perc(as.numeric(global_score_range), 1.96, "gt")
FSA::perc(as.numeric(global_score_range), 3.29, "gt") #0%

# performing trim
y <- trim(clean_data$G_SC, p = 0.05)
# Getting Kurtosis and skew values of Global score, also standardised score:
global_skew <- semTools::skew(y)
global_kurt <- semTools::kurtosis(y)

# standardise the values
global_skew[1]/global_skew[2]
global_kurt[1]/global_kurt[2]

global_score_range<- abs(scale(y))

FSA::perc(as.numeric(global_score_range), 1.96, "gt")
FSA::perc(as.numeric(global_score_range), 3.29, "gt") #0%

# reduced by 2

################################################################################Correlation testing#############################################################333

#Scatterplot relationship, G_SC and MAT_S11
scatter <- ggplot(clean_data, aes(clean_data$MAT_S11, clean_data$G_SC))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Maths scores(MAT_S11)", y = "Global score(G_SC)") 
# Pearson Maths
stats::cor.test(clean_data$G_SC, clean_data$MAT_S11, method='pearson')



#Scatterplot relationship, G_SC and CR_S11
scatter <- ggplot(clean_data, aes(clean_data$CR_S11, clean_data$G_SC))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Creative Reading scores(CR_S11)", y = "Global score(G_SC)") 
# Pearson test Creative reading
stats::cor.test(clean_data$G_SC, clean_data$CR_S11, method='pearson')
# statistically significant result




#Scatterplot relationship, G_SC and BIO_S11
scatter <- ggplot(clean_data, aes(clean_data$BIO_S11, clean_data$G_SC))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Biology scores(BIO_S11)", y = "Global score(G_SC)") 


# Pearson test Biology
stats::cor.test(clean_data$G_SC, clean_data$BIO_S11, method='pearson')


#Scatterplot relationship, G_SC and ENG_S11
scatter <- ggplot(clean_data, aes(clean_data$ENG_S11, clean_data$G_SC))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "English(ENG_S11)", y = "Global score(G_SC)") 
# Pearsons Test
stats::cor.test(clean_data$G_SC, clean_data$ENG_S11, method='pearson')



############################################################################ T-test ############################################################################
# global grade and gender
# Describe the variables
psych::describeBy(clean_data$G_SC, clean_data$GENDER, mat=TRUE)



# Using levene's test to test variance, pvalue needs to be greater than 0.05
car::leveneTest(G_SC ~ GENDER, data=clean_data)

# Levene's test showed unequal variance, therefore need to perform welsh modification to the t-test (will be rejected)
# therefore set var.equal to false
# Perfomring the T-test
stats::t.test(G_SC~GENDER,var.equal=FALSE,data=clean_data)


# Performing Cohen's d
res <- stats::t.test(G_SC~GENDER,var.equal=FALSE,data=clean_data)
effcd=round((2*res$statistic)/sqrt(res$parameter),2)
effectsize::t_to_d(t = res$statistic, res$parameter)





# global grade and Internet 
# Describe the variables
psych::describeBy(clean_data$G_SC, clean_data$INTERNET, mat=TRUE)

# Using levene's test to test variance, pvalue needs to be greater than 0.05
car::leveneTest(G_SC ~ INTERNET, data=clean_data)



# Levene's test showed unequal variance, therefore need to perform welsh modification to the t-test (wull be rejected)
# therefore set var.equal to false
# Perfomring the T-test
stats::t.test(G_SC~INTERNET,var.equal=FALSE,data=clean_data)



# Performing Cohen's d
res <- stats::t.test(G_SC~INTERNET,var.equal=FALSE,data=clean_data)
effcd=round((2*res$statistic)/sqrt(res$parameter),2)
effectsize::t_to_d(t = res$statistic, res$parameter)


# global grade and tv 
# Describe the variables
psych::describeBy(clean_data$G_SC, clean_data$TV, mat=TRUE)



# Using levene's test to test variance, pvalue needs to be greater than 0.05
car::leveneTest(G_SC ~ TV, data=clean_data)

# Levene's 
# therefore set var.equal to true
# Perfomring the T-test
stats::t.test(G_SC~TV,var.equal=TRUE,data=clean_data)



# Performing Cohen's d
res <- stats::t.test(G_SC~TV,var.equal=TRUE,data=clean_data)
effcd=round((2*res$statistic)/sqrt(res$parameter),2)
effectsize::t_to_d(t = res$statistic, res$parameter)




# global grade and computer (will be rejected, failed levene test)
# Describe the variables
psych::describeBy(clean_data$G_SC, clean_data$COMPUTER, mat=TRUE)



# Using levene's test to test variance, pvalue needs to be greater than 0.05
car::leveneTest(G_SC ~ COMPUTER, data=clean_data)



stats::t.test(G_SC~COMPUTER,var.equal=FALSE,data=clean_data)




# Performing Cohen's d
res <- stats::t.test(G_SC~COMPUTER,var.equal=FALSE,data=clean_data)
effcd=round((2*res$statistic)/sqrt(res$parameter),2)
effectsize::t_to_d(t = res$statistic, res$parameter)


# global grade and washing machine 
# Describe the variables
psych::describeBy(clean_data$G_SC, clean_data$WASHING_MCH, mat=TRUE)



# Using levene's test to test variance, pvalue needs to be greater than 0.05
car::leveneTest(G_SC ~ WASHING_MCH, data=clean_data)

stats::t.test(G_SC~WASHING_MCH,var.equal=TRUE,data=clean_data)



# Performing Cohen's d
res <- stats::t.test(G_SC~WASHING_MCH,var.equal=TRUE,data=clean_data)
effcd=round((2*res$statistic)/sqrt(res$parameter),2)
effectsize::t_to_d(t = res$statistic, res$parameter)


# global grade and car
# Describe the variables
psych::describeBy(clean_data$G_SC, clean_data$CAR, mat=TRUE)





# Using levene's test to test variance, pvalue needs to be greater than 0.05
car::leveneTest(G_SC ~ CAR, data=clean_data)




stats::t.test(G_SC~CAR,var.equal=TRUE,data=clean_data)



# Performing Cohen's d
res <- stats::t.test(G_SC~CAR,var.equal=TRUE,data=clean_data)
effcd=round((2*res$statistic)/sqrt(res$parameter),2)
effectsize::t_to_d(t = res$statistic, res$parameter)



# global grade and PHONE
# Describe the variables
psych::describeBy(clean_data$G_SC, clean_data$PHONE, mat=TRUE)



# Using levene's test to test variance, pvalue needs to be greater than 0.05
car::leveneTest(G_SC ~ PHONE, data=clean_data)




stats::t.test(G_SC~PHONE,var.equal=TRUE,data=clean_data)



# Performing Cohen's d
res <- stats::t.test(G_SC~PHONE,var.equal=TRUE,data=clean_data)
effcd=round((2*res$statistic)/sqrt(res$parameter),2)
effectsize::t_to_d(t = res$statistic, res$parameter)



# global grade and MOBILE
# Describe the variables
psych::describeBy(clean_data$G_SC, clean_data$MOBILE, mat=TRUE)



# Using levene's test to test variance, pvalue needs to be greater than 0.05
car::leveneTest(G_SC ~ MOBILE, data=clean_data)


stats::t.test(G_SC~MOBILE,var.equal=TRUE,data=clean_data)



# Performing Cohen's d
res <- stats::t.test(G_SC~MOBILE,var.equal=TRUE,data=clean_data)
effcd=round((2*res$statistic)/sqrt(res$parameter),2)
effectsize::t_to_d(t = res$statistic, res$parameter)




# testing on school nature
# Describe the variables
psych::describeBy(clean_data$G_SC, clean_data$SCHOOL_NAT, mat=TRUE)



car::leveneTest(G_SC ~ SCHOOL_NAT, data=clean_data)


stats::t.test(G_SC~SCHOOL_NAT,var.equal=FALSE,data=clean_data)




# Performing Cohen's d
res <- stats::t.test(G_SC~SCHOOL_NAT,var.equal=FALSE,data=clean_data)
effcd=round((2*res$statistic)/sqrt(res$parameter),2)
effectsize::t_to_d(t = res$statistic, res$parameter)



#################################################################################### Anova tests# ################################################33

colnames(clean_data)

clean_data

# Anova test for fathers education and global grade 
# Check the statistical description of variable of interest
psych::describeBy(clean_data$G_SC, clean_data$EDU_FATHER, mat=TRUE)
# performing Barrets test for homogenity of variance
stats::bartlett.test(G_SC~ EDU_FATHER, data=clean_data)


# One-way Anova test 
anova_result<-userfriendlyscience::oneway(as.factor(clean_data$EDU_FATHER),y=clean_data$G_SC,posthoc='games-howell')
anova_result
# access values in order to get f-statisitc on nect step
res2<-stats::aov(G_SC~ EDU_FATHER, data = clean_data)
fstat<-summary(res2)[[1]][["F value"]][[1]]
fstat
# Get the p-value
anova_p_value<-summary(res2)[[1]][["Pr(>F)"]][[1]]
anova_p_value
# Calculating the effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta


# Anova test for mothers education and global grade 
# Check the statistical description of variable of interest
psych::describeBy(clean_data$G_SC, clean_data$EDU_MOTHER, mat=TRUE)
# performing Barrets test for homogenity of variance
stats::bartlett.test(G_SC~ EDU_MOTHER, data=clean_data)


# One-way Anova test 
anova_result<-userfriendlyscience::oneway(as.factor(clean_data$EDU_MOTHER),y=clean_data$G_SC,posthoc='games-howell')
anova_result
# access values in order to get f-statisitc on nect step
res2<-stats::aov(G_SC~ EDU_MOTHER, data = clean_data)
fstat<-summary(res2)[[1]][["F value"]][[1]]
fstat
# Get the p-value
anova_p_value<-summary(res2)[[1]][["Pr(>F)"]][[1]]
anova_p_value
# Calculating the effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta



# Anova test for mothers education and global grade 
# Check the statistical description of variable of interest
psych::describeBy(clean_data$G_SC, clean_data$OCC_FATHER, mat=TRUE)
# performing Barrets test for homogenity of variance
stats::bartlett.test(G_SC~ OCC_FATHER, data=clean_data)



# One-way Anova test 
anova_result<-userfriendlyscience::oneway(as.factor(clean_data$OCC_FATHER),y=clean_data$G_SC,posthoc='games-howell')
anova_result
# access values in order to get f-statisitc on nect step
res2<-stats::aov(G_SC~ OCC_FATHER, data = clean_data)
fstat<-summary(res2)[[1]][["F value"]][[1]]
fstat
# Get the p-value
anova_p_value<-summary(res2)[[1]][["Pr(>F)"]][[1]]
anova_p_value
# Calculating the effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta



# Anova test for mothers education and global grade 
# Check the statistical description of variable of interest
psych::describeBy(clean_data$G_SC, clean_data$OCC_MOTHER, mat=TRUE)
# performing Barrets test for homogenity of variance
stats::bartlett.test(G_SC~ OCC_MOTHER, data=clean_data)




# One-way Anova test 
anova_result<-userfriendlyscience::oneway(as.factor(clean_data$OCC_MOTHER),y=clean_data$G_SC,posthoc='games-howell')
anova_result
# access values in order to get f-statisitc on nect step
res2<-stats::aov(G_SC~ OCC_MOTHER, data = clean_data)
fstat<-summary(res2)[[1]][["F value"]][[1]]
fstat
# Get the p-value
anova_p_value<-summary(res2)[[1]][["Pr(>F)"]][[1]]
anova_p_value
# Calculating the effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta




# Anova test for mothers education and global grade 
# Check the statistical description of variable of interest
psych::describeBy(clean_data$G_SC, clean_data$PEOPLE_HOUSE, mat=TRUE)
# performing Barrets test for homogenity of variance
stats::bartlett.test(G_SC~ PEOPLE_HOUSE, data=clean_data)



# One-way Anova test 
anova_result<-userfriendlyscience::oneway(as.factor(clean_data$PEOPLE_HOUSE),y=clean_data$G_SC,posthoc='Tukey')
anova_result
# access values in order to get f-statisitc on nect step
res2<-stats::aov(G_SC~ PEOPLE_HOUSE, data = clean_data)
fstat<-summary(res2)[[1]][["F value"]][[1]]
fstat
# Get the p-value
anova_p_value<-summary(res2)[[1]][["Pr(>F)"]][[1]]
anova_p_value
# Calculating the effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta




# Anova test for mothers education and global grade 
# Check the statistical description of variable of interest
psych::describeBy(clean_data$G_SC, clean_data$REVENUE, mat=TRUE)
# performing Barrets test for homogenity of variance
stats::bartlett.test(G_SC~ REVENUE, data=clean_data)




# One-way Anova test 
anova_result<-userfriendlyscience::oneway(as.factor(clean_data$REVENUE),y=clean_data$G_SC,posthoc='Tukey')
anova_result
# access values in order to get f-statisitc on nect step
res2<-stats::aov(G_SC~ REVENUE, data = clean_data)
fstat<-summary(res2)[[1]][["F value"]][[1]]
fstat
# Get the p-value
anova_p_value<-summary(res2)[[1]][["Pr(>F)"]][[1]]
anova_p_value
# Calculating the effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta



# Anova test for mothers education and global grade 
# Check the statistical description of variable of interest
psych::describeBy(clean_data$G_SC, clean_data$SCHOOL_NAT, mat=TRUE)
# performing Barrets test for homogenity of variance
stats::bartlett.test(G_SC~ SCHOOL_NAT, data=clean_data)



# One-way Anova test 
anova_result<-userfriendlyscience::oneway(as.factor(clean_data$SCHOOL_NAT),y=clean_data$G_SC,posthoc='Tukey')
anova_result
# access values in order to get f-statisitc on nect step
res2<-stats::aov(G_SC~ SCHOOL_NAT, data = clean_data)
fstat<-summary(res2)[[1]][["F value"]][[1]]
fstat
# Get the p-value
anova_p_value<-summary(res2)[[1]][["Pr(>F)"]][[1]]
anova_p_value
# Calculating the effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta




# Anova test for mothers education and global grade 
# Check the statistical description of variable of interest
psych::describeBy(clean_data$G_SC, clean_data$SCHOOL_TYPE, mat=TRUE)
# performing Barrets test for homogenity of variance
stats::bartlett.test(G_SC~ SCHOOL_TYPE, data=clean_data)





# One-way Anova test 
anova_result<-userfriendlyscience::oneway(as.factor(clean_data$SCHOOL_TYPE),y=clean_data$G_SC,posthoc='games-howell')
anova_result
# access values in order to get f-statisitc on nect step
res2<-stats::aov(G_SC~ SCHOOL_TYPE, data = clean_data)
fstat<-summary(res2)[[1]][["F value"]][[1]]
fstat
# Get the p-value
anova_p_value<-summary(res2)[[1]][["Pr(>F)"]][[1]]
anova_p_value
# Calculating the effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta



####################################################################Building MLR#############################################################


# making dummy variables to represent the categorical values. Starting with the simple 2 group variables
# Gender
clean_data_v2$dummyGender = ifelse(clean_data_v2$GENDER == "M", 0, ifelse(clean_data_v2$GENDER == "F", 1, NA))
# internet
clean_data_v2$dummyInternet = ifelse(clean_data_v2$INTERNET == "Yes", 0, ifelse(clean_data_v2$INTERNET == "No", 1, NA))
# TV
clean_data_v2$dummyTV = ifelse(clean_data_v2$TV == "Yes", 0, ifelse(clean_data_v2$TV == "No", 1, NA))
# COMPUTER
clean_data_v2$dummyComputer = ifelse(clean_data_v2$COMPUTER == "Yes", 0, ifelse(clean_data_v2$COMPUTER == "No", 1, NA))
# WASHING_MACHINE
clean_data_v2$dummyWmachine = ifelse(clean_data_v2$WASHING_MCH== "Yes", 0, ifelse(clean_data_v2$WASHING_MCH == "No", 1, NA))
# MIC_OVEN
clean_data_v2$dummyMicOven = ifelse(clean_data_v2$MIC_OVEN == "Yes", 0, ifelse(clean_data_v2$MIC_OVEN == "No", 1, NA))
# Car
clean_data_v2$dummyCar = ifelse(clean_data_v2$CAR == "Yes", 0, ifelse(clean_data_v2$CAR == "No", 1, NA))
# DVD
clean_data_v2$dummyDvd = ifelse(clean_data_v2$DVD == "Yes", 0, ifelse(clean_data_v2$DVD == "No", 1, NA))
# PHONE
clean_data_v2$dummyPhone = ifelse(clean_data_v2$PHONE == "Yes", 0, ifelse(clean_data_v2$PHONE == "No", 1, NA))
# MOBILE
clean_data_v2$dummyMobile = ifelse(clean_data_v2$MOBILE == "Yes", 0, ifelse(clean_data_v2$MOBILE == "No", 1, NA))
# SCHOOL_NAT
clean_data_v2$dummySchoolN = ifelse(clean_data_v2$SCHOOL_NAT == "PRIVATE", 0, ifelse(clean_data_v2$SCHOOL_NAT == "PUBLIC", 1, NA))


# dummy data for school type
clean_data_v2$dummySchoolAca= ifelse(clean_data_v2$SCHOOL_TYPE == "ACADEMIC", 1,0)
clean_data_v2$dummySchoolTech= ifelse(clean_data_v2$SCHOOL_TYPE == "TECHNICAL", 1,0)
clean_data_v2$dummySchoolTechAca = ifelse(clean_data_v2$SCHOOL_TYPE == "TECHNICAL/ACADEMIC", 1,0)

#dummy data for job
clean_data_v2$dummyJobNo= ifelse(clean_data_v2$JOB == "No", 1,0)
clean_data_v2$dummyJobPT= ifelse(clean_data_v2$JOB == "Yes, less than 20 hours per week", 1,0)
clean_data_v2$dummyJobFT= ifelse(clean_data_v2$JOB == "Yes, 20 hours or more per week", 1,0)


# dummy data for revenue
clean_data_v2$dummyRevenue1 = ifelse(clean_data_v2$REVENUE == "less than 1 LMMW", 1,
                                  ifelse(clean_data$REVENUE == "Between 1 and less than 2 LMMW", 1,0)) 

clean_data_v2$dummyRevenue2 = ifelse(clean_data_v2$REVENUE == "Between 2 and less than 3 LMMW", 1,
                                  ifelse(clean_data$REVENUE == "Between 3 and less than 5 LMMW", 1,0)) 

clean_data_v2$dummyRevenue3 = ifelse(clean_data_v2$REVENUE == "Between 5 and less than 7 LMMW", 1,
                                  ifelse(clean_data_v2$REVENUE == "Between 7 and less than 10 LMMW", 1,0)) 


# adding extra dummy variables
clean_data_v2$dummyFatherOCSmallEnt = ifelse(clean_data_v2$OCC_FATHER == "Small entrepreneur", 1,0)
clean_data_v2$dummyFatherOCTechOrProf = ifelse(clean_data_v2$OCC_FATHER == "Technical or professional level employee", 1,0)
clean_data_v2$dummyFatherOCOperator = ifelse(clean_data_v2$OCC_FATHER == "Operator", 1,0)
clean_data_v2$dummyFatherOCOther = ifelse(clean_data_v2$OCC_FATHER == "Other occupation", 1,0)
clean_data_v2$dummyFatherOCIndi = ifelse(clean_data_v2$OCC_FATHER == "Independent", 1,0)
clean_data_v2$dummyFatherOCENT = ifelse(clean_data_v2$OCC_FATHER == "Entrepreneur", 1,0)




# adding extra dummies
clean_data_v2$dummyEDUMotherIncProfEdu = ifelse(clean_data_v2$EDU_MOTHER == "Incomplete Professional Education", 1,0)
clean_data_v2$dummyEDUMotherPostGrad = ifelse(clean_data_v2$EDU_MOTHER == "Postgraduate education", 1,0)
clean_data_v2$dummyEDUMotherINCtech = ifelse(clean_data_v2$EDU_MOTHER == "Incomplete technical or technological", 1,0)
clean_data_v2$dummyEDUMotherCompProfEdu = ifelse(clean_data_v2$EDU_MOTHER == "Complete professional education", 1,0)



# adding extra dummies
clean_data_v2$dummySTtech = ifelse(clean_data_v2$SCHOOL_TYPE == "TECHNICAL", 1,0)
clean_data_v2$dummySTtechAca = ifelse(clean_data_v2$SCHOOL_TYPE == "TECHNICAL/ACADEMIC", 1,0)


# dummy variable for Father significant job
clean_data_v2$dummyFather = ifelse(clean_data_v2$OCC_FATHER == "Entrepreneur", 1,0) 


# dummy variable for mother significant job
clean_data_v2$dummyMOTHER = ifelse(clean_data_v2$EDU_MOTHER == "Ninguno", 1,0) 

# dummy data for more than one group
# people house
clean_data_v2$dummyPhouseOne = ifelse(clean_data_v2$PEOPLE_HOUSE == "One", 1,ifelse(clean_data_v2$PEOPLE_HOUSE == "Once", 1,0) ) 
clean_data_v2$dummyPhouse2t3 = ifelse(clean_data_v2$PEOPLE_HOUSE == "Two", 1,ifelse(clean_data_v2$PEOPLE_HOUSE == "Three", 1,0)) 
clean_data_v2$dummyPhouseAbove3 = ifelse(clean_data_v2$PEOPLE_HOUSE == "Four", 1,
                                      ifelse(clean_data_v2$PEOPLE_HOUSE == "Five", 1,
                                            ifelse(clean_data_v2$PEOPLE_HOUSE == "Six", 1,
                                                  ifelse(clean_data_v2$PEOPLE_HOUSE == "Seven",1,
                                                        ifelse(clean_data_v2$PEOPLE_HOUSE == "Eight",1,
                                                              ifelse(clean_data_v2$PEOPLE_HOUSE == "Nueve",1,
                                                                    ifelse(clean_data_v2$PEOPLE_HOUSE == "Ten",1,
                                                                          ifelse(clean_data_v2$PEOPLE_HOUSE == "Twelve or more",1,0)))))))) 
#
# building the model baseline with all the variables of interest
colnames(clean_data_v2)




############################################### model 1 ################################################
baseline_model1<-lm(formula = G_SC ~ MAT_S11 + CR_S11+BIO_S11+ENG_S11+CC_S11+OCC_FATHER+OCC_MOTHER+EDU_FATHER+EDU_MOTHER+dummyJobNo+dummyJobPT+dummyJobFT+SCHOOL_TYPE+
                    dummyGender+dummyInternet+dummyTV+dummyComputer+dummyWmachine+dummyMicOven+
                    dummyCar+dummyDvd+dummyPhone+dummyMobile+dummySchoolN, data= clean_data_v2)

print("Anova test")
anova(baseline_model1)
print("Summary of model")
summary(baseline_model1)
print("model Info comparison")
stargazer(baseline_model1, type="text") #Tidy output of all the required stats
print("Beta values of the model")
lm.beta(baseline_model1)


# check for influencial outliers
cooksd<-sort(cooks.distance(baseline_model1))
# plotting the cooks model
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels


# find the rows that are influential to observation
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
stem(influential)


# Bonferonni p-value for most extreme obs
car::outlierTest(baseline_model1)


#Assess homocedasticity 
plot(baseline_model1,1)
plot(baseline_model1, 3)

#Create histogram and  density plot of the residuals
plot(density(resid(baseline_model1))) 


#Create a QQ plotqqPlot(model, main="QQ Plot") #qq plot for studentized resid 
car::qqPlot(baseline_model1, main="QQ Plot") #qq plot for studentized resid


# #Calculate Collinearity, will not run as you have na's in summary
vifmodel<-car::vif(baseline_model1)
vifmodel
#Calculate tolerance
1/vifmodel



#  better way of getting standardized residuals
max(stdres(baseline_model1))
min(stdres(baseline_model1))



####################################################################################### Model 2 #############################################################

baseline_model12<-lm(formula = G_SC ~ MAT_S11 + CR_S11+BIO_S11+ENG_S11+CC_S11
                    +dummyInternet+dummyWmachine+dummyMicOven+dummyCar+dummyMobile, data= clean_data_v2)


print("Anova test")
anova(baseline_model2)
print("Summary of model")
summary(baseline_model2)
print("model Info comparison")
stargazer(baseline_model2, type="text") #Tidy output of all the required stats
print("Beta values of the model")
lm.beta(baseline_model2)



# get residuals
max(stdres(baseline_model2))
min(stdres(baseline_model2))



# check for influencial outliers
cooksd<-sort(cooks.distance(baseline_model2))
# plotting the cooks model
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels


#Create a QQ plotqqPlot(model, main="QQ Plot") #qq plot for studentized resid 
car::qqPlot(baseline_model2, main="QQ Plot") #qq plot for studentized resid


#Assess homocedasticity 
plot(baseline_model2,1)
plot(baseline_model2, 3)



#Create histogram and  density plot of the residuals
plot(density(resid(baseline_model2))) 


# #Calculate Collinearity, will not run as you have na's in summary
vifmodel<-car::vif(baseline_model2)
vifmodel
#Calculate tolerance
1/vifmodel





############################################################ model 3, selected model #################################################
baseline_model7<-lm(formula = G_SC ~ dummyFatherOCSmallEnt+dummyFatherOCTechOrProf+dummyFatherOCOperator
                    +dummyFatherOCOther+dummyFatherOCIndi+dummyFatherOCENT+dummyEDUMotherIncProfEdu
                    +dummyEDUMotherPostGrad+dummyEDUMotherINCtech+dummyEDUMotherCompProfEdu
                    +dummySTtech+dummySTtechAca+dummyMicOven+dummyMobile+dummySchoolN
                    +dummyGender+dummyInternet, data= clean_data_v2)

#  get residuals 
max(stdres(baseline_model7))
min(stdres(baseline_model7))



print("Anova test")
anova(baseline_model7)
print("Summary of model")
summary(baseline_model7)
print("model Info comparison")
stargazer(baseline_model7, type="text") #Tidy output of all the required stats
print("Beta values of the model")
lm.beta(baseline_model7)




# check for influencial outliers
cooksd<-sort(cooks.distance(baseline_model7))
# plotting the cooks model
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels



plot(baseline_model7,1)
plot(baseline_model7, 3)


car::qqPlot(baseline_model7, main="QQ Plot") 


plot(density(resid(baseline_model7))) 


# #Calculate Collinearity, will not run as you have na's in summary
vifmodel<-car::vif(baseline_model7)
vifmodel
#Calculate tolerance
1/vifmodel