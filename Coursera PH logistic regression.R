#script for public health logistic regression
# Course 3 from Coursera R in Public health programe
setwd("/Users/fancy/Downloads/Coursera_R_Public_Health")
g <- read.csv(file = "/Users/fancy/Downloads/Coursera_R_Public_Health/Logistic regression/Diabetes_data.csv", header=TRUE, sep = ',')
##tell R the first row in the file contains the column names: header="TRUE"
dim(g)
colnames(g)
dimnames(g)[[2]]

#check variable
chol <- g$chol
hdl <- g$hdl
gender<-g$gender
age<-g$age
dm<-as.factor(g[,"dm"])

t<- table(gender) #store the tabulation for further manupulatin
addmargins(t) #this will sum up the gender totals to give an overall total and print the results
round(prop.table(t),digits=3) #get proportions rounded to 3 dp
round(100*prop.table(t),digits=1) #get %s rounded to 1dp

dm2<-factor(dm,exclude=NULL) #make new factor from the old one
table(dm2) #diplay the counts including the missings 
#for continous variable, use summary
summary(chol)

height<-g$height
weight<-g$weight
summary(height)
summary(weight)

#calculate BMI, convert unit first
height.si <-height*0.0254
weight.si <- weight*0.453592
bmi <- weight.si/height.si^2
summary(bmi)

#categorize continous variables:  categorize bmi into underweight, normal, and overweight
bmi_categorized <- ifelse(bmi<18.5, "underweight", 
                          ifelse(bmi>= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi>25 & bmi <=30,"overweight", 
                                        ifelse(bmi>30, "obese", NA))))
#check that the bmi_catogorized variable has worked

table (bmi_categorized,exclude = NULL)

#frequencies of variables by BMI category
dm_by_bmi_catogery <- table (bmi_categorized, dm2, exclude=NULL)
#check 
dm_by_bmi_catogery
#with the row percentages, margin=1 can specify that the table gives us the row percentages, 
# i.e. the frequencies of each column (diabetes status) in each row (BMI category). 
# If enter margin = 2, this would display the inverse, i.e. the frequencies of each row in each column.
round(100*prop.table(dm_by_bmi_catogery, margin=1), digits=1)


#categorize age
age_categorized <- ifelse(age<45, "under_45", 
                          ifelse(age>= 45 & age <= 64, "between_45_64", 
                                 ifelse(age>=65 & age <=74,"between_65_74", 
                                        ifelse(bmi>75, "over_75", NA))))
table (age_categorized,exclude = NULL)

#frequencies of gender by age category
gender_by_age_catogery <- table (age_categorized, gender, exclude=NULL)
#check 
gender_by_age_catogery
#with the row percentages
round(100*prop.table(gender_by_age_catogery, margin=1), digits=0)

#fit logistic model of diabetes with only intercept term (~!) using GLM(general linear model) command
m <- glm(dm~1, family=binomial (link=logit))
summary(m)
table(m$y) # check how R interprete the binary outcome "dm"
#put gender into the model
m <- glm(dm~gender, family=binomial (link=logit))
summary(m)
#exponetiate the coefficient to ge the odds
exp(-1.7047)
#exponentiate coefificient of age to get odds ratios
#check how R lable the variable
contrasts(gender)
#check the order of different levels of a categorical variable
levels (gender)
#change the order to put male first
gender <- relevel(gender, ref="male")

##check whether the relation between age and diabetes is linear
#create a cross tabulation of age and diabetes status
dm_by_age <-table (age,dm)
#output the frequencies of diabetes status by age
freq_table<-prop.table(dm_by_age, margin =1)
#calculate the odds of having diabetes
odds <-freq_table[,"yes"]/freq_table[,"no"]
#calculate the log odds
logodds<-log(odds)
#plot the ages found in the sample against the log odds of having diabetes
plot(rownames(freq_table),logodds)
#see only coefficients
m$coefficients
#exponentiate all coefficients
exp(m$coefficients)


#how many people from Buckingham has diabetes?
#people from Buckingham
location<-g$location
location_by_dm <- table (location, dm, exclude=NULL)
#check 
location_by_dm
#with the row percentages
round(100*prop.table(location_by_dm, margin=1), digits=1)

#put location into the model
m2 <- glm(dm~location, family=binomial (link=logit))
summary(m2)
exp(m2$coefficients)

##week 3
hist(age)
d<- density(age)
plot(d,main="") #gives warnings but the "main" argument supresses the ugly default title
plot(d) #this curve smooth out the noise using kernel smoothing which uses a weighted average of neibouring data
        #the band width plot reflects the amount of data used during the averaging
hist(bmi)
#remove missing data and draw density plot
d2<-density(bmi, na.rm = TRUE)
plot(d2)
hist(hdl)
d3<-density(hdl, na.rm = TRUE)
plot(d3)
#another way to dealth with missing data
hdl.no.na <-hdl[is.na(chol)==0]
d4<-density(hdl.no.na)
plot(d4)
#how gender relates to diabetes (dm)---cross tabulation
gender <- as.factor (g[,"gender"]) #define gender variable
gender_by_dm <- table (gender, dm, exclude=NULL)
gender_by_dm_prop<-prop.table(gender_by_dm, margin = 1) #proportion of diabetes status by gender
odds_gender <-gender_by_dm_prop[,"yes"]/gender_by_dm_prop[,"no"] #calculate the odds of having diabetes by gender
logodds_gender <-log(odds_gender) #calculate the log odds
dotchart(logodds_gender) #plot the log odds of having diabetes by gender
plot(as.factor(names(logodds_gender)), logodds_gender) #same as last one but in line graph

#how age relates to diabetes (dm)
age <-g[,"age"] #define the age variable (continuous)
dm_by_age <- table (age,dm) #not including NA values
dm_by_age_prop<-prop.table(dm_by_age, margin=1) #output the frequencies of diabetes by age
odds_age <- dm_by_age_prop [,"yes"]/dm_by_age_prop[,"no"] #calculate the odds of having diabetes
logodds_age <-log(odds_age) #calculate the log odds
plot(rownames(dm_by_age_prop), logodds_age) #plot the ages found in the sample against the log odds of having diabetes

#how cholesterol relates to dm?
chol <- g[,"chol"] #define chol as a continuous variable
dm_by_chol <- table (chol, dm) #create a cross tabulation of chol and dm
dm_by_chol_prop <- prop.table (dm_by_chol, margin=1) #output the freq of dm status by chol
odds_chol <- dm_by_chol_prop[,"yes"]/dm_by_chol_prop[,"no"] #calculate the odds of having dm
logodds_chol<-log(odds_chol) #calculate the log odds
plot (rownames(dm_by_chol_prop), logodds_chol, xlim=c(150,300))

#categorising chol into an ordinal variable
chol_categorized <- ifelse(chol<200, "healthy",
                           ifelse(chol<240, "borderline high",
                                  ifelse(chol >=240, "high", NA)))
#make sure it is treated as a factor/categorical variable and ordering the levels within the factor for the table
chol_categorized <- factor(chol_categorized, levels = c("healthy", "borderline high", "high"))
#create a cross tabulation of chol and dm
dm_by_chol_categorized <- table(chol_categorized, dm)
#output the freq of dm status by chol
dm_by_bmi_catogerized_prop <- prop.table(dm_by_chol_categorized, margin = 1)
#calculate the odds of having dm
odds_chol_categorized <- dm_by_bmi_catogerized_prop [,"yes"]/dm_by_bmi_catogerized_prop[,"no"]
#calculate the log odds
logodds_chol_categorized <- log(odds_chol_categorized)
#plot the chol categories found in the sample against the log odds of having dm
dotchart(logodds_chol_categorized)

#BMI relates to dm?
#create a cross tabulation of BMI and dm
dm_by_bmi_categorized <- table(bmi_categorized,dm)
#output the freq of dm by BMI
dm_by_bmi_catogerized_prop <- prop.table(dm_by_bmi_categorized, margin=1)
#calculate the odds of having dm
odds_chol_categorized <- dm_by_bmi_catogerized_prop[,"yes"]/dm_by_bmi_catogerized_prop[,"no"]
#calculate the log odds
logodds_chol_categorized <- log(odds_chol_categorized)
#plot the BMI categories found in the sample against the log odds of having dm
dotchart(logodds_chol_categorized)

#fit a multiple logistic regression with age, gender, and BMI
m1 <- glm(dm~ age + gender + bmi, family=binomial (link=logit)) #get odds ratio for age = 0.055, exponentiate is 1.057. this is adjusted for the effects of gender and BMI
summary(m1)
#calculate 95% CI
exp (confint(m1))

colnames(g)
#fit a multiple logistic model with age, cholesterol, and insurance type --- my version
insurance <- g$insurance
head(insurance)
insurance_categorized <- ifelse (insurance==1, "gov",
                                 ifelse (insurance==2, "pri", 
                                         ifelse (insurance ==0, "non", NA)))
insurance_categorized <- factor(insurance_categorized, levels = c("non", "pri", "gov"))
table (insurance_categorized)
m2 <-glm (dm~age + chol + insurance_categorized, family = binomial (link=logit))
summary (m2)
exp(0.049753)
exp (0.008402)
exp (-0.271955)
exp(-0.589803)

#fit a multiple logistic model with age, cholesterol, and insurance type --- better way
insurance <- as.factor(g$insurance) 
class(insurance)
m3 <- glm(dm ~ age + chol + insurance, family=binomial (link=logit)) 
summary(m3) 
exp(m3$coefficients) 
exp(confint(m3)) 


#McFadden's r-squared:
#design the logistic regression
full_model <- glm(dm~age+chol+insurance, family = binomial(link=logit))
#check model
summary(full_model)
#run a null model
null_model <- glm(dm~1, family = binomial(link=logit))
summary(null_model)
#calculate McFadden's R-square
R2<- 1-logLik(full_model)/logLik(null_model)
R2

#c-statistic
install.packages("DescTools")
require(DescTools) #load package
#generate the c-statistic
Cstat(full_model)

#Hosmer-Lemeshow statistic and test
install.packages("ResourceSelection")
require(ResourceSelection)
full_model <- glm(dm~age + chol + insurance, family=binomial(link=logit))
full_model$y
HL <- hoslem.test(x=full_model$y, y=fitted(full_model), g=10)
HL
#plot the observed vs expected number of cases for each of the 10 groups
plot(HL$observed[,"y1"], HL$expected[,"yhat1"])
#plot the observed vs expected number of noncases for each of the 10 groups
plot(HL$observed[,"y0"], HL$expected[,"yhat0"] )
#plot observed vs. expected prevalance for each of the 10 groups
plot(x=HL$observed[,"y1"]/(HL$observed[,"y1"]+HL$observed[,"y0"]),
     y=HL$expected[,"yhat1"]/(HL$expected[,"yhat1"]+HL$expected[,"yhat0"]))
##verify result with another package
install.packages("generalhoslem")
require(generalhoslem)
logitgof(obs=full_model$y, exp=fitted(full_model), g=10)


##check how well the model is 
#analyse table of deviance
anova(full_model, test="Chisq")


colnames(g)
#fit a model with age, gender, hdl, chol, bp.1s, and bp.1d
bp.1s<-g$bp.1s
bp.1d<-g$bp.1d
m5 <- glm(dm~age+gender+chol+hdl+bp.1s+bp.1d, family=binomial (link=logit))
summary (m5)

#backward stepwise deleting variable insignifant chi-square
m6 <- glm(dm~age+gender+chol+hdl+bp.1s, family=binomial (link=logit))
summary (m6)
m7 <- glm(dm~age+chol+hdl+bp.1s, family=binomial (link=logit))
summary (m7)
m8 <- glm(dm~age+chol+hdl, family=binomial (link=logit))
summary (m8)

#script from instructor
dm <- as.factor(g[,"dm"]) 
insurance <- as.factor(g[,"insurance"])# let's say 0=none, 1=gov, 2=private 
fh <- as.factor(g[,"fh"]) # 1=FH, 0=no FH 
smoking <- as.factor(g[,"smoking"]) # 1,2,3 
chol <- g[,'chol'] 
hdl <- g[,'hdl'] 
ratio <- g[,'ratio'] 
location <- as.factor(g[,'location']) 
age <- g[,'age'] 
gender <- as.factor(g[,'gender']) 
frame <- as.factor(g[,'frame']) 
systolic <- g[,'bp.1s'] 
diastolic <- g[,'bp.1d'] 

model <- glm(dm ~ age + bmi + chol + hdl + systolic + diastolic, family = binomial(link = logit)) 

summary(model) 
anova(model, test = "Chisq")
#drop blood pressure
model2 <- glm(dm ~ age + bmi + chol + hdl , family = binomial(link = logit)) 

summary(model2) 
anova(model2, test = "Chisq")
#strange that blood pressues are not significant
cor.test(systolic,hdl) #not sig
cor.test(systolic,bmi) #sig
cor.test(systolic,chol) #very sig
cor.test(systolic,age) #extremely sig

model3 <- glm(dm ~  bmi + chol + hdl + systolic , family = binomial(link = logit)) 
summary(model3)

frame <- as.factor(g[,"frame"])
model4 <- glm(dm ~  bmi + chol + hdl + systolic+ diastolic+gender+location+frame+insurance+smoking , family = binomial(link = logit)) 
summary(model4)
anova(model4, test = "Chisq") 




















