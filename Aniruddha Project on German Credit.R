GermanCredit=read.csv("gc1.csv",sep=',')
GermanCredit
#str() function can helps you know type of variables and a few 
#sample values of each variable
attach(GermanCredit)
head(GermanCredit)
getwd()
# Create dummy variables for  Amount and Duration
GermanCredit$Age_c1 <- ifelse(GermanCredit$Age <=26,1,0)
GermanCredit$Age_c2 <- ifelse(GermanCredit$Age >26 & GermanCredit$Age <=33,1,0)
GermanCredit$Amount_c1 <- ifelse(GermanCredit$Amount <=1260,1,0)
GermanCredit$Amount_c2 <- ifelse(GermanCredit$Amount >1260 & GermanCredit$Amount <=4700,1,0)
GermanCredit$Duration_c1 <- ifelse(GermanCredit$Duration <=15,1,0)
GermanCredit$Duration_c2 <- ifelse(GermanCredit$Duration >15 & GermanCredit$Duration <=30,1,0)

# Test and Train Samples
GermanCredit.flag <- sample(1:nrow(GermanCredit),0.5*nrow(GermanCredit),
                            replace = F)
GermanCredit.dev <- GermanCredit[GermanCredit.flag,]
GermanCredit.val <- GermanCredit[-GermanCredit.flag,]


# Create Model formula
varlist <- names(GermanCredit)
varlist <- varlist[!varlist %in% c("Class")]

german.formula <- as.formula(paste("Class",paste(varlist,collapse = "+"),sep="~"))

#Now we can run a logistic regression model and code is as follows
german.logit <- glm(german.formula,family=binomial,data=GermanCredit)
summary(german.logit)
#"step()" function helps in selecting the smaller set of variables.
step(german.logit,direction = "both")

#we have the final list of variables from step() function, we are re-running
#logistic regression. Also one by one we have removed insignificant variables.  
#Then we are dropping variables based on multicollinearity.   
#We have used "vif()" function to check multicolliearity.
library(car)
vif(german.logit)

#First cut of logistic variables is below.
#We have to ensure that all variables are significant and 
#there is no multicolliearity (vif <2 for each of the variable).


german.logit <- glm(Class ~ #Amount + #InstallmentRatePercentage + #ForeignWorker + 
                      CheckingAccountStatus.lt.0 +
                      CheckingAccountStatus.0.to.200 + 
                      CreditHistory.NoCredit.AllPaid + 
                      CreditHistory.ThisBank.AllPaid +
                      CreditHistory.PaidDuly + 
                      #CreditHistory.Delay + 
                      Purpose.NewCar + 
                      #Purpose.Furniture.Equipment + 
                      #Purpose.Radio.Television + 
                      #Purpose.Repairs +
                      Purpose.Education + 
                      #Purpose.Business + 
                      SavingsAccountBonds.lt.100 + 
                      #SavingsAccountBonds.100.to.500 + 
                      EmploymentDuration.4.to.7 + 
                      #Personal.Male.Single + 
                      OtherDebtorsGuarantors.None + 
                      OtherDebtorsGuarantors.CoApplicant + 
                      #Property.RealEstate + 
                      #OtherInstallmentPlans.Bank + 
                      #Housing.Rent +
                      Age_c1 + 
                      #Amount_c1 + 
                      Duration_c1 + Duration_c2, 
                    family = binomial, 
                    data = GermanCredit)
summary(german.logit )

