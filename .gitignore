PR_dataset.mod =lm(RETPLASMA~AGE+SEX+SMOKSTAT+QUETELET+VITUSE+CALORIES+FAT+FIBER+ALCOHOL+CHOLESTEROL+RETDIET, data=PR_Dataset)
summary(PR_dataset.mod)
plot(PR_dataset.mod)

library(MASS)
bc <- boxcox(RETPLASMA~AGE+SEX+SMOKSTAT+QUETELET+VITUSE+FAT+FIBER+ALCOHOL+FAT++CHOLESTEROL+RETDIET, data=PR_Dataset, family="yjPower", plotit = TRUE)
library(car)
TRM <- yjPower(PR_Dataset$RETPLASMA, 18/100)
PR_matrix<-data.matrix(PR_Dataset, rownames.force = NA)
col12<-PR_matrix[,12]
plot(TRM)
PR_dataset.mod2 =lm(TRM~AGE+SEX+SMOKSTAT+QUETELET+VITUSE+FAT+FIBER+ALCOHOL+FAT++CHOLESTEROL+RETDIET, data=PR_Dataset)
plot(PR_dataset.mod2)
summary(PR_dataset.mod2)


TRMN2<-data.matrix(TRM,rownames.force = NA)
PR_dataset.mod2 =lm(TRM~AGE+SEX+SMOKSTAT+QUETELET+VITUSE+FAT+FIBER+ALCOHOL+FAT++CHOLESTEROL+RETDIET, data=PR_Dataset)
NPR_datasetA<- cbind(TRMN2, PR_matrix)
NPR_datasetB<- as.data.frame(NPR_datasetA)
colnames(NPR_datasetB)[1] <- 'TRMN2'
PR_datasetfull<- lm(TRMN2~AGE+SEX+SMOKSTAT+QUETELET+VITUSE+FAT+FIBER+ALCOHOL++CHOLESTEROL+RETDIET, data=NPR_datasetB)


library(car)
vif(lm(TRM~AGE+SEX+SMOKSTAT+QUETELET+VITUSE+CALORIES+FAT+FIBER+ALCOHOL++CHOLESTEROL+RETDIET, data=PR_Dataset))


sub1<-PR_matrix[61,]
sub2<-PR_matrix[63:315,]
delete62<-rbind(sub1,sub2)
NPR_dataset<-as.data.frame(delete62) 
PR_matrix1<-data.matrix(delete62, rownames.force = NA)
col12new<-PR_matrix1[,12]
TRMN <- yjPower(col12new, 0.18)
TRMN1<-data.matrix(TRMN, rownames.force = NA)
PR_dataset.mod3 <-lm(TRMN~AGE+SEX+SMOKSTAT+QUETELET+VITUSE+FAT+FIBER+ALCOHOL+CHOLESTEROL+RETDIET, data=NPR_dataset)
NPR_dataset1<-cbind(TRMN1,PR_matrix1)
NPR_dataset2<-as.data.frame(NPR_dataset1)
PR_dataset.mod4 <- lm(V1~ AGE+SEX+SMOKSTAT+QUETELET+VITUSE+FAT+FIBER+ALCOHOL+CHOLESTEROL+RETDIET, data=NPR_dataset2)
summary(PR_dataset.mod4)
plot(PR_dataset.mod4)

library(MASS)
fit <- lm(V1~AGE+SEX+SMOKSTAT+QUETELET+VITUSE+FAT+FIBER+ALCOHOL+CHOLESTEROL+RETDIET, data=NPR_dataset2)
step <- stepAIC(fit, direction="backward")
step$anova
fit1 <- lm(V1~AGE+SEX+SMOKSTAT+QUETELET+VITUSE+FAT+FIBER+ALCOHOL+CHOLESTEROL+RETDIET+AGE*SEX+AGE*SMOKSTAT+AGE*QUETELET+AGE*VITUSE+AGE*FAT+AGE*FIBER+AGE*ALCOHOL+AGE*CHOLESTEROL+AGE*RETDIET+SEX*SMOKSTAT+SEX*QUETELET+SEX*VITUSE+SEX*FAT+SEX*FIBER+SEX*ALCOHOL+SEX*CHOLESTEROL+SEX*RETDIET+SMOKSTAT*QUETELET+SMOKSTAT*VITUSE+SMOKSTAT*FAT+SMOKSTAT*FIBER+SMOKSTAT*ALCOHOL+SMOKSTAT*CHOLESTEROL+SMOKSTAT*RETDIET+QUETELET*VITUSE+QUETELET*FAT+QUETELET*FIBER+QUETELET*ALCOHOL+QUETELET*CHOLESTEROL+QUETELET*RETDIET+VITUSE*FAT+VITUSE*FIBER+VITUSE*ALCOHOL+VITUSE*CHOLESTEROL+VITUSE*RETDIET+FAT*FIBER+FAT*ALCOHOL+FAT*CHOLESTEROL+FAT*RETDIET+FIBER*ALCOHOL+FIBER*CHOLESTEROL+FIBER*RETDIET+ALCOHOL*CHOLESTEROL+ALCOHOL*RETDIET+CHOLESTEROL*RETDIET, data=NPR_dataset2)
summary(fit1)
fit2<- lm(V1~AGE+SEX+SMOKSTAT+FAT+ALCOHOL+CHOLESTEROL+RETDIET, data=NPR_dataset2)
summary(fit2)

    
    library(boot)
set.seed(2)
cv.error= rep(0, 5)
(for (i in 1:5) {
  PR_dataset.mod5 <- glm(V1~AGE+SEX+SMOKSTAT+CHOLESTEROL+FAT+ALCOHOL, data=NPR_dataset2)
  cv.error[i] <-cv.glm(NPR_dataset2, PR_dataset.mod5, K = 5)$delta[1]
})
cv.error
sum(cv.error)/5

View(NPR_datasetB)

library(boot)
set.seed(2)
cv.errorfull= rep(0, 5)
(for (i in 1:5) {
  PR_datasetfull<- glm(TRMN2~AGE+SEX+SMOKSTAT+QUETELET+VITUSE+FAT+FIBER+ALCOHOL++CHOLESTEROL+RETDIET, data=NPR_datasetB)
  cv.errorfull[i] <-cv.glm(NPR_datasetB, PR_datasetfull, K = 5)$delta[1]
})
cv.errorfull
plot(cv.errorfull)
sum(cv.errorfull)/5

