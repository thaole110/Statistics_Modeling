###data 4
##Tim hieu yeu to anh huong den muc luong ($ gio) 
##cua nguoi lao dong o Anh nam 1976
#Nhap data
setwd('D:/Master/HK1-DS/Statistic Modeling/data')
data4 <- read_excel('data4.xls')
View(data4)
dim(data4)
#[1] 526  24
#data co 526 quan trac voi 24 bien

#Tong quan bo du lieu
summary(data4)

#So sanh hai bien theo doi wage va lwage
hist(data4$wage)
hist(data4$lwage)

#He so tuong quan giua cac bien
corrplot::corrplot(cor(data4), addCoef.col = "grey", number.cex = 0.7)

###Mo hinh wage
M1 <- lm(wage ~ . -lwage, data = data4)
summary(M1)

###Mo hinh lwage
M2 <- lm(lwage ~ . -wage, data = data4)
summary(M2)

##So sanh hai mo hinh wage va lwage
summary(M1)$adj.r.squared
#0.4694756
summary(M2)$adj.r.squared
#0.5309194

#phuong phap stepwise
#AIC
M2_AIC <- MASS::stepAIC(M2, k=2, direction = "backward", trace = FALSE)
summary(M2_AIC)

#BIC
M2_BIC <- MASS::stepAIC(M2, k=log(nrow(data4)), direction = "backward", trace = FALSE)
summary(M2_BIC)

#Kiem dinh Fisher tung phan cho mo hinh lwage
anova(M2_BIC,M2)

##He so VIF cua mo hinh BIC
car::vif(M2_BIC)

#Mo hinh giu bien exper loai expersq
M2_exper <- lm(formula = lwage ~ educ + exper + tenure + female + smsa + 
                 trade + services + profocc + servocc, data = data4)
summary(M2_exper)
#Kiem dinh Fisher cho mo hinh M2_exper
anova(M2_exper, M2_BIC)

#Mo hinh giu bien expersq loai exper
M2_expersq <- lm(formula = lwage ~ educ + tenure + female + smsa + 
                   trade + services + profocc + servocc + expersq, data = data4)
summary(M2_expersq)
#Kiem dinh Fisher cho mo hinh M2_expersq
anova(M2_expersq, M2_BIC)

##He so cho mo hinh da chon
coef(M2_BIC)
