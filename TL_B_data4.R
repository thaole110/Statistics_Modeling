###data 4
##Tim hieu yeu to anh huong den muc luong ($ gio) 
##cua nguoi lao dong o Anh nam 1976
#Nhap data
library(readxl)
data4 <- read_excel("data/data4.xls")
View(data4)
dim(data4)
#[1] 526  24
#data co 526 quan trac voi 24 bien

##Mo hinh wage
M1 <- lm(wage ~ educ + exper + tenure + nonwhite + female + married + numdep + smsa + northcen + south + west + construc + ndurman + trcommpu + trade + services + profserv + profocc + clerocc + servocc + expersq + tenursq, data = data4)
summary(M1)
##R^2 = 46.9%

##phuong phap stepwise
#BIC
M1_BIC <- MASS::stepAIC(M1, k=log(nrow(data4)), direction = "backward", trace = FALSE)
summary(M1_BIC)

#Kiem dinh Fisher tung phan cho hai mo hinh wage
anova(M1_BIC, M1)

##Mo hinh lwage
M2 <- lm(lwage ~ educ + exper + tenure + nonwhite + female + married + numdep + smsa + northcen + south + west + construc + ndurman + trcommpu + trade + services + profserv + profocc + clerocc + servocc + expersq + tenursq, data = data4)
summary(M2)
##R^2 = 45.42%

#phuong phap stepwise
#BIC
M2_BIC <- MASS::stepAIC(M2, k=log(nrow(data4)), direction = "backward", trace = FALSE)
summary(M2_BIC)


#Kiem dinh Fisher tung phan cho hai mo hinh lwage
anova(M2_BIC,M2)

##So sanh hai mo hinh wage va lwage
summary(M1_BIC)$adj.r.squared
#0.4644986
summary(M2_BIC)$adj.r.squared
#0.5263545
