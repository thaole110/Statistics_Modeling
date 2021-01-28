####Bai 3:
##Tai du lieu
library(readxl)
bai3 <- read_excel("Downloads/bai3.xlsx")
View(bai3) 

y <- bai3$y
x1 <- bai3$x1
x2 <- bai3$x2
x3 <- bai3$x3

###Cau1,2
##Viet mo hinh tuyen tinh theo hai bien doc lap x1, x2
#uoc luong he so hoi quy
M12 <- lm(y ~ x1 + x2)
coef(M12)
#(Intercept)          x1          x2 
#25.8421378   0.7148959  -0.3281129 

##Viet mo hinh tuyen tinh theo hai bien doc lap x2, x3
#uoc luong he so hoi quy
M23 <- lm(y ~ x2 + x3)
coef(M23)
#(Intercept)          x2          x3 
#31.97642386 -0.45389541  0.01996295

##Viet mo hinh tuyen tinh theo hai bien doc lap x1, x3
#uoc luong he so hoi quy
M13 <- lm(y ~ x1 + x3)
coef(M13)
#(Intercept)          x1          x3 
#8.60924098  0.92720866  0.02323681 

##Cau3 
#Voi do tin cay 95%, tim khoang tin cay cho cac tham so trong
#mo hinh 2 bien doc lap x1, x2
confint(M12)
#               2.5 %     97.5 %
#  (Intercept) 12.4938794 39.1903962
#x1           0.1288532  1.3009387
#x2          -0.6242802 -0.0319457

##Cau4
#He so xac dinh cho mo hinh hai bien doc lap x1, x2
summary(M12)
#Multiple R-squared:  0.6875,	Adjusted R-squared:  0.6307 

#He so xac dinh cho mo hinh hai bien doc lap x2, x3
summary(M23)
#Multiple R-squared:  0.488,	Adjusted R-squared:  0.3949 

#He so xac dinh cho mo hinh hai bien doc lap x1, x3
summary(M13)
#Multiple R-squared:  0.5263,	Adjusted R-squared:  0.4402 

##Cau5

##Cau6
#Mo hinh hoi quy tuyen tinh voi nhieu bien nhat co the
model <- lm(y ~ x1 + x2 + x3)

##Cau7
#Uoc luong he so hoi quy trong mo hinh tuyen tinh Cau6
coef(model)
#(Intercept)          x1          x2          x3 
#32.89132428  0.80190069 -0.38136236 -0.03713244 

##Cau8
#Tinh uoc luong V(e) va V(B)
out <- summary(model)
#V(B0)
se_B0 <- out$coefficients[1,2]
se_B0
#11.66331
V_B0 <- (se_B0)**2
V_B0
#136.0328

#V(B1)
se_B1 <- out$coefficients[2,2]
se_B1
#0.2984358
V_B1 <- (se_B1)**2
V_B1
#0.0.08906395

#V(B2)
se_B2 <- out$coefficients[3,2]
se_B2
#0.1565807
V_B2 <- (se_B2)**2
V_B2
#0.08906395

#V(B3)
se_B3 <- out$coefficients[4,2]
se_B3
#0.05202312
V_B3 <- (se_B3)**2
V_B3
#0.002706406

##Tinh V(e)
s_square <- anova_model$`Mean Sq`[4]
s_square
#6.744767
#s_square la uoc luong khong chech cho V(e)

##Cau 10
#Khi them hai bien doc lap x2, x3 vao mo hinh chi co bien x1
#chat luong uoc luong cao hon khong
M1 <- lm(y ~ x1)
#Bang anova cho M1
anova(M1)
#Analysis of Variance Table

#Response: y
#           Df Sum Sq Mean Sq F value   Pr(>F)   
#x1         1 117.66  117.66   12.93 0.003674 **
#Residuals 12 109.20    9.10                    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Bang anova cho mo hinh ba bien doc lap x1, x2, x3
anova(model)
#Analysis of Variance Table
#Response: y
#           Df  Sum Sq Mean Sq F value   Pr(>F)   
# x1         1 117.659 117.659 17.4445 0.001898 **
# x2         1  38.314  38.314  5.6806 0.038389 * 
# x3         1   3.436   3.436  0.5095 0.491694   
#Residuals 10  67.448   6.745                    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

SSE_H0 <- anova_M1$`Sum Sq`[2]
SSE_H0
#109.1983
df_H0 <- anova_M1$Df[2]
df_H0
#12
SSE_H1 <- anova_model$`Sum Sq`[4]
SSE_H1
#67.44767
df_H1 <- anova_model$Df[4]
df_H1
#10
r <- (df_H0 - df_H1)
r
#2
F_obs <- ((SSE_H0 - SSE_H1)/r)/(SSE_H1/df_H1)
F_obs
#3.095036
F_val <- qt(1-0.05,r,df_H1)
F_obs >= F_val
#FALSE
#Voi muc y nghia 0.05, khong du co so bac bo H0
#Voi muc y nghia 0.05, mo hinh hoi quy tuyen tinh voi 1 bien x1
#co chat luong uoc luong cao hon
