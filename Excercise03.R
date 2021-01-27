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
# alpha = 0.05
# df = 11
#Mo hinh hai bien doc lap x1, x2
#H0: B1=B2=0
#H1: Ton tai it nhat Bj !=0 voi j = 1,2
fval <- qt(1-0.05,11)
fval
#1.795885
summary(M12)
#Fobs = 12.1 > fval
#Bac bo H0
#Voi muc y nghia 0.05, y duoc giai thich boi it nhat 1 bien

##Mo hinh hai bien doc lap x2, x3
#H0: B2=B3=0
#H1: Ton tai it nhat Bj !=0 voi j = 2,3
#

##Cau6
#Mo hinh hoi quy tuyen tinh voi nhieu bien nhat co the
M3 <- lm(y ~ x1 + x2 + x3)

##Cau7
#Uoc luong he so hoi quy trong mo hinh tuyen tinh Cau6
coef(M3)
#(Intercept)          x1          x2          x3 
#32.89132428  0.80190069 -0.38136236 -0.03713244 

##Cau8
#Tinh uoc luong V(e) va V(B)
out <- summary(M3)
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

