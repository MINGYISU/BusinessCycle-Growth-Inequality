# Assignment 2
# Reading in data
wage <- read.csv("Wage.csv")

# Defining variables
males <- ts(wage["Males"], start = 1997)

females <- ts(wage["Females"], start = 1997)

CPI <- ts(wage["CPI"], start = 1997)

# Part A
# a1
plot(males, col = "blue", lwd = 2, lty = 1, ylim = c(5, 25), 
     main = " Average Nominal Hourly Wage Versus Years", 
     ylab = "Hourly Nominal Wage (dollars)")
lines(females, col = "red", lwd = 2, lty = 1)
legend("topleft", c("Males", "Females"), col = c("blue", "red"), lwd = 2, lty = 1)



# a2
remales <- males / (CPI / 100)

refemales <- females / (CPI / 100)

plot(remales, col = "blue", lwd = 2, lty = 1, ylim = c(5, 20), 
     main = " Hourly Real Wage Versus Years", 
     ylab = "Hourly Real Wage in dollars of 2002")
lines(refemales, col = "red", lwd = 2, lty = 1)
legend("topleft", c("Males", "Females"), col = c("blue", "red"), lwd = 2, lty = 1)

# a3
# linear trend for real wage
t <- time(males, offset = 0.5)

# males
coef1 <- coef(lm(remales~t))
trend1 <- coef1[1] + coef1[2] * t

# females
coef2 <- coef(lm(refemales~t))
trend2 <- coef2[1] + coef2[2] * t

plot(trend1, col = "blue", lwd = 2, lty = 1, ylim = c(5, 20), 
     main = " Linear Trends for Hourly Real Wage Series", 
     ylab = "Hourly Real Wage")
lines(trend2, col = "red", lwd = 2, lty = 1)
legend("topleft", c("Males", "Females"), col = c("blue", "red"), lwd = 2, lty = 1)


# a4
# Detrend
CSI1 <- males - trend1
CSI2 <- females - trend2



plot(CSI1, col = "blue", lwd = 2, lty = 1, 
     main = "Cyclical Components", 
     ylab = "Hourly Real Wage")
lines(CSI2, col = "red", lwd = 2, lty = 1)
legend("topleft", c("Males", "Females"), col = c("blue", "red"), lwd = 2, lty = 1)

plot(CSI1, CSI2, pch = 21, col = "darkred", 
     bg = "red", xlab = "Males", 
     ylab = "Females", xy.labels=FALSE, xy.lines=FALSE, 
     main = "Comovement Between the Cyclical Components Hourly Real Wage")


# Part B
# b1
# Republic of Korea (South Korea)
# The United Kingdom of Great Britain and Northern Ireland (United Kingdom)
# Japan
# Union of the Comoros (Comoros)

# b2
gdp <- read.csv("realgdp.csv")

kr <- ts(gdp["KOR"], start = 1970)
kr <- log(kr)
uk <- ts(gdp["GBR"], start = 1970)
uk <- log(uk)
jp <- ts(gdp["JPN"], start = 1970)
jp <- log(jp)
co <- ts(gdp["COM"], start = 1970)
co <- log(co)

plot(kr, col = "black", lwd = 3, ylab = "Real Per Capital GDP in log", 
     main = "Real Per Capital GDP of the four countries Versus Years")
lines(uk, col = "red", lwd = 3)
lines(jp, col = "yellow2", lty = 2, lwd = 3)
lines(co, col = "darkgreen", lwd = 3)
legend("right", c("South Korea", "United Kingdom", "Japan", "Comoros"), 
       col = c("black", "red", "yellow2", "darkgreen"), lty = c(1, 1, 2, 1), lwd = 3)

# b3
t <- time(kr, offset = 0.5)
t2 <- t ^ 2

coefqt1 <- coef(lm(kr~t+t2))
qt1 <- coefqt1[1] + coefqt1[2] * t + coefqt1[3] * t2
c1 <- kr - qt1

coefqt2 <- coef(lm(uk~t+t2))
qt2 <- coefqt2[1] + coefqt2[2] * t + coefqt2[3] * t2
c2 <- uk - qt2

coefqt3 <- coef(lm(jp~t+t2))
qt3 <- coefqt3[1] + coefqt3[2] * t + coefqt3[3] * t2
c3 <- jp - qt3

coefqt4 <- coef(lm(co~t+t2))
qt4 <- coefqt4[1] + coefqt4[2] * t + coefqt4[3] * t2
c4 <- co - qt4

plot(c1, col = "black", lwd = 2, ylab = "Cyclical Components of Real Per Capital GDP in log", 
     main = "Cyclical Components of Real Per Capital GDP in log", 
     ylim= c(-0.25, 0.3))
lines(c2, col = "red", lwd = 3)
lines(c3, col = "yellow2", lty = 2, lwd = 3)
lines(c4, col = "darkgreen", lty = 3, lwd = 3)
legend("topleft", c("South Korea", "United Kingdom", "Japan", "Comoros"), cex = 0.5, 
       col = c("black", "red", "yellow2", "darkgreen"), lty = c(1, 1, 2, 3), lwd = c(2, 3, 3, 3))

# b4
g1 <- diff(kr)/lag(kr, -1)*100
g2 <- diff(uk)/lag(uk, -1)*100
g3 <- diff(jp)/lag(jp, -1)*100
g4 <- diff(co)/lag(co, -1)*100

g1m <- mean(g1)
g2m <- mean(g2)
g3m <- mean(g3)
g4m <- mean(g4)

gdp701 <- window(kr, c(1970), c(1970))
gdp702 <- window(uk, c(1970), c(1970))
gdp703 <- window(jp, c(1970), c(1970))
gdp704 <- window(co, c(1970), c(1970))

plot(c(gdp701, gdp702, gdp703, gdp704), c(g1m, g2m, g3m, g4m), pch = 19, 
     col = c("black", "red", "yellow2", "green"),
     xlab = "real per capital GDP in log in 1970",
     ylab="Avg. annual growth rate", main="Average Aunnal Growth Rate versus GDP in 1970 in log")
legend("topright", c("South Korea", "United Kingdom", "Jppan", "Comoros"), pch = 19, cex = 0.75, 
       col = c("black", "red", "yellow2", "green"))

# b5
gdpts <- ts(gdp[, -1], start = 1970)
gdpts <- gdpts / 1000
gdp81 <- window(gdpts, c(1972), c(1972))

hist(gdp81, breaks = 25, xlab = "real per capital GDP (thousands of international dollars of 2011)", 
     main = "real per capital GDP in 1981")

gdp14 <- window(gdpts, c(2000), c(2000))

hist(gdp14, breaks = 25, xlab = "real per capital GDP (thousands of international dollars of 2011)", 
     main = "real per capital GDP in 2014")

# b6
lgdpts <- log(gdpts) / 1000

lgdp81 <- window(lgdpts, c(1981), c(1981))

hist(lgdp81, breaks = 25, xlab = "real per capital GDP in log", 
     main = "real per capital GDP in log in 1981")

lgdp14 <- window(lgdpts, c(2014), c(2014))

hist(lgdp14, breaks = 25, xlab = "real per capital GDP in log", 
     main = "real per capital GDP in log in 2014")

