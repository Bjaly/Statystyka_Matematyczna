#  mamy probe pracownikow budynku kazdy podal mase siala oszacowanc srednia mase ciala
proba <- c(69, 75, 77, 76, 84, 60, 71, 84, 47, 74, 86, 78, 73, 54, 83, 61, 54, 61, 60, 72)
xbar = mean(proba)
s = sd(proba)
n =length(proba)
alpha = 0.5

df =n-1
xbar + c(-qt(1-alpha/2, df), qt(1-alpha/2, df)) * s/sqrt(n)
# Proba bootstrapowa 
proba_boot = sample(proba,replace=TRUE)
mean(proba_boot)
# Powtarzamy 10k razt
wyniki_boot <- replicate(1e4,mean(sample(proba,replace=TRUE)))
wyniki_boot
mean(wyniki)
df_b = length(wyniki) - 1
df_b
hist(wyniki_boot,breaks=25)
quantile(wyniki_boot,c(0.025,1-0.025))
?hist
wz  <- function(x){sd(x)/mean(x)}

library(googlesheets4)
gs4_deauth()
dane <- read_sheet("1bCncEEinU9dcmo1xKkPGmk0wd7f5sufIPGKm0XAnbq4")
cor(dane$oszac,dane$punkty)

cor(dane[sample(1:56, replace=TRUE),])[2,1]

# Mogę to powtorzyc 10000tys razy.
wyniki <- replicate(1e4, cor(dane[sample(1:56, replace=TRUE),])[2,1])
hist(wyniki, breaks=100)
quantile(wyniki, c(0.025, 1-0.025))
#Test permutacyjny 
#Kolumnę x zostawia a y permutuje
#Powtarzam to wiele razy
cor(dane$oszac, sample(dane$punkty))

wyniki_test_permutacyjny <- replicate(1e4,cor(dane$oszac,sample(dane$punkty)))
hist(wyniki_test_permutacyjny,breaks=25)
mean(wyniki_test_permutacyjny > cor(dane$oszac,dane$punkty))

