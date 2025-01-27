#Losujemy 10000 punktów z kwadratu 1X1
nsim <- 1e4
# najpierw X
x <- runif(1e4)
# potem Y
y <- runif(1e4)
# Jeżeli podzielę przez liczbę punktów, mam oszacowanie pi/4
sum(sqrt(x^2+y^2)<1)/nsim
# Jeżeli podzielę przez liczbę punktów, mam oszacowanie pi/4
sum(sqrt(x^2+y^2)<1)/nsim

# Oszacowanie pi
4*sum(sqrt(x^2+y^2)<1)/nsim

#Przedzial ufnosci dla p/4
# Proporcja z proby
sum(sqrt(x^2+y^2)<1)/nsim
#Przedział ufności dla pi/4
binom::binom.confint(sum(sqrt(x^2+y^2)<1),nsim,0.95,method='wilson')
# Przedział ufności dla pi
4*binom::binom.confint(sum(sqrt(x^2+y^2)<1),nsim,0.95,method='wilson')[4:6]



# Szacujemy jakie jest prawdobobienstwo ze w 23 grupie dwie dane osoby maja urodziny w ten sam dzien
grupa <- sample(1:365,23,replace=TRUE)
grupa
table(grupa)
#Sprawdzenie liczebnosci TRUE powtarza się FALSE jesli nie powtarza
max(table(sample(1:365,23,replace=TRUE)))>1
#10 tys razy sprawdzamy
wyniki <- replicate(1e4,max(table(sample(1:365,23,replace=TRUE)))>1)
#Oszacowanie puntkowe
mean(wyniki)
# Przedzial ufosci dla tego prawdobodobienstwa
binom::binom.confint(sum(wyniki),length(wyniki),method='wilson')


# Zadanie 23.2 skrypt
# losujemy 28 prezentów
k = 
s <- sample(1:28)
s == 1:28
# Ponizszy kod zwraca true jezli zadne dziecko nie wylsuje siebie i false kiedy dziecko wylosuje siebie.
# sprawdzamy to 10tys razy
max(s==1:28)==0
data.frame(numer = 1:28,s)
# Jedna linia
max(sample(1:28)==1:28)==0
wyniki <- replicate(1e4,max(sample(1:k)==1:k)==0)
mean(wyniki)
binom::binom.confint(sum(wyniki),length(wyniki),method="wilson")
