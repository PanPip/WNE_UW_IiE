#Zadanie 1

#a)

#ilosc powtorzen
N_iter = 1000
#ilosc generowanych wartosci z rozkladu wykladnicznego
N_wylk = 10000
#parameter dla generowania rozkladu
lambda = 1/10

for (i in 1:N_iter){
  rozkl_wykl = rexp(N_wykl, lambda)
  
  #b)
  
  #estymator stosujac uogolniona metode momentow
  #Ilosc obserwacji
  T = N_wykl
  #Dane
  y = rozkl_wykl
  #Funkcja do minimizacji
  Qmin = function(lambda){
    M = rbind(1/T*sum(y)-1/lambda, 1/T*sum((y-lambda)^2)-1/lambda)
    W = matrix(0, nrow=2, ncol=2)
    W[1,1] = 1/T*sum((y-1/lambda)^2)
    W[1,2] = 1/T*sum((y-1/lambda)*((y-1/lambda)^2-1/lambda))
    W[2,2] = 1/T*sum(((y-1/lambda)^2-1/lambda)^2)
    W[2,1] = W[1,2]
    val= -t(M)%*%solve(W)%*%M
    return(val)
  }
  #biblioteka, uzywana dal minimizacji
  library("maxLik")
  #Minimizujemy (maksymizujemy -func)
  wynik=maxNR(fn=Qmin, start=mean(y))
  
  summary(wynik)
  
  #c)
  
  #Weryfikujemy hipoteze: H0: lambda = 0
  #Model IV z Z02
  #Alternatywna hipoteze - H1: lambda != 0
  
  #Nie rozumiem ... tutaj dzielimy na zero albo co?
  
  
}


#Zadanie 2

#Dane
dane = c(-2.23, 4.25, 0.72, 1.08, -1.78, -0.87, 1.81, 1.39, 1.12, 0.49, 1.61, 1.56)
#zalozenie
mu = 0

#Rysujemy funkcje log-wiarygodnosci. 

n = length(dane)

p = seq(from=0, to=1, by=0.01)
lnL = -n/2*log(2*pi) - n*log(p) - 1/(2 * p**2)*sum((dane-p)**2) - n/2 * log(p**2)

plot(p, lnL, type="l")

#Zadanie 3

#a)

ozemy maksymizowac tylko po jednemu paranetru, wtedy

lnL_2 = function(parametry){
  mu=parametry[1]
  sigma2 = parametry[2]
  ll = -N/2*log(2*pi)-N*log(sqrt(sigma2))-N/2*log(sigma2)-1/(2*sigma2)*sum((x-mu)^2)
  return(ll)
}

#Gradient i hessian?

gradient = function(parametry){
  mu=parametry[1]
  sigma2 = parametry[2]
  gr = rep(0,2)
  gr[1] = 1/sigma2*sum(x-mu)
  gr[2] = -N/2*1/sigma2+1/(2*sigma2^2)*sum((x-mu)^2)
  return(gr)
}

hesjan = function(parametry){
  mu=parametry[1]
  sigma2 = parametry[2]
  h = matrix(0,nrow=2,ncol=2)
  h[1,1] = -N/sigma2
  h[1,2] = -1/(sigma2^2)*sum(x-mu)
  h[2,2] = N/(2*sigma2^2)+1/(sigma2^3)*sum((x-mu)^2)
  h[2,1] = h[1,2] # z symetrycznosci
}


library("maxLik")
wynik = maxNR(fn=lnL_2,grad = gradient, hess = hesjan, start= c(mu=2,sigma2=3))
summary(wynik)







