


y = arima.sim(list(order = c(2,0,0), ar = c(0.6, 0.2)), n = 2000, sd = 1)

c = 0.05
phi = 0.15
omega = 0.01
alpha = 0.1
beta = 0.8
eta = rnorm(5000)

z = rep(0, 5000)
r = eta
sigma2 = rep(omega/(1- alpha - beta), 5000)

for (i in 2:5000) {
  sigma2[i] = omega + alpha*r[i-1]^2 + beta*sigma2[i-1]
  r[i] = sqrt(sigma2[i])*eta[i]
  z[i] = c + phi*z[i-1] + r[i]
}

Data <- seq(as.Date("2010-06-01"), as.Date("2010-06-01") + 2000 - 1,"day")
dados <- data.frame(data = Data, serie1 = y, serie2 = tail(z, 2000))

write.csv(dados, "dados_aula10.csv")




