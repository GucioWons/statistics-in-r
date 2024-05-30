library(readxl)

dane <- read_excel("regresja liniowa.xlsx")

print(dane)

model <- lm(Zgony ~ Urodzenia + Ludnosc, data = dane)

summary(model)

# Wykres dla Ludnosci
plot(Zgony ~ Urodzenia, data = dane, main = "Regresja liniowa: Zgony vs Ludnosc", xlab = "Ludnosc", ylab = "Zgony", pch = 19, col = "blue")
abline(lm(Zgony ~ Ludnosc, data = dane), col = "red", lwd = 2)

# Wykres dla Urodzen
plot(Zgony ~ Urodzenia, data = dane, main = "Regresja liniowa: Zgony vs Urodzenia", xlab = "Urodzenia", ylab = "Zgony", pch = 19, col = "blue")
abline(lm(Zgony ~ Urodzenia, data = dane), col = "red", lwd = 2)
