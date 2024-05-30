setwd("C:/Users/GucioWons/Desktop")
dane <- read.csv("regresja liniowa.csv", sep = ";", header = TRUE)
print(dane)
dane$Zgony <- as.numeric(gsub(",", ".", dane$Zgony))
dane$Urodzenia <- as.numeric(gsub(",", ".", dane$Urodzenia))
dane$Ludnosc <- as.numeric(gsub(",", ".", dane$Ludnosc))

print(dane)

model <- lm(Zgony ~ Urodzenia + Ludnosc, data = dane)

summary(model)

# Wykres dla Ludnosci
plot(Zgony ~ Urodzenia, data = dane, main = "Regresja liniowa: Zgony vs Ludnosc", xlab = "Ludnosc", ylab = "Zgony", pch = 19, col = "blue")
abline(lm(Zgony ~ Ludnosc, data = dane), col = "red", lwd = 2)

# Wykres dla Urodzen
plot(Zgony ~ Urodzenia, data = dane, main = "Regresja liniowa: Zgony vs Urodzenia", xlab = "Urodzenia", ylab = "Zgony", pch = 19, col = "blue")
abline(lm(Zgony ~ Urodzenia, data = dane), col = "red", lwd = 2)
