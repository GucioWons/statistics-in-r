library(readxl)
install.packages("factoextra")
library(factoextra)

setwd("C:/Users/GucioWons/Desktop")
data <- read_excel("analiza3.xlsx")
print(data)

normalized_data <- scale(data[, -1])

print(normalized_data)

# Przeprowadzenie analizy k-średnich dla różnych liczby klastrów
wss <- numeric(10)
for (i in 2:10) {
  kmeans_result <- kmeans(normalized_data, centers = i)
  wss[i] <- kmeans_result$tot.withinss
}

# Wykres metody łokcia
plot(2:10, wss[2:10], type = "b", xlab = "Liczba klastrów", ylab = "Suma kwadratów odległości wewnątrz klastra")

# Przeprowadzenie analizy k-średnich dla optymalnej liczby klastrów
kmeans_result_optimal <- kmeans(normalized_data, centers = 5)

# Wyświetlenie wyników
print(kmeans_result_optimal$centers)
print(kmeans_result_optimal$size)

# Wizualizacja wyników
fviz_cluster(kmeans_result_optimal, data = normalized_data, ellipse.type = "convex", palette = "jco", ggtheme = theme_minimal())

