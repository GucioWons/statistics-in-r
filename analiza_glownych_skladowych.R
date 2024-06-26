library(readxl)
library(psych) 
library(factoextra)

DATA_TABLE <- read_excel("analiza skupień.xlsx")


summary(DATA_TABLE)


PCA_TABLE <- DATA_TABLE[ ,-1]


DATA_STANDARD <- scale(PCA_TABLE)


kmo_result <- KMO(DATA_STANDARD)

print(kmo_result)


pca_result <- prcomp(DATA_STANDARD)

summary(pca_result)

eig_values <- get_eigenvalue(pca_result)

print(eig_values)

fviz_eig(pca_result)


fviz_pca_var(pca_result,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


fviz_pca_ind(pca_result,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


