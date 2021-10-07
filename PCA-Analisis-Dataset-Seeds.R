
#Dataset ini terdiri dari 210 instance, 7 atribut, tidak ada missing value.

#Dataset yang dipakai adalah data biji gandum yang memiliki tiga varietas gandum yg berbeda yaitu : 
#Kama, Rosa and Canadian, masing-masing varietas memiliki masing-masing 70 elemen atau row data yang dipilih secara acak untuk percobaan.
#dataset ini digunakan untuk mendeteksi apakah suatu biji gandum termasuk ke dalam varietas Kama atau Rosa atau Canadian.



#tujuh parameter geometris biji gandum diukur:
#1. area A, (numeric)
#2. perimeter P,
#3. compactness C = 4*pi*A/P^2,
#4. length of kernel,
#5. width of kernel,
#6. asymmetry coefficient
#7. length of kernel groove.
#8. Label/Kategori

data_seeds <- read.csv("seeds.csv", header = T)

head(data_seeds)
#View(data_seeds)
str(data_seeds)

data_seeds_notLabel <- data_seeds[,1:7] #mengeluarkan label/kategori

#standarisasi menggunakan scale (salah satu contoh lain utk standarisasi data)
#standarisasi_data <- scale(x = data_seeds[,1:7], center = TRUE, scale = TRUE)
#View(standarisasi_data)
#head(standarisasi_data)

#Melakukan Standarisasi data
#--------------------------------------------
# membuat matrix utk tampung data_adjust 
data_adjust <- matrix(
  0,
  nrow = nrow(data_seeds_notLabel),
  ncol = ncol(data_seeds_notLabel),
  byrow = TRUE
)

#mencari data adjust, kurangin dengan mean
for(j in 1:ncol(data_seeds_notLabel)){ 
  for(i in 1 : nrow(data_seeds_notLabel)){ 
    data_adjust[i,j] = data_seeds_notLabel[i,j] - mean(data_seeds_notLabel[,j])
  }
}
View(data_adjust)
head(data_adjust)
#--------------------------------------------

#Menghitung Covariance eigen value dan eigen vector
#--------------------------------------------
#cov
cov_seeds <- cov(data_adjust)

#eigen value dan vector
eigen_seeds <- eigen(cov_seeds)
eigen_seeds$values
eigen_seeds$vectors
#-------------------------------------------

# Visualisasi
#-------------------------------------------------
feature_vec = t(eigen_seeds$vectors[,1:3])
row_data_adj = t(data_adjust)
final_data = data.frame(t(feature_vec %*% row_data_adj))
names(final_data) = c('PC1','PC2','PC3')
final_data$Y <- data_seeds[,8]
head(final_data)

#PC1      PC2        PC3 Y
#1  0.6634484 1.417321 0.04123565 1
#2  0.3156665 2.689229 0.23172695 1
#3 -0.6604993 1.131506 0.52708723 1
#4 -1.0552759 1.621190 0.43701526 1
#5  1.6199992 2.183384 0.33399092 1
#6 -0.4769380 1.336494 0.35536061 1

#Visualisasi 1 Dimensi
feature_vec_1d = t(eigen_seeds$vectors[,1:1])
final_D1 <- data.frame(t(feature_vec_1d %*% row_data_adj))
names(final_D1) = c('PC1')
plot(final_D1$PC1, asp=T, col = final_data$Y, xlab='PCA 1', pch=19)

#Visualisasi 2 Dimensi
feature_vec_2d = t(eigen_seeds$vectors[,1:2])
final_D2 <- data.frame(t(feature_vec_2d %*% row_data_adj))
names(final_D2) = c('PC1','PC2')
plot(final_D2$PC1, final_D2$PC2, asp=T, col = final_data$Y, xlab='PCA 1', ylab='PCA 2', pch=19)

#Visualisasi 3 Dimensi
#install.packages("plot3D")
library("plot3D")
scatter3D(final_data$PC1, final_data$PC2, final_data$PC3, bty = "g", pch = 19, col = final_data$Y, xlab='PCA 1', ylab='PCA 2', zlab='PCA 3')

my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(final_data[,1:3], pch = 19,  cex = 0.5,
      col = my_cols[final_data$Y],
      lower.panel=NULL)


variasi_data <- c()
for( i in 1 : 7){
  variasi_data[i] <- (eigen_seeds$values[i] / sum(eigen_seeds$values))
}
print(variasi_data)


sum_var = 0.0
for ( i in 1 : 7){
  sum_var = sum_var + variasi_data[i]
  print(sum_var)
}
#[1] 0.8293852
#[1] 0.9930176
#[1] 0.9986756
#[1] 0.9996659
#[1] 0.999877
#[1] 0.9999977
#[1] 1


