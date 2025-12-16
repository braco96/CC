# ==============================================================================
# PROYECTO: CC (Correlación Canónica)
# AUTOR: Luis Bravo Collado (Braco96)
# ==============================================================================
if(!require(pacman)) install.packages("pacman")
pacman::p_load(CCA, CCP, install=FALSE)

print("=== PARTE 1: CASO PRÁCTICO (Iris) ===")
data(iris)
X <- iris[, 1:2]; Y <- iris[, 3:4]
cca_res <- cc(X, Y)

print("Correlaciones:"); print(cca_res$cor)
print("Coeficientes X:"); print(cca_res$xcoef)
print("Test Wilks:"); p.asym(cca_res$cor, nrow(iris), 2, 2)

# Visualización
U <- as.matrix(X) %*% cca_res$xcoef; V <- as.matrix(Y) %*% cca_res$ycoef
plot(U[,1], V[,1], main="Primer Par Canónico", pch=19, col="blue")

print("=== PARTE 2: ANEXO TEÓRICO (Caprinos Simulado) ===")
set.seed(123)
Y_sim <- matrix(rnorm(100, 100, 10), 50, 2); colnames(Y_sim) <- c("Leche", "Nitro")
X_sim <- matrix(rnorm(100, 300, 20), 50, 2) + Y_sim
cc_sim <- cc(X_sim, Y_sim)
if(require(CCA)) plt.cc(cc_sim, var.label=TRUE)
