# ============================================================
# STATISTICS FOR HIGH DIMENSIONAL DATA — WDI PROJECT
# ============================================================

library(readxl)
library(cluster)
library(corrplot)
library(ggplot2)
library(plotly)

rm(list = ls())
setwd("/Users/matteoseveri/Desktop/University/LMEC/High Dimensional Data/WDI")


# ============================================================
# DATA LOADING & DESCRIPTIVES
# ============================================================

data <- data.frame(read_xlsx("WDI1993EXCEL.xlsx"))
rownames(data) <- data$Country
data$Country <- NULL

str(data)
dim(data)
summary(data)
sapply(data, sd)

data_summary <- data.frame(
  Mean = sapply(data, mean),
  SD   = sapply(data, sd),
  Min  = sapply(data, min),
  Max  = sapply(data, max)
)
print(data_summary)

any(is.na(data))          # no missing values
data[duplicated(data), ]  # no duplicated rows

data.c <- data[33:132, ]  # countries only
data.g <- data[1:32, ]    # groups only


# ============================================================
# RAW DISTRIBUTIONS
# ============================================================

vars <- c("Carbon", "Consume", "Education", "Energy", "Gross", "Mineral", "Forest", "Particulate")
for (v in vars) {
  hist(data.c[[v]], main = paste(v, "distribution"), xlab = v, breaks = 40, col = "lightblue")
}


# ============================================================
# 3. TRANSFORMATIONS
# ============================================================

# --- Standardized only (no log) ---
data.c_sc <- as.data.frame(scale(data.c))
colnames(data.c_sc) <- paste0("sc_", colnames(data.c_sc))

# --- Log only (skewed vars, no Gross/Consume) ---
skewed_vars <- c("Carbon", "Education", "Energy", "Mineral", "Forest", "Particulate")
data.c_log <- data.c
for (v in skewed_vars) data.c_log[[paste0("log_", v)]] <- log(data.c[[v]] + 1)
data.c_log <- data.c_log[, !colnames(data.c_log) %in% skewed_vars]

# --- Log + standardized ---
data.c_lsc <- as.data.frame(scale(data.c_log))
colnames(data.c_lsc) <- sub("^log_", "lsc_", colnames(data.c_lsc))
colnames(data.c_lsc) <- sub("^Gross$", "sc_Gross", colnames(data.c_lsc))
colnames(data.c_lsc) <- sub("^Consume$", "sc_Consume", colnames(data.c_lsc))

for (v in paste0("lsc_", c("Carbon","Education","Energy","Mineral","Forest","Particulate"))) {
  hist(data.c_lsc[[v]], main = paste(v, "distribution"), xlab = v, breaks = 40, col = "lightblue")
}
hist(data.c_lsc$sc_Consume, main = "sc_Consume distribution", xlab = "sc_Consume", breaks = 40, col = "lightblue")
hist(data.c_lsc$sc_Gross,   main = "sc_Gross distribution",   xlab = "sc_Gross",   breaks = 40, col = "lightblue")

# --- Correlation matrix ---
cor_matrix_lsc <- cor(data.c_lsc, use = "complete.obs")
print(cor_matrix_lsc)

corrplot(cor_matrix_lsc,
         method = "color", type = "upper",
         col = colorRampPalette(c("#B2182B","#FEE0B6","#FFFFFF","#BFD3E6","#2166AC"))(200),
         addCoef.col = "black", number.cex = 0.7,
         tl.cex = 0.9, tl.col = "black", tl.srt = 45,
         mar = c(0,0,1,0), cl.cex = 0.7
)


# ============================================================
# APPENDIX — WHY Gross AND Consume ARE NOT LOG-TRANSFORMED
# ============================================================

shapiro.test(data.c_lsc$sc_Gross)    # p < 0.05, not normal
shapiro.test(data.c_lsc$sc_Consume)  # p < 0.05, not normal

data.c_lsc$lsc_Consume <- scale(log(data.c$Consume + 1))
data.c_lsc$lsc_Gross   <- scale(log(data.c$Gross   + 1))
data.c_lsc[is.na(data.c_lsc)] <- 0   # one NaN in lsc_Gross -> 0

shapiro.test(data.c_lsc$lsc_Gross)    # still p < 0.05
shapiro.test(data.c_lsc$lsc_Consume)  # still p < 0.05

# Visual comparison: log doesn't help
par(mfrow = c(2, 2))
hist(data.c_lsc$sc_Gross,  main = "sc_Gross",  xlab = "sc_Gross",  breaks = 40, col = "lightblue")
qqnorm(data.c_lsc$sc_Gross);  qqline(data.c_lsc$sc_Gross,  col = "red")
hist(data.c_lsc$lsc_Gross, main = "lsc_Gross", xlab = "lsc_Gross", breaks = 40, col = "lightblue")
qqnorm(data.c_lsc$lsc_Gross); qqline(data.c_lsc$lsc_Gross, col = "red")

par(mfrow = c(2, 2))
hist(data.c_lsc$sc_Consume,  main = "sc_Consume",  xlab = "sc_Consume",  breaks = 40, col = "lightblue")
qqnorm(data.c_lsc$sc_Consume);  qqline(data.c_lsc$sc_Consume,  col = "red")
hist(data.c_lsc$lsc_Consume, main = "lsc_Consume", xlab = "lsc_Consume", breaks = 40, col = "lightblue")
qqnorm(data.c_lsc$lsc_Consume); qqline(data.c_lsc$lsc_Consume, col = "red")

# Drop the log-scaled versions; not used going forward
data.c_lsc <- subset(data.c_lsc, select = -c(lsc_Consume, lsc_Gross))
par(mfrow = c(1, 1))


# ============================================================
# OUTLIER DETECTION (exploratory, not applied)
# ============================================================

center     <- colMeans(data.c_lsc)
cov_matrix <- cov(data.c_lsc)
md         <- mahalanobis(data.c_lsc, center, cov_matrix)
threshold  <- qchisq(0.975, df = ncol(data.c_lsc))
outliers   <- md > threshold

data.c_lsc[outliers, ]
data.c_lscnoextreme <- data.c_lsc[!outliers, ]

outlier_countries <- rownames(data.c_lsc[outliers, ])
data.c_noextreme  <- data.c[!(rownames(data.c) %in% outlier_countries), ]


# ============================================================
# PCA (WITHOUT EDUCATION)
# ============================================================

data.c_lsc_noeduc <- data.c_lsc[, colnames(data.c_lsc) != "lsc_Education"]
output_pca_noeduc <- prcomp(data.c_lsc_noeduc, center = TRUE)
summary(output_pca_noeduc)

lambda         <- round(output_pca_noeduc$sdev^2, 3)
cumvar_noeduc  <- round(cumsum(lambda / sum(lambda)), 3)
lambda
cumvar_noeduc

screeplot(output_pca_noeduc, type = "lines", main = "Scree Plot — PCA without lsc_Education")

# Component loadings
D       <- diag(lambda)
C       <- as.matrix(output_pca_noeduc$rotation)
loadings <- C %*% sqrt(D)
round(loadings, 3)

# Standardized scores
U <- sweep(output_pca_noeduc$x, 2, output_pca_noeduc$sdev, FUN = "/")
colnames(U) <- paste0("PC", 1:ncol(U), "_std")
round(apply(U[, 1:3], 2, mean), 3)
round(apply(U[, 1:3], 2, sd),   3)

# Apply rotation (identity here, kept for flexibility)
output_pca_mod           <- output_pca_noeduc
output_pca_mod$rotation  <- output_pca_noeduc$rotation
Z                        <- scale(data.c_lsc_noeduc, center = TRUE, scale = TRUE)
output_pca_mod$x         <- Z %*% output_pca_mod$rotation

U <- sweep(output_pca_mod$x, 2, output_pca_mod$sdev, FUN = "/")
colnames(U) <- paste0("PC", 1:ncol(U), "_std")

# Correlation matrix: variables vs components
H <- t(cor(Z, output_pca_mod$x)[, 1:3])
colnames(H) <- colnames(data.c_lsc_noeduc)

ucircle <- cbind(cos((0:360) / 180 * pi), sin((0:360) / 180 * pi))

# --- Score plots ---
for (pair in list(c(1,2), c(1,3), c(2,3))) {
  plot(U[, pair], main = paste("Standardized Scores — PC", pair[1], "vs PC", pair[2]),
       type = "n", xlab = paste0("PC", pair[1]), ylab = paste0("PC", pair[2]))
  text(U[, pair], rownames(Z), cex = 0.7)
  abline(h = 0, v = 0, col = "gray", lty = 3)
  polygon(ucircle, lty = "dashed", border = "gray", lwd = 1)
}

# --- Variable plots ---
for (pair in list(c(1,2), c(1,3), c(2,3))) {
  plot(t(H[pair, ]), main = paste("Variables — PC", pair[1], "vs PC", pair[2]),
       type = "n", xlim = c(-1,1), ylim = c(-1,1),
       xlab = paste0("PC", pair[1]), ylab = paste0("PC", pair[2]))
  text(t(H[pair, ]), colnames(Z), cex = 0.8)
  arrows(0, 0, t(H)[, pair[1]], t(H)[, pair[2]], length = 0.1, col = "black")
  abline(h = 0, v = 0, col = "gray", lty = 3)
}

# --- Biplots ---
for (pair in list(c(1,2), c(1,3), c(2,3))) {
  biplot(x = U[, pair], y = t(H[pair, ]), cex = c(.7,.7), col = c("black","red"),
         expand = 0.5, xlab = paste0("PC", pair[1]), ylab = paste0("PC", pair[2]),
         xlim = c(-3,3), ylim = c(-3,3))
  polygon(ucircle, lty = "dashed", border = "red", lwd = 1)
}

# Variance explained by variables in each plane
apply(H[1:2, ]^2, 2, sum)  # PC1 + PC2
apply(H[1:3, ]^2, 2, sum)  # PC1 + PC2 + PC3


# ============================================================
# CLUSTER ANALYSIS (WITHOUT EDUCATION)
# ============================================================

distance <- dist(data.c_lsc_noeduc, method = "manhattan")

# --- Hierarchical: complete linkage (for reference — unbalanced) ---
hc.ce        <- hclust(distance, method = "complete")
member.ce.3  <- cutree(hc.ce, k = 3)
table(member.ce.3)

# --- Hierarchical: Ward ---
hc.w <- hclust(distance, method = "ward.D")
plot(hc.w)

h3 <- hc.w$height[length(hc.w$height) - 2]
h4 <- hc.w$height[length(hc.w$height) - 3]
abline(h = h3 + 0.03 * diff(range(hc.w$height)), col = "red",  lty = 2)
abline(h = h4 + 0.05 * diff(range(hc.w$height)), col = "blue", lty = 2)

member   <- data.frame(cutree(hc.w, k = c(3, 4, 5, 6, 7, 8, 9)))
member.w.3 <- cutree(hc.w, k = 3)
member.w.4 <- cutree(hc.w, k = 4)
table(member.w.3)
table(member.w.4)

# --- Silhouette ---
sil <- sapply(2:10, function(k) {
  km <- kmeans(data.c_lsc_noeduc, centers = k, nstart = 50)
  mean(silhouette(km$cluster, dist(data.c_lsc_noeduc))[, 3])
})
plot(2:10, sil, type = "b", pch = 19, xlab = "k", ylab = "Average silhouette width")

# --- K-means ---
set.seed(1234)
kc.3 <- kmeans(data.c_lsc_noeduc, 3, nstart = 50)
kc.4 <- kmeans(data.c_lsc_noeduc, 4, nstart = 50)
table(kc.3$cluster)
table(kc.4$cluster)

# WSS elbow
wss <- sapply(1:10, function(k) kmeans(data.c_lsc_noeduc, centers = k, nstart = 50)$tot.withinss)
plot(1:10, wss, type = "b", pch = 19, xlab = "Number of clusters", ylab = "Within-cluster SS")

# BSS/TSS
tss     <- sum(scale(data.c_lsc_noeduc, center = TRUE, scale = FALSE)^2)
bss_tss <- 1 - wss / tss
plot(1:10, bss_tss, type = "b", pch = 19, xlab = "Number of clusters", ylab = "Explained Variance (BSS/TSS)")


# ============================================================
# CLUSTER COMPARISON — WARD VS K-MEANS (k = 4)
# ============================================================

# Re-label clusters by descending size for comparability
relabel_by_size <- function(cluster_vec, row_names) {
  sizes   <- table(cluster_vec)
  mapping <- order(sizes, decreasing = TRUE)
  data.frame(row.names = row_names, Cluster = match(cluster_vec, mapping))
}

member.kc.4 <- relabel_by_size(kc.4$cluster, rownames(data.c_lsc_noeduc))
member.w.4  <- relabel_by_size(member.w.4,    rownames(data.c_lsc_noeduc))

table(member.w.4$Cluster)
table(member.kc.4$Cluster)
table(as.matrix(member.w.4), as.matrix(member.kc.4))  # cross-tab

aggregate(data.c_lsc_noeduc, list(member.w.4$Cluster),  mean)
aggregate(data.c_lsc_noeduc, list(member.kc.4$Cluster), mean)

pcs          <- output_pca_noeduc$x
pc_means.kc4 <- aggregate(pcs, list(Cluster = member.kc.4$Cluster), mean)
pc_means.w4  <- aggregate(pcs, list(Cluster = member.w.4$Cluster),  mean)

# Final cluster membership (k-means k=4)
cluster_membership <- data.frame(Country = rownames(data.c_lsc_noeduc), Cluster = kc.4$cluster)
cluster_membership <- cluster_membership[order(cluster_membership$Cluster), ]
View(cluster_membership)


# ============================================================
# CLUSTER INTERPRETATION — K-MEANS (k = 4)
# ============================================================

aggregate(data.c_lsc_noeduc, list(kc.4$cluster), mean)
aggregate(data.c_lsc_noeduc, list(kc.3$cluster), mean)

# Cluster 1 (n ≈ 48): Low environmental damage, modest savings — balanced/sustainable
# Cluster 2 (n ≈ 33): High particulate, weak savings — urbanizing/pollution-exposed
# Cluster 3 (n ≈ 15): High energy & carbon, strong savings — industrialized/fossil-fuel-heavy
# Cluster 4 (n =   4): Extreme mineral & forest depletion — resource-export economies


# ============================================================
# VISUALIZATIONS — PRO PLOTS
# ============================================================

cluster_colors <- c("1" = "#F8766D", "2" = "#7CAE00", "3" = "#00BFC4", "4" = "#C77CFF")
cluster_levels <- c("1","2","3","4")

theme_paper <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(legend.position = "top", legend.title = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title    = element_text(face = "bold"),
          plot.subtitle = element_text(color = "grey30"))
}

pca_df <- data.frame(
  country = rownames(data.c_lsc_noeduc),
  PC1 = output_pca_noeduc$x[, 1],
  PC2 = output_pca_noeduc$x[, 2],
  PC3 = output_pca_noeduc$x[, 3],
  km4 = factor(kc.4$cluster, levels = 1:4, labels = cluster_levels)
)

# Convex hull helper
make_hulls_xy <- function(df, xvar, yvar, group = "km4") {
  pieces <- lapply(split(df, df[[group]]), function(d) {
    if (nrow(d) < 3) return(NULL)
    d[chull(d[[xvar]], d[[yvar]]), c(xvar, yvar, group)]
  })
  res <- do.call(rbind, Filter(Negate(is.null), pieces))
  if (!is.null(res)) res[[group]] <- factor(res[[group]], levels = levels(df[[group]]))
  res
}

hulls_12 <- make_hulls_xy(pca_df, "PC1", "PC2")
hulls_13 <- make_hulls_xy(pca_df, "PC1", "PC3")
cent_12  <- aggregate(cbind(PC1, PC2) ~ km4, pca_df, mean)
cent_13  <- aggregate(cbind(PC1, PC3) ~ km4, pca_df, mean)

# --- PC1 vs PC2: hull ---
p_12_hull <- ggplot(pca_df, aes(PC1, PC2, color = km4)) +
  geom_polygon(data = hulls_12, aes(PC1, PC2, fill = km4, group = km4),
               inherit.aes = FALSE, alpha = .10, colour = NA) +
  geom_point(size = 3, alpha = .9) +
  ggrepel::geom_text_repel(aes(label = country), size = 3, seed = 1234, show.legend = FALSE) +
  scale_color_manual(values = cluster_colors, breaks = cluster_levels, drop = FALSE) +
  scale_fill_manual(values  = cluster_colors, breaks = cluster_levels, drop = FALSE) +
  labs(title = "K-means (k=4) on PCA space", subtitle = "Convex hull", x = "PC1", y = "PC2") +
  theme_paper()

# --- PC1 vs PC2: ellipses ---
p_12_ell <- ggplot(pca_df, aes(PC1, PC2, color = km4)) +
  geom_point(size = 3, alpha = .9) +
  stat_ellipse(aes(fill = km4), type = "norm", level = .80,
               alpha = .12, geom = "polygon", colour = NA, show.legend = FALSE) +
  geom_point(data = cent_12, aes(PC1, PC2), inherit.aes = FALSE,
             shape = 21, size = 4, colour = "black", fill = "white", stroke = .5) +
  ggrepel::geom_text_repel(aes(label = country), size = 3, seed = 1234, show.legend = FALSE) +
  scale_color_manual(values = cluster_colors, breaks = cluster_levels, drop = FALSE) +
  scale_fill_manual(values  = cluster_colors, breaks = cluster_levels, drop = FALSE) +
  labs(title = "K-means (k=4) on PCA space", subtitle = "80% ellipses + centroids", x = "PC1", y = "PC2") +
  theme_paper()

# --- PC1 vs PC3: hull ---
p_13_hull <- ggplot(pca_df, aes(PC1, PC3, color = km4)) +
  geom_polygon(data = hulls_13, aes(PC1, PC3, fill = km4, group = km4),
               inherit.aes = FALSE, alpha = .10, colour = NA) +
  geom_point(size = 3, alpha = .9) +
  ggrepel::geom_text_repel(aes(label = country), size = 3, seed = 1234, show.legend = FALSE) +
  scale_color_manual(values = cluster_colors, breaks = cluster_levels, drop = FALSE) +
  scale_fill_manual(values  = cluster_colors, breaks = cluster_levels, drop = FALSE) +
  labs(title = "K-means (k=4): PC1 vs PC3", subtitle = "Convex hull", x = "PC1", y = "PC3") +
  theme_paper()

# --- PC1 vs PC3: ellipses ---
p_13_ell <- ggplot(pca_df, aes(PC1, PC3, color = km4)) +
  geom_point(size = 3, alpha = .9) +
  stat_ellipse(aes(fill = km4), type = "norm", level = .80,
               alpha = .12, geom = "polygon", colour = NA, show.legend = FALSE) +
  geom_point(data = cent_13, aes(PC1, PC3), inherit.aes = FALSE,
             shape = 21, size = 4, colour = "black", fill = "white", stroke = .5) +
  ggrepel::geom_text_repel(aes(label = country), size = 3, seed = 1234, show.legend = FALSE) +
  scale_color_manual(values = cluster_colors, breaks = cluster_levels, drop = FALSE) +
  scale_fill_manual(values  = cluster_colors, breaks = cluster_levels, drop = FALSE) +
  labs(title = "K-means (k=4): PC1 vs PC3", subtitle = "80% ellipses + centroids", x = "PC1", y = "PC3") +
  theme_paper()

p_12_hull; p_12_ell; p_13_hull; p_13_ell

# --- Elbow & silhouette (factoextra) ---
p_elbow <- factoextra::fviz_nbclust(data.c_lsc_noeduc, kmeans, method = "wss",       k.max = 10, nstart = 50) + theme_paper()
p_sil   <- factoextra::fviz_nbclust(data.c_lsc_noeduc, kmeans, method = "silhouette", k.max = 10, nstart = 50) + theme_paper()
p_elbow; p_sil

# --- Dendrogram ---
p_dend <- factoextra::fviz_dend(
  hc.w, k = 4,
  k_colors = unname(cluster_colors),
  rect = TRUE, rect_fill = TRUE, cex = .6, color_labels_by_k = TRUE
) + labs(title = "Cluster Dendrogram (ward.D, Manhattan)") + theme_paper()
p_dend

# --- 3D Plotly ---
p3d <- plot_ly(
  pca_df, x = ~PC1, y = ~PC2, z = ~PC3,
  color = ~km4, colors = unname(cluster_colors[cluster_levels]),
  type = "scatter3d", mode = "markers",
  marker = list(size = 4, opacity = .85)
) %>% layout(
  title  = "K-means (k=4) in PCA space",
  legend = list(orientation = "h", x = 0, y = 1.05),
  scene  = list(xaxis = list(title = "PC1"), yaxis = list(title = "PC2"), zaxis = list(title = "PC3"))
)
p3d
