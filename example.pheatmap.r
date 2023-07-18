library(tidyverse)
library(rstatix)
library(see)
library(pheatmap)
rm(list = ls())
# 生成 analysis_data 表格
set.seed(123)  # 设置随机数种子以确保结果可复现

# 创建样本和特征名称
samples <- paste0("S", 1:60)
features <- paste0("F", 1:50)

# 创建分组信息
groups <- rep(c("A", "B", "C"), each = 20)

# 生成数据
data <- matrix(rnorm(60*50), nrow = 50, ncol = 60, dimnames = list(features, samples))

# 创建 analysis_data 表格
analysis_data <- data.frame(data)
rownames(analysis_data) <- features

# 生成 annotation_col 表格
annotation_col <- data.frame(Group = groups)
rownames(annotation_col) <- samples

# 生成 annotation_row 表格
class_types <- rep(paste0("Class", 1:10), length(features)/10)
class_types2 <- rep(paste0("Class", 1:5), length(features)/5)
annotation_row <- data.frame(class_type = class_types,class_types2=class_types2)
rownames(annotation_row) <- features

# 为满足 A、B、C 三者之间具有统计学差异的要求，对数据进行一些修改
# 假设我们将前 20 个样本的数据在 A、B、C 组之间进行加减操作
# 注意：这只是一个示例，具体如何修改数据以满足统计学差异的要求需要根据实际情况进行调整
analysis_data[, groups == "B"] <- analysis_data[, groups == "B"] + 1
analysis_data[, groups == "C"] <- analysis_data[, groups == "C"] - 1


######################################################################
data_arranged=analysis_data  ## in analysis_data, the rowname is feature while colname is sammpleID

# Set color schemes
seeColors <- c(see::metro_colors(), see::social_colors(), see::flat_colors(), see::material_colors(), see::see_colors(), see::pizza_colors())

cols1 <- unname(seeColors)
mycolors1 <- cols1[1:length(unique(annotation_row$class_type))]
names(mycolors1) <- unique(annotation_row$class_type)

cols2 <- unname(seeColors)
mycolors2 <- cols2[1:(length(unique(annotation_row$class_types2)))]
names(mycolors2) <- unique(annotation_row$class_types2)

# Merge two color schemes
mycolors <- list(V2 = mycolors1, V3 = mycolors2)

# Generate heatmap
pdf(file = "KEGG_pathways.pdf", width = 16, height = 16)
pheatmap::pheatmap(
  mat = data_arranged,
  annotation_row = annotation_row,
  annotation_col = annotation_col,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  scale = "row",
  show_colnames = FALSE,
  cellwidth = 3,
  cellheight = 9,
  color = colorRampPalette(c("#0571b0", "#f7f7f7", "#ca0020"))(10),
  gaps_row = head(as.numeric(cumsum(table(annotation_row$class_types2))), -1),
  gaps_col = head(as.numeric(cumsum(table(annotation_col$Group))), -1),
  annotation_colors = c(mycolors, list(Group = c(A = "green", B = "blue", C = "red"))),
  fill = NA
)
dev.off()



