#For practicing coding for RNA_seq analysis
install.packages("rlang")
install.packages("vctrs")
install.packages("Seurat")
install.packages("Matrix")
library("Seurat")
library(dplyr)
library(patchwork)
library("Matrix")
library(ggplot2)
scrna_seq<-readRDS("C:/Users/kragenma/OneDrive - The University of Colorado Denver/Desktop/r_projects/haverkos_lab/bh_full_totalVI_scVI_20220311_v1.Rds")

head(scrna_seq)

View(scrna_seq@metadata)

#normalize data
patient<-NormalizeData(scrna_seq, normalization.method = "LogNormalize")
# Identify variable genes
patient<- FindVariableFeatures(scrna_seq,selection.method="vst",nfeatures=2000)

top10<-head(VariableFeatures(patient),10)

# FeatureScatter is typically used to visualize feature-feature relationships, but can be used
# for anything calculated by the object, i.e. columns in object metadata, PC scores etc.

plot1 <- FeatureScatter(patient, feature1 = "nCount_RNA", feature2 = "percent.mt")
plot2 <- FeatureScatter(patient, feature1 = "nCount_RNA", feature2 = "nFeature_RNA")
plot1 + plot2

# Visualize QC metrics as a violin plot
VlnPlot(patient, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3)

# Scale the data
patient<- ScaleData(patient)

# Run PCA
patient <- RunPCA(patient)

# Examine and visualize PCA results a few different ways
print(patient[["pca"]], dims = 1:5, nfeatures = 5)

VizDimLoadings(patient, dims = 1:5, reduction = "pca")

DimPlot(patient, reduction = "pca")

DimHeatmap(patient, dims = 1, cells = 500, balanced = TRUE)

DimHeatmap(patient, dims = 1:5, cells = 500, balanced = TRUE)

# NOTE: This process can take a long time for big datasets, comment out for expediency. More
# approximate techniques such as those implemented in ElbowPlot() can be used to reduce
# computation time
patient <- JackStraw(patient, num.replicate = 100)
patient <- ScoreJackStraw(patient, dims = 1:20)

JackStrawPlot(pbmc, dims = 1:15)

ElbowPlot(patient)

patient <- FindNeighbors(patient, dims = 1:15)
patient <- FindClusters(patient, resolution = 0.1)

# If you haven't installed UMAP, you can do so via reticulate::py_install(packages =
# 'umap-learn')
patient <- RunUMAP(patient, dims = 1:15)

DimPlot(patient, reduction="umap")

# find all markers of cluster 2
cluster2.markers <- FindMarkers(patient, ident.1 = 6, min.pct = 0.25)
head(cluster2.markers, n = 5)

# find all markers distinguishing cluster 5 from clusters 0 and 3
cluster5.markers <- FindMarkers(patient, ident.1 = 0, ident.2 = c(2, 8), min.pct = 0.25)
head(cluster5.markers, n = 5)

# find markers for every cluster compared to all remaining cells, report only the positive
# ones
pbmc.markers <- FindAllMarkers(patient, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)
pbmc.markers %>%
  group_by(cluster) %>%
  slice_max(n = 2, order_by = avg_log2FC)

VlnPlot(patient, features = top10)

FeaturePlot(patient, features = c("LTB","LEPROTL1","THBS1","CTSS","GNLY","CCL5","MPO","HBB","MS4A1","CD79A"))

FeaturePlot(patient,features=top10)

pbmc.markers %>%
  group_by(cluster) %>%
  top_n(n = 10, wt = avg_log2FC) -> top10
DoHeatmap(patient, features = top10$gene)











