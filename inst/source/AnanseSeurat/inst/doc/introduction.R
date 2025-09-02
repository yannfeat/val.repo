## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,
                      comment = "#>")
knitr::opts_chunk$set(fig.width = 7, fig.height = 5)

## ----setup, eval = FALSE------------------------------------------------------
#  # if (!requireNamespace("remotes", quietly = TRUE)) {
#  #   install.packages("remotes")
#  # }
#  # Sys.unsetenv("GITHUB_PAT")
#  # remotes::install_github("JGASmits/AnanseSeurat")
#  
#  library(AnanseSeurat)
#  library(Seurat)
#  library(Signac)

## ----load_scObject, eval = FALSE----------------------------------------------
#  rds_file <- 'preprocessed_PDMC.Rds'
#  pbmc <- readRDS(rds_file)
#  DimPlot(pbmc,
#          label = TRUE,
#          repel = TRUE,
#          reduction = "umap") + NoLegend()

## ----export_CPMs, eval = FALSE------------------------------------------------
#  export_CPM_scANANSE(
#    pbmc,
#    min_cells <- 25,
#    output_dir = paste0(tempdir(),'/analysis'),
#    cluster_id = 'predicted.id',
#    RNA_count_assay = 'RNA'
#  )

## ----eval = FALSE-------------------------------------------------------------
#  export_ATAC_scANANSE(
#    pbmc,
#    min_cells <- 25,
#    output_dir = paste0(tempdir(),'/analysis'),
#    cluster_id = 'predicted.id',
#    ATAC_peak_assay = 'peaks'
#  )

## ----eval = FALSE-------------------------------------------------------------
#  contrasts <-  list('B-naive_B-memory',
#                     'B-memory_B-naive',
#                     'B-naive_CD16-Mono',
#                     'CD16-Mono_B-naive')
#  
#  config_scANANSE(
#    pbmc,
#    min_cells <- 25,
#    output_dir = paste0(tempdir(),'/analysis'),
#    cluster_id = 'predicted.id',
#    genome = './data/hg38',
#    additional_contrasts = contrasts
#  )

## ----eval = FALSE-------------------------------------------------------------
#  DEGS_scANANSE(
#    pbmc,
#    min_cells <- 25,
#    output_dir = './analysis',
#    cluster_id = 'predicted.id',
#    additional_contrasts = contrasts
#  )

## ----eval = FALSE-------------------------------------------------------------
#  pbmc <- import_seurat_scANANSE(pbmc,
#                                 cluster_id = 'predicted.id',
#                                 anansnake_inf_dir = "./analysis/influence/")

## ----eval = FALSE-------------------------------------------------------------
#  TF_influence <- per_cluster_df(pbmc,
#                                 assay = 'influence',
#                                 cluster_id = 'predicted.id')
#  
#  head(TF_influence)

## ----eval = FALSE-------------------------------------------------------------
#  highlight_TF1 <- c('STAT4', 'MEF2C')
#  
#  DefaultAssay(object = pbmc) <- "RNA"
#  plot_expression1 <-
#    FeaturePlot(pbmc, features = highlight_TF1, ncol = 1)
#  DefaultAssay(object = pbmc) <- "influence"
#  plot_ANANSE1 <-
#    FeaturePlot(
#      pbmc,
#      ncol = 1,
#      features = highlight_TF1,
#      cols = c("darkgrey", "#fc8d59")
#    )
#  print(plot_expression1 | plot_ANANSE1)

