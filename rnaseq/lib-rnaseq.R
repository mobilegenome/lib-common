" Calculate average expression across a Seurat object for a set of gene symbols. 
Returns a tibble.
"
calcAvgGenesetExpressionZScore.Seurat <- function(object, geneset, scorename="gene.set.score") {
  require(Seurat)
  require(tidyverse)
  
  genes_selected <- intersect(rownames(seu),  geneset)
  
  selected_assay <- GetAssayData(object, slot = "scale.data")[which(rownames(object) %in% genes_selected), ]
  avg_values <- selected_assay %>% as_tibble() %>% colMeans() 
  
  # 
  # # store expression means 
  # # according to https://github.com/satijalab/seurat/issues/528
  # if (all(names(x = avg_values) == rownames(x = object@meta.data))) {
  #   cat("Cell names order match in 'mean.exp' and 'object@meta.data':\n", 
  #       "adding gene set mean expression values in 'object@meta.data$gene.set.score'")
  #   object@meta.data[scorename] <- avg_values
  # }
  avg.exp.df <- tibble(cellID = names(avg_values))
  avg.exp.df[[scorename]] = unlist(avg_values)
  
  umap_avgExpr_df <- Embeddings(object, "umap") %>% as_tibble(rownames = "cellID")
  umap_avgExpr_df <- left_join(umap_avgExpr_df, avg.exp.df)
  umap_avgExpr_df[[paste0(scorename, "_Zscore")]] <- scale(umap_avgExpr_df[[scorename]])
  
  return(umap_avgExpr_df)
}


"  Calculate the z-score for a geneset per cell in a Seurat object. 
Return a tibble with columns 'cellID', 'geneSymbol', Zscore' and 'genset'.
"
calcZscore.Seurat <- function(object, geneset, geneset_label, celllabel_prefix = "HNSCC") {
  genes_selected <- intersect(rownames(object),  geneset)
  
  selected_assay <- GetAssayData(object, slot = "scale.data")[which(rownames(object) %in% genes_selected), ]
  
  zscores <- 
    scale(t(selected_assay)) %>%
    t() %>% 
    as_tibble(rownames = "geneSymbol") %>%
    pivot_longer(cols = matches(celllabel_prefix), names_to = "cellID", values_to = "Zscore") %>% 
    mutate(cellID = reorder(cellID, Zscore)) %>%
    mutate(geneset = geneset_label) %>% 
    group_by(cellID, geneSymbol) %>% 
    arrange(Zscore) %>% 
    ungroup() 
  
  return(zscores)
  
}