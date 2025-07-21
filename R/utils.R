# 辅助函数

#' 计算qPCR结果
#'
#' @param sample_df 样本数据框
#' @param gene_names 基因名称向量
#' @return 包含结果和摘要的列表
#' @keywords internal
calculate_qpcr_results <- function(sample_df, gene_names) {
  result_df <- sample_df
  ref_means <- numeric(length(gene_names))

  for (i in seq_along(gene_names)) {
    gene <- gene_names[i]
    gene_ct_col <- paste0(gene, "_ct")
    delta_ct_col <- paste0(gene, "_delta_ct")
    mean_delta_ct_col <- paste0(gene, "_mean_delta_ct")
    delta_delta_ct_col <- paste0(gene, "_delta_delta_ct")
    res_col <- paste0(gene, "_res")

    # 计算△CT = gene_ct - control_ct
    result_df[[delta_ct_col]] <- result_df[[gene_ct_col]] - result_df$control_ct

    # 计算参照平均值（第一组样本的△CT平均值）
    first_group_samples <- which(result_df$Group == "Group1")

    if (length(first_group_samples) > 0) {
      ref_mean <- mean(result_df[[delta_ct_col]][first_group_samples])
    } else {
      cat("警告：未找到第一组样本，将使用所有样本的平均值作为参照\n")
      ref_mean <- mean(result_df[[delta_ct_col]])
    }

    ref_means[i] <- ref_mean
    cat(sprintf("基因 %s 的参照平均值 (mean △CT) = %.4f\n", gene, ref_mean))

    # 所有样本使用相同的参照平均值
    result_df[[mean_delta_ct_col]] <- ref_mean

    # 计算△△CT = delta_ct - ref_mean
    result_df[[delta_delta_ct_col]] <- result_df[[delta_ct_col]] - ref_mean

    # 计算res = 2^(-△△CT)
    result_df[[res_col]] <- round(2^(-result_df[[delta_delta_ct_col]]),6)
  }

  # 创建分组统计摘要
  summary_df <- data.frame(Group = unique(result_df$Group))

  for (gene in gene_names) {
    res_col <- paste0(gene, "_res")

    # 计算每个分组的平均值和标准差
    mean_res <- tapply(result_df[[res_col]], result_df$Group, mean)
    sd_res <- tapply(result_df[[res_col]], result_df$Group, sd)

    # 添加到摘要数据框
    summary_df[[paste0(gene, "_mean")]] <- round(mean_res[summary_df$Group], 4)
    summary_df[[paste0(gene, "_sd")]] <- round(sd_res[summary_df$Group], 4)
  }

  list(results = result_df, summary = summary_df)
}
