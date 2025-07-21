#' 交互式qPCR数据分析
#'
#' 该函数提供了一个交互式界面，用于分析qPCR数据。用户将被提示输入分组信息、
#' 样本数量、Ct值等数据，函数将自动计算△CT、△△CT和相对表达量。
#'
#' @return 包含两个元素的列表：
#'   \item{results}{包含所有计算结果的数据框}
#'   \item{summary}{分组统计摘要数据框}
#' @export
#' @examples
#' \dontrun{
#'   # 在交互式会话中运行
#'   analysis_results <- qpcr_analysis_interactive()
#'   print(analysis_results$results)
#'   print(analysis_results$summary)
#' }
qpcr_analysis_interactive <- function() {
  cat("===== qPCR数据分析工具 =====\n")
  cat("注意：请确保输入的所有Ct值数量与相应组的样本数量一致！\n\n")

  # 获取分组数量
  num_groups <- get_valid_integer("请输入分组数量: ", min_val = 1)

  # 存储每组样本数
  samples_per_group <- integer(num_groups)

  # 创建基础数据框存储样本信息
  sample_df <- data.frame(
    Group = character(0),
    Sample = character(0),
    control_ct = numeric(0),
    stringsAsFactors = FALSE
  )

  # 收集样本信息
  for (group_idx in 1:num_groups) {
    cat("\n===== 正在处理分组", group_idx, "=====\n")

    # 获取当前分组的样本数量
    samples_in_group <- get_valid_integer(paste0("请输入分组 ", group_idx, " 的样本数量: "), min_val = 1)
    samples_per_group[group_idx] <- samples_in_group

    # 输入内参基因Ct值（带确认功能）
    control_ct_values <- get_confirmed_values(
      prompt = sprintf("请输入分组%d所有样本的内参基因Ct值 (需输入%d个数值，用空格分隔): ",
                       group_idx, samples_in_group),
      expected_length = samples_in_group,
      value_name = "内参基因Ct值"
    )

    # 创建当前分组的数据
    group_data <- data.frame(
      Group = rep(paste0("Group", group_idx), samples_in_group),
      Sample = paste0("S", 1:samples_in_group),
      control_ct = control_ct_values,
      stringsAsFactors = FALSE
    )

    # 添加到样本数据框
    sample_df <- rbind(sample_df, group_data)
  }

  # 获取基因数量
  num_genes <- get_valid_integer("\n请输入基因数量: ", min_val = 1)

  # 获取基因名称
  gene_names <- character(num_genes)
  for (i in 1:num_genes) {
    gene_names[i] <- get_valid_gene_name(paste0("请输入基因 ", i, " 的名称: "))
  }

  # 为每个基因添加列
  for (gene in gene_names) {
    sample_df[[paste0(gene, "_ct")]] <- NA
  }

  # 收集基因Ct值（带确认功能）
  cat("\n===== 输入基因Ct值 =====\n")
  cat("注意：每组每个基因需要输入与样本数量相同的Ct值！\n")

  for (group_idx in 1:num_groups) {
    samples_in_group <- samples_per_group[group_idx]

    for (gene in gene_names) {
      # 输入基因Ct值（带确认功能）
      gene_ct_values <- get_confirmed_values(
        prompt = sprintf("请输入分组%d基因%s的Ct值 (需输入%d个数值，用空格分隔): ",
                         group_idx, gene, samples_in_group),
        expected_length = samples_in_group,
        value_name = paste0("基因", gene, "的Ct值")
      )

      # 填充数据
      sample_df[sample_df$Group == paste0("Group", group_idx), paste0(gene, "_ct")] <- gene_ct_values
    }
  }

  # 计算qPCR结果
  results <- calculate_qpcr_results(sample_df, gene_names)

  # 显示最终结果
  cat("\n===== 最终结果 =====\n")
  print(results$results, row.names = FALSE)

  # 导出CSV文件
  utils::write.csv(results$results, "gene_expression_results.csv", row.names = FALSE)
  cat("\n结果已保存到 gene_expression_results.csv\n")

  # 显示分组统计摘要
  cat("\n===== 分组统计摘要 =====\n")
  print(results$summary, row.names = FALSE)

  # 计算完成提示
  cat("\n分析完成！所有结果已保存到CSV文件，并可在R环境中查看。\n")

  # 将结果保存到全局环境，便于用户访问
  assign("qpcr_results", results$results, envir = .GlobalEnv)
  assign("qpcr_summary", results$summary, envir = .GlobalEnv)
  cat("结果已保存到当前环境: \n")
  cat(" - qpcr_results: 详细结果数据框\n")
  cat(" - qpcr_summary: 分组统计摘要\n")

  # 返回结果
  invisible(results)
}
