# 输入验证函数

#' 获取有效整数输入
#'
#' @param prompt 提示信息
#' @param min_val 最小值（默认为1）
#' @return 有效的整数值
#' @keywords internal
get_valid_integer <- function(prompt, min_val = 1) {
  while (TRUE) {
    input <- readline(prompt)
    num <- suppressWarnings(as.integer(input))

    if (is.na(num)) {
      cat("错误：请输入一个有效的整数\n")
    } else if (num < min_val) {
      cat(sprintf("错误：请输入大于等于%d的整数\n", min_val))
    } else {
      return(num)
    }
  }
}

#' 获取有效数值向量
#'
#' @param prompt 提示信息
#' @param expected_length 期望的数值长度
#' @return 有效的数值向量
#' @keywords internal
get_valid_numeric_vector <- function(prompt, expected_length) {
  while (TRUE) {
    cat(prompt)
    input <- readline()
    values <- suppressWarnings(as.numeric(unlist(strsplit(input, " "))))

    # 检查输入有效性
    if (length(values) != expected_length) {
      cat(sprintf("错误：需要输入%d个数值，但您输入了%d个。请重新输入。\n", expected_length, length(values)))
      next
    }

    if (any(is.na(values))) {
      cat("错误：输入包含非数值，请确保所有输入都是数字\n")
      next
    }

    return(values)
  }
}

#' 获取有效基因名称
#'
#' @param prompt 提示信息
#' @return 有效的基因名称
#' @keywords internal
get_valid_gene_name <- function(prompt) {
  while (TRUE) {
    name <- readline(prompt)
    if (nchar(trimws(name)) == 0) {
      cat("错误：基因名称不能为空\n")
    } else {
      return(name)
    }
  }
}
