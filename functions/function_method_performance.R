#' Calculating p-values for various methods
#'
#' The p-values for an RCT dataset with an ordinal response variable are calculated for various methods.
#' The methods include the chi-squared test with and without correction, the Mann-Whitney test, the Fisher's exact test for count data, the Cochran-Armitage test, Stuarts tau_c, a dichotomised logistic regression model and the proportional odds model. 
#'
#' @param data A data.frame containing the ordinal response variable and the allocation variable.
#'
#' @return A data.table object containing the methods p-values for a given dataset.
#' @export
#'
#' @examples
#' method_performance(data_generation(n = 30, K = 3, param = "uniform"))
#' 


method_performance <- function(data) {
  
  # Packages -----
  if (!require('coin')) {
    install.packages('coin')
    library('coin')
  }
  if (!require('data.table')) {
    install.packages('data.table')
    library('data.table')
  }
  if (!require('DescTools')) {
    install.packages('DescTools')
    library('DescTools')
  }
  if (!require('rms')) {
    install.packages('rms')
    library('rms')
  }
  
  # Preliminary data.table -----
  methods <-
    c(
      "Chi^2",
      "Chi^2_corrected",
      "Mann_Whitney",
      "Fisher",
      "Stuarts_C",
      "Cochran_Armitage",
      "Independence_Mann_Whitney_exact",
      "Independence_Cochran_Mantel_Haenszel",
      "Dich_Logistic",
      "Prop_Odds"
    )
  
  performance <- data.table(method = methods,
                            p_value = numeric())
  
  # Preliminary Check -----
  if (length(levels(as.factor(data$y))) == 1) {
    warning("Outcome variable has only one value/category!")
    return(performance)
  } else {
    
    # Chi^2 Test -----
    performance[method == "Chi^2",
                p_value := chisq.test(data$y, data$treat_control, correct = FALSE)$p.value]
    
    performance[method == "Chi^2_corrected",
                p_value := chisq.test(data$y, data$treat_control, correct = TRUE)$p.value]
    
    # Mann-Whitney Test -----
    performance[method == "Mann_Whitney",
                p_value :=  wilcox.test(y ~ treat_control, data = data)$p.value]
    
    # Fisher Test -----
    performance[method == "Fisher",
                p_value := fisher.test(data$y, data$treat_control, simulate.p.value = TRUE)$p.value]
    
    # Kendall's tau-c -----
    kendalls_c <-
      DescTools::StuartTauC(data$treat_control, data$y, conf.level = 0.95)
    kendalls_se <-
      abs(kendalls_c[3] - kendalls_c[1]) / abs(qnorm((1 - 0.95) / 2))
    kendalls_p <-
      2 * pnorm(
        abs(kendalls_c[1]),
        mean = 0,
        sd = kendalls_se,
        lower.tail = FALSE
      )
    
    performance[method == "Stuarts_C",
                p_value := kendalls_p]
    
    # Cochran Armitage Test -----
    performance[method == "Cochran_Armitage",
                p_value := prop.trend.test(table(data)["1",], colSums(table(data)))$p.value]
    
    
    # Independence Tests - coin package -----
    performance[method == "Independence_Mann_Whitney_exact",
                p_value := pvalue(
                  coin::independence_test(
                    y ~ as.factor(treat_control),
                    data = data,
                    ytrafo = rank_trafo,
                    distribution = "exact"
                  )
                )]
    performance[method == "Independence_Cochran_Mantel_Haenszel",
                p_value := pvalue(coin::independence_test(y ~ treat_control,
                                                          data = data,
                                                          teststat = "quad"))]
    
    
    # Dichotmoized Logistic Model -----
    k_level <- levels(as.factor(data$y))
    dich <- list()
    dich_log <- list()
    dich_log_p_value <- c()
    for (i in 2:length(k_level)) {
      dich[[i - 1]] <- data
      dich[[i - 1]]$y_dich <- ifelse(data$y < i, 0, 1)
      dich_log[[i - 1]] <- glm(y_dich ~ treat_control,
                               family = binomial(link = "logit"),
                               data = dich[[i - 1]])
      #print(summary(dich_log[[i-1]]))
      dich_log_p_value[i - 1] <-
        summary(dich_log[[i - 1]])$coefficients[2, 4]
    }
    dich_log_p_value <-
      p.adjust(dich_log_p_value, method = "bonferroni")
    
    performance[method == "Dich_Logistic",
                p_value := min(dich_log_p_value)]
    
    
    # Logistic Regression Model (Prop_Odds) using lrm() -----
    performance[method == "Prop_Odds",
                p_value := rms::lrm(y ~ treat_control, data = data)$stats["P"]]
    
    
    # Return -----
    rm(
      kendalls_c,
      kendalls_se,
      kendalls_p,
      i,
      k_level,
      dich,
      dich_log,
      dich_log_p_value,
      methods
    )
    
    return(performance)
  }
}
