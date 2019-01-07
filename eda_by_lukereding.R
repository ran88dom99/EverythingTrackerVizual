find_type <- function(x) {
  case_when(
    is.factor(x) ~ "factor",
    is.character(x) ~ "character",
    is.numeric(x) ~ "numeric",
    TRUE ~ "not sure"
  )
}

permute_icc <- function(x, y, n = 99) {
  actual <- ICCbare(x, y)
  nulls <- vector(length = length(n), mode = "numeric")
  for(i in seq_along(1:n)) {
    scrambled_x <- sample(x, length(x), replace = F)
    nulls[i] <- ICCbare(scrambled_x, y)
  }
  (sum(abs(nulls) > ifelse(actual > 0, actual, -actual)) + 1) / (n+1)
}

permute_tau <- function(x, y, n = 99) {
  actual <- GKtau(x, y)$tauxy
  nulls <- vector(length = length(n), mode = "numeric")
  for(i in seq_along(1:n)) {
    scrambled_x <- sample(x, length(x), replace = F)
    nulls[i] <- GKtau(scrambled_x, y)$tauxy
  }
  (sum(abs(nulls) > ifelse(actual > 0, actual, -actual)) + 1) / (n+1)
}

# to do:
## get p-values

eda <- function(x, plot = FALSE) {

  x <- as.data.frame(x)

  num_rows <- ncol(x)^2 - ncol(x)
  df <- tibble(var1 = vector(mode = "character", length = 1),
               var2 = vector(mode = "character", length = 1),
               statistic = vector(mode = "character", length = 1),
               value = vector(mode = "double", length = 1),
               p_value = vector(mode = "double", length = 1),
               n = vector(mode = "integer", length = 1))

  for(i in seq_along(1:ncol(x)))
    for(j in seq_along(1:ncol(x))) {
      if(i < j){
        # get type of columns i and j
        var_1_type <- find_type(x[,i])
        var_2_type <- find_type(x[,j])
        #print(paste("var1 type: ", var_1_type, "\nvar2 type: ", var_2_type, "\n\n"))

        x1 <- x[,i]
        x2 <- x[,j]

        # remove NAs for simplicity
        if(any(is.na(x1))){
          # get NA indicies
          ind <- which(is.na(x1))
          x1 <- x1[-ind]
          x2 <- x2[-ind]
        }

        if(any(is.na(x2))){
          # get NA indicies
          ind <- which(is.na(x2))
          x1 <- x1[-ind]
          x2 <- x2[-ind]
        }

        # make sure x1 and x2 are the same length
        stopifnot(length(x1) == length(x2))

        n <- length(x1)

        if(var_1_type == "numeric" & var_2_type == "numeric") {
          # run a correlation
          result <- cor.test(x1, x2)
          df <- add_row(df,
                        var1 = names(x)[i],
                        var2 = names(x)[j],
                        statistic = "r",
                        value = result$estimate,
                        p_value = result$p.value,
                        n = n
          )
        } else if(var_1_type == "factor" & var_2_type == "numeric") {
          # run an ANOVA or t-test, depending on number of levels
          num_levels <- length(levels(x1))
          require(ICC)
          result <- ICCbare(x1, x2)
          p <- permute_icc(x1, x2)
          df <- add_row(df,
                        var1 = names(x)[i],
                        var2 = names(x)[j],
                        statistic = "ICC",
                        value = result,
                        p_value = p,
                        n = n
          )
        } else if(var_1_type == "numeric" & var_2_type == "factor") {
          # run an ANOVA or t-test, depending on number of levels
          num_levels <- length(levels(x2))
          require(ICC)
          result <- ICCbare(x2, x1)
          p <- permute_icc(x2, x1)
          df <- add_row(df,
                        var1 = names(x)[i],
                        var2 = names(x)[j],
                        statistic = "ICC",
                        value = result,
                        p_value = p,
                        n = n
          )
        } else if(var_1_type == "factor" & var_2_type == "factor") {
          require("GoodmanKruskal")
          # compute the GKtau statistic
          stat1 <- GKtau(x1, x2)$tauxy
          stat2 <- GKtau(x1, x2)$tauyx
          p1 <- permute_tau(x1, x2)
          p2 <- permute_tau(x2, x1)
          df <- add_row(df,
                        var1 = names(x)[i],
                        var2 = names(x)[j],
                        statistic = "tau",
                        value = stat1,
                        p_value = p1,
                        n = n
          )
          df <- add_row(df,
                        var1 = names(x)[j],
                        var2 = names(x)[i],
                        statistic = "tau",
                        value = stat2,
                        p_value = p2,
                        n = n
          )
        } else{
          # return an empty row
          df <- add_row(df,
                        var1 = names(x)[i],
                        var2 = names(x)[j],
                        statistic = NA_character_,
                        value = NA_integer_,
                        p_value = NA_real_,
                        n = n
          )
        }
      }
    }
  if(plot == TRUE) {
    df[-1,] %>%
      filter(!is.na(value)) %>%
      unite(variables, var1, var2, sep = " by ") %>%
      mutate(`possibly significant` = if_else(p_value < 0.05, "significant", "NS")) %>%
      ggplot(aes(y = value, x = reorder(variables, value), color = `possibly significant`)) +
      geom_point() +
      coord_flip() +
      facet_wrap(~statistic, scales = "free") +
      theme_minimal() +
      scale_color_manual(values = c("#37454B", "#E84F22"))
  } else{
    df[-1,]
  }

}

# eda(iris)
#
# eda(iris) %>%
#   filter(!is.na(value)) %>%
#   unite(variables, var1, var2, sep = " :: ") %>%
#   mutate(significant = if_else(p_value < 0.05, "significant", "NS")) %>%
#   ggplot(aes(y = value, x = reorder(variables, value), color = significant)) +
#   geom_point() +
#   coord_flip() +
#   facet_wrap(~statistic, scales = "free") +
#   theme_minimal()
# ggsave("~/Desktop/out.pdf", width = 20, height = 15)

