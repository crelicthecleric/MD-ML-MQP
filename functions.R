library(readxl)
library(tidyverse)
library(drc)

get_date <- function(date) {
  return(as.Date(strsplit(as.character(date), ' ')[[1]][1],format='%m/%d/%Y'))
}

get_k <- function(data) {
  model <- drm(y ~ x, data = data, fct = L.4(), type = "continuous")
  k <- model$coefficients[[1]]
  return(k)
}

all_ks <-function(df, x, y, group) {
  colnames(df) <- c("group", "x", "y")
  ks <- by(df[,c("x", "y")], df$group, get_k)
  summary <- data.frame(array(ks, dim(ks), dimnames(ks)))
  summary <- tibble::rownames_to_column(summary, group)
  colnames(summary) <- c(group, "k")
  return(summary)
}

preprocess <- function(file, skip, keys, exclude, sheet) {
  df <- data.frame(read_excel(file, skip=skip, sheet=sheet))
  df$key <- do.call(paste, c(df[keys], sep=" "))
  df <- df[,!(names(df) %in% exclude)]
  #df <- df[,colSums(is.na(df))<nrow(df)]
  df <- remove_empty(df)
  return(df)
}

binarize_list <- function(master, list) {
  binary <- c()
  for (e in master) {
    if (e %in% list) {
      binary <- append(binary, 1)
    }
    else{
      binary <- append(binary, 0)
    }
  }
  return(binary)
}

remove_empty <- function(df) {
  empty_columns <- colSums(is.na(df)) == nrow(df)
  df <- df[, !empty_columns]
  return(df)
}

split_cases <- function(master, df) {
  df <- df[df$individual %in% master,]
  df <- split(df, df$individual)
  df <- lapply(df, remove_empty)
  return(df)
}

save_img <- function(vis) {
  font_sizes <- theme(plot.title = element_text(size = 32),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 24),
        legend.title = element_text(size = 28),
        legend.text = element_text(size = 24))

  vis_name <- deparse(substitute(vis))
  svg_path <- paste0("img/svg/", vis_name, ".svg")
  png_path <- paste0("img/png/", vis_name, ".png")
  ggsave(png_path, vis)
  vis <- vis + font_sizes
  ggsave(svg_path, vis)
}