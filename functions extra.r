fcolnet <- function(data = scholars, discipline = "sociology", waves = list(c(2015,2018), c(2019, 2023)), type = c("first")) {
  
  # step 1
  demographics <- do.call(rbind.data.frame, data$demographics)
  demographics <- demographics %>%
    mutate(Universiteit1.22 = replace(Universiteit1.22, is.na(Universiteit1.22), ""), Universiteit2.22 = replace(Universiteit2.22,
                                                                                                                 is.na(Universiteit2.22), ""), Universiteit1.24 = replace(Universiteit1.24, is.na(Universiteit1.24),
                                                                                                                                                                          ""), Universiteit2.24 = replace(Universiteit2.24, is.na(Universiteit2.24), ""), discipline.22 = replace(discipline.22,
                                                                                                                                                                                                                                                                                  is.na(discipline.22), ""), discipline.24 = replace(discipline.24, is.na(discipline.24), ""))
  
  sample <- which(demographics$discipline.22 %in% discipline | demographics$discipline.24 %in% discipline)
  
  demographics_soc <- demographics[sample, ]
  scholars_sel <- lapply(scholars, "[", sample)
  
  # step 2
  ids <- demographics_soc$au_id
  nwaves <- length(waves)
  nets <- array(0, dim = c(nwaves, length(ids), length(ids)), dimnames = list(wave = 1:nwaves, ids,
                                                                              ids))
  dimnames(nets)
  
  # step 3
  df_works <- tibble(works_id = unlist(lapply(scholars_sel$work, function(l) l$id)), works_author = unlist(lapply(scholars_sel$work,
                                                                                                                  function(l) l$author), recursive = FALSE), works_year = unlist(lapply(scholars_sel$work, function(l) l$publication_year),
                                                                                                                                                                                 recursive = FALSE))
  
  df_works <- df_works[!duplicated(df_works), ]
  
  # step 4
  if (type == "first") {
    for (j in 1:nwaves) {
      df_works_w <- df_works[df_works$works_year >= waves[[j]][1] & df_works$works_year <= waves[[j]][2],
      ]
      for (i in 1:nrow(df_works_w)) {
        ego <- df_works_w$works_author[i][[1]]$au_id[1]
        alters <- df_works_w$works_author[i][[1]]$au_id[-1]
        if (sum(ids %in% ego) > 0 & sum(ids %in% alters) > 0) {
          nets[j, which(ids %in% ego), which(ids %in% alters)] <- 1
        }
      }
    }
  }
  
  if (type == "last") {
    for (j in 1:nwaves) {
      df_works_w <- df_works[df_works$works_year >= waves[[j]][1] & df_works$works_year <= waves[[j]][2],
      ]
      for (i in 1:nrow(df_works_w)) {
        ego <- rev(df_works_w$works_author[i][[1]]$au_id)[1]
        alters <- rev(df_works_w$works_author[i][[1]]$au_id)[-1]
        if (sum(ids %in% ego) > 0 & sum(ids %in% alters) > 0) {
          nets[j, which(ids %in% ego), which(ids %in% alters)] <- 1
        }
      }
    }
  }
  
  if (type == "all") {
    for (j in 1:nwaves) {
      df_works_w <- df_works[df_works$works_year >= waves[[j]][1] & df_works$works_year <= waves[[j]][2],
      ]
      for (i in 1:nrow(df_works_w)) {
        egos <- df_works_w$works_author[i][[1]]$au_id
        if (sum(ids %in% egos) > 0) {
          nets[j, which(ids %in% egos), which(ids %in% egos)] <- 1
        }
      }
    }
  }
  output <- list()
  output$data <- scholars_sel
  output$nets <- nets
  return(output)
}


#save the output of your function
test_all <- fcolnet(data = scholars, 
                    discipline = "sociology", 
                    waves = list(c(2015, 2018), c(2019, 2023)), 
                    type = c("first"))


test_w1 <- igraph::graph_from_adjacency_matrix(
  test_all$nets[1,,], #for this example I take the first wave of data. (thus I select the array of networks and take the first matrix)
  mode = c("directed"),
  weighted = NULL,
  diag = FALSE,
  add.colnames = NULL,
  add.rownames = NULL
)

plot(test_w1,
     vertex.label = NA,
     edge.width = 0.2,
     edge.arrow.size =0.2)

fload <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}

fpackage.check <- function(packages) {
  lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
}
