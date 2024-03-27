remotes::install_github("PrzeChoj/giigTests")
library(giigTests)
library(parallel)
library(purrr)

# eden p=4, middle 0.01 seconds; end 1 second
# eden p=5, middle 7 seconds; end 10 minut
# for p=6, the allEdgePartitions will need ~ 1500 GB of RAM and ~80 days of computing :<
start <- Sys.time()
p <- 5

allVertexPartitions <- partitions::listParts(p) # podział na kolory wierzcholkow
allEdges <- construct_edges(p)
allEdgePartitions <- partitions::listParts(p * (p - 1) / 2) # podział na kolory krawędzi

middle <- Sys.time()
middle - start

length(allVertexPartitions) # p=5 => 52; p=6 => 203; A000110
length(allEdgePartitions)

available_cores <- min(detectCores(), length(allVertexPartitions), 7) # 7 is enough: p <- 5; allVertexPartitions <- partitions::listParts(p); sum(sapply(1:52, function(i){giigTests:::is_color_order(allVertexPartitions[[i]])}))
task_ids <- 1:length(allVertexPartitions)
execute_task <- function(i) {
  if (!giigTests:::is_color_order(allVertexPartitions[[i]])) {
    return(numeric(0))
  }
  last_percent <- 0
  js_that_are_Ishi <- numeric(0)
  for (j in 1:length(allEdgePartitions)) {
    now_percent <- floor(j / length(allEdgePartitions) * 10)
    if (now_percent > last_percent) {
      last_percent <- now_percent
      print(paste0(now_percent * 10, "% on i=", i))
    }
    if (is_Ishi_space(allVertexPartitions[[i]], allEdgePartitions[[j]])) {
      js_that_are_Ishi <- c(js_that_are_Ishi, j)
    }
  }

  js_that_are_Ishi
}
task_results <- mclapply(task_ids, execute_task, mc.cores = available_cores)
result_df <- imap_dfr(task_results, ~ data.frame(vPartition = rep(.y, length(.x)), ePartition = .x))
nrow(result_df)
# dput(result_df)
end <- Sys.time()
end - start


all_ishi_full_graphs_p_4 <- structure(list(vPartition = c(
  1L, 1L, 1L, 1L, 1L, 3L, 3L, 3L,
  3L, 3L, 7L, 7L, 7L, 10L, 10L, 10L, 10L, 15L
), ePartition = c(
  1, 8, 15, 20, 128, 41, 57, 58, 59, 111, 35, 178, 183, 180, 195,
  199, 203, 203
)), class = "data.frame", row.names = c(
  NA, -18L
))

all_ishi_full_graphs_p_5 <- structure(list(vPartition = c(
  1L, 1L, 1L, 1L, 1L, 1L, 1L, 4L,
  4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
  4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
  4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L,
  9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 19L, 19L, 19L, 19L, 19L, 19L,
  19L, 19L, 19L, 19L, 19L, 19L, 19L, 19L, 19L, 19L, 19L, 19L, 19L,
  19L, 19L, 19L, 19L, 19L, 19L, 32L, 32L, 32L, 32L, 32L, 32L, 32L,
  32L, 32L, 32L, 32L, 43L, 43L, 43L, 43L, 43L, 43L, 43L, 43L, 52L
), ePartition = c(
  1, 3898, 3905, 3923, 3941, 3944, 3955, 726,
  1008, 1009, 1010, 1011, 1824, 1825, 1826, 2526, 2527, 2528, 2529,
  2530, 2531, 3666, 17155, 17398, 18140, 39207, 39208, 39209, 39438,
  39439, 39440, 40302, 40303, 40304, 41754, 43001, 43002, 43701,
  43706, 46286, 46287, 51800, 52036, 52894, 107302, 107303, 107304,
  113700, 1693, 30907, 31030, 31210, 31330, 32107, 32467, 33367,
  33487, 34505, 79083, 79088, 79101, 79104, 79113, 79116, 55179,
  64866, 64868, 64869, 64884, 64885, 64887, 76694, 76697, 91702,
  91704, 91705, 91712, 91715, 91716, 91727, 91735, 91758, 103929,
  103930, 103932, 103933, 103940, 103943, 106364, 46296, 52895,
  52898, 54724, 111394, 111681, 114201, 114223, 114495, 115583,
  115639, 114399, 115619, 115625, 115769, 115942, 115950, 115957,
  115975, 115975
)), class = "data.frame", row.names = c(NA, -109L))

usethis::use_data(all_ishi_full_graphs_p_4, overwrite = TRUE)
usethis::use_data(all_ishi_full_graphs_p_5, overwrite = TRUE)
