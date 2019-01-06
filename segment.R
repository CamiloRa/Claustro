# let's classify some whisky

#laod libs
library(dplyr)
library(cluster)
library(caret)
library(Rtsne)
library(readr)

whisky_df <- read_csv("data/raw/whisky.csv")

completre <-whisky_df[complete.cases(whisky_df),]

ld_dmy <- caret::dummyVars(" ~ . ", data =completre, fullRank = TRUE)
trasf_ld <- data.frame(predict(ld_dmy, newdata = completre))
distance_trasf_ld <- cluster::daisy (trasf_ld,
#                           type = list ( asymm = c( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76)),
                            metric= "gower")
distance_trasf_ld.mat <- as.matrix(distance_trasf_ld)


#silhouette
# has to be a function!

sil_width_ld <- c(NA)
for (i in 2:10) {
  pam_fit_trasf_ld <- cluster::pam( distance_trasf_ld, diss = TRUE, k = i)
  sil_width_ld[i] <- pam_fit_trasf_ld$silinfo$avg.width
}

#this is already a function, WHere did I put it

jpeg(filename="Silhouette_Plot_explicit.jpg", width=1280,height=800,units = "px")
  plot (1:10, sil_width_ld, xlab="clusters", ylab="silhouette width")
  lines(1:10, sil_width_ld)
dev.off()


pam_fit_trasf_ld <- cluster::pam(distance_trasf_ld, diss=TRUE, k=5)
pam_results_ld <- trasf_ld %>%
  dplyr::mutate(cluster = pam_fit_trasf_ld$clustering) %>%
  dplyr::group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results_ld$the_summary
prototypes_trasf_ld <- trasf_ld[pam_fit_trasf_ld$medoids,]
write.csv(prototypes_trasf_ld, "prototypes_ld.csv")

# perplexity value has to be smaller
# how calculated
tsne_obj_ld <- Rtsne(distance_trasf_ld.mat,
                     is_distance = TRUE,
                     perplexity = 120)

tsne_data_ld <- tsne_obj_ld$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit_trasf_ld$clustering))

#cluster plot
jpeg(filename="Attendee Clusters LD.jpg", width=1280,height=800,units = "px")
  p_cluster <- ggplot(aes(x = X, y = Y), data = tsne_data_ld) +
  geom_point(aes(color = cluster, size = 5),
             alpha = .5)
  p_cluster
dev.off()
