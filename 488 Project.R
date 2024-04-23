library(tidyverse)
library(cluster)
library(factoextra)
library(knitr)
library(kableExtra)

#Read in all the pitches
pitches <- read_csv("savant_pitch_level.csv")

#Clean the data so each row is the physical charatceristics for pitcher's sepcific pitch
p23 <- pitches |>
  filter(game_date > "2023-01-01",
         pitch_type %in% c("FF", "ST", "FC", "FS", "SI", "CU", "KC", "SL", "CH")) |>
  select(pitch_type, player_name, p_throws, pfx_x, pfx_z, release_spin_rate, release_speed) |>
  group_by(player_name, p_throws, pitch_type) |>
  summarize(velo = mean(release_speed, na.rm=T), 
            spin = mean(release_spin_rate, na.rm=T), 
            horz = mean(pfx_x, na.rm=T) * 12, 
            vert = mean(pfx_z, na.rm=T) * 12, 
            count = n()) |>
  filter(count > 50) |>
  ungroup()

#EDA into the full dataset of pitch characteristics
pairs(p23[,4:7], panel=panel.smooth)



#Classification Table for RHP Clusters & Viz
initial_clustR <- p23 |>
  filter(p_throws == "R") |>
  dplyr::select(4:7) |>
  scale() |>
  kmeans(centers = 9, nstart = 10)

crp23 <- p23 |>
  filter(p_throws == "R") |>
  mutate(clust = initial_clustR$cluster)

table(crp23$pitch_type, crp23$clust)|>
  kable() |>
  kable_minimal(full_width = F, html_font = "Cambria")

fviz_cluster(initial_clustR, data = scale(crp23[4:7]),
             geom = "point",
             ellipse.type = "euclid",
             ggtheme = theme_minimal(),
             main = "RHP Pitch Type Cluster Plot"
)

#Classification Table for LHP Clusters & Viz
initial_clustL <- p23 |>
  filter(p_throws == "L") |>
  dplyr::select(4:7) |>
  scale() |>
  kmeans(centers = 9, nstart = 10)

clp23 <- p23 |>
  filter(p_throws == "L") |>
  mutate(clust = initial_clustL$cluster)

table(clp23$pitch_type, clp23$clust) |>
  kable() |>
  kable_minimal(full_width = F, html_font = "Cambria")

fviz_cluster(initial_clustL, data = scale(clp23[4:7]),
             geom = "point",
             ellipse.type = "euclid",
             ggtheme = theme_minimal(),
             main = "LHP Pitch Type Cluster Plot"
             )



#PCA on RHP and LHP
rpca <- p23 |>
  filter(p_throws == "R") |>
  dplyr::select(4:7) |>
  scale() |>
  prcomp()

summary(rpca)

fviz_pca_var(rpca,
             col.var = "contrib",
             gradient.cols = c("blue", "red"),
             repel= T)

rpca$rotation[,1:2]

lpca <- p23 |>
  filter(p_throws == "L") |>
  dplyr::select(4:7) |>
  scale() |>
  prcomp()

summary(lpca)

fviz_pca_var(lpca,
             col.var = "contrib",
             gradient.cols = c("blue", "red"),
             repel= T)

lpca$rotation[,1:2]



#Clustering 4-seam fastball for RHP
ffR <- p23 |>
  filter(p_throws == "R",
         pitch_type == "FF") |>
  select(4:7) |>
  scale()

fviz_nbclust(ffR, kmeans, method = "wss")

ffR_d <- p23 |>
  filter(p_throws == "R",
         pitch_type == "FF") |>
  select(4:7) |>
  mutate(clust = kmeans(ffR, centers = 4)$cluster) |>
  group_by(clust) |>
  summarize(velo = mean(velo),
            spin = mean(spin), 
            horz = mean(horz), 
            vert = mean(vert)) |>
  as.data.frame() |>
  select(2:5)

colnames(ffR_d) <- c("Velo (MPH)", "Spin (RPM)", "Xbrk (in.)", "Ybrk (in.)")
rownames(ffR_d) <- c("FF1", "FF2", "FF3", "FF4")
ffR_t <- ffR_d |>
  kable() |>
  kable_classic(full_width = F, html_font = "Cambria")
print(ffR_t)

#Clustering 4-seam fastball for LHP
ffL <- p23 |>
  filter(p_throws == "L",
         pitch_type == "FF") |>
  select(4:7) |>
  scale()

fviz_nbclust(ffL, kmeans, method = "wss")

ffL_d <- p23 |>
  filter(p_throws == "L",
         pitch_type == "FF") |>
  select(4:7) |>
  mutate(clust = kmeans(ffL, centers = 3)$cluster) |>
  group_by(clust) |>
  summarize(velo = mean(velo),
            spin = mean(spin), 
            horz = mean(horz), 
            vert = mean(vert)) |>
  as.data.frame() |>
  select(2:5)

colnames(ffL_d) <- c("Velo (MPH)", "Spin (RPM)", "Xbrk (in.)", "Ybrk (in.)")
rownames(ffL_d) <- c("FF1", "FF2", "FF3")
ffL_t <- ffL_d |>
  kable() |>
  kable_classic(full_width = F, html_font = "Cambria")
print(ffL_t)



#Clustering Sweeper for RHP
stR <- p23 |>
  filter(p_throws == "R",
         pitch_type == "ST") |>
  select(4:7) |>
  scale()

fviz_nbclust(stR, kmeans, method = "wss")

stR_d <- p23 |>
  filter(p_throws == "R",
         pitch_type == "ST") |>
  select(4:7) |>
  mutate(clust = kmeans(stR, centers = 4)$cluster) |>
  group_by(clust) |>
  summarize(velo = mean(velo),
            spin = mean(spin), 
            horz = mean(horz), 
            vert = mean(vert)) |>
  as.data.frame() |>
  select(2:5)

colnames(stR_d) <- c("Velo (MPH)", "Spin (RPM)", "Xbrk (in.)", "Ybrk (in.)")
rownames(stR_d) <- c("ST1", "ST2", "ST3", "ST4")
stR_t <- stR_d |>
  kable() |>
  kable_classic(full_width = F, html_font = "Cambria")
print(stR_t)

#Clustering Sweeper for LHP
stL <- p23 |>
  filter(p_throws == "L",
         pitch_type == "ST") |>
  select(4:7) |>
  scale()

fviz_nbclust(stL, kmeans, method = "wss")

stL_d <- p23 |>
  filter(p_throws == "L",
         pitch_type == "ST") |>
  select(4:7) |>
  mutate(clust = kmeans(stL, centers = 3)$cluster) |>
  group_by(clust) |>
  summarize(velo = mean(velo),
            spin = mean(spin), 
            horz = mean(horz), 
            vert = mean(vert)) |>
  as.data.frame() |>
  select(2:5)

colnames(stL_d) <- c("Velo (MPH)", "Spin (RPM)", "Xbrk (in.)", "Ybrk (in.)")
rownames(stL_d) <- c("ST1", "ST2", "ST3")
stL_t <- stL_d |>
  kable() |>
  kable_classic(full_width = F, html_font = "Cambria")
print(stL_t)



#Clustering Cutter for RHP
fcR <- p23 |>
  filter(p_throws == "R",
         pitch_type == "FC") |>
  select(4:7) |>
  scale()

fviz_nbclust(fcR, kmeans, method = "wss")

fcR_d <- p23 |>
  filter(p_throws == "R",
         pitch_type == "FC") |>
  select(4:7) |>
  mutate(clust = kmeans(fcR, centers = 3)$cluster) |>
  group_by(clust) |>
  summarize(velo = mean(velo),
            spin = mean(spin), 
            horz = mean(horz), 
            vert = mean(vert)) |>
  as.data.frame() |>
  select(2:5)

colnames(fcR_d) <- c("Velo (MPH)", "Spin (RPM)", "Xbrk (in.)", "Ybrk (in.)")
rownames(fcR_d) <- c("FC1", "FC2", "FC3")
fcR_t <- fcR_d |>
  kable() |>
  kable_classic(full_width = F, html_font = "Cambria")
print(fcR_t)

#Clustering Cutter for LHP
fcL <- p23 |>
  filter(p_throws == "L",
         pitch_type == "FC") |>
  select(4:7) |>
  scale()

fviz_nbclust(fcL, kmeans, method = "wss")

fcL_d <- p23 |>
  filter(p_throws == "L",
         pitch_type == "FC") |>
  select(4:7) |>
  mutate(clust = kmeans(fcL, centers = 3)$cluster) |>
  group_by(clust) |>
  summarize(velo = mean(velo),
            spin = mean(spin), 
            horz = mean(horz), 
            vert = mean(vert)) |>
  as.data.frame() |>
  select(2:5)

colnames(fcL_d) <- c("Velo (MPH)", "Spin (RPM)", "Xbrk (in.)", "Ybrk (in.)")
rownames(fcL_d) <- c("FC1", "FC2", "FC3")
fcL_t <- fcL_d |>
  kable() |>
  kable_classic(full_width = F, html_font = "Cambria")
print(fcL_t)



#Clustering Splitter for RHP
fsR <- p23 |>
  filter(p_throws == "R",
         pitch_type == "FS") |>
  select(4:7) |>
  scale()

fviz_nbclust(fsR, kmeans, method = "wss")

fsR_d <- p23 |>
  filter(p_throws == "R",
         pitch_type == "FS") |>
  select(4:7) |>
  mutate(clust = kmeans(fsR, centers = 3)$cluster) |>
  group_by(clust) |>
  summarize(velo = mean(velo),
            spin = mean(spin), 
            horz = mean(horz), 
            vert = mean(vert)) |>
  as.data.frame() |>
  select(2:5)

colnames(fsR_d) <- c("Velo (MPH)", "Spin (RPM)", "Xbrk (in.)", "Ybrk (in.)")
rownames(fsR_d) <- c("FS1", "FS2", "FS3")
fsR_t <- fsR_d |>
  kable() |>
  kable_classic(full_width = F, html_font = "Cambria")
print(fsR_t)



#Clustering Sinker for RHP
siR <- p23 |>
  filter(p_throws == "R",
         pitch_type == "SI") |>
  select(4:7) |>
  scale()

fviz_nbclust(siR, kmeans, method = "wss")

siR_d <- p23 |>
  filter(p_throws == "R",
         pitch_type == "SI") |>
  select(4:7) |>
  mutate(clust = kmeans(siR, centers = 3)$cluster) |>
  group_by(clust) |>
  summarize(velo = mean(velo),
            spin = mean(spin), 
            horz = mean(horz), 
            vert = mean(vert)) |>
  as.data.frame() |>
  select(2:5)

colnames(siR_d) <- c("Velo (MPH)", "Spin (RPM)", "Xbrk (in.)", "Ybrk (in.)")
rownames(siR_d) <- c("SI1", "SI2", "SI3")
siR_t <- siR_d |>
  kable() |>
  kable_classic(full_width = F, html_font = "Cambria")
print(siR_t)

#Clustering Sinker for LHP
siL <- p23 |>
  filter(p_throws == "L",
         pitch_type == "SI") |>
  select(4:7) |>
  scale()

fviz_nbclust(siL, kmeans, method = "wss")

siL_d <- p23 |>
  filter(p_throws == "L",
         pitch_type == "SI") |>
  select(4:7) |>
  mutate(clust = kmeans(siL, centers = 3)$cluster) |>
  group_by(clust) |>
  summarize(velo = mean(velo),
            spin = mean(spin), 
            horz = mean(horz), 
            vert = mean(vert)) |>
  as.data.frame() |>
  select(2:5)

colnames(siL_d) <- c("Velo (MPH)", "Spin (RPM)", "Xbrk (in.)", "Ybrk (in.)")
rownames(siL_d) <- c("SI1", "SI2", "SI3")
siL_t <- siL_d |>
  kable() |>
  kable_classic(full_width = F, html_font = "Cambria")
print(siL_t)



