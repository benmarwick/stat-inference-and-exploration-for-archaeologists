# chi-square, from Andrew's RStudio talk https://www.rstudio.com/resources/videos/infer-a-package-for-tidy-statistical-inference/ and https://github.com/andrewpbray/talks/tree/master/2018

library(tidyverse)
load(url("http://bit.ly/2E65g15"))
names(gss)

select(gss, party, NASA)

ggplot(gss, 
       aes(party, 
           fill = NASA)) +
  geom_bar()


chi_test <- chisq.test(gss$party, gss$NASA)

library(infer)

# visualise 
gss %>% 
  specify(NASA ~ party) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000,
           type = "permute") %>% 
  calculate(stat = "Chisq") %>% 
  visualize() +
  geom_vline(xintercept = chi_test$statistic, 
             colour = "red", 
             size = 1)

# get value
gss %>% 
  specify(NASA ~ party) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000,
           type = "permute") %>% 
  calculate(stat = "Chisq") %>% 
  summarise(p_val = mean(stat > chi_test$statistic))


#-----------------
library(tidyverse)
library(infer)

j_data1 <- 
j_data %>% 
  filter(Material %in% c("Volcanic", "Quartzite")) %>% 
  filter(Artclas %in% c("Flake", "Hshat", "FFrag")) 

ggplot(j_data1, 
       aes(Material, 
           fill = Artclas)) +
  geom_bar()

chisq.test(j_data1$Material, 
           j_data1$Artclas, 
           simulate.p.value = TRUE, 
           B = 10000)

# compute chi statistic
point_estimate <- 
  j_data1 %>%
  specify(Material ~ Artclas) %>% 
  calculate(stat = "Chisq") %>%
  dplyr::pull()

# generate data under the null 
j_data1_chi <- 
j_data1 %>% 
  specify(Artclas ~ Material) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000,
           type = "permute") %>% 
  calculate(stat = "Chisq")

# visualise 
j_data1_chi %>% 
  visualize() +
  shade_p_value(obs_stat = point_estimate, 
                direction = "right") +
  theme_bw()

# get p-value
j_data1_chi %>% 
  get_p_value(obs_stat = point_estimate, 
              direction = "right")


j_data1 %>%
  chisq_test(Material ~ Artclas)

#-----------------------------------------------

# prepare data
j_data2 <- 
  j_data %>% 
  filter(Material %in% c("Volcanic", "Chert")) %>% 
  filter(Artclas == "Flake") 

# visualise
ggplot(j_data2) +
  aes(x = Length,
      colour = Material) +
  geom_density() +
  theme_bw()

# Calculate observed statistic
obs_t <- j_data2 %>%
  specify(Length ~ Material) %>%
  calculate(stat = "t", order = c("Volcanic", "Chert"))

# Randomization approach to t-statistic
t_null_perm <- j_data2 %>%
  specify(Length ~ Material) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t")#, order = c("Volcanic", "Chert"))

# visualise 
visualize(t_null_perm) +
  shade_p_value(obs_stat = obs_t, direction = "two_sided") 

# Calculate the randomization-based 𝑝-value
t_null_perm %>% 
  get_p_value(obs_stat = obs_t, direction = "two_sided")

```{r, t-test-4, echo = FALSE, eval = FALSE, cache = TRUE, results = 'hide'}
# get the t-test statistic and p-value 
j_data1 %>% 
  t_test(Length ~ Material, 
         order = c("Volcanic", "Quartzite"))
```

`r chunk_reveal("t-test-4", break_type = "auto", width_code = "50%", width_output = "45%")`

---
  
  
  # How to report the results of our t-test?
  
  > A t-test showed that there was no different in lengths of stone artefacts between Volcanic and Chert raw materials (t (df = 2, N = 297) = 2.63, p = 0.28).



#---------------------------------------------
F_hat <- anova(
  aov(formula = Length ~ origin, data = j_data2)
)$`F value`[1]

null_distn <- fli_small %>%
  specify(arr_delay ~ origin) %>% # alt: response = arr_delay, 
  # explanatory = origin
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")
ggplot(null_distn, aes(x = stat)) +
  geom_density() +
  geom_vline(xintercept = F_hat, color = "red")


```{r, test-chi-square-4, echo = FALSE, eval = FALSE, cache = TRUE, results = 'hide'}
# get the chi-square statistic and p-value 
# chisq.test(j_data1$Material, j_data1$Artclas)
```

`r chunk_reveal("test-chi-square-4", break_type = "auto")`

---
  
  # How to report the results of our chi-square test?
  
  "A chi-square test of independence showed that there was no association between raw material and artefact type (X2 (df = 2, N = 297) = 2.63, p = 0.28)."
 

aov(Length ~ Material,
    data = j_data3) %>% 
  TukeyHSD() %>% 
  tidy() %>%
  ggplot(aes(x = fct_reorder(comparison, 
                             estimate), 
             y = estimate)) +
  geom_pointrange(aes(ymin = conf.low,
                      ymax = conf.high,
                      color = adj.p.value < 0.05)) +
  geom_hline(yintercept = 0, 
             linetype = "dashed") +
  coord_flip() +
  labs(x = NULL,
       colour = "padj < .05")


#------------------------------------------------

# kmeans

# prepare data
j_data4 <- 
j_data %>% 
  filter(Material %in% c("Silcrete")) %>% 
  select(Length, Thick, Width) %>% 
  drop_na() %>% 
  scale

# find an appropriate number of clusters to use.
library(factoextra)
fviz_nbclust(j_data4, 
             kmeans, 
             method = "wss") +
  geom_vline(xintercept = 6)
  
# compute clusters
kmeans_output <- kmeans(j_data4, 
                        centers = 6, 
                        nstart = 50)

# view tidy output
tidy(kmeans_output)

# see which cluster each 
# artefact is in
augment(kmeans_output, j_data4)

# visualise the clusters in data
j_data4 %>%
  as_tibble() %>%
  mutate(cluster = kmeans_output$cluster) %>%
  ggplot(aes(Length, 
             Thick, 
             color = factor(cluster), 
             label = cluster)) +
  geom_text() +
  theme_minimal()

# visualise the clusters in PCA
fviz_cluster(kmeans_output, 
             data = j_data4) +
  theme_minimal()

###-----------------------------------------------
mp <- rio::import("https://www.mattpeeples.net/kmeans_data.csv")

mp_num <- 
  mp %>% 
  select_if(is.numeric)

# find an appropriate number of clusters to use.
library(factoextra)
fviz_nbclust(mp_num, kmeans, method = "wss")

# say 3?

n = 4

# compute clusters
kmeans_output <- kmeans(mp_num, 
                        centers = n, 
                        nstart = 50)



fviz_cluster(kmeans_output, 
             data = mp_num) +
  theme_minimal()


#---------------------------------------------------
# PCA in the tidyverse framework

library(tidyverse)
library(broom)

us_arrests <- USArrests %>% 
  rownames_to_column(var = "state") %>% 
  # I prefer column names to be all lowercase so I am going to change them here
  rename_all(tolower) %>% 
  as_tibble()

us_arrests_pca <- 
  us_arrests %>% 
  nest(data=everything()) %>% 
  mutate(pca = map(data, ~ prcomp(.x %>% select(-state), 
                                  center = TRUE, scale = TRUE)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y)))

us_arrests_pca

# how much variance explained by the PCs
# As a general rule of thumb, you want to look 
# at enough principal components to explain ~90% of your data’s variability.

var_exp <- 
  us_arrests_pca %>% 
  unnest(pca_aug) %>% 
  summarize_at(vars(contains("PC", 
                    ignore.case = FALSE)), 
               funs(var)) %>% 
  gather(key = pc, value = variance) %>% 
  mutate(var_exp = variance/sum(variance),
         cum_var_exp = cumsum(var_exp),
         pc = str_replace(pc, ".fitted", ""))

# plot the variance explained and the cumulative variance explained and look for the “elbow” in the graph

var_exp %>% 
  rename(
    `Variance Explained` = var_exp,
    `Cumulative Variance Explained` = cum_var_exp
  ) %>% 
  gather(key = key, value = value, `Variance Explained`:`Cumulative Variance Explained`) %>% 
  ggplot(aes(pc, value, group = key)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~key, scales = "free_y") +
  theme_bw() +
  lims(y = c(0, 1)) +
  labs(y = "Variance",
       title = "Variance explained by each principal component")

var_exp

# ince the majority of the variance is explained by the first two principal components, let’s plot them against each other.

library(ggfortify)
us_arrests_pca %>%
  mutate(
    pca_graph = map2(
      .x = pca,
      .y = data,
      ~ autoplot(.x, loadings = TRUE, 
                 loadings.label = TRUE,
                 loadings.label.repel = TRUE,
                 data = .y, 
                 label = TRUE,
                 label.label = "state",
                 label.repel = TRUE) +
        theme_bw() +
        labs(x = "Principal Component 1",
             y = "Principal Component 2",
             title = "First two principal components of PCA on USArrests dataset")
    )
  ) %>%
  pull(pca_graph)

#-
# PCA in the tidyverse framework

library(tidyverse)
library(broom)

# prepare data

j_data5 <- 
  j_data %>% 
  filter(Square == "B") %>% 
  select(Spit, Length, Thick, Width)  %>% 
  drop_na

# compute PCA
j_data5_pca <- 
  j_data5 %>% 
  nest(data=everything()) %>% 
  mutate(pca = map(data, ~ prcomp(.x %>% select(-Spit), center = TRUE, scale = TRUE)),
         pca_aug = map2(pca,  data, ~augment(.x, data = .y)))

# how much variance explained by the PCs
# As a general rule of thumb, you want to look 
# at enough principal components to explain ~90% of your data’s variability.

var_exp <- 
  j_data5_pca %>% 
  unnest(pca_aug) %>% 
  summarize_at(vars(contains("PC", 
                             ignore.case = FALSE)), 
               funs(var)) %>% 
  gather(key = pc, value = variance) %>% 
  mutate(var_exp = variance/sum(variance),
         cum_var_exp = cumsum(var_exp),
         pc = str_replace(pc, ".fitted", ""))

# plot the variance explained and the cumulative variance explained and look for the “elbow” in the graph

var_exp %>% 
  rename(
    `Variance Explained` = var_exp,
    `Cumulative Variance Explained` = cum_var_exp
  ) %>% 
  pivot_longer(`Variance Explained`:`Cumulative Variance Explained`,
                names_to = "key", 
               values_to = "value") %>% 
  ggplot(aes(pc, value, group = key)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~key, scales = "free_y") +
  theme_bw() +
  lims(y = c(0, 1)) +
  labs(y = "Variance",
       title = "Variance explained by each principal component")

# Since the majority of the variance is explained by the first two principal components, let’s plot them against each other.

library(ggfortify)
j_data5_pca %>%
  mutate(
    pca_graph = map2(
      .x = pca,
      .y = data,
      ~ autoplot(.x, 
                 loadings = TRUE, 
                 loadings.label = TRUE,
                 loadings.label.repel = TRUE,
                 data = .y, 
                 label = TRUE,
                 label.label = "Spit",
                 label.repel = TRUE) +
        theme_bw() +
        labs(x = "Principal Component 1",
             y = "Principal Component 2",
             title = "First two principal components of PCA")
    )
  ) %>%
  pull(pca_graph)


# ------------ simpler PCA
library(tidyverse)

# get the data
nb_data <- rio::import("https://bit.ly/37is3Dj")

# prepare the data

working_data <- nb_data %>%
  mutate(chipFreq = Chips/Artifacts,
         coreFreq = Cores/Artifacts,
         blankFreq = Blanks/Artifacts,
         retFreq  = RetouchedTools/Artifacts,
         featFreq = Features/SampledVolume,
         toolDiv = ToolTypes/sqrt(RetouchedTools),
         lithDens = Artifacts/SampledVolume
  ) %>%
  select(chipFreq, 
         coreFreq, 
         blankFreq, 
         retFreq, 
         featFreq, 
         toolDiv, 
         lithDens, 
         EstimatedArea,
         Sites)

write_csv(working_data, here::here("data/cascalheira-bicho-2020-data-prepped.csv"))

#-------------------------------------------



# get the prepped data
nb_data <- rio::import("https://bit.ly/2XFcOB5", 
                       setclass = "tibble")

# inspect the data
str(nb_data)

# prepare the data
nb_data_rownames <- nb_data %>% column_to_rownames(var="Sites")

library(FactoMineR)
# compute PCA
res.pca <- PCA(nb_data_rownames, 
               graph = FALSE)

# inspect eigenvalues, values >1 indicate that component captures more
# variability that any of the original measurement variables
res.pca$eig

# inspect distribution of PCs
library(factoextra)
fviz_screeplot(res.pca)


# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

# Visualise output from the PCA ------------------------------------------

# create biplot
fviz_pca_biplot(res.pca)




  



  
  
