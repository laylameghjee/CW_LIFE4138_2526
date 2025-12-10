# # # # # # # # # # # # # # # # # # # #
#       Gene Expression Challenge     #
#           Layla Meghjee             #
#           December 2025             #
#            LIFE4138 CW              #
# # # # # # # # # # # # # # # # # # # #

#importing libraries
library(tidyverse)

#loading in tsv files 
AB <- read_tsv('A_vs_B.deseq2.results.tsv')
AC <- read_tsv('A_vs_C.deseq2.results.tsv')

##      SUMMARY STATISTICS
#PART 1AB - number of signicicantly upregulated & downrefulated genes
#deciding thresholds 
pvt <- 0.05
l2fct <- 1

#catagorising genes as wither upregulated or downregulated
AB <- AB %>%
  mutate(
    direction = case_when(
      padj < pvt & log2FoldChange > l2fct ~ 'up', 
      padj < pvt & log2FoldChange < l2fct ~ 'down',
#any values that dont fall in the above will be assigned ns (non significant)
      TRUE ~ 'ns'
    )
  )

#counting types of genes
AB_counts <- AB %>%
  count(direction, name = 'number of genes')
head(AB_counts)

#PART 2AB - summarising p values and log2 fold changes across all genes
#histogram to show distribution of p values
AB %>%
  ggplot(aes(x = pvalue)) + 
  geom_histogram(
    bins = 50,
    colour = 'black',
    fill = 'skyblue'
    ) +
        labs( 
          title = 'p value distribution for A vs B',
          x = 'p value',
          y = 'number of genes'
        )

#table to show number of p values for certain intervals
AB_pvals <- AB %>%
  mutate(
    pvalue_catg = cut(
      pvalue,
      breaks = c(0, 0.001, 0.01, 0.05, 1),
      labels = c('0-0.001', '0.001 -0.01', '0.01-0.5', '0.5-1')
    )
  ) %>%
  count(pvalue_catg, name = 'number of genes')
head(AB_pvals)

#histogram to show distribution of log2 fold change values
AB %>%
  ggplot(aes(x = log2FoldChange)) + 
  geom_histogram(
    bins = 100,
    colour = 'black',
    fill = 'lightgreen'
    ) +
  labs( 
    title = 'log2 fold change for A vs B',
    x = 'log2 fold change',
    y = 'number of genes'
  )

#PART 1AC - number of signicicantly upregulated & downrefulated genes
#catagorising genes as wither upregulated or downregulated
AC <- AC %>%
  mutate(
    direction = case_when(
      padj < pvt & log2FoldChange > l2fct ~ 'up', 
      padj < pvt & log2FoldChange < l2fct ~ 'down',
      #any values that dont fall in the above will be assigned ns (non significant)
      TRUE ~ 'ns'
    )
  )

#counting types of genes
AC_counts <- AC %>%
  count(direction, name = 'number of genes')
head(AC_counts)

#PART 2AC - summarising p values and log2 fold changes across all genes
#histogram to show distribution of p values
AC %>%
  ggplot(aes(x = pvalue)) + 
  geom_histogram(
    bins = 50,
    colour = 'black',
    fill = 'skyblue'
  ) +
  labs( 
    title = 'p value distribution for A vs C',
    x = 'p value',
    y = 'number of genes'
  )

#table to show number of p values for certain intervals
AC_pvals <- AC %>%
  mutate(
    pvalue_catg = cut(
      pvalue,
      breaks = c(0, 0.001, 0.01, 0.05, 1),
      labels = c('0-0.001', '0.001 -0.01', '0.01-0.5', '0.5-1')
    )
  ) %>%
  count(pvalue_catg, name = 'number of genes')
head(AC_pvals)

#histogram to show distribution of log2 fold change values
AC %>%
  ggplot(aes(x = log2FoldChange)) + 
  geom_histogram(
    bins = 100,
    colour = 'black',
    fill = 'lightgreen'
  ) +
  labs( 
    title = 'log2 fold change for A vs C',
    x = 'log2 fold change',
    y = 'number of genes'
  )
  
##      PLOTS
# Volcano plot AB
#cleaning the data, getting rid of n/a values and -ve log2 values
AB_clean <- AB %>%
  filter(is.finite(pvalue), is.finite(log2FoldChange)) %>%
  mutate(
    neg_log10_p = -log10(pvalue),
    direction = case_when(
      padj < pvt & log2FoldChange > l2fct ~ 'up', 
      padj < pvt & log2FoldChange < l2fct ~ 'down',
      #any values that dont fall in the above will be assigned ns (non significant)
      TRUE ~ 'ns'
    )
  )

#plotting volcano
AB_clean %>%
  ggplot(aes(x = log2FoldChange, y=neg_log10_p, colour = direction)) +
  geom_point(alpha = 0.6, size = 1.2) +
  scale_colour_manual(values = c('up' = 'lightgreen', 'down' = 'lightblue', 'ns' 
                                 = 'grey')) +
  labs(
    title = 'Volcano plot A v B',
    x = 'log2 fold change',
    y = '-log10(p value)',
    colour = 'direction'
  )

#MA plot AB
#creating data set for MA plot
AB_ma <- AB %>%
  filter(is.finite(baseMean), is.finite(log2FoldChange)) %>%
  mutate(
    direction = case_when(
      padj < pvt & log2FoldChange > l2fct ~ 'up', 
      padj < pvt & log2FoldChange < l2fct ~ 'down',
      #any values that dont fall in the above will be assigned ns (non significant)
      TRUE ~ 'ns'
    )
  )

#plotting MA 
AB_ma %>%
  ggplot(aes(x = baseMean, y = log2FoldChange, colour = direction )) +
  geom_point(alpha = 0.5, size = 1) +
  scale_x_log10() +
  scale_colour_manual(values = c('up' = 'lightgreen', 'down' = 'lightblue', 'ns' 
                                 = 'grey')) +
  labs(
    title = 'MA plot, A v B',
    x = 'mean expression',
    y = 'log2 fold change'
  )
  
#Historgram AB
AB %>%
  ggplot(aes(x = pvalue)) + 
  geom_histogram(
    bins = 50,
    colour = 'black',
    fill = 'skyblue'
  ) +
  labs( 
    title = 'p value distribution for A vs B',
    x = 'p value',
    y = 'number of genes'
  )


# Volcano plot AC
#cleaning the data, getting rid of n/a values and -ve log2 values
AC_clean <- AC %>%
  filter(is.finite(pvalue), is.finite(log2FoldChange)) %>%
  mutate(
    neg_log10_p = -log10(pvalue),
    direction = case_when(
      padj < pvt & log2FoldChange > l2fct ~ 'up', 
      padj < pvt & log2FoldChange < l2fct ~ 'down',
      #any values that dont fall in the above will be assigned ns (non significant)
      TRUE ~ 'ns'
    )
  )

#volcano
AC_clean %>%
  ggplot(aes(x = log2FoldChange, y=neg_log10_p, colour = direction)) +
  geom_point(alpha = 0.6, size = 1.2) +
  scale_colour_manual(values = c('up' = 'lightgreen', 'down' = 'lightblue', 'ns' 
                                 = 'grey')) +
  labs(
    title = 'Volcano plot A v C',
    x = 'log2 fold change',
    y = '-log10(p value)',
    colour = 'direction'
  )

#MA plot AC
#creating data set for MA plot
AC_ma <- AC %>%
  filter(is.finite(baseMean), is.finite(log2FoldChange)) %>%
  mutate(
    direction = case_when(
      padj < pvt & log2FoldChange > l2fct ~ 'up', 
      padj < pvt & log2FoldChange < l2fct ~ 'down',
      #any values that dont fall in the above will be assigned ns (non significant)
      TRUE ~ 'ns'
    )
  )

#plotting
AC_ma %>%
  ggplot(aes(x = baseMean, y = log2FoldChange, colour = direction )) +
  geom_point(alpha = 0.5, size = 1) +
  scale_x_log10() +
  scale_colour_manual(values = c('up' = 'lightgreen', 'down' = 'lightblue', 'ns' 
                                 = 'grey')) +
  labs(
    title = 'MA plot, A v C',
    x = 'mean expression',
    y = 'log2 fold change'
  )

#Historgram AC
AC %>%
  ggplot(aes(x = pvalue)) + 
  geom_histogram(
    bins = 50,
    colour = 'black',
    fill = 'skyblue'
  ) +
  labs( 
    title = 'p value distribution for A vs C',
    x = 'p value',
    y = 'number of genes'
  )



#Heatmap (both)
#sorting data out, removes any NA values
AB_hm <- AB %>%
  filter(is.finite(log2FoldChange), is.finite(padj))
AC_hm <- AC %>%
  filter(is.finite(log2FoldChange), is.finite(padj))

#picking top genes
topg <- bind_rows(
  AB_hm %>% mutate(comparison = 'A_vs_B'),
  AC_hm %>% mutate(comparison = 'A_vs_C')
) %>%
  filter(
    padj < pvt,
    abs(log2FoldChange) >= l2fct
  ) %>%
#arranges by most significant first
  arrange(padj) %>%
  distinct(gene_id, .keep_all = TRUE) %>%
#takes only the top 50
  slice_head (n = 50) %>%
  pull(gene_id)

#creating tibble of data for heatmap 
heat_data <-  bind_rows(
  AB_hm %>% mutate(comparison = 'A_vs_B'),
  AC_hm %>% mutate(comparison = 'A_vs_C')
) %>%
  filter(gene_id %in% topg) %>%
  select(gene_id, comparison, log2FoldChange) %>%
  mutate(
    value = log2FoldChange
  )

#ordering genes by log2FC to give gradient 
heat_data <- heat_data %>%
  mutate(
    gene_id = fct_reorder(gene_id, value, .fun = mean),
    comparison = factor(comparison, levels = c('A_vs_B', 'A_vs_C'))
  )

#plotting heatmap
heat_data %>%
  ggplot(aes(x = comparison, y = gene_id, fill = value)) +
  geom_tile(colour = 'white') +
  scale_fill_gradient2( low = 'red', mid = 'orange', high = 'pink',
                        midpoint = 0) +
  labs (
      title = 'Top differentially expressed gemes A v B and A v C',
      x = 'Comparison',
      y = 'Gene',
      fill = 'log2FoldChange'
  )

## SIGNIFICANT GENE LIST
#AB Significant 

#Creating table for just upregulated genes
AB_up <- AB_clean %>%
  filter(direction == 'up') %>%
  arrange(desc(log2FoldChange)) %>%
  select(gene_id, log2FoldChange, pvalue, padj)
head(AB_up, 20)

#Creating table for just downregulated genes
AB_down <- AB_clean %>%
  filter(direction == 'down') %>%
  arrange(log2FoldChange) %>%
  select(gene_id, log2FoldChange, pvalue, padj)
head(AB_down, 20)

#AC Significant 

#Creating table for just upregulated genes
AC_up <- AC_clean %>%
  filter(direction == 'up') %>%
  arrange(desc(log2FoldChange)) %>%
  select(gene_id, log2FoldChange, pvalue, padj)
head(AC_up, 20)

#Creating table for just downregulated genes
AC_down <- AC_clean %>%
  filter(direction == 'down') %>%
  arrange(log2FoldChange) %>%
  select(gene_id, log2FoldChange, pvalue, padj)
head(AC_down, 20)