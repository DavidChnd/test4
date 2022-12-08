---
title: "Aide mémoire"
author : "David Chassagnaud"
date: "`r Sys.Date()`"
output: 
  rmdformats::downcute:
    toc : 2
    default_style: "light"
    downcute_theme: "default"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(cache = TRUE)
```

# Packages

```{r Packages, echo=TRUE, message=FALSE, warning=FALSE, cache=TRUE, eval=FALSE}

library(ggplot2)
library(ggrepel)
library(reshape2)
library(lmerTest)
library(multcomp)
library(lsmeans)
library(gridExtra)
library(cowplot)
library(RGraphics)

library(plotrix)
library(knitr)
library(rstatix)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggpubr)

library(rmarkdown)
library(markdown)

library(ggThemeAssist)         # Ouvre un menu pour modifier paramètres graphiques
                               # Séléctionner Script puis Tools -> Adin  
library(esquisse)
set_i18n("fr")                 # Permet de régler le package "esquisse" en Français
esquisser(viewer = "browser")  # L'ouverture de la fenêtre graphique se fera dans le navigateur internet
esquisse::esquisser(your_data) # Ouvre la fenêtre graphique avec le JDD sélectionné
```

# JDD et préparation

```{r JDD et préparation, echo=TRUE, message=FALSE, warning=FALSE, results="hide", eval=FALSE}
your_data <- read.table("/Users/david/Desktop/R_Lica/Data/lica_R.txt", header=T,dec=",") # Choix du chemin d'accès et import JDD
your_data = subset(your_data, VA =="VA_name") # Subdivision de notre jeu de données en fonction d'une autre VA 

your_data$VA <- as.factor(your_data$VA) # Permet de changer la classe de nos données par exemple pour les transformer en facteur.
str(your_data) # Informations sur le JDD
```

# -Représentation graphique Data

## GeomBar

```{r GeomBar sur JDD, echo=TRUE, message=FALSE, warning=FALSE, results="hide", eval=FALSE}
# Voir script Lica
```

## Dotplot

```{r Dotplot sur JDD, echo=TRUE, message=FALSE, warning=FALSE, results="hide", eval=FALSE}
# Voir script dotplot
```

## Commandes pratiques

```{r Commandes pratiques, echo=TRUE, message=FALSE, warning=FALSE, results="hide", eval=FALSE}
+ theme(legend.background = element_rect(fill="transparent")) # Arrière plan de la légende  transparent
  
"\n" # Retour à la ligne dans un titre, sous titre ou autre

ylab(bquote('cm/j'^-1)) # Ecriture en indice

annotate("text", x=26, y=0.060, label= 'mean~L[site]==15.4~µm',parse = TRUE,hjust=0)  + # Annotation complexe avec exposant 

# Gérer les paramètres du theme d'une figure (Script_R_these)
theme(plot.title = element_text(hjust = 0.5, color="#FF0000", size=14), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
```

## Disposition des figures

```{r Disposition des figures, echo=TRUE, message=FALSE, warning=FALSE, results="hide", eval=FALSE}
par(mfrow = c(1,1))

fig_tot <- ggarrange(b,c, # Méthode avec ggarrange
                     ggarrange(a,
                               nrow = 2, 
                               labels = c("C")),
                     ncol= 2,
                     labels = "A", "B")
fig_tot

fig_tot <- ggdraw() + # Méthode avec ggdraw
  draw_plot(a, x = 0.5, y = 0, width = 0.5, height = 1) +
  draw_plot(b, x = 0, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(c, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C"), size = 15,
                  x = c(0, 0.5, 0), y = c(1, 1, 0.5))
fig_tot

figtot3 <- layout <- "AB" # Méthode avec layout
b + c + plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A', 
                  tag_prefix = '(',
                  tag_suffix = ')')


package(patchwork) # Permet de combiner et d'arranger plusieurs figures sur la même fenêtre graphique

# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
# https://www.datanovia.com/en/fr/blog/ggplot-multiples-rendus-faciles-par-le-package-r-patchwork/
```

## Enregistrement des figures

```{r, Enregistrement des figures, echo=TRUE, message=FALSE, warning=FALSE, results="hide", eval=FALSE}
tiff(file="/Users/david/Desktop/Orléans/R/R_Data_these/Figures_V2/Density_famille/Density_famille_01",# Choix du format, du nom, de l'emplacement de l'enregistrement du fichier ainsi que de la taille de la figure
     width=900, height=465)
     
grid.arrange(pd_01, pl_01, ncol=2, nrow = 1) # Partage de la fenêtre graphique 
dev.off()
```

# -Analasyse Data

## Calcul sur JDD

```{r Calcul sur JDD, echo=TRUE, message=FALSE, warning=FALSE, results="hide", eval=FALSE}
# Permet d'avoir la moyenne, l'écart type, l'erreur standard des conditions d'une VA dans un JDD
a <- your_data %>%
  group_by(VA) %>% # conditions / subsets
  summarize(
    your_data = mean(VA2, na.rm = TRUE) # sd / std.error
  )
a

test <- tapply(your_data$VA, your_data$VA2, mean) # Permet de renvoyer la moyenne d'une condition pour toutes les VA de notre JDD
test

```

## Boucle For

```{r Boucle For, echo=TRUE, message=FALSE, warning=FALSE, results="hide", eval=FALSE}
#(Voir script R_Lica si pb)
subseted_your_data = 0
plot_your_data = 0

for(i in unique (your_data$VA)) {     # Boucle for
  subseted_your_data = subset(your_data, VA == i)  # VA == i, ce sera la VA pour laquelle on aura les différents subsets
  plot = ggplot(subseted_your_data, aes(x= VA1, y = VA2, fill = VA3, colour = VA3)) +
    geom_point(shape = "circle", size = 2L, alpha = 0.6) +
    scale_fill_manual(
      values = c(`05_30` = "#08A0E4",  # A adapter en fonction de la taille du vecteur (fill = VA3)
                 `05_31` = "#0C9B08",
                 `06_02` = "#DD900A",
                 `06_03` = "#ED0606")
    ) +
    scale_color_manual(
      values = c(`05_30` = "#08A0E4",
                 `05_31` = "#0C9B08",
                 `06_02` = "#DD900A",
                 `06_03` = "#ED0606")
    ) +
    ggtitle(paste0(i, "")) +  # Le titre de chaque graphe prendra le noms des différents "i", (subsets) de la variable.
    labs(
      y = "Accroissement (cm)", x = "Humidité (%)",
      fill  = "Dates",
      color = "Dates")
  print(plot_your_data)
  
  ggsave(plot_your_data, file=paste0("lignee_", i,".pdf"), width = 13, height = 6, units = "cm") # Permet d'enregistrer automatiquement l'nsemble des plots
# On choisi le nom, le format, la taille et l'emplacement en choisisant le bon "working directory" 
}
```

## Tests statistiques

```{r Tests statistiques, echo=TRUE, message=FALSE, warning=FALSE, results="hide", eval=FALSE}
shapiro.test() # Permet de vérifier la normalité des données

wilcox.test()

res.kruskal
```

# Autre

```{r, Autre, echo=TRUE, message=FALSE, warning=FALSE, results="hide", eval=FALSE}
== # Pour annoter figure avec symbol =
  
<br> # Saut de ligne 
  
fig.cap="........."} # A placer dans les paramètres de chunk pour ajouter légende à une figure

library(DT) # Tableau dynamique dans script
datatable(data_lica, rownames = FALSE, filter="top", options = list(pageLength = 5, scrollX=T))

# Haut de page rmd
<a id="header"></a> # balise à placer au début du document  
Go to [header](#header)
  
"\n" # retour à la ligne
```

```{r, Rmarkdown, echo=TRUE, message=FALSE, warning=FALSE, results="hide", eval=FALSE}
# Haut de page -------------------
---
title: "Analyse de spectres \nMélèze Projet Presto"
author: 
    "David Chassagnaud"
date: "`r Sys.Date()`"
output:
  rmdformats::downcute:
      code_folding: hide
      lightbox: TRUE
      gallery: TRUE
      toc_depth: 3
  default_style: light
  distill::distill_article: default
  html_document:
    includes:
      downcute_theme: default
      after_body: footer.html
---

<a id="header"></a>
        
# Bas de page --------------------

&nbsp;
<hr />
<p style="text-align: center;">A work by <a href="https://www.univ-orleans.fr/fr/lblgc/david-chassagnaud">David Chassagnaud</a></p>
<p style="text-align: center;"><span style="color: #808080;"><em>david.chassagnaud@gmail.com</em></span></p>

<!-- Add icon library -->
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">

<!-- Add font awesome icons -->
<p style="text-align: center;">
    <a href="https://www.linkedin.com/in/davidchassagnaud/?originalSubdomain=fr" class="fa fa-linkedin"></a>
</p>

&nbsp;
```





