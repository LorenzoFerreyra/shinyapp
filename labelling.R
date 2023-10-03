library(stringdist)
library(dplyr)



label_path <- "data/etiquetado.csv"
df <- read.csv(label_path)

print(head(df))


umbral_similitud <- 0.78

## We apply Levenshtein distance. 
## WIKIPEDIA: The Levenshtein distance is a string metric
##for measuring the difference between two sequences.
### Informally, the Levenshtein distance between two words is
## the minimum number of single-character edits
## (insertions, deletions or substitutions)
## required to change one word into the other.
encontrar_fuzzy_match <- function(palabra) {
  similitudes <- stringdistmatrix(palabra, df$etiqueta, method = "lv")
  similitud_maxima <- max(similitudes)
  
  if (similitud_maxima >= umbral_similitud) {
    etiqueta_correspondiente <- df$etiqueta[which.max(similitudes)]
    return(etiqueta_correspondiente)
  } else {
    return(NA)
  }
}


df$etiqueta <- sapply(df$palabra, encontrar_fuzzy_match)

## En la próxima sesión, hacer una tabla dinámica con las etiquetas y que estas se conviertan en el nombre del conjunto que contiene las palabras.
## Así, el fuzzy matching debe hacerse sobre todas las palabras que pertenecen al conjunto y no solo con el nombre del título del conjunto.


