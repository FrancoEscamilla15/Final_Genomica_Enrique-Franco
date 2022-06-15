####Enrique Franco Garcia####
####Problema 2####

#Cargo las librerias que voy a ocupar
library (phyloseq)
library (ggplot2)

psd <- readRDS("ps.RDS")
psd
#Cargo un archivo phyloseq que ya había generado para poder usarlo

prevdf <- apply(X = otu_table(psd),
                MARGIN = ifelse(taxa_are_rows(psd), yes = 1, no = 2),
                FUN = function(x){sum(x > 0)})
prevdf
#Esto es para la identificación taxonomica con base en el gen 16s ribosomal


prevdf <- data.frame(Prevalence = predf,
                     TotalAbundance = taxa_sums(psd),
                     tax_table(psd))
#Con esto le agrego la taxonomía


filterPhyla <- c("BRC1", "Deinococcus-Thermus", "Gemmatimonadetes", "Kiritimatiellaeota", "Nanoarchaeaeota", "Ochrophyta", "Schekmanbacteria", "Ciliophora", "Spirochaetes", NA)
#Esto me sirve para definir la taxa a filtrar

(psd1 = subset_taxa(psd, !Phylum %in% filterPhyla))
#Con esto sigo filtrando

psd2 <- filter_taxa(psd1, function(x) mean(x) > 1e-5, TRUE)
# Filtro la taxa de acuerdo a un umbral de numero medio de _read counts_, en este caso 1e-5

psd3 <- filter_taxa(psd2, function(x) sum(x > 2) > (0.1*length(x)), TRUE)
#Con esto elimino la taxa que no se observa mas de X veces en al menos 10% de las muestras

psd4 <- prune_samples(sample_sums(psd3) > 1000, psd3)
psd4
#Con esto lo único que hago es filtrar muestras con menos de 1000 reads


(prevalenceThreshold = 0.05 * nsamples(psd4))
#Con esto lo que hago es definir el umbral de prevalencia al 5%


####1. Calcule distintas medidas de diversidad ####
plot_richness (psd4, color = "species", x = "species")
#Esto lo hago con la función plot_richness, esta función sirve para observar 
#medidas de diversidad

####2. Elabore una grAfica de barras de abundancias por muestras####

psd4.moraxella <- subset_taxa(psd4, Genus=="Moraxella")
#Lo primero que hago es seleccionar un genero, para este caso use "Moraxella"


psd4.moraxella <- prune_samples(sample_sums(psd4.moraxella)>=5, psd4.moraxella)
#Ahora lo que hago es uqedarme con las muestras que solo cumplen con 
#tener una abundancia mayor a 5 reads

plot_tree(psd4.moraxella, color="species", shape="Family", label.tips="Genus", size="abundance")
#Por último lo grafico


####3. Elabore un an?lisis de reducci?n de dimensionalidad####
ord.nmds.bray <- ordinate(psd, method="NMDS", distance="bray") 
#Lo primero que se hace es ordenar las dimensiones con NMDS asi como la distancia ente las muestras
#para así posteriormente toma en cuenta los conteos transformados
ord.nmds.bray

####4.  Muestre el microbioma core de las muestras####

prevdf1 <- subset(prevdf, Phylum %in% get_taxa_unique(psd4, "Phylum"))
#Primero selecciono la taxa de interes

ggplot(prevdf1, aes(TotalAbundance, Prevalence / nsamples(psd),color=Phylum)) +
  geom_hline(yintercept = 0.08, alpha = 0.10, linetype = 2) +  geom_point(size = 2, alpha = 0.7) +
  scale_x_log10() +  xlab("Abundancia") + ylab("Prevalencia") +
  facet_wrap(~Phylum) + theme(legend.position="none")
#Creo la grafica

# Lo que se hizo aqui es que con ayuda de ggplot2 se realizo una grafica, al inicio, te pide de donde sacar dicha informacion, que es de nuestros datos
# despues colocas que es lo que vas a estar comparando, es decir, prevalencia y el total de abundancia, se van a separar los colores por Phylum
# y xlab y ylab unicamente es para especificar el nombre con el cual se vera en la grafica
# al final sale: facet_wrap que hace referencia a que cada muestra tenga su propio grafico.

####5. Ejercicio opcional - genere redes de co-abundacia por muestra.####

library(devtools)
install_github("zdk123/SpiecEasi")
library(SpiecEasi)
#Lo hice con ayuda del trabajo de una chica en GitHub

red.psd <- spiec.easi(psd, method='mb', lambda.min.ratio=1e-2,
                         nlambda=20, icov.select.params=list(rep.num=50))

ig2.mb <- adj2igraph(se.mb.ps$refit,  vertex.attr=list(name=taxa_names(ps)))

plot_network(ig2.mb, ps, type='taxa', color="Phylum")
#Esta es la red de coabundancia