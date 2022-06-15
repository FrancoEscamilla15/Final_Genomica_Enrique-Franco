####Enrique Franco Garcia####
####Problema 3####

library (BoolNet) #cargamos la libreria
data ("cellcycle")
cellcycle #para visualizarla

####1. Encuentre los atractores####
atract_cell <- getAttractors (network = cellcycle) 
atract_cell
#con esta funcion observamos cuantos atractores hay
#en este caso son dos, ambos tienen una cuenca de atraccion de 512 y tienen 1 y 7estados

plotAttractors(atract_cell)
#con esta funcion dibujamos a los atractores

####2. Discuta el significado de los atractores####
#Los atractores son una propiedad emergente que lo que hace es que son valores 
#en los cuales un sistema tiende a caer, dadas varias condiciones iniciales 

####3. Describe  verbalmente al menos un par de reglas, (¡distintas a las que están en la ayuda-manual!)####
print (cellcycle) #para observar las reglas

#REGLAS:
# E2F = (! Rb & ! CycA & ! CycB) | (p27 & ! Rb & ! CycB)
# Para que se exprese E2F debe no estar presente Rb, CycA y CycB o debe estar presente p27 y que no esté presente Rb y CycB

###4.Selecciona distintos condiciones iniciales y determina a qué atractor se van####
getTransitionTable(atract_cell) #Esto me muestra una tabla para los atractores, en donde se pueden observar las transiciones de estados
