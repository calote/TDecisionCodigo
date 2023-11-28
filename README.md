# Introducción


En este repositorio **"TDecisionCodigo"** se mantendrán actualizados todos los scripts R que se usan para la asignatura "Teoría de la Decisión" del Grado en Estadística de la Universidad de Sevilla.


Se persiguen varios objetivos con este repositorio:

1. Encontrar fácilmente el código actualizado que se utiliza en esta asignatura.

2. Conocer e interactuar con el proceso del control de versiones de scripts de un código con el sistema Git-Github.

# Organización

Los scripts R asociados a cada tema son:

- **Tema 1: "Incertidumbre"**
    
    + "teoriadecision_funciones_incertidumbre.R" 

- **Tema 2: "Multicriterio"**
    
    + "teoriadecision_funciones_multicriterio.R" 
    + "teoriadecision_funciones_multicriterio_diagram.R" 
    + "teoriadecision_funciones_multicriterio_utiles.R" 

- **Tema 4: "Bayesiana"**
    
    + "teoriadecision_funciones_bayesiana.R" 

# Qué paquetes o librerías R hay que instalar

Para el uso de las funciones definidas en los scripts R para el tema 2: "Multicriterio", 
hacer la siguiente instalación ejecutando el siguiente código en la consola de R-RStudio:

```r
install.packages("formattable")
install.packages("webshot")
webshot::install_phantomjs()
install.packages("devtools")  
install.packages("kableExtra")
# Si hay problemas con kableExtra instalarlo con el siguiente comando
# devtools::install_github("kupietz/kableExtra")
install.packages("factoextra")
install.packages("diagram")
install.packages("qgraph")
devtools::install_github("calote/ahp")
```

También puede ser necesario instalar los siguientes paquetes si no estuviesen ya instalados:

```r
install.packages("knitr")
install.packages("tidyverse")
```

**Nota 1:** Si tiene problemas con la instalación de "factoextra", no lo intente más, pero abra el fichero: "teoriadecision_funciones_multicriterio_utiles.R" y comente la línea 5 que hace referencia a ese paquete colocando un hastag `#` al inicio de la línea.

# Qué hacer si se detectan errores

Varios métodos:

- Crear un "Issues" ("propuesta") específicando dónde se ha encontrado la errata.

- Crear un "Pull requests" (PR) ("solicitud de cambios"). 

