<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/tidy_eaii)](https://CRAN.R-project.org/package=tidy_eaii)

<!-- badges: end -->

# Presentación 

El objetivo de este paquete es brindar una herramienta para procesar encuestas provenientes del INE. Su desarrollo principal fue enfocado a la EAII (Encuesta de Actividades de inversión e Innovación). También puede utilizarse para la Encuesta Continua de Hogares (ECH) y la Encuesta de Nutrición, Desarrollo Infantil y Salud (ENDIS).

En el paquete se encuentran diferentes tipos de funciones : 

- ## Funciones \<easy\>

  Este tipo de funciones dependen esencialmente de la encuesta en formato tibble y la edición a la que refiere. Se debe de indicar la edición para fijar el ponderador de forma automática y el armado de variables de interés. Como resultado obtendremos las estimaciones poblacionales que hemos indicado.


- ## Funciones \<utils\>

  Las funciones utils refieren a aquellas funciones con las que el usuario puede cargar la encuesta, crear sus propias variables en base a la encuesta con la que este trabajando, fijar el ponderador, crear variables categóricas, convertir variables en moneda uruguaya a dolares en base a cotizaciones predefinidas y demás.


- ## Funciones \<stats\>

  Las mismas refieren a la parte de la estimación, son variaciones del estimador
  Horvitz–Thompson para poblaciones finitas. Actualmente se encuentran implementadas las siguientes variaciones : 

- ## Procesamiento interactivo
  
  Interfaz interactiva para armar variables y obtener estimaciones con salidas de código para aprender el uso de nuestro paquete.

  ### Interfaz interactiva

  ```r
  run_interactive.()
  ```

# Instalación 

```r
remotes::install_gitlab(
  "cognus/proyectos/anii/srvyuru", 
  auth_token = TOKEN
)
```
