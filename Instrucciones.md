

### ¿Qué hace la aplicación?

A partir de los datos que se cargan sobre el tema de análisis realiza una proyección para los años siguientes al último año de la serie, con su región de confianza del 95%.
Los años para los cuales se calcula la proyección se deben definir en la aplicación; y, el cálculo de la proyección se puede realizar para un máximo de cinco años posteriores al último dato cargado.
La proyección se muestra de manera gráfica con la serie completa que se cargó y la proyección para el período seleccionado, incluyendo el valor y la región de confianza del 95%; y, se incluye una tabla con los datos proyectados para el período seleccionado.
Si la serie de datos sobre el tema de análisis incluye una cifra relativ
Se analiza la serie temporal desde el presente hacia el pasado con el objetivo de identificar posibles puntos de quiebre que definan el período a utilizar para la proyección; si no se identifica un punto de quiebre, se utiliza toda la serie para la proyección.


### ¿Qué procedimiento se sigue para hacer la proyección de la serie temporal?


Se realiza una exploración completa de la serie temporal cargada, analizándola desde el presente hacia el pasado con el objetivo de identificar posibles puntos de quiebre.
Para ello, se emplean herramientas estadísticas que permitieran detectar cambios estructurales.
En caso de encontrarse un quiebre estadísticamente significativo (se utiliza una significancia estadística de 0,05), la proyección se efectúa a partir de ese punto; de lo contrario, se utiliza la serie completa.
Para la proyección se utilizó el método de Holt sobre los datos observados para el período identificado; considerando el valor alfa el valor beta, calculados utilizando el Error Absoluto Medio Escalonado (MASE, por su sigla en inglés).
Para calcular la región de confianza de la proyección, se realiza en primer lugar una regresión no paramétrica con kernel gaussiano de los datos observados para el período identificado, que reduce el impacto del ruido residual y las fluctuaciones aleatorias de corto plazo y permite reestimar las observaciones y obtener una representación más estable del fenómeno. Esta técnica es especialmente útil ante posibles no linealidades o patrones irregulares, ya que no impone una forma funcional fija.
En segundo lugar, se utiliza el método de Holt sobre los datos suavizados obtenidos del procedimiento anterior y se obtiene una nueva serie que se utiliza para calcular los residuos.
La variabilidad de los residuos se utiliza para calcular la región de confianza de los datos proyectados.


### Instrucciones para la carga de los datos y para el cálculo de la proyección:

- Paso 1

Construya una tabla en un archivo de Excel que debe tener cuatro columnas, tituladas AÑO, POBLACION, CASOS y TASA, con por lo menos diez renglones con los valores para cada año.
Si cuenta con los datos de POBLACIÓN del período para el cual realizará la proyección, construya una tabla adicional con dos columnas tituladas AÑO y POBLACION.
Si no cuenta con los datos de población para el período de la proyección, la aplicación hará una proyección de los datos de población que incluyó en la primera tabla, utilizando mismo procedimiento que se emplea para la proyección de los datos cargados.
Las tablas no deben tener ni filas ni columnas vacías antes de la tabla; y, si la variable que se analiza tiene frecuencia cero en algún período, ese período debe omitirse en la tabla.

- Paso 2

Indique el lugar al que pertenecen los datos: país, estado, región, grupo poblacional. Esto se utilizará para el título del gráfico que confecciona la aplicación.

- Paso 3

Cargue la serie de tiempo utilizando el botón Browse de la aplicación, bajo el título “Cargar la serie de tiempo”; recuerde que, el archivo a cargar debe ser en formato .xlsx y debe tener la estructura mencionada en el Paso 1.

- Paso 4

Indique el período que quiere proyectar bajo el título Período de la proyección; seleccione el año inicial de la proyección y el año final.

- Paso 5

Indique si cuenta con los datos de la población para el período de la proyección que indicó en el paso anterior; si no tiene esos datos, deje indicado lo que muestra la aplicación, que asume que no cuenta con los datos de población para ese período. En ese caso, la aplicación estimará los datos de población proyectando los de la población cargada en el Paso 3.
Si tiene los datos, al marcar Si, se abrirá una nueva opción para que cargue la tabla con los datos; realice esta carga del mismo modo que en el Paso 3; y, también en este paso, recuerde que, el archivo a cargar debe ser en formato .xlsx y debe tener la estructura mencionada en el Paso 1.

- Paso 6

Si quiere realizar la proyección a partir de los datos anteriores, puede omitir las preguntas que siguen (están indicadas con respuesta negativa) y realizar la proyección presionando el botón al final de la pantalla (calcular proyección).
En este caso, la aplicación realizará la proyección sin considerar ningún cambio en la situación sino considerando que el evento evolucionará tal como lo viene realizando el último período.

- Paso 7

Si quiere realizar la proyección incorporando una brecha en el registro del evento -por ejemplo, si tiene información sobre el porcentaje de falta de registro o de detección del evento-, indique Sí a la pregunta ¿Existe una brecha entre los casos registrados y los existentes? Al hacerlo, se abrirá un recuadro para que indique el porcentaje de la brecha; esta cifra debe ser indicada como porcentaje -por ejemplo, 15- y no como proporción -en este ejemplo, 0,15-.
Si tiene previsto modificar la brecha que registró en la pregunta anterior, indque Sí a la pregunta ¿Tiene previsto reducir la brecha en el período de la proyección? Al hacerlo, se abrirá un recuadro para que indique el porcentaje de la brecha; esta cifra debe ser indicada como porcentaje -por ejemplo, 10- y no como proporción -en este ejemplo, 0,10-. También se abrirán dos opciones que debe completar, indicando el año en el que se iniciará la reducción indicada -este año, no puede ser el año inicial de la proyección indicado en el Paso 4; la aplicación hará el cálculo a partir del año siguiente al inicio de la proyección.
Una vez completados los pasos, presionando el botón al final de la pantalla (calcular proyección) se realizará la proyección con base en lo que se indicó previamente.

Puede copiar la figura con el mouse y copiar o descargar la tabla en los diferentes formatos que se indican.









