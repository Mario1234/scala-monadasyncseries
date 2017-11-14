package comun.comun

trait Aplicativo [COLECCION[_]] extends Functor[COLECCION] {
  //constructor unidad
  //dado un elemento crea la coleccion inicial
  def unidadInterno[TIPO1](elemento: TIPO1): COLECCION[TIPO1]

  //los aplicativos a diferencia de los functores pueden iterar sobre 2 o mas colecciones
  //recibe 2 coleccion como entrada, coleccionEntrada1 y coleccionEntrada2, del tipo COLECCION[TIPO1] y COLECCION[TIPO2] respectivamente
  //crea "pares" de elementos de ambas colecciones de entrada y aplica funcion1 al par
  //funcion1 devuelve TIPO3 y esta funcion map2 lo recoge en la coleccion de salida del tipo COLECCION[TIPO3]
  def map2Interno[TIPO1,TIPO2,TIPO3](coleccionEntrada1: COLECCION[TIPO1], coleccionEntrada2: COLECCION[TIPO2])(funcion1: (TIPO1,TIPO2) => TIPO3): COLECCION[TIPO3]

}