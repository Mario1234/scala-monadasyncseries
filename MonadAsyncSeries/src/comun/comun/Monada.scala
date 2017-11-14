package comun.comun

trait Monada[COLECCION[_]] extends Aplicativo[COLECCION] {
  //constructor unidad monadico
  //dado un elemento crea la coleccion inicial
  def unidadInterno[TIPO1](elemento: TIPO1): COLECCION[TIPO1]
  
  //no se usa?
  //la monada se diferencia del aplicativo en que tiene operacion redux o flat
  //def reduceInterno[TIPO1](coleccionEntrada1: COLECCION[COLECCION[TIPO1]]): COLECCION[TIPO1]  
  
  //lo hereda del aplicativo padre
  def map2Interno[TIPO1,TIPO2,TIPO3](coleccionEntrada1: COLECCION[TIPO1], coleccionEntrada2: COLECCION[TIPO2])(funcion1: (TIPO1,TIPO2) => TIPO3): COLECCION[TIPO3]
  
  //esto es del functor abuelo
  def mapInterno[TIPO1,TIPO2](coleccionEntrada: COLECCION[TIPO1])(funcion1: TIPO1 => TIPO2): COLECCION[TIPO2]
}