package comun.comun

trait Functor [COLECCION[_]] {
  //map recive una coleccion de entrada coleccionEntrada del tipo COLECCION[TIPO1]
  //y devuelve una coleccion de salida del tipo COLECCION[TIPO2]
  //aplicando una funcion funcion1 del tipo TIPO1 => TIPO2
  def mapInterno[TIPO1,TIPO2](coleccionEntrada: COLECCION[TIPO1])(funcion1: TIPO1 => TIPO2): COLECCION[TIPO2]
}