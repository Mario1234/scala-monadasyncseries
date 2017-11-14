package comun.comun

import monix.eval.Task

class TareaMonada extends Monada[Task] {

  //heredado de monada, lo sobredefine
  def unidadInterno[A](a: A): Task[A] ={
    Task.pure(a);
  }
  
  //no se usa?
//  def flatMap[TIPO1,TIPO2](tareaEntrada1: Task[TIPO1])(funcion1: TIPO1 => Task[TIPO2]): Task[TIPO2] ={
//    tareaEntrada1.flatMap(funcion1);
//  }

  def map2Interno[TIPO1,TIPO2,TIPO3](tareaEntrada1: Task[TIPO1], tareaEntrada2: Task[TIPO2])(funcion1: (TIPO1,TIPO2) => TIPO3)
      : Task[TIPO3] ={
    for (elemTipo1 <- tareaEntrada1; elemTipo2 <- tareaEntrada2) 
      yield funcion1(elemTipo1,elemTipo2);
  }
  
  def mapInterno[TIPO1,TIPO2](tareaEntrada1: Task[TIPO1])(funcion1: TIPO1 => TIPO2): 
    Task[TIPO2] ={
      tareaEntrada1.map(funcion1);
    }
}