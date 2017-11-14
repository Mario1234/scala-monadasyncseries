package comun.comun

import monix.eval.Task

//el caso especifico de utilizar el aplicativo sobre una coleccion del tipo Tarea, Task
class TareaAplicativo extends Aplicativo[Task]{
    def unidadInterno[TIPO1](a: TIPO1): Task[TIPO1] ={
      Task.now(a);
    }
    def map2Interno[TIPO1,TIPO2,TIPO3]
        (tareaEntrada1: Task[TIPO1], tareaEntrada2: Task[TIPO2])(funcion1: (TIPO1,TIPO2) => TIPO3)
        : Task[TIPO3] ={
      //esto hace un flatmap(map());
      for (elemTipo1 <- tareaEntrada1; elemTipo2 <- tareaEntrada2) 
        yield funcion1(elemTipo1,elemTipo2);
    }    
    def mapInterno[TIPO1,TIPO2](tareaEntrada1: Task[TIPO1])(funcion1: TIPO1 => TIPO2): 
    Task[TIPO2] ={
      tareaEntrada1.map(funcion1);
    }    
    def secuenciaInterno[TIPO1]
      (lista1: List[Task[TIPO1]])
        : Task[List[TIPO1]] = {
      val semilla = unidadInterno(List.empty[TIPO1]);
      
      //concatena una cosa a la lista de cosas
      def concatenar(cosa:TIPO1, lista:List[TIPO1]) :List[TIPO1] ={
        return cosa :: lista;
      }
      
      //acumula las cosas en la tarea de lista de cosas
      //concatenando cada cosa (de coleccion1cosa) a la lista
      def agregar(tareaListaCosas:Task[List[TIPO1]], tarea1cosa:Task[TIPO1]) :Task[List[TIPO1]] ={
        return map2Interno(tareaListaCosas,tarea1cosa)((lista,cosa) => concatenar(cosa,lista));
      }
      //recorre la lista de tareas con cosa dentro, agrupa todas las cosas de todas las tareas en una lista dentro de una unica tarea
      //tareaListaCosas es de tipo Task[List[TIPO1]] y tarea1cosa es de tipo Task[TIPO1]
      val resultado = lista1.foldLeft(semilla)((tareaListaCosas,tarea1cosa) =>agregar(tareaListaCosas,tarea1cosa) );
      return mapInterno(resultado)(_.reverse);
    }
}