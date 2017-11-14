package comun.comun

import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
//no se usa
//para no tener que poner la palabra implicit delante de cada metodo de TareaMonada
//object objTareaMonada {
//  implicit def instance: TareaMonada =  new TareaMonada
//}

object Ejecutor extends App {
  val Capacidad = 10000;
  val Valores = List(0,9,13,153,50,15,68,27,39,23,52,11,32,24,48,73,42,43,22,7,18,4,30);
  val Pesos = List(0,150,35,200,160,60,45,60,40,30,10,70,30,15,10,40,70,75,80,20,12,50,10);
  
  //funcion resuelve problema de la mochila
  def m(numeroElementos:Int, capacidad:Int):Int ={
    if(numeroElementos<0){
      return 0;
    }else if(capacidad==0){
      return 0;
    }else if(Pesos(numeroElementos)>capacidad){
      return m(numeroElementos-1,capacidad);
    }else{
      val sinoMete = m(numeroElementos-1,capacidad);
      val siMete = m(numeroElementos-1,capacidad-Pesos(numeroElementos))+Valores(numeroElementos);
      return math.max(sinoMete, siMete);
     }
  }
  
//  println(m(Pesos.length-1,Capacidad));
  
  //-------------------IMPLAMENTACION APLICATIVO TAREA---------------------------
   
  //el caso especifico de utilizar el aplicativo sobre una coleccion del tipo Tarea, Task
  implicit object TareaAplicativo extends Aplicativo[Task]{
    def unidadInterno[TIPO1](a: TIPO1): Task[TIPO1] ={
      Task.now(a);
    }
    def map2Interno[TIPO1,TIPO2,TIPO3]
        (tareaEntrada1: Task[TIPO1], tareaEntrada2: Task[TIPO2])(funcion1: (TIPO1,TIPO2) => TIPO3)
        : Task[TIPO3] ={
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
  
  //---------------AUTOINFIERE funciones de APLICATIVO-----------------
  
  //estas declaraciones nos dan la opcion de utilizar el map2 del aplicativo sobre distintos tipos de colecciones
  //necesita 2 colecciones como parametros
  implicit def llamadasImplicitasAplicativo2col[COLECCION[_],TIPO1,TIPO2,TIPO3](col1: COLECCION[TIPO1], col2:  COLECCION[TIPO2]) = new {    
    def llamadaImplicitaMap2
      (funcion: (TIPO1,TIPO2) => TIPO3)(implicit aplicativo: Aplicativo[COLECCION])//instancia de manera implicita
        : COLECCION[TIPO3] ={
      return aplicativo.map2Interno(col1,col2)(funcion);
    }        
  }
  //necesita 1 coleccion como parametros
  implicit def llamadasImplicitasAplicativo1col[COLECCION[_],TIPO1,TIPO2](col1: COLECCION[TIPO1]) = new {      
    def llamadaImplicitaMap
      (funcion: TIPO1 => TIPO2)(implicit aplicativo: Aplicativo[COLECCION])//instancia de manera implicita
        : COLECCION[TIPO2] ={
      return aplicativo.mapInterno(col1)(funcion);
    }        
  }  
  //necesita 1 lista de colecciones como parametros
  implicit def llamadasImplicitasAplicativoLista[COLECCION[_],TIPO1](lista1: List[COLECCION[TIPO1]]) = new { 
    //aniadimos una funcion no definida en la interfaz aplicativo
    //recibe una lista de colecciones de cosas y devuelve una coleccion con una lista de cosas
    def llamadaImplicitaSecuencia(implicit aplicativo: Aplicativo[COLECCION])
        : COLECCION[List[TIPO1]] = {
      //return aplicativo.secuenciaInterno(lista1);
      val semilla = aplicativo.unidadInterno(List.empty[TIPO1]);
      
      //concatena una cosa a la lista de cosas
      def concatenar(cosa:TIPO1, lista:List[TIPO1]) :List[TIPO1] ={
        return cosa :: lista;
      }
      
      //acumula las cosas en la coleccionListaCosas(coleccion con lista de cosas)
      //concatenando cada cosa (de coleccion1cosa) a la lista
      def agregar(coleccionListaCosas:COLECCION[List[TIPO1]], coleccion1cosa:COLECCION[TIPO1]) :COLECCION[List[TIPO1]] ={
        return aplicativo.map2Interno(coleccionListaCosas,coleccion1cosa)((lista,cosa) => concatenar(cosa,lista));
      }
      //recorre la lista de colecciones con cosa dentro, agrupa todas las cosas de todas las colecciones en una lista dentro de una unica coleccion
      //coleccionListaCosas es de tipo COLECCION[List[TIPO1]] y coleccion1cosa es de tipo COLECCION[TIPO1]
      val resultado = lista1.foldLeft(semilla)((coleccionListaCosas,coleccion1cosa) =>agregar(coleccionListaCosas,coleccion1cosa) );
      return aplicativo.mapInterno(resultado)(_.reverse);//revierte el orden de la lista
    }
  }
  
  //no necesita coleccion como parametro
  implicit def llamadasImplicitasAplicativoLista[COLECCION[_],TIPO1] = new { 
    def llamadaImplicitaUnidad[TIPO1](elemento: TIPO1)
       (implicit aplicativo: Aplicativo[COLECCION])//instancia de manera implicita
        : COLECCION[TIPO1] ={
      return aplicativo.unidadInterno(elemento);
    }   
  }
  
  //---------------AUTOINFIERE funciones de MONADA-----------------
  
  //estas declaraciones nos dan la opcion de utilizar la monada sobre distintos tipos de colecciones
  implicit def llamadasImplicitasMonada[COLECCION[_],TIPO1,TIPO2](col1: COLECCION[TIPO1], col2:  COLECCION[TIPO2]) = new {    
    def llamadaImplicitaUnidad[TIPO1](elemento: TIPO1)
       (implicit monada: Monada[COLECCION])//instancia monada de manera implicita
        : COLECCION[TIPO1] ={
      return monada.unidadInterno(elemento);
    }
    def llamadaImplicitaMap[TIPO2]
      (funcion: TIPO1 => TIPO2)(implicit monada: Monada[COLECCION])//instancia monada de manera implicita
        : COLECCION[TIPO2] ={
      return monada.mapInterno(col1)(funcion);
    }
    def llamadaImplicitaMap2[TIPO3]
      (funcion: (TIPO1,TIPO2) => TIPO3)(implicit monada: Monada[COLECCION])//instancia monada de manera implicita
        : COLECCION[TIPO3] ={
      return monada.map2Interno(col1,col2)(funcion);
    }    
  }
  
  //---------------------------------------
  //----------------EJECUCION--------------
  //tarea1 : Task[Int]
  val tarea1 = Task{m(Pesos.length-1,Capacidad)};
  val tarea2 = Task{m(Pesos.length-1,600)};
  val tarea3 = Task{m(Pesos.length-1,400)};
  //listaTareas : List[Task[Int]]
  val listaTareas = List(tarea1, tarea2, tarea3);
  //tareaListaAgregadas : Task[List[Int]]
  //val tareaListaAgregadas = listaTareas llamadaImplicitaSecuencia;
  (listaTareas llamadaImplicitaSecuencia).foreach(println);
  //al realizar la ejecucion de la lista de tareas de manera "monadica-secuencial"
  //conseguimos asegurar que preserva el orden
  Thread.sleep(1000);
}

