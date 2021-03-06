import java.io._

/**
 * logica principal
 */
object Core {
  
    //el "cache"
    val cache = collection.mutable.Map[String, String]()
    
    /**
     * comandos desde el cache, esta clase contiene la funcion que atiende un comando especifico
     */
    class CacheCommand(val cmd:String, val key:String, val value:String, val fnc:(String, String)=>String)

    /**
     * object para manejar la classe de comandos de una forma mas amena
     */
    object CacheCommand {

      val CmdParserRE = "([^ ]+)\\s?([^ ]+)?\\s?(.*)".r;
      
      //constructor simple
      def apply(word:String, key:String, value:String, fnc:(String, String)=>String) = new CacheCommand(word, key, value, fnc)
      def apply(word:String, key:String, fnc:(String, String)=>String) = new CacheCommand(word, key, null, fnc)
      def apply(word:String, fnc:(String, String)=>String) = new CacheCommand(word, null, null, fnc)

      //splitea el string y decide la funcion
      def apply(str:String) : CacheCommand = str match  {
        case CmdParserRE(word, key, value) => word toLowerCase match {
          case "set" => CacheCommand(word, key, value, (k:String,v:String)=> {cache +=(k->v); "STORED"} )
          case "get" => CacheCommand(word, key, (k:String,v:String)=> {"VALUE " + cache.getOrElse(k, "")} )
          case "remove" => CacheCommand(word, key, (k:String,v:String)=> {cache -= k; "REMOVED"} )
          case "list" => CacheCommand(word, (k:String,v:String)=> {
            cache.foldLeft("")( (acc, kv) => acc + "TUPLE " + kv._1 + " " + kv._2 + "\r\n")
          } )
          case "size" => CacheCommand(word, (k:String,v:String)=> { "TUPLES " + cache.size} )
          case default => CacheCommand(word, key, value, "cmd: k " + _ + ", v" + _ )
        }
        case "" => CacheCommand("ERROR", (k:String,v:String)=>"ERROR" ) 
        case nop => CacheCommand("ERROR", (k:String,v:String)=>"ERROR:" + str)
      
      }

      //esto es util para el case
      def unapply(aCmd:CacheCommand) = Some(aCmd.cmd,aCmd.key,aCmd.value,aCmd.fnc)
    }
  
    //helper para los comandos
    implicit def str2Command(str:String) = CacheCommand(str)  

    /**
     *bucle princiapal, una funcion recursiva que atiende los comandos
     */
    def loopCommands(cmd:CacheCommand, in:BufferedReader, out:PrintWriter) : Unit = cmd match {
      case CacheCommand("quit", k, v, f) => out.println( "QUIT" ); 
      case CacheCommand(n, k, v, f) =>
                        out.println( f(k,v) );
                        //el llamado va ultimo para usar tail recursion
                        loopCommands(in readLine, in, out)
    }

}
