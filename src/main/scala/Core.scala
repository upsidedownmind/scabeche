import java.io._

object Core {
  
    val cache = collection.mutable.Map[String, String]()
    val CmdParser = "([^ ]+)\\s?([^ ]+)?\\s?(.*)".r;

    case class CacheCommand(cmd:String, key:String, value:String, fnc:(String, String)=>String)

    implicit def str2Command(str:String) : CacheCommand = str match  {
        case CmdParser(word, key, value) => word toLowerCase match {
          case "set" => new CacheCommand(word, key, value, (k:String,v:String)=> {cache +=(k->v); "STORED"} )
          case "get" => new CacheCommand(word, key, value, (k:String,v:String)=> {"VALUE " + cache.getOrElse(k, "")} )
          case "list" => new CacheCommand(word, key, value, (k:String,v:String)=> {
            cache.foldLeft("")( (acc, kv) => acc + "TUPLE " + kv._1 + " " + kv._2 + "\r\n")
          } )
          case "size" => new CacheCommand(word, key, value, (k:String,v:String)=> { "TUPLES " + cache.size} )
          case default => new CacheCommand(word, key, value, "cmd: k " + _ + ", v" + _ )
        }
        case "" => new CacheCommand("ERROR", "", "", (k:String,v:String)=>"ERROR" ) 
        case nop => new CacheCommand("ERROR", "", "", (k:String,v:String)=>"ERROR:" + str)
      
    }

    def loopCommands(cmd:CacheCommand, in:BufferedReader, out:PrintWriter) : Unit = cmd match {
      case CacheCommand("quit", k, v, f) => out.println( "QUIT" ); 
      case CacheCommand(n, k, v, f) =>
                        out.println( f(k,v) );
                        loopCommands(in readLine, in, out)
    }

}
