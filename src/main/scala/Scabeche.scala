import java.io._
import java.net.{InetAddress,ServerSocket,Socket,SocketException}

import scala.actors.Actor
import scala.actors.Actor._

case class CacheCommand(cmd:String, key:String, value:String, fnc:(String, String)=>String)

object Scabeche extends App {

    val cache = collection.mutable.Map[String, String]()
    val CmdParser = "([^ ]+)\\s?([^ ]+)?\\s?(.*)".r;


    oneShotServer()


    def loopCommands(cmd:CacheCommand, in:BufferedReader, out:PrintWriter) : Unit = cmd match {
      case CacheCommand("quit", k, v, f) => out.println( "QUIT" ); 
      case CacheCommand(n, k, v, f) =>
                        out.println( f(k,v) );
                        loopCommands(in readLine, in, out)
    }


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
    
  
    def oneShotServer() = {
      try {
        val listener = new ServerSocket(5544);
        val client = listener.accept()
      
        val out = new PrintWriter(client.getOutputStream(), true)
        val in = new BufferedReader(new InputStreamReader(client.getInputStream()), 3 )

        loopCommands(in readLine, in, out )
     
        out.close
        in.close

        listener.close()
    
      } catch {
        case e: IOException =>
          System.err.println("Could not listen on port: 5544.");
            System.exit(-1)
      }

    }

}
