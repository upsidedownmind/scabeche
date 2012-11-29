import java.io._
import java.net.{InetAddress,ServerSocket,Socket,SocketException}

object Servers {

  def simpleServer() ={

      val listener = new ServerSocket(5544)

      try {
         while (true) {
           val client = listener.accept()
         
           new Thread( new Runnable{
             override def run() = {
               val out = new PrintWriter(client.getOutputStream(), true)
               val in = new BufferedReader(new InputStreamReader(client.getInputStream()), 3 )

               Core.loopCommands(in readLine, in, out )
     
               out.close
               in.close
             }
           } ).start();
         }
         
      }  catch {
        case e: Exception =>
          e.printStackTrace()

      } finally {
        listener.close();
      } 

    }

  
    def oneShotServer() = {
      try {
        val listener = new ServerSocket(5544);
        val client = listener.accept()
      
        val out = new PrintWriter(client.getOutputStream(), true)
        val in = new BufferedReader(new InputStreamReader(client.getInputStream()), 3 )

        Core.loopCommands(in readLine, in, out )
     
        out.close
        in.close

        listener.close()
    
      } catch {
        case e: IOException =>
          System.err.println("Could not listen on port: 5544.");
        
      }

    }


}
