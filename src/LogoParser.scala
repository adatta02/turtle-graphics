import scala.swing.SimpleSwingApplication
import scala.swing.Swing._
import scala.swing.{MainFrame, Panel}
import scala.swing.event._
import javax.swing.Timer
import java.awt.{Color, Graphics2D, Point, geom}
import java.awt.event.{ActionEvent, ActionListener}
import java.nio.file._
import scala.collection.JavaConversions._
import scala.sys.process._

object LogoParser extends SimpleSwingApplication {
  
  object TurtleShape extends java.awt.geom.Path2D.Double {    
    moveTo(0, 0)
    
    lineTo(20, 0)
    lineTo(20, 20)
    lineTo(0, 20)
    
    closePath()        
  }  
    
  lazy val ui = new Panel with ActionListener {
    
    background = Color.white
    preferredSize = (800, 600)
    focusable = true
    
    val input = """
      setpos 100 200
      pendown
      up 100 right 100 down 100 left 100 penup
      """
    val turtleActions = TurtleScriptParser.parseAll(TurtleScriptParser.parseTurtleExpressions, input)
    
    println(turtleActions)    
    if(turtleActions.isEmpty){
      throw new Exception("Could not parse input")
    }    
        
    val parsedTurtles = turtleActions.get.foldLeft( List[Turtle](Turtle()) )((list, turtleExpression) => {      
      list ::: turtleExpression.updateTurtle(list.last)
    })        
    
    println(parsedTurtles)
    
    var actionIndex = 1
    def actionPerformed(e: ActionEvent) {
      actionIndex += 1      
      if( actionIndex <= parsedTurtles.size ){
    	  repaint()
      }
    }
        
    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      
      val currentTurtles = parsedTurtles.take(actionIndex)            
      val pathTurtles = currentTurtles.foldLeft((new geom.GeneralPath(), List[Turtle]()))((p, turtle) => {
        if( p._2.size == 0 || p._2.last.penDown == false ){
          p._1.moveTo(turtle.position._1, turtle.position._2)  
        }else{
          p._1.lineTo(turtle.position._1, turtle.position._2)  
        }
                       
        (p._1, p._2 :+ turtle)
      })
      
      println(currentTurtles)
      
      g.setColor(Color.BLACK)
      g.draw(pathTurtles._1)            
      
      g.setColor(Color.BLUE)
      g.translate(currentTurtles.last.position._1, currentTurtles.last.position._2)
      g.fill(TurtleShape)
            
      g.dispose()      
    }
    
  } 
  
  def top = new MainFrame {
    title = "Turtle Logo Demo"
    contents = ui      
  }
  
  val timer = new Timer(500, ui)
  timer.start()
  
  def startWatcher: Unit = {
    
	  val fileWatcher = new Thread(new Runnable {
	    def run(){
	      
	      val watcher = FileSystems.getDefault.newWatchService
	      val file = Paths.get("/home/ashish/Downloads/")
	      
	      file.register(watcher, StandardWatchEventKinds.ENTRY_MODIFY)
	      
	      val key = watcher.take
	      val events = key.pollEvents
	      
	      val e = events.toList
	      				.map(a => a.context().asInstanceOf[Path].toAbsolutePath.endsWith("logo_test.txt"))
	      				.exists(_ == true)
	      println(e)
	      
	      startWatcher
	      // events.isEmpty
	      // key.reset
	    }
	  })
	  
	  fileWatcher.start
  }
  
  startWatcher
}