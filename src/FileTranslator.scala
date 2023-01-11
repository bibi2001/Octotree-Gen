import javafx.scene.Group
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.scene.shape._

import java.io.FileNotFoundException
import scala.annotation.tailrec
import scala.io.Source

//T1 class:
object FileTranslator {

  //addShapes: adds the shapes in the list (arg 1) to the group (arg 2)
  @tailrec
  def addShapes(l:List[Shape3D], g:Group): Group = l match {
    case Nil => g
    case h :: t => g.getChildren.add(h); addShapes(t,g)
  }

  //readFromFile receives the file's name (arg 1) and, if valid, returns a list of shapes
  def readFromFile(file: String):List[Shape3D] = {
    try Source.fromFile(file) catch { //if invalid prints warning message
      case _: FileNotFoundException =>
        print("file_name.txt invalid!")
        System exit (-1)
    }

    var models = List[String]()
    val bufferedSource = Source.fromFile(file)
    for (line <- bufferedSource.getLines)
      models = line :: models
    bufferedSource.close()
    models.map(t => toShapeConverter(t))
  }

  //toShapeConverter: transforms a line (arg 1) into a shape. Useful function to use when reading the file
  def toShapeConverter(line: String): Shape3D = {
    val model = line.split(Array('(', ',', ')', ' '))
    def aux(figure:Shape3D):Shape3D = {
      //set material
      val material = new PhongMaterial ()
      material.setDiffuseColor (Color.rgb (model (2).toInt, model (3).toInt, model (4).toInt) )
      figure.setMaterial (material)
      //set translate
      figure.setTranslateX (model (6).toInt)
      figure.setTranslateY (model (7).toInt)
      figure.setTranslateZ (model (8).toInt)
      //set scale
      figure.setScaleX (model (9).toDouble)
      figure.setScaleY (model (10).toDouble)
      figure.setScaleZ (model (11).toDouble)
      figure
    }
    model(0) match { //match figure name
      case "Cylinder" => aux(new Cylinder(0.5,1,10))
      case "Box" => aux(new Box(1,1,1))
    }
  }
}
