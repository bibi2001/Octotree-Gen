import javafx.scene.{Group, Node}
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.scene.shape.{Box, DrawMode, Shape3D}

import scala.jdk.StreamConverters.StreamHasToScala

object ColorUtils {
  //Auxiliary types
  type Point = (Double, Double, Double)
  type Size = Double
  type Placement = (Point, Size)
  type Section = (Placement, List[Node])

  //T3
  def colorNode(material:PhongMaterial,node:Node):Node={
    node.asInstanceOf[Shape3D].setMaterial(material)
    node
  }
  def intersectsField(node:Node, volume:Shape3D):Boolean={
    node.asInstanceOf[Shape3D].getBoundsInParent.intersects(volume.getBoundsInParent)
  }
  def isWiredBox(node:Node):Boolean={
    if(node.isInstanceOf[Box]) node.asInstanceOf[Shape3D].getDrawMode.equals(DrawMode.LINE) else false
  }
  def colorInField(l:List[Node],volume:Shape3D,material: PhongMaterial):List[Node]= {
    l.filter(n =>isWiredBox(n)).map(n =>
      if(intersectsField(n.asInstanceOf[Shape3D],volume)) colorNode(rndMaterial(), n)
      else colorNode(material,n))
  }

  //T5
  def mapColourEffect(func: Color => Color, oct:Octree[Placement]): Octree[Placement]= {
    def applyFunc(l:List[Node]):List[Node]={
      l.map(n => {
        val r = n.asInstanceOf[Shape3D].getMaterial.asInstanceOf[PhongMaterial].getDiffuseColor.getRed
        val g = n.asInstanceOf[Shape3D].getMaterial.asInstanceOf[PhongMaterial].getDiffuseColor.getGreen
        val b = n.asInstanceOf[Shape3D].getMaterial.asInstanceOf[PhongMaterial].getDiffuseColor.getBlue
        n.asInstanceOf[Shape3D].setMaterial(new PhongMaterial(func(Color.color(r,g,b)))); n})
    }
    oct match {
      case OcEmpty => OcEmpty
      case OcNode(a, b, c, d, e, f, g, h, i) => OcNode(a, mapColourEffect(func,b), mapColourEffect(func,c),
        mapColourEffect(func,d), mapColourEffect(func,e), mapColourEffect(func,f), mapColourEffect(func,g),
        mapColourEffect(func,h), mapColourEffect(func,i))
      case OcLeaf(section:Section) => OcLeaf(section._1,applyFunc(section._2))
    }
  }

  def rndMaterial(): PhongMaterial={
    val rnd = scala.util.Random
    val material = new PhongMaterial()
    material.setDiffuseColor(Color.rgb(rnd.nextInt(250), rnd.nextInt(250), rnd.nextInt(250)))
    material
  }
  def sepia(color: Color):Color={
    val r = color.getRed; val g = color.getGreen; val b = color.getBlue
    val newR = 0.4*r + 0.77*g + 0.2*b
    val newG = 0.35*r + 0.69*g + 0.17*b
    val newB =0.27*r + 0.53*g + 0.13*b
    if(newR<=1.0 && newR>=0.0 && newG<=1.0 && newG>=0.0 && newB<=1.0 && newB>=0.0) Color.color(newR,newG,newB)
    else color
  }
  def greenRemove(color: Color):Color = {
    val r = color.getRed
    val b = color.getBlue
    Color.color(r,0,b)
  }

  def getWiredBoxesColour(g:Group): PhongMaterial = {
    g.getChildren.stream().toScala(List).filter(n=> isWiredBox(n)).head
      .asInstanceOf[Shape3D].getMaterial.asInstanceOf[PhongMaterial]
  }
}