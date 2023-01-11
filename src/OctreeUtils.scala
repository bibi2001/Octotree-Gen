import javafx.scene.{Group, Node}
import javafx.scene.paint.PhongMaterial
import javafx.scene.shape.{Box, DrawMode}

import scala.annotation.tailrec

object OctreeUtils {
  //Auxiliary types
  type Point = (Double, Double, Double)
  type Size = Double
  type Placement = (Point, Size)
  type Section = (Placement, List[Node])

  //T2 functions:
  //hasNoIntersection: returns false if there is any Node in the list (arg 2) that intersects the Placement (arg 1).
  def hasNoIntersection(p:Placement, l:List[Node]):Boolean = {
    val box:Box = new Box(p._2,p._2,p._2)
    box.setTranslateX(p._2/2-p._1._1)
    box.setTranslateY(p._2/2-p._1._2)
    box.setTranslateZ(p._2/2-p._1._3)
    @tailrec
    def aux(l1:List[Node],acc:List[Node]):List[Node]= l1 match {
      case Nil => acc
      case h::t =>  if(h.getBoundsInParent.intersects(box.getBoundsInParent) &&
        !box.getBoundsInParent.contains(h.getBoundsInParent)) aux(t,h::acc)  else aux(t,acc)
    }
    aux(l,Nil).isEmpty
  }

  /*turnToLeaf: checks if there are any intersections with the list of Placements (arg 3) that corresponds to the
  OcNode's (arg 1) children's Placements and the list of Nodes(arg 2). If so, then the function returns an OcLeaf with
  the Placement of the OcNode and the list of Nodes as arguments. Otherwise, the function returns
  the OcNode.
  */
  def turnToLeaf(o: OcNode[Placement], l:List[Node], pList:List[Placement]):Octree[Placement]={
    if(!(pList foldRight true) ((x,y)=> hasNoIntersection(x,l) && y))
      OcLeaf(o.coords,l)
    else o
  }

  //treeContains: returns true if the Node (arg 2) is inside the Placement (arg 1).
  def treeContains(p: Placement, n:Node):Boolean ={
    val box:Box = new Box(p._2,p._2,p._2)
    box.setTranslateX(p._2/2-p._1._1)
    box.setTranslateY(p._2/2-p._1._2)
    box.setTranslateZ(p._2/2-p._1._3)
    box.getBoundsInParent.contains(n.getBoundsInParent)
  }

  /*createOctree: recursive function that is used to create an Octree. It receives the Placement (arg 1), that will be
  possibly given as argument to initialize the Octree, and list of Nodes (arg 2) that will be given as argument to create
  a Section in the case that the Octree is an OcLeaf. The function creates and stores all the smaller Placements that
  correspond to this Octree's children. Checks what Nodes from the list are contained in the Placement and, if there are
  none, returns OcEmpty, else returns an Octree, calls itself in each of the Octree's children and also calls the
  turnToLeaf function to decide whether the Octree is a OcNode or a OcLeaf.
   */
  def createOctree(p:Placement, l:List[Node], maxLevel: Int, minSize: Int): Octree[Placement]={
    val p00up = (p._1,p._2/2)
    val p01up = ((p._1._1-p._2/2,p._1._2,p._1._3),p._2/2)
    val p10up = ((p._1._1,p._1._2-p._2/2,p._1._3),p._2/2)
    val p11up = ((p._1._1-p._2/2,p._1._2-p._2/2,p._1._3),p._2/2)
    val p00down = ((p._1._1,p._1._2,p._1._3-p._2/2),p._2/2)
    val p01down = ((p._1._1-p._2/2,p._1._2,p._1._3-p._2/2),p._2/2)
    val p10down = ((p._1._1,p._1._2-p._2/2,p._1._3-p._2/2),p._2/2)
    val p11down = ((p._1._1-p._2/2,p._1._2-p._2/2,p._1._3-p._2/2),p._2/2)
    val pList = List(p00up,p01up,p10up,p11up,p00down,p01down,p10down,p11down)

    def aux(func:(Placement,Node)=>Boolean,l1:List[Node]):List[Node]= l1 match {
      case Nil => Nil
      case h::t => if(func(p,h)) h::aux(func,t) else aux(func,t)
    }
    val containedShapes = aux(treeContains,l)
    if(maxLevel==0 && containedShapes.nonEmpty) OcLeaf(p,containedShapes)
    else if(p._2.toInt.equals(minSize) && containedShapes.nonEmpty) OcLeaf(p,containedShapes)
    else if(containedShapes.nonEmpty)
      turnToLeaf(OcNode(p,createOctree(p00up,containedShapes,maxLevel-1,minSize),createOctree(p01up,containedShapes,maxLevel-1,minSize),
        createOctree(p10up,containedShapes,maxLevel-1,minSize), createOctree(p11up,containedShapes,maxLevel-1,minSize),
        createOctree(p00down,containedShapes,maxLevel-1,minSize),createOctree(p01down,containedShapes,maxLevel-1,minSize),
        createOctree(p10down,containedShapes,maxLevel-1,minSize),createOctree(p11down,containedShapes,maxLevel-1,minSize)),
        containedShapes, pList)
    else OcEmpty
  }


  //T4 functions:
  //scaleNode: returns the Node (arg 2) but with the scale and translate adjusted according to the fact (arg 1).
  def scaleNode(fact:Double, node: Node):Node={
    node.setScaleX(fact*node.getScaleX)
    node.setScaleY(fact*node.getScaleY)
    node.setScaleZ(fact*node.getScaleZ)
    node.setTranslateX(fact*node.getTranslateX)
    node.setTranslateY(fact*node.getTranslateY)
    node.setTranslateZ(fact*node.getTranslateZ)
    node
  }

  //scaleLeaf: scales each Node inside the OcLeaf's (arg 2) section's list of Nodes according to the fact (arg 1).
  def scaleLeaf(fact:Double, leaf: OcLeaf[Placement,Section]):OcLeaf[Placement,Section] ={
    leaf.section._2.map(n => scaleNode(fact,n))
    leaf
  }

  /*scaleOctree: recursive function that checks which type of Octree is the Octree (arg 2) given. If it's an OcEmpty,
  returns OcEmpty.If it's an OcLeaf, returns a scaled version of the OcLeaf according to the fact (arg 1). If it's an
  OcNode, returns a scaled version of the OcNode according to the fact, and calls itself in each of the new OcNode's
  children.
   */
  def scaleOctree(fact:Double, oct:Octree[Placement]):Octree[Placement] ={
    if(fact==0.5 || fact==2)
      oct match {
        case OcEmpty => OcEmpty
        case OcLeaf(section:Section) =>
          scaleLeaf(fact, OcLeaf(((section._1._1._1*fact,section._1._1._2*fact,section._1._1._3*fact),section._1._2*fact),section._2))
        case OcNode(a, b, c, d, e, f, g, h, i) =>
          OcNode(((a._1._1*fact,a._1._2*fact,a._1._3*fact),a._2*fact), scaleOctree(fact, b),
            scaleOctree(fact, c), scaleOctree(fact, d), scaleOctree(fact, e),
            scaleOctree(fact, f), scaleOctree(fact, g), scaleOctree(fact, h), scaleOctree(fact, i))
      }
    else oct
  }

  //aux functions used to create the wired boxes that represent the OcNodes
  def createWiredBox(s:(Double,Double,Double),trans:(Double,Double,Double), group: Group, m: PhongMaterial): Boolean={
    val box= new Box(s._1,s._2,s._3)
    box.setTranslateX(trans._1)
    box.setTranslateY(trans._2)
    box.setTranslateZ(trans._3)
    box.setDrawMode(DrawMode.LINE)
    box.setMaterial(m)
    group.getChildren.add(box)
  }
  def createWiredBoxes(oct:Octree[Placement], group: Group, m: PhongMaterial):Boolean = oct match{
    case OcEmpty => true
    case OcNode(a, b, c, d, e, f, g, h, i) => createWiredBox((a._2,a._2,a._2),(a._2/2-a._1._1,a._2/2-a._1._2,a._2/2-a._1._3),group,m)
      createWiredBoxes(b,group,m);createWiredBoxes(c,group,m);createWiredBoxes(d,group,m);createWiredBoxes(e,group,m)
      createWiredBoxes(f,group,m);createWiredBoxes(g,group,m);createWiredBoxes(h,group,m);createWiredBoxes(i,group,m)
    case OcLeaf(section:Section) => createWiredBox((section._1._2,section._1._2,section._1._2),
      (section._1._2/2-section._1._1._1,section._1._2/2-section._1._1._2,section._1._2/2-section._1._1._3),group,m)
  }
}