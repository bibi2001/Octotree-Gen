import OctreeUtils._
import TextBasedUserInterfaceUtils._
import ColorUtils.{Placement, _}
import FileTranslator.readFromFile
import javafx.scene.paint.PhongMaterial
import javafx.scene.Group

import scala.annotation.tailrec

case class OctreeState(oct: Octree[Placement], group: Group, m: PhongMaterial)

//T6 class out of 2
object TextBasedUserInterface extends App {

  //startUI: asks the user for all the necessary information to create an Octree and some optional information too
  def startUI(group: Group):Unit = {
    showPromptToGetFile()
    val filename = getUserInput
    val models = readFromFile(filename)
    FileTranslator.addShapes(models,group)

    //advanced options (extra)
    showPromptMinSize()
    val minSize = getUserInput //smallest partition size
    showPromptMaxLevel()
    val maxLevel = getUserInput //depth limit of octree
    showPromptColour()
    val m= getUserInputMaterial
    (minSize.toDoubleOption,maxLevel.toDoubleOption) match {
      case (None, None) =>  mainLoop(OctreeState(createOctree(((0, 0, 0), 32),models,-1,-1),group,m))
      case (None, _) => mainLoop(OctreeState(createOctree(((0, 0, 0), 32),models,maxLevel.toInt, -1),group,m))
      case (_, None) => mainLoop(OctreeState(createOctree(((0, 0, 0), 32),models,-1, minSize.toInt),group,m))
      case (_, _) => mainLoop(OctreeState(createOctree(((0, 0, 0), 32),models,maxLevel.toInt, minSize.toInt),group,m))
    }
  }

  //mainLoop: tail recursive function that serves as a menu
  @tailrec
  def mainLoop(octreeState: OctreeState): Unit = {
    showPromptToApplyMethods()
    val userInput = getUserInput

    //handle the user's choice
    userInput match {
      case "q" | "Q" => //QUIT
        printQuit()
        createWiredBoxes(octreeState.oct,octreeState.group, octreeState.m)

      case "SC" | "sc" => //SCALE
        showPromptToScale()
        val userInput2 = getUserInput
        userInput2 match { //checks if it's a valid option, if so scales the Octree, else prints a warning
          case "2" | "2.0" | "0.5" =>
            val aux = scaleOctree(userInput2.toDouble, octreeState.oct)
            mainLoop(OctreeState(aux, octreeState.group, octreeState.m))
          case _ => invalidOption()
            mainLoop(octreeState)
        }

      case "SE" | "se" => //SEPIA
        val aux = mapColourEffect(sepia, octreeState.oct)
        mainLoop(OctreeState(aux,octreeState.group,octreeState.m))

      case "G" | "g" => //REMOVE GREEN COLOUR
        val aux = mapColourEffect(greenRemove, octreeState.oct)
        mainLoop(OctreeState(aux, octreeState.group,octreeState.m))

      case _ => //DEFAULT
        invalidOption()
        mainLoop(octreeState)

    }
  }
}