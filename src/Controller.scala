import FileTranslator._
import OctreeUtils.{Placement, createOctree, createWiredBoxes, scaleOctree}
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control._
import javafx.scene.{Group, Parent, Scene, SubScene}
import InitSubScene._
import javafx.application.Application
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.stage.Stage


class Controller extends Application {
  val redMaterial = new PhongMaterial()
  redMaterial.setDiffuseColor(Color.RED)


  @FXML
  private var subScene1: SubScene = _
  @FXML
  private var buttonOK: Button = _ //OK
  @FXML
  private var buttonS: Button = _ //sepia
  @FXML
  private var buttonGR: Button = _ //green remove
  @FXML
  private var button05: Button = _ //scale 0.5
  @FXML
  private var button2: Button = _ //scale 2
  @FXML
  private var textFieldFile: TextField = _ //file name
  @FXML
  private var textFieldML: TextField = _ //max level
  @FXML
  private var textFieldMP: TextField = _ //min partition size
  @FXML
  private var choiceBoxWBC: ColorPicker = _

  // buttonOK.setDisable(true) //only enables when text field is filled

  @FXML
  def mouseEnteredOkButton(): Unit = {
    //make button lighter
  }

  @FXML
  //when user presses OK
  def okButtonEvent(): Unit = {
    if (textFieldFile.getText.trim.nonEmpty) {
      buttonOK.setDisable(false)
      val models = readFromFile(textFieldFile.getText)
      FileTranslator.addShapes(models, worldRoot)

      val minSize = textFieldMP.getText.trim
      val maxLevel = textFieldML.getText.trim
      (minSize.toDoubleOption, maxLevel.toDoubleOption) match {
        case (None, None) => val o = createOctree(((0, 0, 0), 32), models, -1, -1)
        case (None, _) => val o = createOctree(((0, 0, 0), 32), models, maxLevel.toInt, -1)
        case (_, None) => val o = createOctree(((0, 0, 0), 32), models, -1, minSize.toInt)
        case (_, _) => val o = createOctree(((0, 0, 0), 32), models, maxLevel.toInt, minSize.toInt)
      }


    }
  }

  @FXML
  def onScale2Clicked(): Unit = {
    //scaleOctree(oct)
    //removeWiredBoxes
    //CreateWiredBoxes
  }

  @FXML
  def onScale05Clicked(): Unit = {
    //scaleOctree()
  }

  @FXML
  def onRemoveGreenClicked(): Unit = {
    //mapColourEffect(greenRemove, oct)
  }

  @FXML
  def onSepiaClicked(): Unit = {
    //mapColourEffect(sepia, oct)
  }

  //method automatically invoked after the @FXML fields have been injected
  @FXML
  def initialize(): Unit = {
    InitSubScene.subScene.widthProperty.bind(subScene1.widthProperty)
    InitSubScene.subScene.heightProperty.bind(subScene1.heightProperty)
    subScene1.setRoot(InitSubScene.root)
  }

  override def start(stage: Stage): Unit = {
    stage.setTitle("PPM Project 21/22")
    val fxmlLoader = new FXMLLoader(getClass.getResource("Controller.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)
    stage.setScene(scene)
    stage.show
  }
}