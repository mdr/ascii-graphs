package com.github.mdr.ascii.gui

import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.layout.coordAssign._
import com.github.mdr.ascii.layout._
import com.github.mdr.ascii.layout.prefs.LayoutPrefsImpl
import com.github.mdr.ascii.graph.GraphFromText

object GUI extends App {

  SwingUtilities.invokeLater(new Runnable() {
    override def run {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
      Frame.pack()
      Frame.setVisible(true)
    }
  })

}

object Frame extends JFrame {

  setTitle("ASCII Graph Layout")
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  setPreferredSize(new Dimension(1024, 640))
  val splitPane = new JSplitPane
  val inputTextPane = new JTextPane
  inputTextPane.setText("""|A,B,C
                           |A,C
                           |B,B
                           |D,E,F
                           |E,G""".stripMargin)

  inputTextPane.setText("""|A,B
                           |A,C
                           |A,D
                           |A,E
                           |A,F""".stripMargin)
  val outputWrapper = new JPanel(new BorderLayout)
  val outputTextPane = new JTextPane
  inputTextPane.setFont(new Font(Font.MONOSPACED, inputTextPane.getFont.getStyle, inputTextPane.getFont.getSize))
  outputTextPane.setFont(new Font(Font.MONOSPACED, outputTextPane.getFont.getStyle, outputTextPane.getFont.getSize))
  outputTextPane.setEditable(false)
  inputTextPane.getDocument.addDocumentListener {
    new DocumentListener() {
      def insertUpdate(e: DocumentEvent) { refreshDiagram() }
      def removeUpdate(e: DocumentEvent) { refreshDiagram() }
      def changedUpdate(e: DocumentEvent) { refreshDiagram() }
    }
  }

  private def refreshDiagram() {
    try {
      val text = inputTextPane.getText
      val graph = GraphFromText(text)
      outputTextPane.setText(GraphLayout.renderGraph(graph, ToStringVertexRenderingStrategy, OptionsPanel.layoutPrefs))
    } catch {
      case e: Throwable ⇒
        outputTextPane.setText(e.getMessage)
        e.printStackTrace()
    }
  }

  outputWrapper.add(outputTextPane)
  splitPane.setTopComponent(new JScrollPane(inputTextPane))
  splitPane.setBottomComponent(new JScrollPane(outputWrapper))
  splitPane.setOrientation(JSplitPane.VERTICAL_SPLIT)
  splitPane.setDividerLocation(0.5)
  splitPane.setResizeWeight(0.5)

  add(splitPane, BorderLayout.CENTER)
  add(OptionsPanel, BorderLayout.EAST)

  object OptionsPanel extends JPanel {

    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))

    def addBox(name: String, default: Boolean) = {
      val checkBox = new JCheckBox(name)
      add(checkBox)
      checkBox.setSelected(default)
      checkBox.addActionListener(refreshListener)
      checkBox
    }

    val removeKinksBox = addBox("Remove kinks", true)
    val compactifyBox = addBox("Compactify", true)
    val elevateEdgesBox = addBox("Elevate edges", true)
    val unicodeBox = addBox("Unicode", true)
    val verticalBox = addBox("Vertical", true)
    val doubleVerticesBox = addBox("Double Vertices", false)
    val roundedBox = addBox("Rounded", true)
    val explicitAsciiBendsBox = addBox("Explicit ASCII bends", true)

    def refreshListener = new ActionListener() { def actionPerformed(e: ActionEvent) = refreshDiagram() }

    def layoutPrefs =
      LayoutPrefsImpl(
        removeKinks = removeKinksBox.isSelected,
        compactify = compactifyBox.isSelected,
        elevateEdges = elevateEdgesBox.isSelected,
        unicode = unicodeBox.isSelected,
        vertical = verticalBox.isSelected,
        doubleVertices = doubleVerticesBox.isSelected,
        rounded = roundedBox.isSelected,
        explicitAsciiBends = explicitAsciiBendsBox.isSelected
      )

  }

}