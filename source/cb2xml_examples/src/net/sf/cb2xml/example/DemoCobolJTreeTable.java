package net.sf.cb2xml.example;
/*
 * This program is provided as an example, you may use it or distribute it
 * anyway you choose
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
import java.awt.Dimension;
import java.io.IOException;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.TableColumn;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.xml.bind.JAXBException;

import net.sf.cb2xml.example.cobolItemTT.CobolItemModel;
import net.sf.cb2xml.example.cobolItemTT.CobolItemNode;
import net.sf.cb2xml.example.swing.treeTable.JTreeTable;
import net.sf.cb2xml.jaxb.Copybook;
import net.sf.cb2xml.jaxb.Item;
import net.sf.cb2xml.parse.CobolParser;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;

/**
 * 
 * A basic program to display a Cobol Copybook in a TreeTable.
 * 
 * 
 * @author Bruce Martin
 *
 */
public class DemoCobolJTreeTable {

	public static void main(String[] args) throws ParserException, LexerException, IOException, JAXBException {
//        JAXBContext jc = JAXBContext.newInstance(Condition.class, Copybook.class, Item.class);
//        
//        Unmarshaller unmarshaller = jc.createUnmarshaller();
//        Document doc = Cb2Xml2.convertToXMLDOM(new File(Code.getFullName("Vendor.cbl").getFile()), false, Cb2xmlConstants.USE_STANDARD_COLUMNS);
//
//        JAXBElement<Copybook> copybookElement = unmarshaller.unmarshal(doc, Copybook.class);
//        JFrame frame = new JFrame();
//        Copybook copybook = copybookElement.getValue();
		String copybookName;
		
		if (args != null && args.length > 0) {
			copybookName = args[0];
		} else {
			copybookName = Code.getFullName("Vendor.cbl").getFile();
		}
        
        JFrame frame = new JFrame();
		Copybook copybook = CobolParser.newParser() .parseCobol(copybookName);

		DefaultMutableTreeNode root = new DefaultMutableTreeNode(copybook.getFilename());
        List<Item> items = copybook.getItem();
        for (Item item : items) {
        	root.add(new CobolItemNode(item));
        }
        
        CobolItemModel model = new CobolItemModel(root);
        JTreeTable tt = new JTreeTable(model);
       
        TableColumn tc;
        
        tt.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        tc = tt.getColumnModel().getColumn(0);
        tc.setPreferredWidth(220);
        
        frame.add(new JScrollPane(tt));
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        Dimension preferredSize = frame.getPreferredSize();
		frame.setPreferredSize(new Dimension(preferredSize.width + 100, preferredSize.height));
        frame.pack();

        frame.setVisible(true);
	}

}
