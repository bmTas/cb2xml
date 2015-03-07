package net.sf.cb2xml.example.cobolItemTT;
/*
 * This program is provided as an example, you may use it or distribute it
 * anyway you choose, just keep the author notice !!!
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
import net.sf.cb2xml.example.swing.treeTable.AbstractTreeTableModel;

/**
 * Create a TreeTableModel for displaying a Cobol Copybook
 * 
 * @author Bruce Martin
 *
 */
public class CobolItemModel extends AbstractTreeTableModel {

	private static final String[] colNames = {
		"Cobol Item", "Position", "Length", "Picture", "Usage"
	};
	public CobolItemModel(Object root) {
		super(root);
	}

	/* (non-Javadoc)
	 * @see net.sf.cb2xml.example.swing.treeTable.TreeTableModel#getColumnCount()
	 */
	@Override
	public int getColumnCount() {
		return colNames.length;
	}

	/* (non-Javadoc)
	 * @see net.sf.cb2xml.example.swing.treeTable.TreeTableModel#getColumnName(int)
	 */
	@Override
	public String getColumnName(int column) {
		return colNames[column];
	}

	/* (non-Javadoc)
	 * @see net.sf.cb2xml.example.swing.treeTable.TreeTableModel#getValueAt(java.lang.Object, int)
	 */
	@Override
	public Object getValueAt(Object node, int column) {
		Object ret = "";
		if (node instanceof CobolItemNode) {
			CobolItemNode n = (CobolItemNode) node;
			switch (column) {
			case 1: ret = n.cobolItem.getPosition();		break; 
			case 2: ret = n.cobolItem.getStorageLength();	break; 
			case 3: ret = n.cobolItem.getPicture();			break; 
			case 4: ret = n.cobolItem.getUsage();			break; 
			}
		}
		if (ret == null) {
			ret = "";
		}
		return ret;
	}

    public Class<?> getColumnClass(int column) {

    	switch (column) {
    	case 0: return net.sf.cb2xml.example.swing.treeTable.TreeTableModel.class;
    	case 1:
    	case 2: return Integer.class;
        }
        return super.getColumnClass(column);
    }

}
