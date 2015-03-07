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

import java.util.List;

import javax.swing.tree.DefaultMutableTreeNode;

import net.sf.cb2xml.jaxb.Item;

 
/**
 * Create a Cobol-Item node. Each node corresponds to one group/field in a 
 * Cobol Copybook
 * 
 * @author Bruce Martin
 *
 */
@SuppressWarnings("serial")
public class CobolItemNode extends DefaultMutableTreeNode {
	final Item cobolItem;

	public CobolItemNode(Item cobolItem) {
		super(cobolItem.getLevel() + " " + cobolItem.getName());
		this.cobolItem = cobolItem;
		
		List<Item> children = cobolItem.getItem();
		for (Item o : children) {
			super.add(new CobolItemNode(o));
		}
	}
	
}
