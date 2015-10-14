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

import java.net.URL;
import java.util.Arrays;
import java.util.List;

import net.sf.cb2xml.jaxb.Item;



/**
 * Basic program to process cb2xml using JAXB
 * 
 * @author Bruce Martin
 *
 */
public class Code {
	
	public static void printItem(String indent, Item item) {
		char[] nc = new char[Math.max(1, 45 - indent.length() - item.getName().length())];
		String picture = item.getPicture();
		Arrays.fill(nc, ' ');
		
		if (picture == null) {
			picture = "";
		}
		
		System.out.println(indent + item.getLevel() + " " + item.getName() + new String(nc) +"\t" + item.getPosition() 
				+ "\t " + item.getStorageLength() + "\t" + picture);
		
		List<Item> items = item.getItem();
		
		for (Item child : items) {
//			if (child instanceof Item) {
				printItem(indent + "   ", child);
//			}
		}
	}
	
	public static URL getFullName(String filename) {
    	return Code.class.getResource(filename);
    }
}
