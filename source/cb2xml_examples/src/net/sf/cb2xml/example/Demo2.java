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


import java.util.List;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.def.ICopybookJrUpd;
import net.sf.cb2xml.def.IItem;



/**
 * Basic program to process a cb2xml-xml-document using JAXB
 * 
 * @author Bruce Martin
 *
 */
public class Demo2 {
	public static void main(String[] args) throws Exception {
		String copybookName;
		
		if (args != null && args.length > 0) {
			copybookName = args[0];
		} else {
			copybookName = Code.getFullName("BitOfEverything.cbl").getFile();
		}

		ICopybookJrUpd copybook = Cb2Xml3.newBuilder(copybookName).asCobolItemTree();

        System.out.println();
        System.out.println("Printing Items");
        System.out.println();
        List<? extends IItem> items = copybook.getChildItems();
        for (IItem item : items) {
        	Code2.printItem("   ", item);
        }
    }
}
