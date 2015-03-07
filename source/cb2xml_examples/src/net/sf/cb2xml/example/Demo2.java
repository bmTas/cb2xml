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
import java.io.File;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;

import net.sf.cb2xml.Cb2Xml2;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.jaxb.Condition;
import net.sf.cb2xml.jaxb.Copybook;
import net.sf.cb2xml.jaxb.Item;

import org.w3c.dom.Document;

/**
 * Basic program to process a cb2xml-xml-document using JAXB
 * 
 * @author Bruce Martin
 *
 */
public class Demo2 {
	public static void main(String[] args) throws Exception {
        JAXBContext jc = JAXBContext.newInstance(Condition.class, Copybook.class, Item.class);
   
        Unmarshaller unmarshaller = jc.createUnmarshaller();
        Document doc = Cb2Xml2.convertToXMLDOM(new File(Code.getFullName("BitOfEverything.cbl").getFile()), false, Cb2xmlConstants.USE_STANDARD_COLUMNS);

        JAXBElement<Copybook> copybook = unmarshaller.unmarshal(doc, Copybook.class);

        System.out.println();
        System.out.println("Printing Items");
        System.out.println();
        List<Item> items = copybook.getValue().getItem();
        for (Item item : items) {
        	Code.printItem("   ", item);
        }
    }
}
