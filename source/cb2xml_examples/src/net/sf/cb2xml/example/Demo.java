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
import java.io.InputStream;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;





import javax.xml.transform.stream.StreamSource;

import net.sf.cb2xml.jaxb.Condition;
import net.sf.cb2xml.jaxb.Copybook;
import net.sf.cb2xml.jaxb.Item;

/**
 * Basic program to process xml produced by cb2xml using JAXB
 * 
 * @author Bruce Martin
 *
 */
public class Demo {
	public static void main(String[] args) throws Exception {
        JAXBContext jc = JAXBContext.newInstance(Condition.class, Copybook.class, Item.class);
 
        Unmarshaller unmarshaller = jc.createUnmarshaller();
        //URL url = new URL("http://bdoughan.blogspot.com/atom.xml");
        InputStream xml = Code.getFullName("cb2xml_Output111.xml").openStream();
        JAXBElement<Copybook> copybook = unmarshaller.unmarshal(new StreamSource(xml), Copybook.class);
        xml.close();
 
        System.out.println();
        System.out.println("Printing Xml");
        System.out.println();
        Marshaller marshaller = jc.createMarshaller();
        marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
        marshaller.marshal(copybook, System.out);
        
        System.out.println();
        System.out.println("Printing Items");
        System.out.println();
        List<Item> items = copybook.getValue().getItem();
        for (Item item : items) {
        	Code.printItem("   ", item);
        }
    }
	

}
