package net.sf.cb2xml.util;

import java.util.ArrayList;
import java.util.Arrays;

import javax.xml.namespace.NamespaceContext;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

public class IndentXmlWriter implements XMLStreamWriter {
	
	private static final int INDENT_LEVELS = 15;
	private final XMLStreamWriter xmlWriter;
	private int indent = 0;
	
	private char[] fill;
	private int indentChars = 4;
	
	private Boolean doIndent = Boolean.FALSE;
	private ArrayList<Boolean> doIndentStack = new ArrayList<Boolean>();


	public IndentXmlWriter(XMLStreamWriter xmlWriter) {
		super();
		this.xmlWriter = xmlWriter;
		
		fill = new char[INDENT_LEVELS * 4]; // Indent by 4 spaces
		Arrays.fill(fill, ' ');
		doIndentStack.add(Boolean.FALSE); // Precaution; not strickly needed
	}
	
	private void start() throws XMLStreamException {
		doIndentStack.add(Boolean.TRUE);
		doIndent = Boolean.FALSE;
		if (indent > 0) {
			writeIndent();
		}
		indent++;
	}
	
	private void end() throws XMLStreamException {
		indent--;
		if (doIndent) {
			writeIndent();
		}
		doIndent = doIndentStack.remove(doIndentStack.size() - 1);
	}

	private void empty() throws XMLStreamException {
		doIndent = Boolean.TRUE;
		if (indent > 0) {
			writeIndent();
		}
	}


	private void writeIndent() throws XMLStreamException {
		int ind = indent;
		
		xmlWriter.writeCharacters("\n");
		
		while (ind > INDENT_LEVELS) {
			xmlWriter.writeCharacters(fill, 0, fill.length);
			ind = ind - INDENT_LEVELS;
		}
		if (ind > 0) {
			xmlWriter.writeCharacters(fill, 0, ind * indentChars);
		}
	}
	/**
	 * @param localName
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeStartElement(java.lang.String)
	 */
	@Override public void writeStartElement(String localName) throws XMLStreamException {
		start();
		xmlWriter.writeStartElement(localName);
	}

	/**
	 * @param namespaceURI
	 * @param localName
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeStartElement(java.lang.String, java.lang.String)
	 */
	@Override public void writeStartElement(String namespaceURI, String localName) throws XMLStreamException {
		start();
		xmlWriter.writeStartElement(namespaceURI, localName);
	}

	/**
	 * @param prefix
	 * @param localName
	 * @param namespaceURI
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeStartElement(java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override public void writeStartElement(String prefix, String localName, String namespaceURI) throws XMLStreamException {
		start();
		xmlWriter.writeStartElement(prefix, localName, namespaceURI);
	}

	/**
	 * @param namespaceURI
	 * @param localName
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeEmptyElement(java.lang.String, java.lang.String)
	 */
	@Override public void writeEmptyElement(String namespaceURI, String localName) throws XMLStreamException {
		empty();
		xmlWriter.writeEmptyElement(namespaceURI, localName);
	}

	/**
	 * @param prefix
	 * @param localName
	 * @param namespaceURI
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeEmptyElement(java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override public void writeEmptyElement(String prefix, String localName, String namespaceURI) throws XMLStreamException {
		empty();
		xmlWriter.writeEmptyElement(prefix, localName, namespaceURI);
	}

	/**
	 * @param localName
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeEmptyElement(java.lang.String)
	 */
	@Override public void writeEmptyElement(String localName) throws XMLStreamException {
		empty();
		xmlWriter.writeEmptyElement(localName);
	}

	/**
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeEndElement()
	 */
	@Override public void writeEndElement() throws XMLStreamException {
		end();
		xmlWriter.writeEndElement();
	}

	/**
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeEndDocument()
	 */
	@Override public void writeEndDocument() throws XMLStreamException {
		empty();
		xmlWriter.writeEndDocument();
	}

	/**
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#close()
	 */
	@Override public void close() throws XMLStreamException {
		xmlWriter.close();
	}

	/**
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#flush()
	 */
	@Override public void flush() throws XMLStreamException {
		xmlWriter.flush();
	}

	/**
	 * @param localName
	 * @param value
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeAttribute(java.lang.String, java.lang.String)
	 */
	@Override public void writeAttribute(String localName, String value) throws XMLStreamException {
		xmlWriter.writeAttribute(localName, value);
	}

	/**
	 * @param prefix
	 * @param namespaceURI
	 * @param localName
	 * @param value
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeAttribute(java.lang.String, java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override public void writeAttribute(String prefix, String namespaceURI, String localName, String value)
			throws XMLStreamException {
		xmlWriter.writeAttribute(prefix, namespaceURI, localName, value);
	}

	/**
	 * @param namespaceURI
	 * @param localName
	 * @param value
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeAttribute(java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override public void writeAttribute(String namespaceURI, String localName, String value) throws XMLStreamException {
		xmlWriter.writeAttribute(namespaceURI, localName, value);
	}

	/**
	 * @param prefix
	 * @param namespaceURI
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeNamespace(java.lang.String, java.lang.String)
	 */
	@Override public void writeNamespace(String prefix, String namespaceURI) throws XMLStreamException {
		xmlWriter.writeNamespace(prefix, namespaceURI);
	}

	/**
	 * @param namespaceURI
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeDefaultNamespace(java.lang.String)
	 */
	@Override public void writeDefaultNamespace(String namespaceURI) throws XMLStreamException {
		xmlWriter.writeDefaultNamespace(namespaceURI);
	}

	/**
	 * @param data
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeComment(java.lang.String)
	 */
	@Override public void writeComment(String data) throws XMLStreamException {
		writeIndent();
		xmlWriter.writeComment(data);
	}

	/**
	 * @param target
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeProcessingInstruction(java.lang.String)
	 */
	@Override public void writeProcessingInstruction(String target) throws XMLStreamException {
		xmlWriter.writeProcessingInstruction(target);
	}

	/**
	 * @param target
	 * @param data
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeProcessingInstruction(java.lang.String, java.lang.String)
	 */
	@Override public void writeProcessingInstruction(String target, String data) throws XMLStreamException {
		xmlWriter.writeProcessingInstruction(target, data);
	}

	/**
	 * @param data
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeCData(java.lang.String)
	 */
	@Override public void writeCData(String data) throws XMLStreamException {
		doIndent = Boolean.FALSE;
		xmlWriter.writeCData(data);
	}

	/**
	 * @param dtd
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeDTD(java.lang.String)
	 */
	@Override public void writeDTD(String dtd) throws XMLStreamException {
		xmlWriter.writeDTD(dtd);
	}

	/**
	 * @param name
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeEntityRef(java.lang.String)
	 */
	@Override public void writeEntityRef(String name) throws XMLStreamException {
		xmlWriter.writeEntityRef(name);
	}

	/**
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeStartDocument()
	 */
	@Override public void writeStartDocument() throws XMLStreamException {
		xmlWriter.writeStartDocument();
		xmlWriter.writeCharacters("\n");
	}

	/**
	 * @param version
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeStartDocument(java.lang.String)
	 */
	@Override public void writeStartDocument(String version) throws XMLStreamException {
		xmlWriter.writeStartDocument(version);
		xmlWriter.writeCharacters("\n");
	}

	/**
	 * @param encoding
	 * @param version
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeStartDocument(java.lang.String, java.lang.String)
	 */
	@Override public void writeStartDocument(String encoding, String version) throws XMLStreamException {
		xmlWriter.writeStartDocument(encoding, version);
		xmlWriter.writeCharacters("\n");
	}

	/**
	 * @param text
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeCharacters(java.lang.String)
	 */
	@Override public void writeCharacters(String text) throws XMLStreamException {
		doIndent = Boolean.FALSE;
		xmlWriter.writeCharacters(text);
	}

	/**
	 * @param text
	 * @param start
	 * @param len
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeCharacters(char[], int, int)
	 */
	@Override public void writeCharacters(char[] text, int start, int len) throws XMLStreamException {
		doIndent = Boolean.FALSE;
		xmlWriter.writeCharacters(text, start, len);
	}

	/**
	 * @param uri
	 * @return
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#getPrefix(java.lang.String)
	 */
	@Override public String getPrefix(String uri) throws XMLStreamException {
		return xmlWriter.getPrefix(uri);
	}

	/**
	 * @param prefix
	 * @param uri
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#setPrefix(java.lang.String, java.lang.String)
	 */
	@Override public void setPrefix(String prefix, String uri) throws XMLStreamException {
		xmlWriter.setPrefix(prefix, uri);
	}

	/**
	 * @param uri
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#setDefaultNamespace(java.lang.String)
	 */
	@Override public void setDefaultNamespace(String uri) throws XMLStreamException {
		xmlWriter.setDefaultNamespace(uri);
	}

	/**
	 * @param context
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#setNamespaceContext(javax.xml.namespace.NamespaceContext)
	 */
	@Override public void setNamespaceContext(NamespaceContext context) throws XMLStreamException {
		xmlWriter.setNamespaceContext(context);
	}

	/**
	 * @return
	 * @see javax.xml.stream.XMLStreamWriter#getNamespaceContext()
	 */
	@Override public NamespaceContext getNamespaceContext() {
		return xmlWriter.getNamespaceContext();
	}

	/**
	 * @param name
	 * @return
	 * @throws IllegalArgumentException
	 * @see javax.xml.stream.XMLStreamWriter#getProperty(java.lang.String)
	 */
	@Override public Object getProperty(String name) throws IllegalArgumentException {
		return xmlWriter.getProperty(name);
	}
}
