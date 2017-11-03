package net.sf.cb2xml;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import javax.xml.stream.XMLStreamException;
import javax.xml.transform.Result;

import net.sf.cb2xml.analysis.CopyBookAnalyzer;

import net.sf.cb2xml.analysis.Copybook;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.DialectManager;
import net.sf.cb2xml.def.IBasicDialect;
import net.sf.cb2xml.def.NumericDefinition;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.node.Start;
import net.sf.cb2xml.sablecc.parser.Parser;
import net.sf.cb2xml.sablecc.parser.ParserException;

/**
 * cb2xml interface program (Number 3). It provides a 
 * builder like interface to cb2xml.
 * 
 * @author Bruce Martin (version 0.96)
 *
 */
public class Cb2Xml3 {
	public static final int USE_DEFAULT_THREADSIZE = 0;
	public static final int CALCULATE_THREAD_SIZE = -1;
	
	static final int ONE_MEG = 1024 * 1024;
	static final int DEFAULT_THREAD_SIZE = 4 * ONE_MEG;
	private static final int FIRST_COBOL_COLUMN = 6;
	private static final int LAST_COBOL_COLUMN = 72;

	
	/**
	 * Create a new Builder for Cobol Copybook Analysis / writing as Xml 
	 * 
	 * @param copybookFile Cobol-Copybook-File
	 * 
	 * @return Cobol CopybookAnalysis Builder
	 */
	public static ICb2XmlBuilder newBuilder(String copybookFile) throws FileNotFoundException {
		return newBuilder(new File(copybookFile));
	}

	/**
	 * Create a new Builder for Cobol Copybook Analysis / writing as Xml 
	 * 
	 * @param copybookFile Cobol-Copybook-File
	 * 
	 * @return Cobol CopybookAnalysis Builder
	 */
	public static ICb2XmlBuilder newBuilder(File copybookFile) throws FileNotFoundException {
		return newBuilder(new FileReader(copybookFile), copybookFile.getName());
	}
	
	/**
	 * Create a new Builder for Cobol Copybook Analysis / writing as Xml 
	 * 
	 * @param reader reader for the 
	 * @param copybookName Name of the Cobol Copybook
	 * 
	 * @return Cobol CopybookAnalysis Builder
	 */
	public static ICb2XmlBuilder newBuilder(Reader reader, String copybookName) {
		return new BldrImp(reader, copybookName);
	}
	
	/**
	 * Builder specifically for JRecord. It provides a lower level
	 * Copybook interface
	 */
	public static BldrImp newBuilderJRec(Reader reader, String copybookName) {
		return new BldrImp(reader, copybookName);
	}

	
	/**
	 * Builder Implementation
	 * 
	 * @author Bruce Martin
	 *
	 */
	public static class BldrImp implements ICb2XmlBuilder {

		private final Reader reader;
		private final String copybookName;
		
		private String encoding="UTF-8";
		private boolean debug = false, 
						indent = false, 
						newXmlFormat=true,
						loadComments=true;
		private int cblLineFormat = Cb2xmlConstants.USE_STANDARD_COLUMNS, 
		 			startingColumn = FIRST_COBOL_COLUMN,
		 			lastColumn = LAST_COBOL_COLUMN;
		private long stackSize = CALCULATE_THREAD_SIZE;
		
		private NumericDefinition dialect = DialectManager.MAINFRAME_COBOL.getNumericDefinition();
		
		private Copybook copybook;
		
		
		public BldrImp(Reader reader, String copybookName) {
			this.reader = reader;
			this.copybookName = copybookName;
		}
		
		/* (non-Javadoc)
		 * @see net.sf.cb2xml.ICb2XmlBuilder#setDebug(boolean)
		 */
		@Override
		public BldrImp setDebug(boolean debug) {
			this.debug = debug;
			
			return this;
		}

		/* (non-Javadoc)
		 * @see net.sf.cb2xml.ICb2XmlBuilder#setCobolFormat(int)
		 */
		@Override
		public BldrImp setCobolLineFormat(int lineFormat) {
			this.cblLineFormat = lineFormat;
			clear();
			
			return this;
		}

		/* (non-Javadoc)
		 * @see net.sf.cb2xml.ICb2XmlBuilder#setCobolColumns(int, int)
		 */
		@Override
		public BldrImp setCobolColumns(int startingColumn, int lastColumn) {
			this.cblLineFormat = Cb2xmlConstants.USE_SUPPLIED_COLUMNS;
			this.startingColumn = startingColumn;
			this.lastColumn = lastColumn;
			clear();
			
			return this;
		}

		/**
		 * @param stackSize the stackSize to set
		 */
		@Override
		public final BldrImp setStackSize(long stackSize) {
			this.stackSize = stackSize;
			return this;
		}
		
		
		
		/* (non-Javadoc)
		 * @see net.sf.cb2xml.ICb2XmlBuilder#setIndent(boolean)
		 */
		@Override
		public BldrImp setIndent(boolean indent) {
			this.indent = indent;
			return this;
		}

		/**
		 * @param newXmlFormat the newXmlFormat to set
		 */
		@Override
		public final BldrImp setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat xmlFormat) {
			this.newXmlFormat = xmlFormat == Cb2xmlConstants.Cb2xmlXmlFormat.FORMAT_2017;
			clear();
			return this;
		}
		
		@Override
		public final BldrImp setDialect(IBasicDialect dialectDetails) {
			this.dialect = dialectDetails.getNumericDefinition();
			return this;
		}

		@Override
		public final BldrImp setXmlEncoding(String encoding) {
			this.encoding = encoding;
			return this;
		}

		@Override
		public final BldrImp setLoadComments(boolean loadComments) {
			this.loadComments = loadComments;
			return this;
		}

		private void clear() {
			copybook = null;
		}
		
		
		
		/* (non-Javadoc)
		 * @see net.sf.cb2xml.ICb2XmlBuilder#asXmlString()
		 */
		@Override
		public String asXmlString() throws XMLStreamException, LexerException, IOException, ParserException {
			StringWriter sw = new StringWriter();
			writeXml(sw);
			return sw.toString();
		}

		/**
		 * @see net.sf.cb2xml.ICb2XmlBuilder#asCobolItemTree()
		 */
		@Override
		public Copybook asCobolItemTree() {
			try {
				return getCopybook();
			} catch (LexerException e) {
				throw new RuntimeException(e);
			} catch (IOException e) {
				throw new RuntimeException(e);
			} catch (ParserException e) {
				throw new RuntimeException(e);
			}
		}
		
		private Copybook getCopybook() throws LexerException, IOException, ParserException {
			if (copybook == null) {
				DoCblAnalyse da = new DoCblAnalyse(copybookName, dialect, loadComments);
				da.doAnalysis(debug, cblLineFormat, startingColumn, lastColumn, reader, stackSize);
				copybook = da.copybook;
			}
			return copybook;
		}

		/* (non-Javadoc)
		 * @see net.sf.cb2xml.ICb2XmlBuilder#writeXml(java.lang.String)
		 */
		@Override
		public void writeXml(String filename) throws XMLStreamException, LexerException, IOException, ParserException {
			writeXml(new FileOutputStream(filename));
		}
		
		@Override
		public void writeXml(OutputStream out) throws XMLStreamException, LexerException, IOException, ParserException {
			String e = encoding == null || encoding.length() == 0 
							? Cb2xmlConstants.STANDARD_FONT
							: encoding;
			Writer writer = new OutputStreamWriter(out, e);
			new net.sf.cb2xml.util.WriteXml(newXmlFormat, true, e)
						.writeCopybook(writer, getCopybook(), indent);
			writer.close();
		}


		/* (non-Javadoc)
		 * @see net.sf.cb2xml.ICb2XmlBuilder#writeXml(java.io.Writer)
		 */
		@Override
		public void writeXml(Writer writer) throws XMLStreamException, LexerException, IOException, ParserException {
			new net.sf.cb2xml.util.WriteXml(newXmlFormat, true, encoding)
						.writeCopybook(writer, getCopybook(), indent);
		}
		
		@Override
		public void writeXml(Result result) throws XMLStreamException, LexerException, IOException, ParserException {
			new net.sf.cb2xml.util.WriteXml(newXmlFormat, false, encoding)
						.writeCopybook(result, getCopybook());
		}
		
	

		private static final class DoCblAnalyse extends DoCobolAnalyse {
			
			private final String name;
			private final NumericDefinition dialect;
			private final boolean loadComments;
			Copybook copybook;
			
			public DoCblAnalyse(String name, NumericDefinition dialect, boolean loadComments) {
				super(name);
				this.name = name;
				this.dialect = dialect;
				this.loadComments = loadComments;
			}

			protected void analyseCobolCopybook(Parser parser, Start ast) {
				CopyBookAnalyzer copyBookAnalyzer = new CopyBookAnalyzer(name, parser, dialect, loadComments);
				ast.apply(copyBookAnalyzer);
				copybook = copyBookAnalyzer.getCopybook();
			}
		}
	}

}
