package net.sf.cb2xml;

import java.io.IOException;
import java.io.PushbackReader;
import java.io.Reader;

import net.sf.cb2xml.copybookReader.ICobolCopybookTextSource;
import net.sf.cb2xml.copybookReader.BasicReadCobolCopybook;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.sablecc.lexer.Lexer;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.node.Start;
import net.sf.cb2xml.sablecc.parser.Parser;
import net.sf.cb2xml.sablecc.parser.ParserException;

/**
 *   Run the Cobol Copybook Analysis in a separate thread so
 * that the stack size can be increased to cater for large copybooks.
 *   This class should be subclassed to do the actual analysis.
 * This allows the class to be used for the old DOM Analysis and the new
 * Copybook / Item analysis.
 * 
 * @author Bruce Martin
 *
 */
public abstract class DoCobolAnalyse implements Runnable  {
//	static final int USE_DEFAULT_THREADSIZE = 0;
//	static final int USE_DEFAULT_CB2XML_THREADSIZE = 1;
	static final int ONE_MEG = 1024 * 1024;
	static final int DEFAULT_THREAD_SIZE = 4 * ONE_MEG;

	private final static Object SYNC_PARSE_COBOL = new Object();
	private static final int FIRST_COBOL_COLUMN = 6;
	private static final int LAST_COBOL_COLUMN = 72;

	private static int[] END_COLS = new int[Cb2xmlConstants.FREE_FORMAT];
	
	static {
		END_COLS[Cb2xmlConstants.USE_STANDARD_COLUMNS] = LAST_COBOL_COLUMN;
		END_COLS[Cb2xmlConstants.USE_COLS_6_TO_80] =  80;
		END_COLS[Cb2xmlConstants.USE_LONG_LINE] = 16000;
	}
	
	private final String name;
	private boolean debug;
	//private int initialColumn;
	private PushbackReader pbr;
	
	private DebugParserException debugException;
	private LexerException lexerException;
	private IOException ioException;
	private ParserException parseException;
	private ICobolCopybookTextSource copyBookSource;
	

	public DoCobolAnalyse(String name) {
		super();
		this.name = name;
	}

	public DoCobolAnalyse setCopybookReader(ICobolCopybookTextSource copyBookReader,
			int cblLineFormat, int firstColumn, int lastColumn,
			String copybookName, Reader r) throws IOException {
		if (copyBookReader == null) {
			switch (cblLineFormat) {
			case Cb2xmlConstants.FREE_FORMAT:
				copyBookReader = BasicReadCobolCopybook.newCopybookReader(r, copybookName)
										.setColumns( 0, END_COLS[Cb2xmlConstants.USE_LONG_LINE]);
				break;
			case Cb2xmlConstants.USE_STANDARD_COLUMNS:
			case Cb2xmlConstants.USE_COLS_6_TO_80:
			case Cb2xmlConstants.USE_LONG_LINE:
				copyBookReader = BasicReadCobolCopybook.newCopybookReader(r, copybookName)
										.setColumns( FIRST_COBOL_COLUMN, END_COLS[cblLineFormat]);
				break;
			case Cb2xmlConstants.USE_SUPPLIED_COLUMNS:
				copyBookReader = BasicReadCobolCopybook.newCopybookReader(r, copybookName)
						.setColumns( firstColumn, lastColumn);
			    break;
			default:
				copyBookReader = BasicReadCobolCopybook.newReaderUsePropertiesFile(r, copybookName);
				break;
			}
		}
		this.copyBookSource = copyBookReader;
		return this;
	}

	public void doAnalysis(
			boolean debug,
			long threadSize) throws LexerException, IOException, ParserException {
		this.debug = debug;

		Reader sr;
		long tSize = threadSize;
		
		sr = copyBookSource.getFreeFormatCopybookReader();
		
		if (tSize < 0) {
			int length = copyBookSource.length();
			if (length > 0) {
				threadSize = calculateThreadSize(length);
				tSize = threadSize;
			} else if (length < 0) {
				tSize = DEFAULT_THREAD_SIZE;
				threadSize = DEFAULT_THREAD_SIZE;
			}
		}

		pbr = new PushbackReader(sr, 4000);
		
		if (tSize < ONE_MEG) {
			//System.out.println("run in normal thread ....");
			this.run();
		} else {
			//System.out.println("thread size: " + threadSize);
			Thread t = new Thread(null, this, name, Math.max(DEFAULT_THREAD_SIZE, threadSize));
			
			t.start();
			
			try {
				t.join();
			} catch (InterruptedException e) {
				throw new RuntimeException(e);
			}
		}
		this.checkExceptions();
	}
	
	public static int calculateThreadSize(int fileLength) {
		return fileLength * 7 + ONE_MEG / 4;
	}

	/* (non-Javadoc)
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public final void run()  {
		try {
			Parser parser;
			Start ast;
			synchronized (SYNC_PARSE_COBOL) {
				if (debug) {
					System.err.println("*** debug mode ***");
					DebugLexer lexer = new DebugLexer(pbr);
					parser = new Parser(lexer);
					try {
						ast = parser.parse(copyBookSource);
					} catch (ParserException pe) {
						StringBuffer buffer = lexer.getBuffer();
						String s = "";
						if (buffer != null) {
							s = buffer.toString();
						}
						throw new DebugParserException(pe, s);
					}
				} else {
					parser = new Parser(new Lexer(pbr));
					ast = parser.parse(copyBookSource);
				}
	
	
				analyseCobolCopybook(parser, ast);
			}
		} catch (DebugParserException e) {
			debugException = e;
		} catch (LexerException e) {
			lexerException = e;
		} catch (IOException e) {
			ioException = e;
		} catch (ParserException e) {
			parseException = e;
		}
	}


	public final void checkExceptions() throws LexerException, IOException, ParserException {
		if (debugException != null) { throw debugException; };
		if (lexerException != null) { throw lexerException; };
		if (ioException != null) { throw ioException; };
		if (parseException != null) { throw parseException; };			
	}

	protected abstract void analyseCobolCopybook(Parser parser, Start ast);
}
