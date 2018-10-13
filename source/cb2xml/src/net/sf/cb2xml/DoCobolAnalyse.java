package net.sf.cb2xml;

import java.io.IOException;
import java.io.PushbackReader;
import java.io.Reader;
import java.io.StringReader;

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
	private int initialColumn;
	private PushbackReader pbr;
	
	private DebugParserException debugException;
	private LexerException lexerException;
	private IOException ioException;
	private ParserException parseException;
	

	public DoCobolAnalyse(String name) {
		super();
		this.name = name;
	}


	public void doAnalysis(
			boolean debug,
			int cblLineFormat, int firstColumn, int lastColumn, 
			Reader r, long threadSize) throws LexerException, IOException, ParserException {
		this.debug = debug;

		Reader sr;
		long tSize = threadSize;
		String preProcessed = null;
		
		firstColumn = FIRST_COBOL_COLUMN;
		switch (cblLineFormat) {
		case Cb2xmlConstants.FREE_FORMAT:
			sr = r;
			initialColumn = 0;
			if (threadSize < 0) {
				tSize = DEFAULT_THREAD_SIZE;
				threadSize = DEFAULT_THREAD_SIZE;
			}
			break;
		case Cb2xmlConstants.USE_STANDARD_COLUMNS:
		case Cb2xmlConstants.USE_COLS_6_TO_80:
		case Cb2xmlConstants.USE_LONG_LINE:
			preProcessed = CobolPreprocessor.preProcess(r, FIRST_COBOL_COLUMN, END_COLS[cblLineFormat]);
			sr = new StringReader(preProcessed);
			break;
		case Cb2xmlConstants.USE_SUPPLIED_COLUMNS:
			initialColumn = firstColumn;
		    preProcessed = CobolPreprocessor.preProcess(r, firstColumn, lastColumn);
		    sr = new StringReader(preProcessed);
		    break;
		default:
			preProcessed = CobolPreprocessor.preProcess(r);
			sr = new StringReader(preProcessed);
			break;
		}
		
		//System.out.println("tSize .... " + tSize);
		if (preProcessed != null && tSize < 0) {
			tSize = preProcessed.length() * 7;
			threadSize = tSize + ONE_MEG;
		} else if (tSize > 0 ){
			threadSize = tSize;
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
						ast = parser.parse(initialColumn);
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
					ast = parser.parse(initialColumn);
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
