package net.sf.cb2xml;

import net.sf.cb2xml.sablecc.parser.ParserException;

public class DebugParserException extends ParserException {

	public final String buffer;
	
	DebugParserException(ParserException oe, String buffer) {
		super(oe.getToken(), oe.getMessage());

		this.buffer = buffer;
		super.setStackTrace(oe.getStackTrace());
	}

}
