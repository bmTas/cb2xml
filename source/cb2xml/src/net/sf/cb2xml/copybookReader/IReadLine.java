package net.sf.cb2xml.copybookReader;

import java.io.IOException;

public interface IReadLine {
	String readLine() throws IOException;
	void close() throws IOException;
}
