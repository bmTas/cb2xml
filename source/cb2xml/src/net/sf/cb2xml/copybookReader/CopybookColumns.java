package net.sf.cb2xml.copybookReader;

import net.sf.cb2xml.def.Cb2xmlConstants;

/**
 * Cobol Copybook File format.<ul>
 * <li><b>STANDARD_COLUMNS</b> Standard Cobol columns (7 -> 72). 1->6 and/or 73 -> 80 hold line numbers
 * <li><b>COLUMNS_7_TO_80</b> Columns 7 to 80
 * <li><b>LONG_LINE</b> Start at Columns 7 ....
 * <li><b>FREE_FORMAT</b> Free format cobol (Start at column 1
 * </ul>
 * 
 * @author Bruce Martin
 *
 */
public enum CopybookColumns {
	STANDARD_COLUMNS(Cb2xmlConstants.USE_STANDARD_COLUMNS, 6, 72),
	USE_SUPPLIED_COLUMNS(Cb2xmlConstants.USE_SUPPLIED_COLUMNS),
	COLUMNS_7_TO_80(Cb2xmlConstants.USE_COLS_6_TO_80, 6, 80),
	LONG_LINE(Cb2xmlConstants.USE_LONG_LINE, 6, 16000),
	PROPERTIES_FILE(Cb2xmlConstants.USE_PROPERTIES_FILE),
	FREE_FORMAT(Cb2xmlConstants.FREE_FORMAT)
	
	
	;
	public final int columnTypeConstant, firstColumn, lastColumn;
	public final boolean useColumns;

	private CopybookColumns(int columnTypeConstant) {
		this(columnTypeConstant, 0, 0);
	}
	private CopybookColumns(int columnTypeConstant, int firstColumn, int lastColumn) {
		this.columnTypeConstant = columnTypeConstant;
		this.firstColumn = firstColumn;
		this.lastColumn = lastColumn;
		this.useColumns = lastColumn > firstColumn;
	}
	
}
