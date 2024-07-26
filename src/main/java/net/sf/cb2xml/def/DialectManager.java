package net.sf.cb2xml.def;

/**
 * Holds Definitions of Supported cobols
 * 
 * @author Bruce Martin
 *
 */
public class DialectManager {
	
	/**
	 * Internal use only
	 */
	private static final NumericDefinition MAINFRAME_NUMERIC_DEFINITION = new BasicNumericDefinition(
			"Mainframe", BasicNumericDefinition.MAINFRAME_SIZES, BasicNumericDefinition.MAINFRAME_SYNC, false, 4, 4, 4
	);
	private static final NumericDefinition MAINFRAME_NUMERIC_DEFINITION_64_BIT = new BasicNumericDefinition(
			"Mainframe_64_Bit", BasicNumericDefinition.MAINFRAME_SIZES, BasicNumericDefinition.MAINFRAME_SYNC, false, 4, 4, 8
	);


	private static final NumericDefinition GNU_COBOL_NUMERIC_DEFINITION = new BasicNumericDefinition(
			"GNU_Cobol", BasicNumericDefinition.GNU_COBOL_SIZES, BasicNumericDefinition.GNU_COBOL_SYNC, false, 4, 8, 4
	);


	private static final NumericDefinition FUJITSU_COBOL_NUMERIC_DEFINITION = new BasicNumericDefinition(
			"Fujitsu", BasicNumericDefinition.MAINFRAME_SIZES, BasicNumericDefinition.MAINFRAME_SYNC, false, 4, 8, 4
	);

	/** Mainframe (IBM in general) Cobol **/
	public static final IBasicDialect MAINFRAME_COBOL = new DialectImp(MAINFRAME_NUMERIC_DEFINITION);
	public static final IBasicDialect MAINFRAME_COBOL_64_BIT = new DialectImp(MAINFRAME_NUMERIC_DEFINITION_64_BIT);
	/** GNU Cobol (https://sourceforge.net/projects/open-cobol/) **/
	public static final IBasicDialect GNU_COBOL = new DialectImp( GNU_COBOL_NUMERIC_DEFINITION);
	/** Fujitsu PC Cobol **/
	public static final IBasicDialect FUJITSU_COBOL = new DialectImp( FUJITSU_COBOL_NUMERIC_DEFINITION);

	private static class DialectImp implements IBasicDialect {

		private final NumericDefinition numericDefinition;
		
		
		
		public DialectImp(NumericDefinition numericDefinition) {
			super();
			this.numericDefinition = numericDefinition;
		}

		@Override
		public NumericDefinition getNumericDefinition() {
			return numericDefinition;
		}

		@Override
		public String toString() {
			return numericDefinition.getName() + "_" + super.toString()
			;
		}
		
		
	}
}
