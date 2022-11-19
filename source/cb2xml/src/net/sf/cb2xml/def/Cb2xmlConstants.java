package net.sf.cb2xml.def;

import java.util.HashMap;

public class Cb2xmlConstants {

	public static final int USE_DEFAULT_THREADSIZE = 0;
	public static final int CALCULATE_THREAD_SIZE = -1;

	public static final String STANDARD_FONT = "UTF-8";
	
	public  static final int USE_STANDARD_COLUMNS = 1;
	public  static final int USE_SUPPLIED_COLUMNS = 2;
	public  static final int USE_COLS_6_TO_80     = 3;
	public  static final int USE_LONG_LINE        = 4;
	public  static final int USE_PROPERTIES_FILE  = 8;
	public  static final int FREE_FORMAT          = 9;


	/*
	 * Cb2xml Xml tags, attribute names and values  
	 */
    public static final String ITEM           = "item";
    
       /* Xml Attributes used in cb2xml */
    public static final String COPYBOOK        = "copybook";
    public static final String NAME           = "name";
    public static final String PICTURE        = "picture";
    public static final String NUMERIC        = "numeric";
    public static final String EDITTED_NUMERIC  = "editted-numeric";
    public static final String REDEFINED      = "redefined";
    public static final String REDEFINES      = "redefines";
    public static final String POSITION       = "position";
    public static final String LEVEL          = "level";
    public static final String OCCURS         = "occurs";
    public static final String USAGE          = "usage";
    public static final String JUSTIFIED      = "justified";
    public static final String INHERITED_USAGE= "inherited-usage";
    public static final String DISPLAY_LENGTH = "display-length";
    public static final String DISPLAY_POSITION = "display-position";
    public static final String STORAGE_LENGTH = "storage-length";
    public static final String DOUBLE_BYTE_CHARS = "double-byte-chars";
    public static final String ASSUMED_DIGITS = "assumed-digits";
    public static final String SIGN_SEPARATE  = "sign-separate";
    public static final String SIGN_POSITION  = "sign-position";
    public static final String SIGN_CLAUSE    = "sign-clause";
    public static final String SIGNED         = "signed";
    
    /** Numeric Scaling factor i.e if +ve there is an assumed decimal, -ve ==> assumed 0 (P Picture) i.e. 999ppp **/
    public static final String SCALE          = "scale";           
    public static final String BLANK_WHEN_ZERO= "blank-when-zero";
    public static final String VALUE          = "value";
    public static final String ALL            = "all";
    public static final String THROUGH        = "through";
    public static final String FILENAME       = "filename";
    public static final String DIALECT        = "dialect";
    public static final String CB2XML_FORMAT  = "cb2xml-format";
    public static final String DEPENDING_ON   = "depending-on";
    public static final String OCCURS_MIN     = "occurs-min"; 
    public static final String SYNC           = "sync";
    public static final String INSERT_DECIMAL_POINT = "insert-decimal-point";
    
    public static final String CONDITION      = "condition";
         /* Some Attribute values    */
    public static final String TRUE           = "true";
    public static final String LEADING        = "leading";
    public static final String TRAILING       = "trailing";
    public static final String RIGHT          = "right";
   
    public static final String PACKED_DECIMAL = "packed-decimal";
    public static final String BINARY         = "binary";
    public static final String COMP           = "computational";
    public static final String COMP_1         = "computational-1";
    public static final String COMP_2         = "computational-2";
    public static final String COMP_3         = "computational-3";
    public static final String COMP_4         = "computational-4";
    public static final String COMP_5         = "computational-5"; 
    public static final String COMP_6         = "computational-6";
   
    public static final String DISPLAY        = "display";
    public static final String DISPLAY_1      = "display-1";

    public static final String INDEX          = "index";
    public static final String NATIONAL       = "national";
    public static final String POINTER        = "pointer";
    public static final String PROCEDURAL_POINTER = "procedure-pointer";
    public static final String FUNCTION_POINTER   = "function-pointer";
 
    

    /**
     * Get the string value to be stored in the Xml
     *
     */
    
    public static interface IGetName {
    	public String getName();
    }
    
    
    /**
     * Cobol sign Position
     */
    public static enum SignPosition implements IGetName {
    	/** either non numeric or unsigned or no position specified for the sign */
    	NOT_SPECIFIED(null),
    	/** Sign is at the start of the field */
    	LEADING_SIGN(Cb2xmlConstants.LEADING),
    	/** Sign is at the end of the field */
    	TRAILING_SIGN(Cb2xmlConstants.TRAILING);
   
    	public final String name;
    	
    	SignPosition(String usageName) {
    		name = usageName;
    	}

    	@Override
		public String getName() {
			return name;
		}
    }
    
    /**
     * Cobol sign clause
     */
    public static enum SignClause implements IGetName  {
    	/** No sign clause specified */
    	NO_SIGN_CLAUSE(SignPosition.NOT_SPECIFIED, false),
    	SIGN_SEPARATE(SignPosition.NOT_SPECIFIED, true),
    	/** Sign at the start of the field but included in the first digit as an over-type */
    	SIGN_LEADING(SignPosition.LEADING_SIGN, false),
    	/** Sign at the end of the field but included in the last digit as an over-type */
    	SIGN_TRAILING(SignPosition.TRAILING_SIGN, false),
    	/** Sign at the start of the field and is a separate from the numeric values e.g. -123 */
    	SIGN_LEADING_SEPARATE(SignPosition.LEADING_SIGN, true),
    	/** Sign at the end of the field and is a separate from the numeric values e.g. 123- */
    	SIGN_TRAILING_SEPARATE(SignPosition.TRAILING_SIGN, true),	
    	;
    	
    	public final SignPosition signPosition;
    	public final boolean signSeparate;
    	public final String name;
    	SignClause(SignPosition signPosition, boolean separate) {
    		this.signPosition = signPosition;
    		this.signSeparate = separate;
    		
    		StringBuilder b = new StringBuilder(20);
    		if (signPosition.name != null) {
    			b.append(signPosition.name);
    		}
    		
    		if (separate) {
    			if (b.length() > 0) {
    				b.append('_');
    			}
    			b.append("separate");
    		}
    		name = b.length() == 0 ? null : b.toString();
    	}
    	
    	@Override
		public String getName() {
			return name;
		}
    }
 
    public static enum Cb2xmlXmlFormat {
    	CLASSIC,
    	FORMAT_2017
    }
     
     /**
      * Numeric Status of the field
      */
     public static enum NumericClass implements IGetName  {
     		/** Non numeric field */
     		NON_NUMERIC(null, false, false),
     		/**
     		 * Numeric edited fields (e.g. -,---,--9.99 are not strictly numeric
     		 * in Cobol but are often used to send numeric values to non Cobol
     		 * Systems. So you should consider these fields as numeric
     		 */
     		NUMERIC_EDITED("Numeric_Edited", true, false),
     		/** True Cobol numeric field  */
     		NUMERIC_IN_COBOL("COBOL_NUMERIC", true, true)
     		
     	;
     	public final boolean numeric, numericInCobol, editNumeric;
     	public final String name;
     	
     	NumericClass(String name, boolean numeric, boolean numericInCobol) {
     		this.name = name;
     		this.numeric = numeric;
     		this.numericInCobol = numericInCobol;
     		this.editNumeric = numeric && ! numericInCobol;
     	}

     	@Override
 		public String getName() {
 			return name;
 		}
     }
     
     /**
      * Cobol Justified clause
      */
     public static enum Justified  implements IGetName {
    	NOT_JUSTIFIED(null),
    	JUSTIFIED(Cb2xmlConstants.TRUE),
    	RIGHT(Cb2xmlConstants.RIGHT)
    	;
    	
    	private final String name;
    	public final boolean isJustified;
    	
    	Justified(String usageName) {
    		name = usageName;
    		isJustified = name != null;
    	}

    	@Override
		public String getName() {
			return name;
		}
    }
    
 
    /**
     * Cobol Usage clause
     */
    public static enum Usage  implements IGetName {
    	NONE(null),
    	PACKED_DECIMAL (Cb2xmlConstants.PACKED_DECIMAL),
    	BINARY         (Cb2xmlConstants.BINARY        ),
    	COMP           (Cb2xmlConstants.COMP          ),
    	COMP_1         (Cb2xmlConstants.COMP_1        ),
    	COMP_2         (Cb2xmlConstants.COMP_2        ),     
    	COMP_3         (Cb2xmlConstants.COMP_3        ),
    	COMP_4         (Cb2xmlConstants.COMP_4        ),
    	COMP_5         (Cb2xmlConstants.COMP_5        ),
    	COMP_6         (Cb2xmlConstants.COMP_6        ),
    	               
    	DISPLAY        (Cb2xmlConstants.DISPLAY       ),
    	DISPLAY_1      (Cb2xmlConstants.DISPLAY_1     ),  

    	INDEX              (Cb2xmlConstants.INDEX             ),
    	NATIONAL           (Cb2xmlConstants.NATIONAL          ),
    	POINTER            (Cb2xmlConstants.POINTER           ),
    	PROCEDURAL_POINTER (Cb2xmlConstants.PROCEDURAL_POINTER),
    	FUNCTION_POINTER   (Cb2xmlConstants.FUNCTION_POINTER  ),
    	OBJECT_REFERENCE ("object-reference");    	
    	
    	private final String name;
    	
    	Usage(String usageName) {
    		name = usageName;
    	}

    	@Override
		public String getName() {
			return name;
		}
    }
    
    private static HashMap<String, Usage> usageMap = new HashMap<String, Usage>(30);
    private static HashMap<String, SignClause> signMap = new HashMap<String, SignClause>(15);
   
    /**
     * Convert a String value to Cb2xml <i>usage enum</i> 
     * @param name
     * @return
     */
    public static Usage toUsage(String name) {
    	Usage u = null;
    	if (name != null) { 
    		u = usageMap.get(name.toLowerCase());
    	}
    	return u == null ? Usage.NONE : u;
    }
    
    public static SignClause toSignClause(String name) {
    	SignClause u = null;
    	if (name != null && name.length() > 0) { 
    		u = signMap.get(name.toLowerCase());
    	}
    	return u == null ? SignClause.NO_SIGN_CLAUSE : u;
    }
    
    
    public static Justified toJustified(String name) {
    	Justified just = Justified.NOT_JUSTIFIED;
		
		if (Cb2xmlConstants.Justified.JUSTIFIED.getName().equals(name)) {
			just = Cb2xmlConstants.Justified.JUSTIFIED;
		} else if (Cb2xmlConstants.Justified.RIGHT.getName().equals(name)) {
			just = Cb2xmlConstants.Justified.RIGHT;
		}
		
		return just;
    }
    
    public static NumericClass toNumeric(String name) {
    	NumericClass numeric = NumericClass.NON_NUMERIC;
		
		if (NumericClass.NUMERIC_IN_COBOL.getName().equals(name)) {
			numeric = NumericClass.NUMERIC_IN_COBOL;
		} else if (NumericClass.NUMERIC_EDITED.getName().equals(name)) {
			numeric = NumericClass.NUMERIC_EDITED;
		}
		
		return numeric;
    }

    static {
    	for (Usage u : Usage.values()) {
    		if (u.name != null) {
    			usageMap.put(u.name.toLowerCase(), u);
    		}
    	}
    	for (SignClause sign : SignClause.values()) {
    		if (sign.name != null) {
    			signMap.put(sign.name.toLowerCase(), sign);
    		}
    	}
   }
}
