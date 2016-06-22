package net.sf.cb2xml.def;

public class Cb2xmlConstants {
	
	public  static final int USE_STANDARD_COLUMNS = 1;
	public  static final int USE_SUPPLIED_COLUMNS = 2;
	public  static final int USE_COLS_6_TO_80     = 3;
	public  static final int USE_LONG_LINE        = 4;
	public  static final int USE_PROPERTIES_FILE  = 8;
	public  static final int FREE_FORMAT          = 9;


    public static final String ITEM           = "item";
    
       /* Xml Attributes used in cb2xml */
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
    public static final String STORAGE_LENGTH = "storage-length";
    public static final String DOUBLE_BYTE_CHARS = "double-byte-chars";
    public static final String ASSUMED_DIGITS = "assumed-digits";
    public static final String SIGN_SEPARATE  = "sign-separate";
    public static final String SIGN_POSITION  = "sign-position";
    public static final String SIGNED         = "signed";
    
    /** Numeric Scaling factor i.e if +ve there is an assumed decimal, -ve ==> assumed 0 (P Picture) i.e. 999ppp **/
    public static final String SCALE          = "scale";           
    public static final String BLANK_WHEN_ZERO= "blank-when-zero";
    public static final String VALUE          = "value";
    public static final String ALL            = "all";
    public static final String THROUGH        = "through";
    public static final String FILENAME       = "filename";
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

}
