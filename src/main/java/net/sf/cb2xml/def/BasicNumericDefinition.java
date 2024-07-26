package net.sf.cb2xml.def;

/**
 * Basic Definition of Binary Types Sizes for Cobol:
 * 
 * Typical Usage:
 * 
 * BasicNumericDefinition("Mainframe", BasicNumericDefinition.MAINFRAME_SIZES, BasicNumericDefinition.MAINFRAME_SYNC, false, 4, 4)
 * BasicNumericDefinition("Fujitsu", BasicNumericDefinition.FUJITSU_SIZES, BasicNumericDefinition.FUJITSU_SYNC, true, 4, 8)
 * BasicNumericDefinition("GNU Cobol", BasicNumericDefinition.OPEN_COBOL_SIZES, BasicNumericDefinition.OPEN_COBOL_SYNC, true, 4, 8)
 * BasicNumericDefinition("GNU Cobol bs2000", BasicNumericDefinition.BS2000_SIZES, BasicNumericDefinition.BS2000_SYNC, true, 4, 8)
 * BasicNumericDefinition("GNU Cobol MVS", BasicNumericDefinition.OPEN_COBOL_MVS_SIZES, 
 *                                  BasicNumericDefinition.OPEN_COBOL_MVS_SYNC, true, 4, 8)
 * BasicNumericDefinition("GNU Cobol Micro Focus", BasicNumericDefinition.MICROFOCUS_SIZES, 
 *                                   BasicNumericDefinition.MICROFOCUS_SIZES_SYNC, , true, 1, 1)
 *
 * 
 * @author bm
 *
 */
public class BasicNumericDefinition implements NumericDefinition {
	
	public final static int[] DEFAULT_SYNC = {1, 1, 4, 8};
	
	public final static int[]  MAINFRAME_SIZES = {2, 4, 8};
	public final static int[]  MAINFRAME_SYNC = {2, 2, 4, 4}; /* not sure wether the last size is 4 or 8 */
	private final static int[]  MAINFRAME_DIGITS = {4, 9, 18};
	
//	private static int[] tmpSizesUsed = {2, 4, 8};
	
	public final static int[]  FUJITSU_SIZES = MAINFRAME_SIZES;
	public final static int[]  FUJITSU_SYNC = {2, 2, 4, 4};
	
	public final static int[]  GNU_COBOL_SIZES = {1, 2, 4, 8};
	public final static int[]  GNU_COBOL_SYNC = {2, 2, 4, 4};
	
	public final static int[]  BS2000_SIZES = MAINFRAME_SIZES;
	public final static int[]  BS2000_SYNC = {2, 2, 4, 8};

	public final static int[]  GNU_COBOL_MVS_SIZES = MAINFRAME_SIZES;
	public final static int[]  GNU_COBOL_MVS_SYNC = BS2000_SYNC;

	public final static int[]  MICROFOCUS_SIZES = {1, 2, 3, 4, 5, 6, 7, 8};
	public final static int[]  MICROFOCUS_SIZES_SYNC = {1, 1, 1, 1};
	
	public static final int[] MAX_COMP_SIZE = {2, 4, 6, 9, 11, 14, 16, 18};
	public static final int[] MAX_POSITIVE_COMP_SIZE = {2, 4, 7, 9, 12, 14, 16, 19};
	
//	public static final NumericDefinition MAINFRAME_NUMERIC_DEFINITION = new BasicNumericDefinition(
//			"", tmpSizesUsed, MAINFRAME_SYNC, false, 4 ,4
//	);


	// initialise for mainframe
	private int[] compSizesUsed = MAINFRAME_SIZES;
	private int[] digitsAvailable = MAINFRAME_DIGITS;
	private int[] positiveDigitsAvailable = digitsAvailable;
	private int[] syncSizes = DEFAULT_SYNC;
	
	private int floatSync =4;
	private int doubleSync = 8;
	
	private final int pointerSizeInBytes;
	
	private String name;
	
	public BasicNumericDefinition(String encodingName, int[] sizes, int[] syncSize, boolean usePositiveInt, 
			int floatSyncAt,  int doubleSyncAt) {
		this(encodingName, sizes, syncSize, usePositiveInt, floatSyncAt, doubleSyncAt, 4);
	}
	
	public BasicNumericDefinition(String encodingName, int[] sizes, int[] syncSize, boolean usePositiveInt, 
			int floatSyncAt,  int doubleSyncAt, int pointerSizeInBytes) {
		int idx;
		int[] tmpPositive = new int[sizes.length];
		compSizesUsed = new int[sizes.length];
		
		name = encodingName;
		
		syncSizes = DEFAULT_SYNC;
		if (syncSize != null) {
			syncSizes = syncSize; 
		}
		 
		digitsAvailable = new int[sizes.length];
		for (int i = 0; i < sizes.length; i++) {
			 compSizesUsed[i] = sizes[i];
			 idx = Math.min(MAX_COMP_SIZE.length, sizes[i]) - 1;
			 digitsAvailable[i] = MAX_COMP_SIZE[idx];
			 tmpPositive[i] = MAX_POSITIVE_COMP_SIZE[idx];
		}
		
		positiveDigitsAvailable = digitsAvailable;
		if (usePositiveInt) {
			positiveDigitsAvailable = tmpPositive;
		}
		
		floatSync = floatSyncAt;
		doubleSync =doubleSyncAt;
		this.pointerSizeInBytes = pointerSizeInBytes;
	}
	
    public String getName() {
    	return name;
    }

    
	public int getBinarySize(String usage, int numDigits, boolean positive, boolean sync) {
		int storageLength = numDigits;
        if (Cb2xmlConstants.COMP_1.equalsIgnoreCase(usage)) {
        	storageLength = 4;
        } else if (Cb2xmlConstants.COMP_2.equalsIgnoreCase(usage)) {
	        	storageLength = 8;
        } else if (Cb2xmlConstants.COMP_3.equalsIgnoreCase(usage) 
        		|| Cb2xmlConstants.PACKED_DECIMAL.equalsIgnoreCase(usage)
        		|| Cb2xmlConstants.COMP_6.equalsIgnoreCase(usage)) {
            storageLength = (numDigits) / 2 + 1;
        } else if (Cb2xmlConstants.POINTER.equalsIgnoreCase(usage) 
        		|| Cb2xmlConstants.FUNCTION_POINTER.equalsIgnoreCase(usage)
        		|| Cb2xmlConstants.PROCEDURAL_POINTER.equalsIgnoreCase(usage)) {
            storageLength = pointerSizeInBytes;
        } else if (isBinary(usage)) {
        	storageLength = compSizesUsed[compSizesUsed.length - 1];
        	for (int i = 0; i < digitsAvailable.length - 1; i++) {
        		if (digitsAvailable[i] >= numDigits
        		|| (positive && positiveDigitsAvailable[i] >= numDigits)) {
        			storageLength = compSizesUsed[Math.max(0, i)];
        			break;
        		} 
        	}
        	
        }

        return storageLength;
	}
	
	public int getSyncAt(String usage, int actualLength) {
		int syncOn = 1;
		if  (isBinary(usage)) {
			switch (actualLength) {
				case (1) : syncOn = syncSizes[0];	break;
				case (2) : syncOn = syncSizes[1];	break;
				case (3):  
				case (4) : syncOn = syncSizes[2];	break;
				default : syncOn = syncSizes[3];					
			}
		} else if (Cb2xmlConstants.COMP_1.equals(usage)) {
			syncOn =floatSync;
		} else if (Cb2xmlConstants.COMP_2.equals(usage)) {
			syncOn =doubleSync;
		}
		return syncOn;
	}
	
	
	public int chkStorageLength(int storageLength, String usage) {
		int ret = storageLength;
		if (storageLength <= 0) {
			if  (Cb2xmlConstants.COMP_1.equalsIgnoreCase(usage)) {
				ret = 10;
			} else if  (Cb2xmlConstants.COMP_2.equalsIgnoreCase(usage)) {
				ret = 18;
			}
		}
		
		return ret;
	}
	
	private boolean isBinary(String usage) {
		return Cb2xmlConstants.BINARY.equalsIgnoreCase(usage) ||Cb2xmlConstants.COMP.equalsIgnoreCase(usage) 
			|| Cb2xmlConstants.COMP_4.equalsIgnoreCase(usage) || Cb2xmlConstants.COMP_5.equalsIgnoreCase(usage)
			|| Cb2xmlConstants.COMP_6.equalsIgnoreCase(usage);
	}
	

}
