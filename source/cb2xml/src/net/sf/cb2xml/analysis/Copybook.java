/**
 * 
 */
package net.sf.cb2xml.analysis;

import net.sf.cb2xml.def.ICopybookJrUpd;
import net.sf.cb2xml.def.IItemBase;

/**
 * @author bruce
 *
 */
public class Copybook extends BaseItem implements ICopybookJrUpd {

	final String filename, dialect;
	
	/**
	 * 
	 */
	public Copybook(String filename, String dialect) {
		super(200);
		
		this.filename = filename;
		this.dialect = dialect;
	}

	/* (non-Javadoc)
	 * @see net.sf.cb2xml.def.ICopybook#getFilename()
	 */
	@Override
	public String getFilename() {
		return filename;
	}

	@Override
	public String getDialect() {
		return dialect;
	}

	/* (non-Javadoc)
	 * @see net.sf.cb2xml.def.IItemBase#getParent()
	 */
	@Override
	public IItemBase getParent() {
		return null;
	}

}
