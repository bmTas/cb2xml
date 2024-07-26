package net.sf.cb2xml.analysis;


public class Positions {
	int storage=1, display=1;
	
	public Positions set(int actual, int display) {
		this.storage = actual;
		this.display = display;
		return this;
	}

	Positions clonePos() {		
		return new Positions().set(storage, display);
	}
	
	void max(Positions p) {
		storage = Math.max(storage, p.storage);
		display = Math.max(display, p.display);
	}
	
	
	void add(Positions p) {
		storage  +=  p.storage;
		display += p.display;
	}


}
