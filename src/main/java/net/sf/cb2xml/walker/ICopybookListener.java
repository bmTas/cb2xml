package net.sf.cb2xml.walker;

import net.sf.cb2xml.def.ICondition;
import net.sf.cb2xml.def.ICopybook;
import net.sf.cb2xml.def.IItem;

public interface ICopybookListener {

    void processComment(String Comment);

    void startCopybook(ICopybook copybook);

    void endCopybook(ICopybook copybook);

    void startCondition(ICondition condition);

    void endCondition(ICondition condition);

    void startItem(IItem item);

    void endItem(IItem item);

}
