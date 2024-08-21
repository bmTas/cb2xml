/* This file was generated by SableCC (http://www.sablecc.org/). */

package net.sf.cb2xml.sablecc.node;

import net.sf.cb2xml.sablecc.analysis.*;

@SuppressWarnings("nls")
public final class ASingleLiteralSequence extends PLiteralSequence
{
    private TAll _all_;
    private PLiteral _literal_;

    public ASingleLiteralSequence()
    {
        // Constructor
    }

    public ASingleLiteralSequence(
        @SuppressWarnings("hiding") TAll _all_,
        @SuppressWarnings("hiding") PLiteral _literal_)
    {
        // Constructor
        setAll(_all_);

        setLiteral(_literal_);

    }

    @Override
    public Object clone()
    {
        return new ASingleLiteralSequence(
            cloneNode(this._all_),
            cloneNode(this._literal_));
    }

    @Override
    public void apply(Switch sw)
    {
        ((Analysis) sw).caseASingleLiteralSequence(this);
    }

    public TAll getAll()
    {
        return this._all_;
    }

    public void setAll(TAll node)
    {
        if(this._all_ != null)
        {
            this._all_.parent(null);
        }

        if(node != null)
        {
            if(node.parent() != null)
            {
                node.parent().removeChild(node);
            }

            node.parent(this);
        }

        this._all_ = node;
    }

    public PLiteral getLiteral()
    {
        return this._literal_;
    }

    public void setLiteral(PLiteral node)
    {
        if(this._literal_ != null)
        {
            this._literal_.parent(null);
        }

        if(node != null)
        {
            if(node.parent() != null)
            {
                node.parent().removeChild(node);
            }

            node.parent(this);
        }

        this._literal_ = node;
    }

    @Override
    public String toString()
    {
        return ""
            + toString(this._all_)
            + toString(this._literal_);
    }

    @Override
    void removeChild(@SuppressWarnings("unused") Node child)
    {
        // Remove child
        if(this._all_ == child)
        {
            this._all_ = null;
            return;
        }

        if(this._literal_ == child)
        {
            this._literal_ = null;
            return;
        }

        throw new RuntimeException("Not a child.");
    }

    @Override
    void replaceChild(@SuppressWarnings("unused") Node oldChild, @SuppressWarnings("unused") Node newChild)
    {
        // Replace child
        if(this._all_ == oldChild)
        {
            setAll((TAll) newChild);
            return;
        }

        if(this._literal_ == oldChild)
        {
            setLiteral((PLiteral) newChild);
            return;
        }

        throw new RuntimeException("Not a child.");
    }
}