/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/* $Id: MathMLElement.java 627367 2008-02-13 12:03:30Z maxberger $ */
 
package org.apache.fop.mathml;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.geom.Point2D;

import net.sourceforge.jeuclid.DOMMathBuilder;
import net.sourceforge.jeuclid.MathBase;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;

import org.apache.batik.dom.svg.SVGDOMImplementation;
import org.apache.batik.svggen.SVGGraphics2D;

import org.apache.fop.apps.FOPException;
import org.apache.fop.fo.FONode;
import org.apache.fop.fo.PropertyList;

/**
 * Defines the top-level element for MathML.
 */
public class MathMLElement extends MathMLObj {

    private Document svgDoc = null;
    private float width;
    private float height;
    private boolean converted = false;

    /**
     * @see org.apache.fop.fo.FONode#FONode(FONode)
     */
    public MathMLElement(FONode parent) {
        super(parent);
    }

    /** {@inheritDoc} */
    public void processNode(String elementName, 
                            Locator locator, 
                            Attributes attlist, 
                            PropertyList propertyList) throws FOPException {
        super.processNode(elementName, locator, attlist, propertyList);
        createBasicDocument();
    }

    /**
     * Converts the MathML to SVG.
     */
    public void convertToSVG() {
        try {
            if (!converted) {
                converted = true;
                String fontname = "Helvetica";
                int fontstyle = 0;
                //int inlinefontstyle = 0;
                int displayfontsize = 12;
                int inlinefontsize = 12;

                MathBase base = new MathBase(
                                  (new DOMMathBuilder(doc)).getMathRootElement(),
                                  fontname, fontstyle, inlinefontsize,
                                  displayfontsize);

                base.setDebug(false);

                svgDoc = createSVG(base);

                width = base.getWidth();
                height = base.getHeight();

                doc = svgDoc;
            }
        } catch (Throwable t) {
            getLogger().error("Could not convert MathML to SVG", t);
            width = 0;
            height = 0;
        }

    }

    /**
     * Create the SVG from MathML.
     * @param base the root element
     * @return the DOM document containing SVG
     */
    public static Document createSVG(MathBase base) {

        DOMImplementation impl = SVGDOMImplementation.getDOMImplementation();
        String svgNS = SVGDOMImplementation.SVG_NAMESPACE_URI;
        Document svgdocument = impl.createDocument(svgNS, "svg", null);

        SVGGraphics2D g = new SVGGraphics2D(svgdocument);

        g.setSVGCanvasSize(
          new Dimension(base.getWidth(), base.getHeight()));

        //g.setColor(Color.white);
        //g.fillRect(0, 0, base.getWidth(), base.getHeight());
        g.setColor(Color.black);

        base.paint(g);

        //if (antialiasing)
        //element.setAttribute("text-rendering", "optimizeLegibility");
        //else
        //element.setAttribute("text-rendering", "geometricPrecision");

        // this should be done in a better way
        Element root = g.getRoot();
        svgdocument = impl.createDocument(svgNS, "svg", null);
        Node node = svgdocument.importNode(root, true);
        ((org.apache.batik.dom.svg.SVGOMDocument) svgdocument).
        getRootElement().appendChild(node);

        return svgdocument;

    }

    /** {@inheritDoc} */
    public Document getDOMDocument() {
        convertToSVG();
        return doc;
    }

    /** {@inheritDoc} */
    public String getNamespaceURI() {
        if (svgDoc == null) {
            return MathMLElementMapping.NAMESPACE;
        }
        return "http://www.w3.org/2000/svg";
    }

    /** {@inheritDoc} */
    public Point2D getDimension(Point2D view) {
        convertToSVG();
        return new Point2D.Float(width, height);
    }
}

