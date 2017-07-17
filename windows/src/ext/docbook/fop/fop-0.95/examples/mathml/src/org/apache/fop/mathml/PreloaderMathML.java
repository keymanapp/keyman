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

/* $Id: PreloaderMathML.java 611278 2008-01-11 19:50:53Z jeremias $ */
 
package org.apache.fop.mathml;

import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.stream.StreamSource;

import net.sourceforge.jeuclid.MathBase;
import net.sourceforge.jeuclid.SAXMathBuilder;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import org.apache.xmlgraphics.image.loader.Image;
import org.apache.xmlgraphics.image.loader.ImageContext;
import org.apache.xmlgraphics.image.loader.ImageInfo;
import org.apache.xmlgraphics.image.loader.ImageSize;
import org.apache.xmlgraphics.image.loader.impl.AbstractImagePreloader;
import org.apache.xmlgraphics.image.loader.impl.ImageGraphics2D;
import org.apache.xmlgraphics.image.loader.util.ImageUtil;
import org.apache.xmlgraphics.java2d.Graphics2DImagePainter;

import org.apache.fop.util.UnclosableInputStream;

/**
 * Image preloader for MathML images.
 */
public class PreloaderMathML extends AbstractImagePreloader {

    /** Logger instance */
    private static Log log = LogFactory.getLog(PreloaderMathML.class);

    private boolean jeuclidAvailable = true;
    
    /** {@inheritDoc} */ 
    public ImageInfo preloadImage(String uri, Source src, ImageContext context)
            throws IOException {
        if (!ImageUtil.hasInputStream(src)) {
            //TODO Remove this and support DOMSource and possibly SAXSource
            return null;
        }
        ImageInfo info = null;
        if (jeuclidAvailable) {
            try {
                Loader loader = new Loader();
                info = loader.getImage(uri, src, context);
            } catch (NoClassDefFoundError e) {
                jeuclidAvailable = false;
                log.warn("JEuclid not in class path", e);
                return null;
            }
        }
        if (info != null) {
            ImageUtil.closeQuietly(src); //Image is fully read
        }
        return info;
    }

    /**
     * This method is put in another class so that the class loader does not
     * attempt to load JEuclid related classes when constructing the MathMLPreloader
     * class.
     */
    class Loader {
        
        private ImageInfo getImage(String uri, Source src, ImageContext context) {

            InputStream in = new UnclosableInputStream(ImageUtil.needInputStream(src));
            try {
                int length = in.available();
                in.mark(length + 1);
                
                TransformerFactory tFactory = TransformerFactory.newInstance();
                Transformer transformer = tFactory.newTransformer();
                Source source = new StreamSource(in);
                SAXMathBuilder mathBuilder = new SAXMathBuilder();
                SAXResult res = new SAXResult(mathBuilder);
                transformer.transform(source, res);
                
                String fontname = "Helvetica";
                int fontstyle = 0;
                int displayfontsize = 12;
                int inlinefontsize = 12;

                if (mathBuilder.getMathRootElement() == null) {
                    //not a MathML document
                    try {
                        in.reset();
                    } catch (IOException ioe) {
                        log.error("Error while resetting ImageInputStream", ioe);
                    }
                    return null;
                }
                final MathBase base = new MathBase(
                                  mathBuilder.getMathRootElement(),
                                  fontname, fontstyle, inlinefontsize,
                                  displayfontsize);
                
                ImageInfo info = new ImageInfo(uri, "text/mathml");
                final ImageSize size = new ImageSize();
                size.setSizeInMillipoints(
                        Math.round(base.getWidth() * 1000),
                        Math.round(base.getHeight() * 1000));
                //Set the resolution to that of the FOUserAgent
                size.setResolution(context.getSourceResolution());
                size.calcPixelsFromSize();
                info.setSize(size);

                Graphics2DImagePainter painter = new Graphics2DImagePainter() {

                    public Dimension getImageSize() {
                        return size.getDimensionMpt();
                    }

                    public void paint(Graphics2D g2d, Rectangle2D area) {
                        base.paint(g2d);
                    }
                    
                };
                
                //The whole image had to be loaded for this, so keep it
                Image image = new ImageGraphics2D(info, painter);
                info.getCustomObjects().put(ImageInfo.ORIGINAL_IMAGE, image);
                
                return info;
            } catch (NoClassDefFoundError ncdfe) {
                try {
                    in.reset();
                } catch (IOException ioe) {
                    // we're more interested in the original exception
                }
                jeuclidAvailable = false;
                log.warn("JEuclid not in class path", ncdfe);
                return null;
            } catch (IOException e) {
                // If the MathML is invalid then it throws an IOException
                // so there is no way of knowing if it is an svg document

                log.debug("Error while trying to load stream as an MathML file: "
                                       + e.getMessage());
                // assuming any exception means this document is not svg
                // or could not be loaded for some reason
                try {
                    in.reset();
                } catch (IOException ioe) {
                    // we're more interested in the original exception
                }
                return null;
            } catch (TransformerException e) {
                try {
                    in.reset();
                } catch (IOException ioe) {
                    // we're more interested in the original exception
                }
                log.debug("Error while trying to parsing a MathML file: "
                        + e.getMessage());
                return null;
            }
        }
    }

}
