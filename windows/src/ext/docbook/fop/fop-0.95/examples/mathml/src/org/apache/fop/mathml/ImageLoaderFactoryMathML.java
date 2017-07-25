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

/* $Id: ImageLoaderFactoryMathML.java 611278 2008-01-11 19:50:53Z jeremias $ */

package org.apache.fop.mathml;

import org.apache.xmlgraphics.image.loader.ImageFlavor;
import org.apache.xmlgraphics.image.loader.impl.AbstractImageLoaderFactory;
import org.apache.xmlgraphics.image.loader.spi.ImageLoader;

/**
 * Factory class for the ImageLoader for MathML.
 */
public class ImageLoaderFactoryMathML extends AbstractImageLoaderFactory {

    /** MathML MIME type */
    public static final String MIME_MATHML = "text/mathml";
    
    private static final ImageFlavor[] FLAVORS = new ImageFlavor[] {
        ImageFlavor.GRAPHICS2D};
    
    private static final String[] MIMES = new String[] {
        MIME_MATHML};
    
    /** {@inheritDoc} */
    public String[] getSupportedMIMETypes() {
        return MIMES;
    }
    
    /** {@inheritDoc} */
    public ImageFlavor[] getSupportedFlavors(String mime) {
        return FLAVORS;
    }
    
    /** {@inheritDoc} */
    public ImageLoader newImageLoader(ImageFlavor targetFlavor) {
        return new ImageLoaderMathML(targetFlavor);
    }
    
    /** {@inheritDoc} */
    public int getUsagePenalty(String mime, ImageFlavor flavor) {
        return 0;
    }

    /** {@inheritDoc} */
    public boolean isAvailable() {
        //TODO Refine!
        return true;
    }

}
