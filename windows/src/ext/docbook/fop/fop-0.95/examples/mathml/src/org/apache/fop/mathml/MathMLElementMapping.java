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

/* $Id: MathMLElementMapping.java 627367 2008-02-13 12:03:30Z maxberger $ */
 
package org.apache.fop.mathml;

import java.util.HashMap;

import org.w3c.dom.DOMImplementation;

import org.apache.fop.fo.ElementMapping;
import org.apache.fop.fo.FONode;

/**
 * This class provides the element mapping for FOP.
 */
public class MathMLElementMapping extends ElementMapping {

    /** MathML Namespace */
    public static final String NAMESPACE = "http://www.w3.org/1998/Math/MathML"; 

    /** Main constructor. */
    public MathMLElementMapping() {
        this.namespaceURI = NAMESPACE;
    }

    /** {@inheritDoc} */
    public DOMImplementation getDOMImplementation() {
        return getDefaultDOMImplementation();
    }

    /** {@inheritDoc} */
    protected void initialize() {
        if (foObjs == null) {
            foObjs = new HashMap();
            foObjs.put("math", new ME());
            foObjs.put(DEFAULT, new MathMLMaker());
        }
    }

    static class MathMLMaker extends ElementMapping.Maker {
        public FONode make(FONode parent) {
            return new MathMLObj(parent);
        }
    }

    static class ME extends ElementMapping.Maker {
        public FONode make(FONode parent) {
            return new MathMLElement(parent);
        }
    }

}
