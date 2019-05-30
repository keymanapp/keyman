package com.tavultesoft.kmea.data.adapters;

/**
 * Used by NestedAdapters to selectively filter which elements are selected from the original Adapter.
 * @param <Element> The type of both Adapters' elements.
 */
public interface AdapterFilter<Element> {
  /**
   * Determines whether or not the given Element matches the filter.
   * @param elem
   * @return <code>true</code> if the Element matches the filter, else <code>false</code>
   */
  boolean matches(Element elem);
}
